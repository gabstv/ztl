const std = @import("std");
const lib = @import("lib.zig");

const Token = lib.Token;
const Scanner = lib.Scanner;
const Position = lib.Position;
const ByteCode = lib.ByteCode;
const Allocator = std.mem.Allocator;

const MAX_ERRORS = 20;

const Compiler = @This();

// the ByteCode that our compiler is generating
_byte_code: ByteCode,

// Arena for memory that can be discarded after compilation. This arena, and
// its allocator, are NOT used for anything to do with byte code generation.
// Their main goal is for generating errors.
_arena: Allocator,

_errors: [MAX_ERRORS]Error = undefined,

// number of errors in _errors
_error_len: usize = 0,

// will be set when compile() is called
_scanner: Scanner = undefined,

_current_token: Token,
_previous_token: Token,

pub fn init(allocator: Allocator) !Compiler {
    const arena = try allocator.create(std.heap.ArenaAllocator);
    errdefer allocator.destroy(arena);

    arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    return .{
        ._arena = arena.allocator(),
        ._byte_code = try ByteCode.init(arena.allocator()),
        ._current_token = .{.value = .{.START = {}}, .position = Position.ZERO},
        ._previous_token = .{.value = .{.START = {}}, .position = Position.ZERO},
    };
}

pub fn deinit(self: *const Compiler) void {
    const arena: *std.heap.ArenaAllocator = @ptrCast(@alignCast(self._arena.ptr));
    arena.deinit();
    arena.child_allocator.destroy(arena);
}

pub fn errors(self: *const Compiler) []const Error {
    return self._errors[0..self._error_len];
}

pub fn compile(self: *Compiler, src: []const u8) !void{
    self._scanner = Scanner.init(self, src);
    try self.advance();
    try self.expression();
    try self.consume(.EOF, "Expected of of expression");

    const byte_code = &self._byte_code;
    try byte_code.addOp(.RETURN);
    if (self._error_len > 0) {
        return error.CompilationFailed;
    }
}

pub fn byteCode(self: *const Compiler, allocator: Allocator) ![]const u8 {
    const code = self._byte_code.code;
    const data = self._byte_code.data;

    const buf = try allocator.alloc(u8, 4 + code.pos + data.pos);

    const data_start = 4 + code.pos;
    const code_len: u32 = @intCast(code.pos);

    @memcpy(buf[0..4], std.mem.asBytes(&code_len));
    @memcpy(buf[4..data_start], code.buf[0..code.pos]);
    @memcpy(buf[data_start..], data.buf[0..data.pos]);

    return buf;
}

fn advance(self: *Compiler) !void {
    self._previous_token = self._current_token;
    self._current_token = try self._scanner.next();
}

fn consume(self: *Compiler, expected: Token.Type, comptime message: []const u8) !void {
    if (self._current_token.value != expected) {
        return self.addErrorFmt(error.UnexpectedToken, message ++ ", got '{s}'", .{@tagName(expected)}, null);
    }
    return self.advance();
}

fn expression(self: *Compiler) !void {
    return self.parsePrecedence(.ASSIGNMENT);
}

fn grouping(self: *Compiler) !void {
    try self.expression();
    return self.consume(.RIGHT_PARENTHESIS, "Expected closing parathesis ')'");
}

fn unary(self: *Compiler) !void {
    const previous = self._previous_token.value;

    try self.expression();
    switch (previous) {
        .BANG => try self._byte_code.addOp(.NOT),
        .MINUS => try self._byte_code.addOp(.NEGATE),
        else => unreachable,
    }
}

fn binary(self: *Compiler) !void {
    const previous = self._previous_token.value;
    const rule = getRule(previous);
    try self.parsePrecedence(@enumFromInt(rule.precedence + 1));

    switch (previous) {
        .PLUS => try self._byte_code.addOp(.ADD),
        .MINUS => try self._byte_code.addOp(.SUBTRACT),
        .STAR => try self._byte_code.addOp(.MULTIPLY),
        .SLASH => try self._byte_code.addOp(.DIVIDE),
        .EQUAL_EQUAL => try self._byte_code.addOp(.EQUAL),
        .BANG_EQUAL => try self._byte_code.addOp2(.EQUAL, .NOT),
        .GREATER => try self._byte_code.addOp(.GREATER),
        .GREATER_EQUAL => try self._byte_code.addOp2(.LESSER, .NOT),
        .LESSER => try self._byte_code.addOp(.LESSER),
        .LESSER_EQUAL => try self._byte_code.addOp2(.GREATER, .NOT),
        else => unreachable,
    }
}

fn literal(self: *Compiler, token: Token) !void {
    switch (token) {
        .INTEGER => |value| try self._byte_code.addI64(value),
        .FLOAT => |value| try self._byte_code.addF64(value),
        .BOOL => |value| try self._byte_code.addBool(value),
        .NULL => try self._byte_code.addNull(),
        .STRING => unreachable,
    }
}

fn number(self: *Compiler) !void {
    switch (self._previous_token.value) {
        .INTEGER => |value| try self._byte_code.addI64(value),
        .FLOAT => |value| try self._byte_code.addF64(value),
        else => unreachable,
    }
}

fn boolean(self: *Compiler) !void {
    return self._byte_code.addBool(self._previous_token.value.BOOLEAN);
}

fn string(self: *Compiler) !void {
    return self._byte_code.addString(self._previous_token.value.STRING);
}

fn @"null"(self: *Compiler) !void {
    return self._byte_code.addNull();
}

fn parsePrecedence(self: *Compiler, precedence: Precedence) !void {
    try self.advance();
    {
        const rule = getRule(self._previous_token.value);
        if (rule.prefix) |prefix| {
            try prefix(self);
        } else {
            return self.addErrorFmt(error.ExpressionExpected, "Expected an expression for token of type '{s}'", .{@tagName(self._previous_token.value)}, null);
        }
    }

    const nprec = @intFromEnum(precedence);
    while (true) {
        const rule = getRule(self._current_token.value);
        if (nprec > rule.precedence) {
            break;
        }
        try self.advance();
        try rule.infix.?(self);
    }
}

pub fn addErrorFmt(self: *Compiler, err: anyerror, comptime fmt: []const u8, args: anytype, position: ?Position) !void {
    const error_index = self._error_len;
    if (error_index == MAX_ERRORS) {
        return error.TooManyErrors;
    }
    const desc = try std.fmt.allocPrint(self._arena, fmt, args);
    return self.addError(err, desc, position);
}

pub fn addError(self: *Compiler, err: anyerror, desc: []const u8, position: ?Position) !void {
    const len = self._error_len;
    if (len == MAX_ERRORS) {
        return error.TooManyErrors;
    }

    self._errors[len] = Error{
        .err = err,
        .desc = desc,
        .position = position orelse self._scanner.position(null),
    };

    self._error_len = len + 1;
}

fn invalidToken(self: *Compiler, err: anyerror, comptime desc: []const u8, token: Token) !void {
    return self.addErrorFmt(err, desc ++ ", got: '{s}'", .{self._scanner.srcAt(token.position)}, null);
}

const Precedence = enum {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,
};

inline fn getRule(token_type: Token.Type) *const ParseRule {
    return &rules[@intFromEnum(token_type)];
}

const ParseRule = struct {
    infix: ?Fn,
    prefix: ?Fn,
    precedence: i32,

    const Fn = *const fn(*Compiler) anyerror!void;
};

const rules = buildParseRules(&.{
    .{Token.Type.AND, null, null, Precedence.NONE},
    .{Token.Type.BANG, null, Compiler.unary, Precedence.NONE},
    .{Token.Type.BANG_EQUAL, Compiler.binary, null, Precedence.EQUALITY},
    .{Token.Type.BOOLEAN, null, Compiler.boolean, Precedence.NONE},
    .{Token.Type.COMMA, null, null, Precedence.NONE},
    .{Token.Type.DOT, null, null, Precedence.NONE},
    .{Token.Type.ELSE, null, null, Precedence.NONE},
    .{Token.Type.EOF, null, null, Precedence.NONE},
    .{Token.Type.EQUAL, null, null, Precedence.NONE},
    .{Token.Type.EQUAL_EQUAL, Compiler.binary, null, Precedence.EQUALITY},
    .{Token.Type.FLOAT, null, Compiler.number, Precedence.NONE},
    .{Token.Type.GREATER, Compiler.binary, null, Precedence.COMPARISON},
    .{Token.Type.GREATER_EQUAL, Compiler.binary, null, Precedence.COMPARISON},
    .{Token.Type.IDENTIFIER, null, null, Precedence.NONE},
    .{Token.Type.IF, null, null, Precedence.NONE},
    .{Token.Type.INTEGER, null, Compiler.number, Precedence.NONE},
    .{Token.Type.LEFT_BRACE, null, null, Precedence.NONE},
    .{Token.Type.LEFT_PARENTHESIS, null, Compiler.grouping, Precedence.NONE},
    .{Token.Type.LESSER, Compiler.binary, null, Precedence.COMPARISON},
    .{Token.Type.LESSER_EQUAL, Compiler.binary, null, Precedence.COMPARISON},
    .{Token.Type.MINUS, Compiler.binary, Compiler.unary, Precedence.TERM},
    .{Token.Type.NULL, null, Compiler.@"null", Precedence.NONE},
    .{Token.Type.OR, null, null, Precedence.NONE},
    .{Token.Type.PLUS, Compiler.binary, null, Precedence.TERM},
    .{Token.Type.RETURN, null, null, Precedence.NONE},
    .{Token.Type.RIGHT_BRACE, null, null, Precedence.NONE},
    .{Token.Type.RIGHT_PARENTHESIS, null, null, Precedence.NONE},
    .{Token.Type.SEMICOLON, null, null, Precedence.NONE},
    .{Token.Type.SLASH, Compiler.binary, null, Precedence.FACTOR},
    .{Token.Type.STAR, Compiler.binary, null, Precedence.FACTOR},
    .{Token.Type.STRING, null, Compiler.string, Precedence.NONE},
    // .{Token.Type.VAR, null, null, Precedence.NONE},
    // .{Token.Type.WHILE, null, null, Precedence.NONE},
    // .{Token.Type.CLASS, null, null, Precedence.NONE},
    // .{Token.Type.ERROR, null, null, Precedence.NONE},
    // .{Token.Type.FOR, null, null, Precedence.NONE},
    // .{Token.Type.FUN, null, null, Precedence.NONE},
    // .{Token.Type.PRINT, null, null, Precedence.NONE},
    // .{Token.Type.SUPER, null, null, Precedence.NONE},
    // .{Token.Type.THIS, null, null, Precedence.NONE},
});

fn buildParseRules(definitions: anytype) [maxRuleIndex(Token.Type)]ParseRule {
    var _rules: [maxRuleIndex(Token.Type)]ParseRule = undefined;
    for (&_rules) |*r| {
        r.* = .{
            .infix = null,
            .prefix = null,
            .precedence = 0,
        };
    }
    for (definitions) |definition| {
        const index = @intFromEnum(definition.@"0");
        _rules[index] = .{
            .infix = definition.@"1",
            .prefix = definition.@"2",
            .precedence = @intFromEnum(definition.@"3"),
        };
    }
    return _rules;
}

fn maxRuleIndex(comptime E: type) usize {
    var max: usize = 0;
    for (@typeInfo(E).@"enum".fields) |f| {
        max = @max(max, f.value);
    }
    return max + 1;
}

pub const Error = struct {
    err: anyerror,
    desc: []const u8,
    position: Position,
};

