const std = @import("std");
const lib = @import("lib.zig");

const Token = lib.Token;
const Scanner = lib.Scanner;
const Position = lib.Position;
const ByteCode = lib.ByteCode;
const Allocator = std.mem.Allocator;

const Compiler = @This();

// the ByteCode that our compiler is generating
_byte_code: ByteCode,

// Arena for memory that can be discarded after compilation. This arena, and
// its allocator, are NOT used for anything to do with byte code generation.
// Their main goal is for generating errors.
_arena: Allocator,

err: ?Error = null,

// will be set when compile() is called
_scanner: Scanner = undefined,

// we just need to keep track of our current token and the
// previous token to successfully parsed.
_current_token: Token,
_previous_token: Token,

// used to track local variables
_locals: []Local,
_scope_depth: usize = 0,
_local_count: usize = 0,

pub fn init(allocator: Allocator) !Compiler {
    const arena = try allocator.create(std.heap.ArenaAllocator);
    errdefer allocator.destroy(arena);

    arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    const aa = arena.allocator();

    return .{
        ._arena = aa,
        ._locals = try aa.alloc(Local, 10),
        ._byte_code = try ByteCode.init(aa),
        ._current_token = .{ .value = .{ .START = {} }, .position = Position.ZERO },
        ._previous_token = .{ .value = .{ .START = {} }, .position = Position.ZERO },
    };
}

pub fn deinit(self: *const Compiler) void {
    const arena: *std.heap.ArenaAllocator = @ptrCast(@alignCast(self._arena.ptr));
    arena.deinit();
    arena.child_allocator.destroy(arena);
}

pub fn byteCode(self: *const Compiler, allocator: Allocator) ![]const u8 {
    return self._byte_code.toBytes(allocator);
}

pub fn compile(self: *Compiler, src: []const u8) CompileError!void {
    self._scanner = Scanner.init(self, src);

    try self.advance();

    while (try self.match(.EOF) == false) {
        try self.declaration();
    }
}

fn advance(self: *Compiler) !void {
    self._previous_token = self._current_token;
    self._current_token = try self._scanner.next();
}

fn consume(self: *Compiler, expected: Token.Type, comptime message: []const u8) !void {
    if (self._current_token.value != expected) {
        return self.setErrorFmt(error.UnexpectedToken, message ++ ", got '{s}'", .{@tagName(self._current_token.value)}, null);
    }
    return self.advance();
}

fn match(self: *Compiler, expected: Token.Type) !bool {
    if (self._current_token.value != expected) {
        return false;
    }
    try self.advance();
    return true;
}

fn declaration(self: *Compiler) CompileError!void {
    switch (self._current_token.value) {
        .VAR => {
            try self.advance();
            try self.consume(.IDENTIFIER, "Expected variable name");

            const variable_name = self._previous_token.value.IDENTIFIER;
            // TODO: check self._locals for a variable with the same name
            // in the same scope. Start at _locals[_local_count - 1] and work
            // backwards, until we find a conflict, or the scope chang
            const lc = self._local_count;
            self._locals[lc] = .{
                .name = variable_name,
                .depth = self._scope_depth,
            };
            self._local_count = lc + 1;

            try self.consume(.EQUAL, "Expected assignment operator ('=')");
            try self.expression();
            try self.consume(.SEMICOLON, "Expected ';'");
        },
        else => return self.statement(),
    }
}

fn statement(self: *Compiler) CompileError!void {
    switch (self._current_token.value) {
        .PRINT => {
            try self.advance();
            try self.expression();
            try self.consume(.SEMICOLON, "Expected ';'");
            try self._byte_code.op(.PRINT);
        },
        .RETURN => {
            try self.advance();
            if (try self.match(.SEMICOLON)) {
                try self._byte_code.op(.RETURN);
            } else {
                try self.expression();
                try self.consume(.SEMICOLON, "Expected ';'");
                try self._byte_code.op(.RETURN);
            }
        },
        .LEFT_BRACE => {
            try self.advance();
            const scope = self._scope_depth;
            self._scope_depth = scope + 1;
            try self.block();
            self._scope_depth = scope;

            // pop up any locals from this scope
            const locals = self._locals;
            var i = self._local_count - 1;
            while (i >= 0) : (i -= 1) {
                if (locals[i].depth > scope) {
                    try self._byte_code.op(.POP);
                } else {
                    self._local_count = i + 1;
                    break;
                }
            } else {
                self._local_count = 0;
            }
        },
        else => {
            try self.expression();
            try self.consume(.SEMICOLON, "Expected ';'");
            try self._byte_code.op(.POP);
        },
    }
}

fn block(self: *Compiler) CompileError!void {
    while (true) {
        switch (self._current_token.value) {
            .RIGHT_BRACE => return self.advance(),
            .EOF => return self.setError(error.UnexpectedEOF, "Expected closing block ('}')", null),
            else => try self.declaration(),
        }
    }
}

fn expression(self: *Compiler) CompileError!void {
    return self.parsePrecedence(.ASSIGNMENT);
}

fn parsePrecedence(self: *Compiler, precedence: Precedence) CompileError!void {
    try self.advance();
    {
        const rule = getRule(self._previous_token.value);
        if (rule.prefix) |prefix| {
            const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
            try prefix(self, can_assign);
        } else {
            return self.setErrorFmt(error.ExpressionExpected, "Expected an expression for token of type '{s}'", .{@tagName(self._previous_token.value)}, null);
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

fn grouping(self: *Compiler, _: bool) CompileError!void {
    try self.expression();
    return self.consume(.RIGHT_PARENTHESIS, "Expected closing parathesis ')'");
}

fn binary(self: *Compiler) CompileError!void {
    const previous = self._previous_token.value;
    const rule = getRule(previous);
    try self.parsePrecedence(@enumFromInt(rule.precedence + 1));

    switch (previous) {
        .PLUS => try self._byte_code.op(.ADD),
        .MINUS => try self._byte_code.op(.SUBTRACT),
        .STAR => try self._byte_code.op(.MULTIPLY),
        .SLASH => try self._byte_code.op(.DIVIDE),
        .EQUAL_EQUAL => try self._byte_code.op(.EQUAL),
        .BANG_EQUAL => try self._byte_code.op2(.EQUAL, .NOT),
        .GREATER => try self._byte_code.op(.GREATER),
        .GREATER_EQUAL => try self._byte_code.op2(.LESSER, .NOT),
        .LESSER => try self._byte_code.op(.LESSER),
        .LESSER_EQUAL => try self._byte_code.op2(.GREATER, .NOT),
        else => unreachable,
    }
}

fn unary(self: *Compiler, _: bool) CompileError!void {
    const previous = self._previous_token.value;

    try self.expression();
    switch (previous) {
        .BANG => try self._byte_code.op(.NOT),
        .MINUS => try self._byte_code.op(.NEGATE),
        else => unreachable,
    }
}

fn number(self: *Compiler, _: bool) CompileError!void {
    switch (self._previous_token.value) {
        .INTEGER => |value| try self._byte_code.i64(value),
        .FLOAT => |value| try self._byte_code.f64(value),
        else => unreachable,
    }
}

fn boolean(self: *Compiler, _: bool) CompileError!void {
    return self._byte_code.bool(self._previous_token.value.BOOLEAN);
}

fn string(self: *Compiler, _: bool) CompileError!void {
    return self._byte_code.string(self._previous_token.value.STRING);
}

fn @"null"(self: *Compiler, _: bool) CompileError!void {
    return self._byte_code.null();
}

fn variable(self: *Compiler, can_assign: bool) CompileError!void {
    const name = self._previous_token.value.IDENTIFIER;

    const locals = self._locals;
    var idx = self._local_count - 1;
    while (idx >= 0) : (idx -= 1) {
        if (std.mem.eql(u8, name, locals[idx].name)) {
            break;
        }
    } else {
        return setErrorFmt(error.UndefinedVariable, "Undefined variable: '{s}'", .{name}, .{});
    }

    if (can_assign and try self.match(.EQUAL)) {
        try self.expression();
        try self._byte_code.setLocal(@intCast(idx));
    } else {
        try self._byte_code.getLocal(@intCast(idx));
    }
}

pub fn setErrorFmt(self: *Compiler, err: CompileError, comptime fmt: []const u8, args: anytype, position: ?Position) CompileError!void {
    const desc = try std.fmt.allocPrint(self._arena, fmt, args);
    return self.setError(err, desc, position);
}

pub fn setError(self: *Compiler, err: CompileError, desc: []const u8, position: ?Position) CompileError!void {
    self.err = .{
        .err = err,
        .desc = desc,
        .position = position orelse self._scanner.position(null),
    };
    return err;
}

fn invalidToken(self: *Compiler, err: anyerror, comptime desc: []const u8, token: Token) !void {
    return self.setErrorFmt(err, desc ++ ", got: '{s}'", .{self._scanner.srcAt(token.position)}, null);
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
    infix: ?*const fn (*Compiler) CompileError!void,
    prefix: ?*const fn (*Compiler, bool) CompileError!void,
    precedence: i32,
};

const rules = buildParseRules(&.{
    .{ Token.Type.AND, null, null, Precedence.NONE },
    .{ Token.Type.BANG, null, Compiler.unary, Precedence.NONE },
    .{ Token.Type.BANG_EQUAL, Compiler.binary, null, Precedence.EQUALITY },
    .{ Token.Type.BOOLEAN, null, Compiler.boolean, Precedence.NONE },
    .{ Token.Type.COMMA, null, null, Precedence.NONE },
    .{ Token.Type.DOT, null, null, Precedence.NONE },
    .{ Token.Type.ELSE, null, null, Precedence.NONE },
    .{ Token.Type.EOF, null, null, Precedence.NONE },
    .{ Token.Type.EQUAL, null, null, Precedence.NONE },
    .{ Token.Type.EQUAL_EQUAL, Compiler.binary, null, Precedence.EQUALITY },
    .{ Token.Type.FLOAT, null, Compiler.number, Precedence.NONE },
    .{ Token.Type.GREATER, Compiler.binary, null, Precedence.COMPARISON },
    .{ Token.Type.GREATER_EQUAL, Compiler.binary, null, Precedence.COMPARISON },
    .{ Token.Type.IDENTIFIER, null, Compiler.variable, Precedence.NONE },
    .{ Token.Type.IF, null, null, Precedence.NONE },
    .{ Token.Type.INTEGER, null, Compiler.number, Precedence.NONE },
    .{ Token.Type.LEFT_BRACE, null, null, Precedence.NONE },
    .{ Token.Type.LEFT_PARENTHESIS, null, Compiler.grouping, Precedence.NONE },
    .{ Token.Type.LESSER, Compiler.binary, null, Precedence.COMPARISON },
    .{ Token.Type.LESSER_EQUAL, Compiler.binary, null, Precedence.COMPARISON },
    .{ Token.Type.MINUS, Compiler.binary, Compiler.unary, Precedence.TERM },
    .{ Token.Type.NULL, null, Compiler.null, Precedence.NONE },
    .{ Token.Type.OR, null, null, Precedence.NONE },
    .{ Token.Type.PLUS, Compiler.binary, null, Precedence.TERM },
    .{ Token.Type.RETURN, null, null, Precedence.NONE },
    .{ Token.Type.RIGHT_BRACE, null, null, Precedence.NONE },
    .{ Token.Type.RIGHT_PARENTHESIS, null, null, Precedence.NONE },
    .{ Token.Type.SEMICOLON, null, null, Precedence.NONE },
    .{ Token.Type.SLASH, Compiler.binary, null, Precedence.FACTOR },
    .{ Token.Type.STAR, Compiler.binary, null, Precedence.FACTOR },
    .{ Token.Type.STRING, null, Compiler.string, Precedence.NONE },
    .{ Token.Type.VAR, null, Compiler.variable, Precedence.NONE },
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

const Local = struct {
    name: []const u8,
    depth: usize,
};

const CompileError = error{
    ParseError,
    OutOfMemory,
    CompileError,
    TooManyErrors, // TODO: remove
    UnexpectedToken,
    UnexpectedEOF,
    ExpressionExpected,
} || Scanner.ScanError;
