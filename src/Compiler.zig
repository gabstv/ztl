const std = @import("std");

const Allocator = std.mem.Allocator;
const Token = @import("scanner.zig").Token;
const Config = @import("config.zig").Config;
const Scanner = @import("scanner.zig").Scanner;
const Position = @import("scanner.zig").Position;
const ByteCode = @import("byte_code.zig").ByteCode;

pub const CompileError = error{
    ScanError,
    OutOfMemory,
    CompileError,
};

pub fn Compiler(comptime config: Config) type {
    return struct {
        // the ByteCode that our compiler is generating
        _byte_code: ByteCode(config),

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
        _scope_depth: usize = 0,
        _locals: std.ArrayListUnmanaged(Local),

        const Self = @This();

        pub fn init(allocator: Allocator) !Self {
            const arena = try allocator.create(std.heap.ArenaAllocator);
            errdefer allocator.destroy(arena);

            arena.* = std.heap.ArenaAllocator.init(allocator);
            errdefer arena.deinit();

            const aa = arena.allocator();

            return .{
                ._arena = aa,
                ._locals = .{},
                ._byte_code = try ByteCode(config).init(aa),
                ._current_token = .{ .value = .{ .START = {} }, .position = Position.ZERO, .src = "" },
                ._previous_token = .{ .value = .{ .START = {} }, .position = Position.ZERO, .src = "" },
            };
        }

        pub fn deinit(self: *const Self) void {
            const arena: *std.heap.ArenaAllocator = @ptrCast(@alignCast(self._arena.ptr));
            arena.deinit();
            arena.child_allocator.destroy(arena);
        }

        pub fn reset(self: *Self, retain_limit: usize) void {
            self._locals.capacity = 0;
            self._locals.items.len = 0;
            self._byte_code.reset();
            const arena: *std.heap.ArenaAllocator = @ptrCast(@alignCast(self._arena.ptr));
            _ = arena.reset(.{.retain_with_limit = retain_limit});
        }

        pub fn byteCode(self: *const Self, allocator: Allocator) ![]const u8 {
            return self._byte_code.toBytes(allocator);
        }

        pub fn compile(self: *Self, src: []const u8) CompileError!void {
            self._scanner = Scanner.init(self._arena, src);

            try self.advance();

            while (try self.match(.EOF) == false) {
                try self.declaration();
            }
        }

        fn advance(self: *Self) !void {
            self._previous_token = self._current_token;
            self._current_token = self._scanner.next() catch |err| {
                if (self._scanner.err) |se| {
                    self.setError(se, null);
                    return error.ScanError;
                }
                return err;
            };
        }

        fn consume(self: *Self, expected: Token.Type, comptime message: []const u8) !void {
            if (self._current_token.value != expected) {
                return self.setExpectationError(message);
            }
            return self.advance();
        }

        fn consumeSemicolon(self: *Self) !void {
            return self.consume(.SEMICOLON, "semicolon (';')");
        }

        fn match(self: *Self, expected: Token.Type) !bool {
            if (self._current_token.value != expected) {
                return false;
            }
            try self.advance();
            return true;
        }

        fn declaration(self: *Self) CompileError!void {
            switch (self._current_token.value) {
                .VAR => {
                    try self.advance();
                    return self.variableDeclaration();
                },
                else => return self.statement(),
            }
        }

        fn variableDeclaration(self: *Self) CompileError!void {
            // "var" already consumed
            try self.consume(.IDENTIFIER, "variable name");

            const variable_name = self._previous_token.value.IDENTIFIER;
            // TODO: check self._locals for a variable with the same name
            // in the same scope. Start at _locals[_local_count - 1] and work
            // backwards, until we find a conflict, or the scope chang
            var locals = &self._locals;
            if (locals.items.len == config.max_locals) {
                try self.setErrorFmt("maximum number of local variable ({d}) exceeded", .{config.max_locals}, null);
                return error.CompileError;
            }
            try locals.append(self._arena, .{
                .depth = null, // can't be used until after the expression
                .name = variable_name,
            });

            try self.consume(.EQUAL, "assignment operator ('=')");
            try self.expression();
            try self.consumeSemicolon();

            // prevents things like: var count = count + 1;
            locals.items[locals.items.len - 1].depth = self._scope_depth;
        }

        fn statement(self: *Self) CompileError!void {
            var bc = &self._byte_code;
            switch (self._current_token.value) {
                .LEFT_BRACE => {
                    try self.advance();
                    const scope = self._scope_depth;
                    self._scope_depth = scope + 1;
                    try self.block();
                    self._scope_depth = scope;

                    // pop off any locals from this scope
                    const locals = &self._locals;
                    const count = locals.items.len;
                    if (count > 0) {
                        var i = count - 1;
                        while (i >= 0) : (i -= 1) {
                            if (locals.items[i].depth.? > scope) {
                                try bc.op(.POP);
                            } else {
                                locals.items.len = i + 1;
                                break;
                            }
                        } else {
                            locals.clearRetainingCapacity();
                        }
                    }
                },
                .IF => {
                    try self.advance();
                    try self.consume(.LEFT_PARENTHESIS, "opening parentheses ('(')");
                    try self.expression();
                    try self.consume(.RIGHT_PARENTHESIS, "closing parentheses (')')");

                    const if_jump = try bc.prepareJump(.JUMP_IF_FALSE);
                    try bc.op(.POP); // pop the if condition (inside the if block)

                    try self.statement();

                    if (try self.match(.ELSE)) {
                        const else_jump = try bc.prepareJump(.JUMP);
                        try bc.op(.POP); // pop the if condition (inside the else block)
                        try self.finalizeJump(if_jump);
                        try self.statement();
                        try self.finalizeJump(else_jump);
                    } else {
                        try bc.op(.POP); // pop the if condition (when false with no else
                        try self.finalizeJump(if_jump);
                    }
                },
                .FOR => {
                    try self.advance();
                    try self.consume(.LEFT_PARENTHESIS, "opening parentheses ('(')");

                    // initializer variable needs its own scope
                    const scope = self._scope_depth;
                    self._scope_depth = scope + 1;

                    if (try self.match(.VAR)) {
                        try self.variableDeclaration();
                    } else if (try self.match(.SEMICOLON) == false) {
                        try self.expression();
                        try self.consumeSemicolon();
                        try bc.op(.POP);
                    }

                    // this is where we jump back to after every loop
                    const loop_start = bc.currentPos();

                    var jump_loop: ?usize = null;
                    if (try self.match(.SEMICOLON) == false) {
                        // we have a condition!
                        try self.expression();
                        try self.consumeSemicolon();

                        jump_loop = try bc.prepareJump(.JUMP_IF_FALSE);
                        try bc.op(.POP);
                    }

                    var incr: []const u8 = &.{};
                    if (try self.match(.RIGHT_PARENTHESIS) == false) {
                        // increment
                        bc.beginCapture();
                        try self.expression();
                        try bc.op(.POP);
                        incr = bc.endCapture();
                        try self.consume(.RIGHT_PARENTHESIS, "closing parentheses (')')");
                    }

                    // body
                    try self.statement();
                    try bc.write(incr);

                    // back to condition check
                    try self.jump(loop_start);

                    if (jump_loop) |jl| {
                        // this is where we exit when the condition is false
                        try self.finalizeJump(jl);
                        try bc.op(.POP);
                    }

                    // restore our scope
                    self._scope_depth = scope;

                },
                .WHILE => {
                    try self.advance();
                    const loop_start = bc.currentPos();
                    try self.consume(.LEFT_PARENTHESIS, "opening parentheses ('(')");
                    try self.expression();
                    try self.consume(.RIGHT_PARENTHESIS, "closing parentheses (')')");

                    const while_jump = try bc.prepareJump(.JUMP_IF_FALSE);
                    // pop the result of the condition when it's true
                    try bc.op(.POP);
                    try self.statement();
                    try self.jump(loop_start);
                    try self.finalizeJump(while_jump);

                    // pop the result of the condition when it's [finally] false
                    try bc.op(.POP);
                },
                .PRINT => {
                    try self.advance();
                    try self.expression();
                    try self.consumeSemicolon();
                    try bc.op(.PRINT);
                },
                .RETURN => {
                    try self.advance();
                    if (try self.match(.SEMICOLON)) {
                        try bc.op(.RETURN);
                    } else {
                        try self.expression();
                        try self.consumeSemicolon();
                        try bc.op(.RETURN);
                    }
                },
                else => {
                    try self.expression();
                    try self.consumeSemicolon();
                    try bc.op(.POP);
                },
            }
        }

        fn jump(self: *Self, pos: usize) !void  {
            self._byte_code.jump(pos) catch {
                self.setError("Jump size exceeded maximum allowed value", null);
                return error.CompileError;
            };
        }

        fn finalizeJump(self: *Self, pos: usize) !void  {
            self._byte_code.finalizeJump(pos) catch {
                self.setError("Jump size exceeded maximum allowed value", null);
                return error.CompileError;
            };
        }

        fn block(self: *Self) CompileError!void {
            while (true) {
                switch (self._current_token.value) {
                    .RIGHT_BRACE => return self.advance(),
                    .EOF => return self.setExpectationError("closing block ('}}')"),
                    else => try self.declaration(),
                }
            }
        }

        fn expression(self: *Self) CompileError!void {
            return self.parsePrecedence(.ASSIGNMENT);
        }

        fn parsePrecedence(self: *Self, precedence: Precedence) CompileError!void {
            try self.advance();
            {
                const rule = ParseRule(Self).get(self._previous_token.value);
                if (rule.prefix) |prefix| {
                    const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
                    try prefix(self, can_assign);
                } else {
                    return self.setExpectationError("an expression");
                }
            }

            const nprec = @intFromEnum(precedence);
            while (true) {
                const rule = ParseRule(Self).get(self._current_token.value);
                if (nprec > rule.precedence) {
                    break;
                }
                try self.advance();
                try rule.infix.?(self);
            }
        }

        fn grouping(self: *Self, _: bool) CompileError!void {
            try self.expression();
            return self.consume(.RIGHT_PARENTHESIS, "Expected closing parentheses ')'");
        }

        fn binary(self: *Self) CompileError!void {
            const previous = self._previous_token.value;
            const rule = ParseRule(Self).get(previous);
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

        fn unary(self: *Self, _: bool) CompileError!void {
            const previous = self._previous_token.value;

            try self.expression();
            switch (previous) {
                .BANG => try self._byte_code.op(.NOT),
                .MINUS => try self._byte_code.op(.NEGATE),
                else => unreachable,
            }
        }

        fn number(self: *Self, _: bool) CompileError!void {
            switch (self._previous_token.value) {
                .INTEGER => |value| try self._byte_code.i64(value),
                .FLOAT => |value| try self._byte_code.f64(value),
                else => unreachable,
            }
        }

        fn boolean(self: *Self, _: bool) CompileError!void {
            return self._byte_code.bool(self._previous_token.value.BOOLEAN);
        }

        fn string(self: *Self, _: bool) CompileError!void {
            return self._byte_code.string(self._previous_token.value.STRING);
        }

        fn @"null"(self: *Self, _: bool) CompileError!void {
            return self._byte_code.null();
        }

        fn variable(self: *Self, can_assign: bool) CompileError!void {
            const name = self._previous_token.value.IDENTIFIER;

            const idx = blk: {
                const locals = self._locals.items;
                if (locals.len == 0) {
                    break :blk null;
                }
                var idx = locals.len - 1;
                while (idx >= 0) : (idx -= 1) {
                    const local = locals[idx];
                    if (std.mem.eql(u8, name, local.name)) {
                        if (local.depth == null) {
                            try self.setErrorFmt("Variable '{s}' used before being initialized", .{name}, null);
                            return error.CompileError;
                        }
                        break :blk idx;
                    }
                }
                break :blk null;
            } orelse {
                try self.setErrorFmt("Variable '{s}' is unknown", .{name}, null);
                return error.CompileError;
            };

            const bc = &self._byte_code;
            if (can_assign) {
                if (try self.match(.EQUAL)) {
                    try self.expression();
                    try bc.setLocal(@intCast(idx));
                    return;
                }

                if (try self.match(.PLUS_EQUAL)) {
                    try bc.getLocal(@intCast(idx));
                    try self.expression();
                    try bc.op(.ADD);
                    try bc.setLocal(@intCast(idx));
                    return;
                }

                if (try self.match(.MINUS_EQUAL)) {
                    try bc.getLocal(@intCast(idx));
                    try self.expression();
                    try bc.op(.SUBTRACT);
                    try bc.setLocal(@intCast(idx));
                    return;
                }
            }


            if (try self.match(.PLUS_PLUS)) {
                try bc.incr(@intCast(idx), true);
            } else if (try self.match(.MINUS_MINUS)) {
                try bc.incr(@intCast(idx), false);
            } else {
                try bc.getLocal(@intCast(idx));
            }
        }

        fn @"and"(self: *Self) CompileError!void {
            const bc = &self._byte_code;

            // shortcircuit, the left side is already executed, if it's false, we
            // can skip the rest.
            const end_pos = try bc.prepareJump(.JUMP_IF_FALSE);
            try bc.op(.POP);
            try self.parsePrecedence(.AND);
            try self.finalizeJump(end_pos);
        }

        fn @"or"(self: *Self) CompileError!void {
            const bc = &self._byte_code;

            // Rather than add a new op (JUMP_IF_TRUE), we can simulate this by
            // combining JUMP_IF_FALSE to jump over a JUMP. IF the left side (
            // which we've already executed) is true, the JUMP_IF_FALSE will
            // be skipped, and we'll execute the JUMP, which will skip the
            // right of the conditions. In other words, the JUMP_IS_FALSE only
            // exists to jump over the JUMP, which exists to shortcircuit on true.
            const else_pos = try bc.prepareJump(.JUMP_IF_FALSE);
            const end_pos = try bc.prepareJump(.JUMP);
            try self.finalizeJump(else_pos);
            try bc.op(.POP);
            try self.parsePrecedence(.OR);
            try self.finalizeJump(end_pos);
        }

        pub fn setExpectationError(self: *Self, comptime message: []const u8) CompileError!void {
            const current_token = self._current_token;
            try self.setErrorFmt("Expected " ++ message ++ ", got '{s}' ({s})", .{ current_token.src, @tagName(current_token.value) }, null);
            return error.CompileError;
        }

        pub fn setErrorFmt(self: *Self, comptime fmt: []const u8, args: anytype, position: ?Position) !void {
            const desc = try std.fmt.allocPrint(self._arena, fmt, args);
            return self.setError(desc, position);
        }

        pub fn setError(self: *Self, desc: []const u8, position: ?Position) void {
            self.err = .{
                .desc = desc,
                .position = position orelse self._scanner.position(null),
            };
        }

        fn invalidToken(self: *Self, err: anyerror, comptime desc: []const u8, token: Token) !void {
            try self.setErrorFmt(err, desc ++ ", got: '{s}'", .{self._scanner.srcAt(token.position)}, null);
            return error.CompileError;
        }
    };
}

fn ParseRule(comptime C: type) type {
    return struct {
        infix: ?*const fn (*C) CompileError!void,
        prefix: ?*const fn (*C, bool) CompileError!void,
        precedence: i32,

        const Self = @This();

        inline fn get(token_type: Token.Type) *const Self {
            return &rules[@intFromEnum(token_type)];
        }

        const rules = buildParseRules(&.{
            .{ Token.Type.AND, C.@"and", null, Precedence.AND },
            .{ Token.Type.BANG, null, C.unary, Precedence.NONE },
            .{ Token.Type.BANG_EQUAL, C.binary, null, Precedence.EQUALITY },
            .{ Token.Type.BOOLEAN, null, C.boolean, Precedence.NONE },
            .{ Token.Type.COMMA, null, null, Precedence.NONE },
            .{ Token.Type.DOT, null, null, Precedence.NONE },
            .{ Token.Type.ELSE, null, null, Precedence.NONE },
            .{ Token.Type.EOF, null, null, Precedence.NONE },
            .{ Token.Type.EQUAL, null, null, Precedence.NONE },
            .{ Token.Type.EQUAL_EQUAL, C.binary, null, Precedence.EQUALITY },
            .{ Token.Type.FLOAT, null, C.number, Precedence.NONE },
            .{ Token.Type.GREATER, C.binary, null, Precedence.COMPARISON },
            .{ Token.Type.GREATER_EQUAL, C.binary, null, Precedence.COMPARISON },
            .{ Token.Type.IDENTIFIER, null, C.variable, Precedence.NONE },
            .{ Token.Type.IF, null, null, Precedence.NONE },
            .{ Token.Type.INTEGER, null, C.number, Precedence.NONE },
            .{ Token.Type.LEFT_BRACE, null, null, Precedence.NONE },
            .{ Token.Type.LEFT_PARENTHESIS, null, C.grouping, Precedence.NONE },
            .{ Token.Type.LESSER, C.binary, null, Precedence.COMPARISON },
            .{ Token.Type.LESSER_EQUAL, C.binary, null, Precedence.COMPARISON },
            .{ Token.Type.MINUS, C.binary, C.unary, Precedence.TERM },
            .{ Token.Type.NULL, null, C.null, Precedence.NONE },
            .{ Token.Type.OR, C.@"or", null, Precedence.OR },
            .{ Token.Type.PLUS, C.binary, null, Precedence.TERM },
            .{ Token.Type.RETURN, null, null, Precedence.NONE },
            .{ Token.Type.RIGHT_BRACE, null, null, Precedence.NONE },
            .{ Token.Type.RIGHT_PARENTHESIS, null, null, Precedence.NONE },
            .{ Token.Type.SEMICOLON, null, null, Precedence.NONE },
            .{ Token.Type.SLASH, C.binary, null, Precedence.FACTOR },
            .{ Token.Type.STAR, C.binary, null, Precedence.FACTOR },
            .{ Token.Type.STRING, null, C.string, Precedence.NONE },
            .{ Token.Type.VAR, null, null, Precedence.NONE },
            // .{Token.Type.WHILE, null, null, Precedence.NONE},
            // .{Token.Type.CLASS, null, null, Precedence.NONE},
            // .{Token.Type.ERROR, null, null, Precedence.NONE},
            // .{Token.Type.FOR, null, null, Precedence.NONE},
            // .{Token.Type.FUN, null, null, Precedence.NONE},
            // .{Token.Type.PRINT, null, null, Precedence.NONE},
            // .{Token.Type.SUPER, null, null, Precedence.NONE},
            // .{Token.Type.THIS, null, null, Precedence.NONE},
        });

        fn buildParseRules(definitions: anytype) [maxRuleIndex(Token.Type)]Self {
            var _rules: [maxRuleIndex(Token.Type)]Self = undefined;
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
    };
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

fn maxRuleIndex(comptime E: type) usize {
    var max: usize = 0;
    for (@typeInfo(E).@"enum".fields) |f| {
        max = @max(max, f.value);
    }
    return max + 1;
}

pub const Error = struct {
    desc: []const u8,
    position: Position,

    pub fn format(self: Error, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return writer.print("{s}", .{self.desc});
    }
};

const Local = struct {
    name: []const u8,
    depth: ?usize,
};
