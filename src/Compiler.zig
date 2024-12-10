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

        _functions: std.StringHashMapUnmanaged(Function),

        // Used to track the scope that we're in
        // Also assigned to a local to determine in what scope the local can be used
        _scopes: std.ArrayListUnmanaged(Scope),
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
                ._scopes = .{},
                ._locals = .{},
                ._functions = .{},
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

        pub fn compile(self: *Self, src: []const u8) CompileError!void {
            self._scanner = Scanner.init(self._arena, src);

            try self.advance();

            self._byte_code.beginScript();
            try self.beginScope(false);
            while (try self.match(.EOF) == false) {
                try self.declaration();
            }
        }

        pub fn byteCode(self: *const Self, allocator: Allocator) CompileError![]const u8 {
            return self._byte_code.toBytes(allocator);
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
                .FN => {
                    try self.advance();
                    const bc = &self._byte_code;

                    try self.consume(.IDENTIFIER, "function name");
                    const name = self._previous_token.value.IDENTIFIER;
                    try self.consume(.LEFT_PARENTHESIS, "'(' after function name'");

                    try self.beginScope(false);
                    var arity: u16 = 0;
                    if (try self.match(.RIGHT_PARENTHESIS) == false) {
                        // parse argument list
                        const scope_depth = self.scopeDepth();
                        var locals = &self._locals;
                        while (true) {
                            arity += 1;
                            if (arity > 255) {
                                try self.setErrorFmt("Function '{s}' has more than 255 parameters", .{name}, null);
                                return error.CompileError;
                            }

                            try self.consume(.IDENTIFIER, "variable name");

                            // Function parameters are just locals to the function
                            // The way the VM loads them means that they'll be
                            // "initialized" in the caller.
                            const variable_name = self._previous_token.value.IDENTIFIER;
                            try locals.append(self._arena, .{
                                .depth = scope_depth,
                                .name = variable_name,
                            });

                            if (try self.match(.RIGHT_PARENTHESIS)) {
                                break;
                            }
                            try self.consume(.COMMA, "parameter separator (',')");
                        }
                    }

                    try self.consume(.LEFT_BRACE, "'{{' before function body");

                    var gop = try self._functions.getOrPut(self._arena, name);
                    if (gop.found_existing) {
                        if (gop.value_ptr.code_pos != null) {
                            try self.setErrorFmt("Function '{s}' already declared", .{name}, null);
                            return error.CompileError;
                        }
                    } else {
                        gop.value_ptr.* = try self.newFunction(name);
                    }

                    try bc.beginFunction(name);
                    try self.block();
                    if (self.currentScope().has_return == false) {
                        try bc.@"null"();
                        try bc.op(.RETURN);
                    }
                    try self.endScope(true);

                    gop.value_ptr.arity = @intCast(arity);
                    gop.value_ptr.code_pos = try bc.endFunction(gop.value_ptr.data_pos, @intCast(arity));
                },
                else => return self.statement(),
            }
        }

        fn variableDeclaration(self: *Self) CompileError!void {
            // "var" already consumed
            try self.consume(.IDENTIFIER, "variable name");

            var locals = &self._locals;

            if (locals.items.len == config.max_locals) {
                try self.setErrorFmt("Maximum number of local variable ({d}) exceeded", .{config.max_locals}, null);
                return error.CompileError;
            }

            const name = self._previous_token.value.IDENTIFIER;
            const scope_depth = self.scopeDepth();

            if (self.localVariableIndex(name)) |idx| {
                if (locals.items[idx].depth == scope_depth) {
                    try self.setErrorFmt("Variable '{s}' already declared", .{name}, null);
                    return error.CompileError;
                }
            }

            try locals.append(self._arena, .{
                .depth = null, // can't be used until after the expression
                .name = name,
            });

            try self.consume(.EQUAL, "assignment operator ('=')");
            try self.expression();
            try self.consumeSemicolon();

            // prevents things like: var count = count + 1;
            locals.items[locals.items.len - 1].depth = scope_depth;
        }

        fn statement(self: *Self) CompileError!void {
            const scope = self.currentScope();
            if (scope.has_return) {
                self.setError("Unreachable code detected", null);
                return error.CompileError;
            }
            var bc = &self._byte_code;
            switch (self._current_token.value) {
                .LEFT_BRACE => {
                    try self.advance();
                    try self.beginScope(true);
                    try self.block();
                    try self.endScope(false);
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
                        try self.finalizeJump(if_jump);
                        try bc.op(.POP); // pop the if condition (when if is false with no else)
                    }
                },
                .FOR => {
                    try self.advance();
                    try self.consume(.LEFT_PARENTHESIS, "opening parentheses ('(')");

                    // initializer variable needs its own scope

                    try self.beginScope(true);

                    if (try self.match(.VAR)) {
                        try self.variableDeclaration();
                    } else if (try self.match(.SEMICOLON) == false) {
                        try self.expression();
                        try self.consumeSemicolon();
                        try bc.op(.POP);
                    }

                    // this is where we jump back to after every loop
                    const loop_start = bc.currentPos();

                    var jump_loop: ?u32 = null;
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

                    try self.endScope(false);

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
                .RETURN => {
                    try self.advance();
                    if (try self.match(.SEMICOLON)) {
                        try bc.op(.RETURN);
                    } else {
                        try self.expression();
                        try self.consumeSemicolon();
                        try bc.op(.RETURN);
                    }
                    scope.has_return = true;
                },
                .PRINT => {
                    try self.advance();
                    try self.expression();
                    try self.consumeSemicolon();
                    try bc.op(.PRINT);
                },
                else => {
                    try self.expression();
                    try self.consumeSemicolon();
                    try bc.op(.POP);
                },
            }
        }

        fn jump(self: *Self, pos: u32) !void  {
            self._byte_code.jump(pos) catch {
                self.setError("Jump size exceeded maximum allowed value", null);
                return error.CompileError;
            };
        }

        fn finalizeJump(self: *Self, pos: u32) !void  {
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
                .PERCENT => try self._byte_code.op(.MODULUS),
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

        fn identifier(self: *Self, can_assign: bool) CompileError!void {
            if (self._current_token.value == .LEFT_PARENTHESIS) {
                return self.call();
            }
            return self.variable(can_assign);
        }

        fn variable(self: *Self, can_assign: bool) CompileError!void {
            const name = self._previous_token.value.IDENTIFIER;

            const idx = self.localVariableIndex(name) orelse {
                try self.setErrorFmt("Variable '{s}' is unknown", .{name}, null);
                return error.CompileError;
            };

            if (self._locals.items[idx].depth == null) {
                try self.setErrorFmt("Variable '{s}' used before being initialized", .{name}, null);
                return error.CompileError;
            }

            const bc = &self._byte_code;
            if (can_assign) {
                if (try self.match(.EQUAL)) {
                    try self.expression();
                    try bc.setLocal(@intCast(idx));
                    return;
                }

                if (try self.match(.PLUS_EQUAL)) {
                    switch (self._current_token.value) {
                        .INTEGER => |n| switch (n) {
                            -1 => {
                                try self.advance();
                                return bc.incr(@intCast(idx), 0);
                            },
                            1...10 => {
                                try self.advance();
                                return bc.incr(@intCast(idx), @intCast(n));
                            },
                            else => {},
                        },
                        else => {},
                    }

                    try bc.getLocal(@intCast(idx));
                    try self.expression();
                    try bc.op(.ADD);
                    try bc.setLocal(@intCast(idx));
                    return;
                }

                if (try self.match(.MINUS_EQUAL)) {
                    switch (self._current_token.value) {
                        .INTEGER => |n| switch (n) {
                            1 => {
                                try self.advance();
                                return bc.incr(@intCast(idx), 0);
                            },
                            else => {},
                        },
                        else => {},
                    }
                    try bc.getLocal(@intCast(idx));
                    try self.expression();
                    try bc.op(.SUBTRACT);
                    try bc.setLocal(@intCast(idx));
                    return;
                }
            }


            if (try self.match(.PLUS_PLUS)) {
                try bc.incr(@intCast(idx), 1);
            } else if (try self.match(.MINUS_MINUS)) {
                try bc.incr(@intCast(idx), 0);
            } else {
                try bc.getLocal(@intCast(idx));
            }
        }

        fn array(self: *Self, _: bool) CompileError!void {
            var value_count: u32 = 0;
            if (try self.match(.RIGHT_BRACKET) == false) {
                while (true) {
                    value_count += 1;
                    try self.expression();
                    if (try self.match(.RIGHT_BRACKET)) {
                        break;
                    }
                    try self.consume(.COMMA, "value separator (',')");
                }
            }
            try self._byte_code.initializeArray(value_count);
        }

        fn call(self: *Self) CompileError!void {
            const name = self._previous_token.value.IDENTIFIER;

            try self.advance(); // consume '(

            var arity: u16 = 0;
            if (try self.match(.RIGHT_PARENTHESIS) == false) {
                while (true) {
                    if (try self.match(.RIGHT_PARENTHESIS)) {
                    }
                    arity += 1;
                    try self.expression();
                    if (try self.match(.RIGHT_PARENTHESIS)) {
                        break;
                    }
                    try self.consume(.COMMA, "parameter separator (',')");
                }
            }

            // TODO: check arity (assuming we've declared the function)
            const gop = try self._functions.getOrPut(self._arena, name);
            if (gop.found_existing == false) {
                gop.value_ptr.* = try self.newFunction(name);
            }
            try self._byte_code.call(gop.value_ptr.data_pos);
        }

        fn newFunction(self: *Self, name: []const u8) CompileError!Function {
            const data_pos = try self._byte_code.newFunction(name);
            return .{
                .arity = null,
                .code_pos = null,
                .data_pos = data_pos,
            };
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

        fn localVariableIndex(self: *const Self, name: []const u8) ?usize {
            const locals = self._locals.items;
            const local_scope_start = self.currentScope().local_start;
            var idx = locals.len;
            while (idx > local_scope_start) {
                idx -= 1;
                const local = locals[idx];
                if (std.mem.eql(u8, name, local.name)) {
                    return idx - local_scope_start;
                }
            }

            return null;
        }

        fn scopeDepth(self: *const Self) usize {
            return self._scopes.items.len;
        }

        fn currentScope(self: *const Self) *Scope {
            return &self._scopes.items[self._scopes.items.len - 1];
        }

        fn beginScope(self: *Self, inherit_locals: bool) !void {
            try self._scopes.append(self._arena , .{
                .has_return = false,
                .local_start = if (inherit_locals) self.currentScope().local_start else self._locals.items.len,
            });
        }

        // Fast_pop is true when we're returning from a function.
        // In this case, locals added by the functions don't need to be
        // individually popped, the VM can simply restore the stack back to
        // the caller's previous state.
        fn endScope(self: *Self, comptime fast_pop: bool) !void {
            _ = self._scopes.pop();
            const scope_depth = self.scopeDepth();

            // pop off any locals from this scope
            var bc = &self._byte_code;
            const locals = &self._locals;

            var i = locals.items.len;
            while (i > 0) {
                i -= 1;
                if (locals.items[i].depth.? <= scope_depth) {
                    break;
                }
                if (fast_pop == false) {
                    try bc.op(.POP);
                }
                locals.items.len -= 1;
            }
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

// For top-level functions we store a small function header in the bytecode's
// data section. This indirection allows the function to be called before its
// declared by giving the function header a known location.
// The first time we see a call to `sum()` we can reserve space in the data
// section:
//   0010 0 0 0 0 0
// i.e:
//   at address 0x10 of our data (arbitrary for this example), we reserve 5 bytes
//   (function metadata is always 5 bytes).
// And we can generate a Op.Call with operand 0x10
//
// Whe the function is actually declared, we can fill in those 5 bytes
// The first is the arity, and the other 4 is the address of the actual function
// code.
const Function = struct {
    arity: ?u8 = null,
    data_pos: u32,
    code_pos: ?u32 = null,
};

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
            .{ Token.Type.IDENTIFIER, null, C.identifier, Precedence.NONE },
            .{ Token.Type.IF, null, null, Precedence.NONE },
            .{ Token.Type.INTEGER, null, C.number, Precedence.NONE },
            .{ Token.Type.LEFT_BRACE, null, null, Precedence.NONE },
            .{ Token.Type.LEFT_BRACKET, null, C.array, Precedence.NONE },
            .{ Token.Type.LEFT_PARENTHESIS, null, C.grouping, Precedence.NONE },
            .{ Token.Type.LESSER, C.binary, null, Precedence.COMPARISON },
            .{ Token.Type.LESSER_EQUAL, C.binary, null, Precedence.COMPARISON },
            .{ Token.Type.MINUS, C.binary, C.unary, Precedence.TERM },
            .{ Token.Type.NULL, null, C.null, Precedence.NONE },
            .{ Token.Type.OR, C.@"or", null, Precedence.OR },
            .{ Token.Type.PERCENT, C.binary, null, Precedence.FACTOR },
            .{ Token.Type.PLUS, C.binary, null, Precedence.TERM },
            .{ Token.Type.RETURN, null, null, Precedence.NONE },
            .{ Token.Type.RIGHT_BRACE, null, null, Precedence.NONE },
            .{ Token.Type.RIGHT_PARENTHESIS, null, null, Precedence.NONE },
            .{ Token.Type.SEMICOLON, null, null, Precedence.NONE },
            .{ Token.Type.SLASH, C.binary, null, Precedence.FACTOR },
            .{ Token.Type.STAR, C.binary, null, Precedence.FACTOR },
            .{ Token.Type.STRING, null, C.string, Precedence.NONE },
            .{ Token.Type.VAR, null, null, Precedence.NONE },
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
    FACTOR, // * / %
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

const Scope = struct {
    has_return: bool,
    local_start: usize,
};

const Local = struct {
    name: []const u8,
    depth: ?usize,
};
