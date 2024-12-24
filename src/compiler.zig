const std = @import("std");
const ztl = @import("ztl.zig");

const Allocator = std.mem.Allocator;

const Error = ztl.Error;
const Position = ztl.Position;

const asUint = @import("scanner.zig").asUint;
const config = @import("config.zig");
const Token = @import("scanner.zig").Token;
const Scanner = @import("scanner.zig").Scanner;
const ByteCode = @import("byte_code.zig").ByteCode;

pub const CompileError = error{
    ScanError,
    OutOfMemory,
    CompileError,
};

const CompileOpts = struct {
    force_locals:  []const []const u8 = &.{},
};

pub fn Compiler(comptime App: type) type {
    const MAX_LOCALS = config.extract(App, "ztl_max_locals");
    const DEDUPLICATE_STRING_LITERALS = config.extract(App, "ztl_deduplicate_string_literals");

    return struct {
        // the ByteCode that our compiler is generating
        _byte_code: ByteCode(App),

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

        // Used to dedupe string literals. Stores the data_start value of
        // a given string. Can be turned off by setting zt_deduplicate_string_literals = false;
        _string_literals: std.StringHashMapUnmanaged(u32),

        // Jumping is one of the messier parts of the code. So we try to put as
        // much of the logic into its own struct.
        _jumper: Jumper(App),

        const Self = @This();

        pub fn init(allocator: Allocator) !Self {
            const arena = try allocator.create(std.heap.ArenaAllocator);
            errdefer allocator.destroy(arena);

            arena.* = std.heap.ArenaAllocator.init(allocator);
            errdefer arena.deinit();
            return initWithArena(arena.allocator());
        }

        pub fn initWithArena(allocator: Allocator) !Self {
            return .{
                ._scopes = .{},
                ._locals = .{},
                ._functions = .{},
                ._arena = allocator,
                ._string_literals = .{},
                ._jumper = Jumper(App).init(allocator),
                ._byte_code = try ByteCode(App).init(allocator),
                ._current_token = .{ .value = .{ .START = {} }, .position = Position.ZERO, .src = "" },
                ._previous_token = .{ .value = .{ .START = {} }, .position = Position.ZERO, .src = "" },
            };
        }

        pub fn deinit(self: *const Self) void {
            const arena: *std.heap.ArenaAllocator = @ptrCast(@alignCast(self._arena.ptr));
            arena.deinit();
            arena.child_allocator.destroy(arena);
        }

        pub fn compile(self: *Self, src: []const u8, opts: CompileOpts) CompileError!void {
            self._scanner = Scanner.init(self._arena, src);

            try self.advance();

            self._byte_code.beginScript();
            try self.beginScope(false);

            // see template.zig's hack around globals to understand what this is
            for (opts.force_locals) |name| {
                try self._locals.append(self._arena, .{
                    .depth = 0,
                    .name = name,
                });
            }

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
                    return self.setError(error.ScanError, se);
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
                    return self.variableInitialization();
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
                                return self.setErrorFmt(error.CompileError, "Function '{s}' has more than 255 parameters", .{name});
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
                            return self.setErrorFmt(error.CompileError, "Function '{s}' already declared", .{name});
                        }
                    } else {
                        gop.value_ptr.* = try self.newFunction(name);
                    }

                    try bc.beginFunction(name);
                    try self.block();
                    if (self.currentScope().has_return == false) {
                        try bc.null();
                        try bc.op(.RETURN);
                    }
                    try self.endScope(true);

                    gop.value_ptr.arity = @intCast(arity);
                    gop.value_ptr.code_pos = try bc.endFunction(gop.value_ptr.data_pos, @intCast(arity));
                },
                else => return self.statement(),
            }
        }

        fn variableInitialization(self: *Self) CompileError!void {
            try self.variableDeclaration(false);

            try self.consume(.EQUAL, "assignment operator ('=')");
            try self.expression();
            try self.consumeSemicolon();

            // prevents things like: var count = count + 1;
            self._locals.items[self._locals.items.len - 1].depth = self.scopeDepth();
        }

        fn variableDeclaration(self: *Self, give_scope: bool) CompileError!void {
            // "var" already consumed
            try self.consume(.IDENTIFIER, "variable name");

            var locals = &self._locals;

            if (locals.items.len == MAX_LOCALS) {
                return self.setErrorFmt(error.CompileError, "Maximum number of local variable ({d}) exceeded", .{MAX_LOCALS});
            }

            const name = self._previous_token.value.IDENTIFIER;
            const scope_depth = self.scopeDepth();

            if (self.localVariableIndex(name)) |idx| {
                if (locals.items[idx].depth == scope_depth) {
                    return self.setErrorFmt(error.CompileError, "Variable '{s}' already declared", .{name});
                }
            }

            if (comptime config.shouldDebug(App, .full) == true) {
                try self._byte_code.debugVariableName(name, @intCast(locals.items.len));
            }

            try locals.append(self._arena, .{
                .name = name,
                .depth = if (give_scope) scope_depth else null,
            });
        }

        fn statement(self: *Self) CompileError!void {
            const scope = self.currentScope();
            if (scope.has_return) {
                return self.setError(error.CompileError, "Unreachable code detected");
            }
            var bc = &self._byte_code;
            var jumper = &self._jumper;
            switch (self._current_token.value) {
                .LEFT_BRACE => {
                    try self.advance();
                    try self.beginScope(true);
                    try self.block();
                    try self.endScope(false);
                },
                .DOLLAR => {
                    try self.advance();
                    try self.expression();
                    try self.consume(.SEMICOLON, "semicolon (';')");
                    try bc.op(.OUTPUT);
                },
                .IF => {
                    try self.advance();
                    try self.consume(.LEFT_PARENTHESIS, "opening parenthesis ('(')");
                    try self.expression();
                    try self.consume(.RIGHT_PARENTHESIS, "closing parenthesis (')')");

                    const jump_if_false = try jumper.forward(self, .JUMP_IF_FALSE_POP);

                    try self.statement();

                    if (try self.match(.ELSE)) {
                        const jump_if_true = try jumper.forward(self, .JUMP);
                        try jump_if_false.goto();
                        try self.statement();
                        try jump_if_true.goto();
                    } else {
                        try jump_if_false.goto();
                    }
                },
                .FOR => {
                    try self.advance();
                    try self.consume(.LEFT_PARENTHESIS, "opening parenthesis ('(')");

                    // initializer variable needs its own scope

                    try self.beginScope(true);

                    if (try self.match(.VAR)) {
                        try self.variableInitialization();
                    } else if (try self.match(.SEMICOLON) == false) {
                        try self.expression();
                        try self.consumeSemicolon();
                        try bc.op(.POP);
                    }

                    // this is where we jump back to after every loop
                    const jump_loop_top = jumper.backward(self);

                    // if we have a condition, we'll need to jump to the end of
                    // the for loop if/when it becomes false.
                    var jump_loop_false: ?Jumper(App).Forward = null;
                    if (try self.match(.SEMICOLON) == false) {
                        // we have a condition!
                        try self.expression();
                        try self.consumeSemicolon();

                        jump_loop_false = try jumper.forward(self, .JUMP_IF_FALSE_POP);
                    }

                    var incr: []const u8 = &.{};
                    if (try self.match(.RIGHT_PARENTHESIS) == false) {
                        // increment
                        bc.beginCapture();
                        try self.expression();
                        try bc.op(.POP);
                        // need to dupe, because the temp space used by scanner
                        // to capture might get reused (in a nested for, for example)
                        // because we use the value later on.
                        incr = try self._arena.dupe(u8, bc.endCapture());
                        try self.consume(.RIGHT_PARENTHESIS, "closing parenthesis (')')");
                    }

                    // body

                    const breakable_scope = try jumper.newBreakableScope(self);
                    defer breakable_scope.deinit();

                    try self.statement();

                    // Continue here. NOT at the top of the loop, because we need
                    // to execute the increment step. (But note that after incr
                    // is called, the code naturally jumps back to the top)
                    try breakable_scope.continueHere();

                    try bc.write(incr);

                    // back to condition check
                    try jump_loop_top.goto();

                    if (jump_loop_false) |jlf| {
                        // this is where we exit when the condition is false
                        try jlf.goto();
                    }
                    // any breaks we have registered for this loop will jump here
                    try breakable_scope.breakHere();

                    try self.endScope(false);
                },
                .FOREACH => {
                    try self.advance();
                    try self.consume(.LEFT_PARENTHESIS, "opening parenthesis ('(')");

                    // iterators need their own scope
                    try self.beginScope(true);

                    var iterable_count: u16 = 0;
                    while (true) {
                        iterable_count += 1;
                        try self.expression();
                        if (try self.match(.RIGHT_PARENTHESIS)) {
                            break;
                        }
                        try self.consume(.COMMA, "iteratable separator (',')");
                    }

                    if (iterable_count == 0) {
                        return self.setError(error.CompileError, "foreach requires at least 1 value to iterate");
                    }

                    if (iterable_count > 8) {
                        return self.setError(error.CompileError, "foreach cannot iterate over more tha 8 values");
                    }

                    try self._locals.appendNTimes(self._arena, .{
                        .name = "",
                        .depth = self.scopeDepth(),
                    }, iterable_count);


                    const breakable_scope = try jumper.newBreakableScope(self);
                    defer breakable_scope.deinit();

                    try self.beginScope(true);

                    try self.consume(.PIPE, "variable group start ('|')");
                    var variable_count: u16 = 0;
                    while (true) {
                        variable_count += 1;
                        try self.variableDeclaration(true);
                        if (try self.match(.PIPE)) {
                            break;
                        }
                        try self.consume(.COMMA, "variable separator (',')");
                    }

                    if (iterable_count != variable_count) {
                        return self.setErrorFmt(error.CompileError, "foreach must have the same number of iterables as variables (iterables: {d}, variables: {d})", .{iterable_count, variable_count});
                    }

                    try bc.opWithOperand(.FOREACH, @intCast(iterable_count));

                    // this is where we jump back to after every loop
                    const jump_loop_top = jumper.backward(self);

                    // this SEEMS wrong, but it's how our foreach works with
                    // continue and break. On a normal loop, we'll jump back here
                    // and pop off the iterable variables. This only works because
                    // on the initial loop, the FOREACH will inject N dummy values
                    // (so that these pops are safe on the 1st iteration).
                    // On continue (and break), the typical continue/break scope
                    // restoration works (that is, they issue their own POPs).
                    // For break, that's fine, because it'll just exit the loop.
                    // For continue, it works only because our conitnue will
                    // jump AFTER these POPs (since the continue will issue its
                    // own pops).
                    // This is necessary because, as-is, break and continue follow
                    // the same scope-restoration logic (which works fine for FOR
                    // and WHILE, but requires this hack for FOREACH).
                    for (0..variable_count) |_| {
                        try bc.op(.POP);
                    }

                    const continue_pos = bc.currentPos();

                    try bc.opWithOperand(.FOREACH_ITERATE, @intCast(iterable_count));
                    const jump_if_false = try jumper.forward(self, .JUMP_IF_FALSE_POP);

                    // BODY
                    try self.statement();

                    try breakable_scope.continueAt(continue_pos);


                    // back to condition check
                    try jump_loop_top.goto();
                    try jump_if_false.goto();

                    // any breaks we have registered for this loop will jump here
                    try breakable_scope.breakHere();

                    _ = self._scopes.pop();
                    self._locals.items.len -= variable_count;
                    try self.endScope(false);
                },
                .WHILE => {
                    try self.advance();
                    const jump_loop_top = jumper.backward(self);   // at the end of each iteration, we want to jump back here

                    try self.consume(.LEFT_PARENTHESIS, "opening parenthesis ('(')");
                    try self.expression();
                    try self.consume(.RIGHT_PARENTHESIS, "closing parenthesis (')')");

                    const breakable_scope = try jumper.newBreakableScope(self);
                    defer breakable_scope.deinit();

                    const jump_if_false = try jumper.forward(self, .JUMP_IF_FALSE_POP);
                    try self.statement();
                    try breakable_scope.continueAt(jump_loop_top.jump_to);
                    try jump_loop_top.goto();

                    try jump_if_false.goto(); // if our condition is false, this is where we want to jump to

                    try breakable_scope.breakHere();
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
                .BREAK => {
                    try self.advance();
                    var break_count: usize = 1;
                    if (try self.match(.INTEGER)) {
                        const value = self._previous_token.value.INTEGER;
                        if (value < 0) {
                            return self.setErrorFmt(error.CompileError, "break count must be a positive integer, got {d}", .{value});
                        }
                        break_count = @intCast(value);
                    }
                    try self.consumeSemicolon();
                    try self._jumper.insertBreak(self, break_count);
                },
                .CONTINUE => {
                    try self.advance();
                    var continue_count: usize = 1;
                    if (try self.match(.INTEGER)) {
                        const value = self._previous_token.value.INTEGER;
                        if (value < 0) {
                            return self.setErrorFmt(error.CompileError, "continue count must be a positive integer, got {d}", .{value});
                        }
                        continue_count = @intCast(value);
                    }
                    try self.consumeSemicolon();
                    try self._jumper.insertContinue(self, continue_count);
                },
                .PRINT => {
                    try self.advance();
                    try self.consume(.LEFT_PARENTHESIS, "left parenthesis ('(')");
                    var arity: u16 = 0;
                    if (try self.match(.RIGHT_PARENTHESIS) == false) {
                        while (true) {
                            arity += 1;
                            try self.expression();
                            if (try self.match(.RIGHT_PARENTHESIS)) {
                                break;
                            }
                            try self.consume(.COMMA, "parameter separator (',')");
                        }
                    }
                    if (arity > 32) {
                        return self.setErrorFmt(error.CompileError, "print supports up to 32 parameters, got: {d}\n", .{arity});
                    }
                    try self.consumeSemicolon();
                    try bc.opWithOperand(.PRINT, @intCast(arity));
                },
                else => {
                    try self.expression();
                    try self.consumeSemicolon();
                    try bc.op(.POP);
                },
            }
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
            const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
            {
                const rule = ParseRule(Self).get(self._previous_token.value);
                if (rule.prefix) |prefix| {
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
                try rule.infix.?(self, can_assign);
            }
        }

        fn grouping(self: *Self, _: bool) CompileError!void {
            try self.expression();
            return self.consume(.RIGHT_PARENTHESIS, "Expected closing parenthesis ')'");
        }

        fn binary(self: *Self, _: bool) CompileError!void {
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
            const string_token = self._previous_token.value.STRING;
            return self.stringLiteral(string_token.value, string_token.escaped);
        }

        fn stringLiteral(self: *Self, literal: []const u8, needs_dupe: bool) !void {
            if (DEDUPLICATE_STRING_LITERALS == false) {
                _ = try self._byte_code.string(literal);
                return;
            }

            if (self._string_literals.get(literal)) |data_start| {
                return self._byte_code.stringRef(data_start);
            }

            const data_start = try self._byte_code.string(literal);
            // if the string was escaped, we need to dupe it since it's only
            // being held short-term by the scanner.
            const owned = if (needs_dupe) try self._arena.dupe(u8, literal) else literal;
            try self._string_literals.put(self._arena, owned, data_start);
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
                return self.setErrorFmt(error.CompileError, "Variable '{s}' is unknown", .{name});
            };

            if (self._locals.items[idx].depth == null) {
                return self.setErrorFmt(error.CompileError, "Variable '{s}' used before being initialized", .{name});
            }

            const bc = &self._byte_code;
            if (can_assign) {
                if (try self.match(.EQUAL)) {
                    try self.expression();
                    return bc.setLocal(@intCast(idx));
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
                return bc.incr(@intCast(idx), 1);
            }

            if (try self.match(.MINUS_MINUS)) {
                return bc.incr(@intCast(idx), 0);
            }

            return bc.getLocal(@intCast(idx));
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

        fn map(self: *Self, _: bool) CompileError!void {
            var entry_count: u32 = 0;
            var bc = self._byte_code;
            if (try self.match(.RIGHT_BRACE) == false) {
                while (true) {
                    entry_count += 1;
                    const current_token = self._current_token;
                    switch (current_token.value) {
                        .INTEGER => |k| try bc.i64(k),
                        .IDENTIFIER => |k| try self.stringLiteral(k, false),
                        .STRING => |k| try self.stringLiteral(k.value, k.escaped),
                        else => {
                            return self.setErrorFmt(error.CompileError, "Map key must be an integer, string or identifier, got '{s}' ({s})", .{ current_token.src, @tagName(current_token.value) });
                        }
                    }
                    try self.advance();
                    try self.consume(.COLON, "key : value separator (':')");
                    try self.expression();
                    if (try self.match(.RIGHT_BRACE)) {
                        break;
                    }
                    try self.consume(.COMMA, "value separator (',')");
                    if (try self.match(.RIGHT_BRACE)) {
                        break;
                    }
                }
            }
            try self._byte_code.initializeMap(entry_count);
        }

        fn index(self: *Self, can_assign: bool) CompileError!void {
            try self.expression();
            try self.consume(.RIGHT_BRACKET, "left bracket (']')");

            const bc = &self._byte_code;
            if (can_assign) {
                if (try self.match(.EQUAL)) {
                    try self.expression();
                    return bc.op(.INDEX_SET);
                }

                if (try self.match(.PLUS_EQUAL)) {
                    try self.expression();
                    return bc.op(.INCR_REF);
                }

                if (try self.match(.MINUS_EQUAL)) {
                    try self.expression();
                    return bc.op2(.NEGATE, .INCR_REF);
                }

                if (try self.match(.PLUS_PLUS)) {
                    try bc.i64(1);
                    return bc.op(.INCR_REF);
                }

                if (try self.match(.MINUS_MINUS)) {
                    try bc.i64(-1);
                    return bc.op(.INCR_REF);
                }
            }

            return bc.op(.INDEX_GET);
        }

        fn call(self: *Self) CompileError!void {
            const name = self._previous_token.value.IDENTIFIER;

            try self.advance(); // consume '(

            var arity: u16 = 0;
            if (try self.match(.RIGHT_PARENTHESIS) == false) {
                while (true) {
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

        fn dot(self: *Self, can_assign: bool) CompileError!void {
            try self.consume(.IDENTIFIER, "property name");
            const name = self._previous_token.value.IDENTIFIER;

            var _code: ?i32 = null;
            switch (name.len) {
                3 => switch (@as(u24, @bitCast(name[0..3].*))) {
                    asUint("len") => _code = -1,
                    asUint("key") => _code = -2,
                    else => {},
                },
                5 => switch (@as(u40, @bitCast(name[0..5].*))) {
                    asUint("value") => _code = -3,
                    else => {},
                },
                else => {},
            }

            const code = _code orelse {
                return self.setErrorFmt(error.CompileError, "'{s}' is not a valid property", .{name});
            };

            const bc = &self._byte_code;
            if (can_assign) {
                // if (try self.match(.EQUAL)) {
                //     try self.expression();
                //     return bc.op(.INDEX_SET);
                // }
            }
            try bc.property(code);
            return bc.op(.INDEX_GET);
        }

        fn @"and"(self: *Self, _: bool) CompileError!void {
            // shortcircuit, the left side is already executed, if it's false, we
            // can skip the rest.
            const jump_if_false = try self._jumper.forward(self, .JUMP_IF_FALSE);
            try self.parsePrecedence(.AND);
            try jump_if_false.goto();
        }

        fn @"or"(self: *Self, _: bool) CompileError!void {
            const bc = &self._byte_code;
            // Rather than add a new op (JUMP_IF_TRUE), we can simulate this by
            // combining JUMP_IF_FALSE to jump over a JUMP. IF the left side (
            // which we've already executed) is true, the JUMP_IF_FALSE will
            // be skipped, and we'll execute the JUMP, which will skip the
            // right of the conditions. In other words, the JUMP_IS_FALSE only
            // exists to jump over the JUMP, which exists to shortcircuit on true.
            const jump_if_false = try self._jumper.forward(self, .JUMP_IF_FALSE);

            // the above jump_if_false only exists to skip over this jump
            // and this jump only exists to shortcircuit the condition because the condition is true
            const jump_if_true = try self._jumper.forward(self, .JUMP);
            try jump_if_false.goto();
            try bc.op(.POP);
            try self.parsePrecedence(.OR);
            try jump_if_true.goto();
        }

        fn @"orelse"(self: *Self, _: bool) CompileError!void {
            const bc = &self._byte_code;
            try bc.op(.PUSH);

            try bc.null();
            try bc.op(.EQUAL);

            const jump_if_false = try self._jumper.forward(self, .JUMP_IF_FALSE_POP);
            try bc.op(.POP); // pop off the left hand side (which was false)
            try self.parsePrecedence(.OR);
            try jump_if_false.goto();
        }

        fn ternary(self: *Self, _: bool) CompileError!void {
            const jump_if_false = try self._jumper.forward(self, .JUMP_IF_FALSE_POP);
            try self.expression();
            try self.consume(.COLON, "colon (':')");

            const jump_if_true = try self._jumper.forward(self, .JUMP);
            try jump_if_false.goto();
            try self.expression();
            try jump_if_true.goto();
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

        fn newFunction(self: *Self, name: []const u8) CompileError!Function {
            const data_pos = try self._byte_code.newFunction(name);
            return .{
                .arity = null,
                .code_pos = null,
                .data_pos = data_pos,
            };
        }

        fn scopeDepth(self: *const Self) usize {
            return self._scopes.items.len;
        }

        fn currentScope(self: *const Self) *Scope {
            return &self._scopes.items[self._scopes.items.len - 1];
        }

        fn beginScope(self: *Self, inherit_locals: bool) !void {
            try self._scopes.append(self._arena, .{
                .has_return = false,
                .local_start = if (inherit_locals) self.currentScope().local_start else self._locals.items.len,
            });
        }

        // Fast_pop is true when we're returning from a function.
        // In this case, locals added by the functions don't need to be
        // individually popped, the VM can simply restore the stack back to
        // the caller's previous state.
        fn endScope(self: *Self, comptime fast_pop: bool) !void {
            const scope_pop_count = self.scopePopCount(self.scopeDepth() - 1);
            _ = self._scopes.pop();

            self._locals.items.len -= scope_pop_count;
            if (fast_pop) {
                return;
            }
            var bc = &self._byte_code;
            for (0..scope_pop_count) |_| {
                try bc.op(.POP);
            }
        }

        // Returns the number of variables declared, up to this point, in the
        // current scope. Used in endScope to pop off all block-scoped variables.
        // Used in break & continue, which both must pop off any block-scoped variables.
        fn scopePopCount(self: *Self, depth: usize) usize {
            const locals = self._locals.items;

            var i = locals.len;
            while (i > 0) {
                i -= 1;
                if (locals[i].depth.? <= depth) {
                    return locals.len - i - 1;
                }
            }
            return locals.len;
        }

        pub fn setExpectationError(self: *Self, comptime message: []const u8) CompileError!void {
            const current_token = self._current_token;
            return self.setErrorFmt(error.CompileError, "Expected " ++ message ++ ", got '{s}' ({s})", .{ current_token.src, @tagName(current_token.value) });
        }

        pub fn setErrorFmt(self: *Self, err: anytype, comptime fmt: []const u8, args: anytype) !void {
            const desc = try std.fmt.allocPrint(self._arena, fmt, args);
            return self.setError(err, desc);
        }

        pub fn setError(self: *Self, err: anytype, desc: []const u8) @TypeOf(err)!void {
            self.err = .{
                .desc = desc,
                .position = .{}, // TODO
            };
            return err;
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
// When the function is actually declared, we can fill in those 5 bytes
// The first is the arity, and the other 4 is the address of the actual function
// code.
const Function = struct {
    arity: ?u8 = null,
    data_pos: u32,
    code_pos: ?u32 = null,
};

// The messiest thing our compiler does is deal with JUMP (and its variants).
// 1 - When we're jumping ahead (like when a condition is false), we don't yet know
// where we're jumping to.
// 2 - When we're jumping behind (like when we're returning to the top of the loop),
// then we need to capture the address we intend to jump back to, and insert
// it into the jump.
// 3 - break and continue add a bunch of complexity.
//     - Both need to pop any values of the scope they are breaking/continuing
//     - For a for, a continue jumps forwards to the increment step (which
//       then jumps back to the top of the loop), but for a while, a continue
//       jumps back to the top of the loop
//     - break and continue both take a jump count, i.e. break 2, which makes the
//       above even more complicated.
fn Jumper(comptime App: type) type {

    const OpCode = @import("byte_code.zig").OpCode;

    return struct {
        arena: Allocator,

        // We replace a break; with a JUMP command, but the location where
        // we are jumping to isn't known yet. So we leave an empty placeholder
        // something like {JUMP, 0, 0} and we register the position of the
        // placeholder. Once we know where the break should jump to, we can
        // fill in all placeholders.
        // The position is u32, but the jump address is i16, because the jump
        // is relative to the current position, and we only allow jumping i16
        // bytes (negative because we can jump negative)
        // This is a list of lists, because..
        //   The outer list is like a stack, since we can have nested loops
        //   every new for/while adds a new entry
        //   The inner lists is because 1 loop can have multiple breaks
        //   so we potentially need to register/fill multiple JUMP locations
        break_scopes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(u32)),

        // Same as breaks. In the case of a "while" loop, a continue will jump
        // back to the top of the loop. But in the case of a "for" loop, the
        // continue will jump forwards to the increment part of the loop (and
        // then jump back to the top)
        continue_scopes: std.ArrayListUnmanaged(std.ArrayListUnmanaged(u32)),


        // For each scope, we record how deep we should pop on a break/continue
        pop_depths: std.ArrayListUnmanaged(usize),

        const Self = @This();

        fn init(arena: Allocator) Self {
            return .{
                .arena = arena,
                .pop_depths = .{},
                .break_scopes = .{},
                .continue_scopes = .{}
            };
        }

        fn forward(_: *const Self, compiler: *Compiler(App), op_code: OpCode) !Forward {
            const bc = &compiler._byte_code;

            try bc.op(op_code);

            // create placeholder for jump address
            const jump_from = bc.currentPos();
            try bc.write(&.{ 0, 0 });

            return .{
                .compiler = compiler,
                .jump_from = jump_from,
            };
        }

        fn backward(_: *const Self, compiler: *Compiler(App)) Backward {
            return .{
                .compiler = compiler,
                .jump_to = compiler._byte_code.currentPos(),
            };
        }

        fn newBreakableScope(self: *Self, compiler: *Compiler(App)) !BreakableScope {
            try self.break_scopes.append(self.arena, .{});
            try self.continue_scopes.append(self.arena, .{});
            try self.pop_depths.append(self.arena, compiler.scopeDepth());
            return .{
                .jumper = self,
                .compiler = compiler,
            };
        }

        fn insertBreak(self: *Self, compiler: *Compiler(App), levels: usize) !void {
            return self.recordBreakable(compiler, levels, self.break_scopes.items, "break");
        }

        fn insertContinue(self: *Self, compiler: *Compiler(App), levels: usize) !void {
            return self.recordBreakable(compiler, levels, self.continue_scopes.items, "continue");
        }

        fn recordBreakable(self: *Self, compiler: *Compiler(App), levels: usize, list: []std.ArrayListUnmanaged(u32), comptime op: []const u8) !void {
            const pop_depths = self.pop_depths.items;
            if (levels > pop_depths.len) {
                if (pop_depths.len == 0) {
                    return compiler.setError(error.CompileError, "'" ++ op ++ "' cannot be used outside of loop");
                }
                return compiler.setErrorFmt(error.CompileError, "'" ++ op ++ " {d}' is invalid (current loop nesting: {d})", .{levels, pop_depths.len});
            }

            // so we want to revert the scope by N levels. To figure this out,
            // we look at what the recorded scope was N levels ago. Every time
            // we enter a breakable scope, we record its depth, and on break/continue
            // we can use that recorded depths to know how many values we need
            // to pop from the stack.
            const target_scope = pop_depths[pop_depths.len - levels];
            const pop_count = compiler.scopePopCount(target_scope);

            // TODO: Add POPN op?
            const bc = &compiler._byte_code;
            for (0..pop_count) |_| {
                try bc.op(.POP);
            }

            try bc.op(.JUMP);
           // create placeholder for jump address
            const jump_from = bc.currentPos();
            try bc.write(&.{ 0, 0 });

            var target = &list[list.len - levels];
            return target.append(self.arena, jump_from);
        }

        const Forward = struct {
            jump_from: u32,
            compiler: *Compiler(App),

            pub fn goto(self: Forward) !void {
                const bc = &self.compiler._byte_code;

                // this is where we're jumping from. It's the start of the 2-byte
                // address containing the jump delta.
                const jump_from = self.jump_from;

                // this is where we want to jump to
                const jump_to = bc.currentPos();

                std.debug.assert(jump_to > jump_from);

                // +2 because we need to jump over the jump_from location itself
                const relative: i64 = jump_to - @as(i64, jump_from);
                if (relative > 32_767) {
                    return self.compiler.setError(error.CompileError, "Jump size exceeded maximum allowed value");
                }

                return bc.insertInt(i16, jump_from, @intCast(relative));
            }
        };

        const Backward = struct {
            jump_to: u32,
            compiler: *Compiler(App),

            pub fn goto(self: Backward) !void {
                const bc = &self.compiler._byte_code;

                // This is where we're jumping to
                const jump_to = self.jump_to;

                // This is where we're jumping from (+1 since we need to jump
                // back over the JUMP instruction we're writing).
                const jump_from = bc.currentPos() + 1;

               const relative: i64 = -(@as(i64, jump_from) - jump_to);
                if (relative < -32_768) {
                    return self.compiler.setError(error.CompileError, "Jump size exceeded maximum allowed value");
                }
                var relative_i16: i16 = @intCast(relative);

                try bc.op(.JUMP);
                return bc.write(std.mem.asBytes(&relative_i16));
            }
        };

        const BreakableScope = struct {
            jumper: *Jumper(App),
            compiler: *Compiler(App),

            fn deinit(self: BreakableScope) void {
                _ = self.jumper.pop_depths.pop();
                _ = self.jumper.break_scopes.pop();
                _ = self.jumper.continue_scopes.pop();
            }

            fn breakHere(self: BreakableScope) !void {
                return self.insertJumps(self.jumper.break_scopes.getLast().items, self.compiler._byte_code.currentPos());
            }

            fn continueHere(self: BreakableScope) !void {
                return self.insertJumps(self.jumper.continue_scopes.getLast().items, self.compiler._byte_code.currentPos());
            }

            fn continueAt(self: BreakableScope, jump_to: u32) !void {
                return self.insertJumps(self.jumper.continue_scopes.getLast().items, jump_to);
            }

            fn insertJumps(self: BreakableScope, list: []u32, jump_to: u32) !void {
                var bc = &self.compiler._byte_code;

                for (list) |jump_from| {
                   const relative: i64 = @as(i64, jump_to) - jump_from;
                    if (relative > 32_767 or relative < -32_768) {
                        return self.compiler.setError(error.CompileError, "Jump size exceeded maximum allowed value");
                    }
                    const relative_i16: i16 = @intCast(relative);
                    bc.insertInt(i16, jump_from, @intCast(relative_i16));
                }
            }
        };
    };
}

fn ParseRule(comptime C: type) type {
    return struct {
        infix: ?*const fn (*C, bool) CompileError!void,
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
            .{ Token.Type.DOT, C.dot, null, Precedence.CALL },
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
            .{ Token.Type.LEFT_BRACKET, C.index, C.array, Precedence.CALL },
            .{ Token.Type.LEFT_PARENTHESIS, null, C.grouping, Precedence.NONE },
            .{ Token.Type.LESSER, C.binary, null, Precedence.COMPARISON },
            .{ Token.Type.LESSER_EQUAL, C.binary, null, Precedence.COMPARISON },
            .{ Token.Type.MINUS, C.binary, C.unary, Precedence.TERM },
            .{ Token.Type.NULL, null, C.null, Precedence.NONE },
            .{ Token.Type.OR, C.@"or", null, Precedence.OR },
            .{ Token.Type.ORELSE, C.@"orelse", null, Precedence.OR },
            .{ Token.Type.PERCENT, C.binary, null, Precedence.FACTOR },
            .{ Token.Type.PERCENT_BRACE, null, C.map, Precedence.CALL },
            .{ Token.Type.PLUS, C.binary, null, Precedence.TERM },
            .{ Token.Type.QUESTION_MARK, C.ternary, null, Precedence.OR },
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

const Scope = struct {
    has_return: bool,
    local_start: usize,
};

const Local = struct {
    name: []const u8,
    depth: ?usize,
};
