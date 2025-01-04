const std = @import("std");
const ztl = @import("ztl.zig");

const Allocator = std.mem.Allocator;

const asUint = @import("scanner.zig").asUint;
const config = @import("config.zig");
const Value = @import("value.zig").Value;
const Token = @import("scanner.zig").Token;
const Scanner = @import("scanner.zig").Scanner;
const Method = @import("vm.zig").Method;
const Property = @import("vm.zig").Property;
const OpCode = @import("byte_code.zig").OpCode;
const ByteCode = @import("byte_code.zig").ByteCode;
const ErrorReport = @import("error_report.zig").Compile;

const CompileOpts = struct {
    force_locals: []const []const u8 = &.{},
    error_report: ?*ErrorReport = null,
};

pub fn Compiler(comptime A: type) type {
    const App = switch (@typeInfo(A)) {
        .@"struct" => A,
        .pointer => |ptr| ptr.child,
        .void => void,
        else => @compileError("Template App must be a struct, got: " ++ @tagName(@typeInfo(A))),
    };

    const MAX_LOCALS = config.extract(App, "max_locals");
    const DEDUPLICATE_STRING_LITERALS = config.extract(A, "deduplicate_string_literals");

    const CustomFunctions = ztl.Functions(App);

    const CustomFunctionLookup: std.StaticStringMap(CustomFunctionMeta) = if (App == void or @hasDecl(App, "ZtlFunctions") == false) blk: {
        break :blk std.StaticStringMap(CustomFunctionMeta).initComptime(.{});
    } else blk: {
        const fields = @typeInfo(CustomFunctions).@"enum".fields;
        var metas: [fields.len]struct { []const u8, CustomFunctionMeta } = undefined;
        for (fields, 0..) |field, i| {
            metas[i] = .{ field.name, .{
                .function_id = field.value,
                .arity = @field(App.ZtlFunctions, field.name),
            } };
        }
        break :blk std.StaticStringMap(CustomFunctionMeta).initComptime(metas);
    };

    return struct {
        err: ?[]const u8,

        // the ByteCode that our compiler is generating
        writer: ByteCode(App),

        // Arena for memory that can be discarded after compilation. This arena, and
        // its allocator, are NOT used for anything to do with byte code generation.
        // Their main goal is for generating errors.
        arena: Allocator,

        // the index of the current token
        index: usize,
        tokens: []const Token,
        // the position of each token
        positions: []const u32,

        // we just need to keep track of our current token and the
        // previous token to successfully parsed.
        current: Token,
        previous: Token,
        statement_start: u32,

        functions: std.StringHashMapUnmanaged(Function),
        function_calls: std.ArrayListUnmanaged(Function.Call),

        // Used to track the scope that we're in
        // Also assigned to a local to determine in what scope the local can be used
        scopes: std.ArrayListUnmanaged(Scope),
        locals: std.ArrayListUnmanaged(Local),

        // Used to dedupe string literals. Stores the data_start value of
        // a given string. Can be turned off by setting zt_deduplicatestring_literals = false;
        string_literals: std.StringHashMapUnmanaged(u32),

        // Jumping is one of the messier parts of the code. So we try to put as
        // much of the logic into its own struct.
        jumper: Jumper(App),

        const Self = @This();

        // we expect allocator to be an arena
        pub fn init(allocator: Allocator, tokens: []const Token, positions: []const u32) !Self {
            return .{
                .err = null,
                .scopes = .{},
                .locals = .{},
                .functions = .{},
                .arena = allocator,
                .function_calls = .{},
                .string_literals = .{},
                .index = 0,
                .tokens = tokens,
                .positions = positions,
                .jumper = Jumper(App).init(allocator),
                .writer = try ByteCode(App).init(allocator),
                .statement_start = 0,
                .current = .{ .BOF = {} },
                .previous = .{ .BOF = {} },
            };
        }

        pub fn compile(self: *Self, opts: CompileOpts) CompilerError!void {
            errdefer |err| if (opts.error_report) |er| {
                std.debug.print("ERR: {d}\n", .{self.statement_start});
                er.* = .{
                    .src = "", // will be set by our caller
                    .err = err,
                    .pos = self.statement_start,
                    .message = self.err orelse @errorName(err),
                };
            };

            self.advance();
            self.writer.beginScript();
            try self.beginScope(false);

            // see template.zig's hack around globals to understand what this is
            for (opts.force_locals) |name| {
                try self.locals.append(self.arena, .{
                    .depth = 0,
                    .name = name,
                });
            }

            while (self.current != .EOF) {
                try self.declaration();
            }

            var it = self.functions.iterator();
            while (it.next()) |kv| {
                if (kv.value_ptr.code_pos == null) {
                    try self.setErrorFmt("Unknown function: '{s}'", .{kv.key_ptr.*});
                    return error.UnknownFunction;
                }
            }

            for (self.function_calls.items) |fc| {
                const arity = self.functions.get(fc.name).?.arity.?;
                if (fc.arity != arity) {
                    try self.setErrorWrongArity(fc.name, arity, fc.arity);
                    return error.WrongParameterCount;
                }
            }
        }

        fn advance(self: *Self) void {
            self.previous = self.current;

            const index = self.index;
            self.current = self.tokens[index];
            self.index = index + 1;
        }

        fn consumeSemicolon(self: *Self) !void {
            return self.consume(.SEMICOLON, "semicolon (';')");
        }

        fn consume(self: *Self, expected: std.meta.Tag(Token), comptime message: []const u8) CompilerError!void {
            if (try self.match(expected)) {
                return;
            }
            try self.setExpectationError(message);
            return error.UnexpectedToken;
        }

        fn match(self: *Self, expected: std.meta.Tag(Token)) !bool {
            if (@intFromEnum(self.current) == @intFromEnum(expected)) {
                self.advance();
                return true;
            }
            return false;
        }

        fn declaration(self: *Self) CompilerError!void {
            // -1, because, at this point, index is at +1
            self.statement_start = self.positions[self.index - 1];
            switch (self.current) {
                .VAR => {
                    self.advance();
                    return self.variableInitialization();
                },
                .FN => {
                    self.advance();
                    const bc = &self.writer;

                    if (try self.match(.PRINT)) {
                        self.setError("Function 'print' reserved as built-in function");
                        return error.ReservedFunction;
                    }
                    try self.consume(.IDENTIFIER, "function name");
                    const name = self.previous.IDENTIFIER;
                    if (CustomFunctionLookup.get(name) != null) {
                        try self.setErrorFmt("Function '{s}' reserved by custom application function", .{name});
                        return error.ReservedFunction;
                    }

                    try self.consume(.LEFT_PARENTHESIS, "'(' after function name'");

                    try self.beginScope(false);
                    var arity: u16 = 0;
                    if (try self.match(.RIGHT_PARENTHESIS) == false) {
                        // parse argument list
                        const scope_depth = self.scopeDepth();
                        var locals = &self.locals;
                        while (true) {
                            arity += 1;
                            if (arity > 255) {
                                try self.setErrorMaxArity(name);
                                return error.WrongParameterCount;
                            }

                            try self.consume(.IDENTIFIER, "variable name");

                            // Function parameters are just locals to the function
                            // The way the VM loads them means that they'll be
                            // "initialized" in the caller.
                            const variable_name = self.previous.IDENTIFIER;
                            try locals.append(self.arena, .{
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

                    var gop = try self.functions.getOrPut(self.arena, name);
                    if (gop.found_existing) {
                        if (gop.value_ptr.code_pos != null) {
                            try self.setErrorFmt("Function '{s}' already declared", .{name});
                            return error.FunctionRedeclared;
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

        fn variableInitialization(self: *Self) CompilerError!void {
            try self.variableDeclaration(false);

            try self.consume(.EQUAL, "assignment operator ('=')");
            try self.expression();
            try self.consumeSemicolon();

            // prevents things like: var count = count + 1;
            self.locals.items[self.locals.items.len - 1].depth = self.scopeDepth();
        }

        fn variableDeclaration(self: *Self, give_scope: bool) CompilerError!void {
            // "var" already consumed
            try self.consume(.IDENTIFIER, "variable name");

            var locals = &self.locals;

            if (locals.items.len == MAX_LOCALS) {
                try self.setErrorFmt("Maximum number of local variable ({d}) exceeded", .{MAX_LOCALS});
                return error.MaximumLocalsDeclared;
            }

            const name = self.previous.IDENTIFIER;
            const scope_depth = self.scopeDepth();

            if (self.localVariableIndex(name)) |idx| {
                if (locals.items[idx].depth == scope_depth) {
                    try self.setErrorFmt("Variable '{s}' already declared", .{name});
                    return error.VariableRedeclared;
                }
            }

            if (comptime config.shouldDebug(App, .full) == true) {
                try self.writer.debugVariableName(name, @intCast(locals.items.len));
            }

            try locals.append(self.arena, .{
                .name = name,
                .depth = if (give_scope) scope_depth else null,
            });
        }

        fn statement(self: *Self) CompilerError!void {
            const scope = self.currentScope();
            if (scope.has_return) {
                self.setError("Unreachable code detected");
                return error.UnreachableCode;
            }
            var bc = &self.writer;
            var jumper = &self.jumper;
            switch (self.current) {
                .LEFT_BRACE => {
                    self.advance();
                    try self.beginScope(true);
                    try self.block();
                    try self.endScope(false);
                },
                .DOLLAR => {
                    self.advance();
                    const op =  if (try self.match(.DOLLAR)) OpCode.OUTPUT_ESCAPE else OpCode.OUTPUT;
                    try self.expression();
                    try self.consume(.SEMICOLON, "semicolon (';')");
                    try bc.op(op);

                },
                .IF => {
                    self.advance();
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
                    self.advance();
                    try self.consume(.LEFT_PARENTHESIS, "opening parenthesis ('(')");

                    // initializer variable needs its own scope

                    try self.beginScope(true);

                    if (try self.match(.VAR)) {
                        try self.variableInitialization();
                    } else if (try self.match(.SEMICOLON) == false) {
                        try self.expression();
                        try self.consumeSemicolon();
                        try bc.pop();
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
                        try bc.pop();
                        // need to dupe, because the temp space used by scanner
                        // to capture might get reused (in a nested for, for example)
                        // because we use the value later on.
                        incr = try self.arena.dupe(u8, bc.endCapture());
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
                    self.advance();
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
                        self.setError("foreach requires at least 1 value to iterate");
                        return error.InvalidIterableCount;
                    }

                    if (iterable_count > 8) {
                        self.setError("foreach cannot iterate over more tha 8 values");
                        return error.InvalidIterableCount;
                    }

                    try self.locals.appendNTimes(self.arena, .{
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
                        try self.setErrorFmt("foreach must have the same number of iterables as variables (iterables: {d}, variables: {d})", .{ iterable_count, variable_count });
                        return error.InvalidIterableCount;
                    }

                    try bc.opWithData(.FOREACH, &.{@intCast(iterable_count)});

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
                        try bc.pop();
                    }

                    const continue_pos = bc.currentPos();

                    try bc.opWithData(.FOREACH_ITERATE, &.{@intCast(iterable_count)});
                    const jump_if_false = try jumper.forward(self, .JUMP_IF_FALSE_POP);

                    // BODY
                    try self.statement();

                    try breakable_scope.continueAt(continue_pos);

                    // back to condition check
                    try jump_loop_top.goto();
                    try jump_if_false.goto();

                    // any breaks we have registered for this loop will jump here
                    try breakable_scope.breakHere();

                    _ = self.scopes.pop();
                    self.locals.items.len -= variable_count;
                    try self.endScope(false);
                },
                .WHILE => {
                    self.advance();
                    const jump_loop_top = jumper.backward(self); // at the end of each iteration, we want to jump back here

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
                    self.advance();
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
                    self.advance();
                    var break_count: usize = 1;
                    if (try self.match(.INTEGER)) {
                        const value = self.previous.INTEGER;
                        if (value < 0) {
                            try self.setErrorFmt("break count must be a positive integer, got {d}", .{value});
                            return error.InvalidBreakCount;
                        }
                        break_count = @intCast(value);
                    }
                    try self.consumeSemicolon();
                    try self.jumper.insertBreak(self, break_count);
                },
                .CONTINUE => {
                    self.advance();
                    var continue_count: usize = 1;
                    if (try self.match(.INTEGER)) {
                        const value = self.previous.INTEGER;
                        if (value < 0) {
                            try self.setErrorFmt("continue count must be a positive integer, got {d}", .{value});
                            return error.InvalidContinueCount;
                        }
                        continue_count = @intCast(value);
                    }
                    try self.consumeSemicolon();
                    try self.jumper.insertContinue(self, continue_count);
                },
                .PRINT => {
                    self.advance();
                    try self.consume(.LEFT_PARENTHESIS, "left parenthesis ('(')");
                    const arity = try self.parameterList(32);
                    try self.consumeSemicolon();
                    try bc.opWithData(.PRINT, &.{arity});
                },
                else => {
                    try self.expression();
                    try self.consumeSemicolon();
                    try bc.pop();
                },
            }
        }

        fn block(self: *Self) CompilerError!void {
            while (true) {
                switch (self.current) {
                    .RIGHT_BRACE => return self.advance(),
                    .EOF => {
                        try self.setExpectationError("closing block ('}}')");
                        return error.UnexpectedEOF;
                    },
                    else => try self.declaration(),
                }
            }
        }

        fn expression(self: *Self) CompilerError!void {
            return self.parsePrecedence(.ASSIGNMENT);
        }

        fn parsePrecedence(self: *Self, precedence: Precedence) CompilerError!void {
            self.advance();
            const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
            {
                const rule = ParseRule(Self).get(self.previous);
                if (rule.prefix) |prefix| {
                    try prefix(self, can_assign);
                } else {
                    try self.setExpectationError("an expression");
                    return error.Invalid;
                }
            }

            const nprec = @intFromEnum(precedence);
            while (true) {
                const rule = ParseRule(Self).get(self.current);
                if (nprec > rule.precedence) {
                    break;
                }
                self.advance();
                try rule.infix.?(self, can_assign);
            }
        }

        fn grouping(self: *Self, _: bool) CompilerError!void {
            try self.expression();
            return self.consume(.RIGHT_PARENTHESIS, "Expected closing parenthesis ')'");
        }

        fn binary(self: *Self, _: bool) CompilerError!void {
            const previous = self.previous;
            const rule = ParseRule(Self).get(previous);
            try self.parsePrecedence(@enumFromInt(rule.precedence + 1));

            switch (previous) {
                .PLUS => try self.writer.op(.ADD),
                .MINUS => try self.writer.op(.SUBTRACT),
                .STAR => try self.writer.op(.MULTIPLY),
                .SLASH => try self.writer.op(.DIVIDE),
                .EQUAL_EQUAL => try self.writer.op(.EQUAL),
                .BANG_EQUAL => try self.writer.op2(.EQUAL, .NOT),
                .GREATER => try self.writer.op(.GREATER),
                .GREATER_EQUAL => try self.writer.op2(.LESSER, .NOT),
                .LESSER => try self.writer.op(.LESSER),
                .LESSER_EQUAL => try self.writer.op2(.GREATER, .NOT),
                .PERCENT => try self.writer.op(.MODULUS),
                else => unreachable,
            }
        }

        fn unary(self: *Self, _: bool) CompilerError!void {
            const previous = self.previous;

            try self.expression();
            switch (previous) {
                .BANG => try self.writer.op(.NOT),
                .MINUS => try self.writer.op(.NEGATE),
                else => unreachable,
            }
        }

        fn number(self: *Self, _: bool) CompilerError!void {
            switch (self.previous) {
                .INTEGER => |value| try self.writer.i64(value),
                .FLOAT => |value| try self.writer.f64(value),
                else => unreachable,
            }
        }

        fn boolean(self: *Self, _: bool) CompilerError!void {
            return self.writer.bool(self.previous.BOOLEAN);
        }

        fn string(self: *Self, _: bool) CompilerError!void {
            const string_token = self.previous.STRING;
            return self.stringLiteral(string_token);
        }

        fn stringLiteral(self: *Self, literal: []const u8) !void {
            if (DEDUPLICATE_STRING_LITERALS == false) {
                _ = try self.writer.string(literal);
                return;
            }

            if (self.string_literals.get(literal)) |data_start| {
                return self.writer.stringRef(data_start);
            }

            const data_start = try self.writer.string(literal);
            try self.string_literals.put(self.arena, literal, data_start);
        }

        fn @"null"(self: *Self, _: bool) CompilerError!void {
            return self.writer.null();
        }

        fn identifier(self: *Self, can_assign: bool) CompilerError!void {
            if (self.current == .LEFT_PARENTHESIS) {
                return self.call();
            }
            return self.variable(can_assign);
        }

        fn variable(self: *Self, can_assign: bool) CompilerError!void {
            const name = self.previous.IDENTIFIER;

            const idx = self.localVariableIndex(name) orelse {
                try self.setErrorFmt("Variable '{s}' is unknown", .{name});
                return error.UnknownVariable;
            };

            if (self.locals.items[idx].depth == null) {
                try self.setErrorFmt("Variable '{s}' used before being initialized", .{name});
                return error.VaraibleNotInitialized;
            }

            const bc = &self.writer;
            if (can_assign) {
                if (try self.match(.EQUAL)) {
                    try self.expression();
                    return bc.setLocal(@intCast(idx));
                }

                if (try self.match(.PLUS_EQUAL)) {
                    switch (self.current) {
                        .INTEGER => |n| switch (n) {
                            -1 => {
                                self.advance();
                                return bc.incr(@intCast(idx), 0);
                            },
                            1...10 => {
                                self.advance();
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
                    switch (self.current) {
                        .INTEGER => |n| switch (n) {
                            1 => {
                                self.advance();
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

        fn array(self: *Self, _: bool) CompilerError!void {
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
            try self.writer.initializeArray(value_count);
        }

        fn map(self: *Self, _: bool) CompilerError!void {
            var entry_count: u32 = 0;
            var bc = self.writer;
            if (try self.match(.RIGHT_BRACE) == false) {
                while (true) {
                    entry_count += 1;
                    const current_token = self.current;
                    switch (current_token) {
                        .INTEGER => |k| try bc.i64(k),
                        .IDENTIFIER => |k| try self.stringLiteral(k),
                        .STRING => |k| try self.stringLiteral(k),
                        else => {
                            try self.setErrorFmt("Map key must be an integer, string or identifier, got {} ({s})", .{ current_token, @tagName(current_token) });
                            return error.InvalidMapKeyType;

                        },
                    }
                    self.advance();
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
            try self.writer.initializeMap(entry_count);
        }

        fn arrayIndex(self: *Self, can_assign: bool) CompilerError!void {
            try self.expression();
            try self.consume(.RIGHT_BRACKET, "left bracket (']')");

            const bc = &self.writer;
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

        fn call(self: *Self) CompilerError!void {
            const name = self.previous.IDENTIFIER;

            self.advance(); // consume '(

            const arity = try self.parameterList(255);
            if (CustomFunctionLookup.get(name)) |cf| {
                if (cf.arity != arity) {
                    try self.setErrorWrongArity(name, cf.arity, @intCast(arity));
                    return error.WrongParameterCount;
                }

                var buf: [3]u8 = undefined;
                buf[0] = @intCast(arity);
                @memcpy(buf[1..], std.mem.asBytes(&cf.function_id));
                return self.writer.opWithData(.CALL_ZIG, &buf);
            }

            // TODO: check arity (assuming we've declared the function)
            const gop = try self.functions.getOrPut(self.arena, name);
            if (gop.found_existing == false) {
                gop.value_ptr.* = try self.newFunction(name);
            }
            try self.function_calls.append(self.arena, .{ .name = name, .arity = @intCast(arity) });
            return self.writer.opWithData(.CALL, std.mem.asBytes(&gop.value_ptr.data_pos));
        }

        fn dot(self: *Self, _: bool) CompilerError!void {
            try self.consume(.IDENTIFIER, "property name");
            const name = self.previous.IDENTIFIER;

            if (try self.match(.LEFT_PARENTHESIS)) {
                return self.method(name);
            }
            return self.property(name);
        }

        fn method(self: *Self, name: []const u8) CompilerError!void {
            var _method: ?Method = null;

            const arity = try self.parameterList(5);

            switch (name.len) {
                3 => switch (@as(u24, @bitCast(name[0..3].*))) {
                    asUint("pop") => _method = .POP,
                    else => {},
                },
                4 => switch (@as(u32, @bitCast(name[0..4].*))) {
                    asUint("last") => _method = .LAST,
                    asUint("sort") => _method = .SORT,
                    else => {},
                },
                5 => switch (@as(u40, @bitCast(name[0..5].*))) {
                    asUint("first") => _method = .FIRST,
                    else => {},
                },
                6 => switch (@as(u48, @bitCast(name[0..6].*))) {
                    asUint("append") => _method = .APPEND,
                    asUint("remove") => _method = .REMOVE,
                    asUint("concat") => _method = .CONCAT,
                    else => {},
                },
                7 => switch (@as(u56, @bitCast(name[0..7].*))) {
                    asUint("indexOf") => _method = .INDEX_OF,
                    else => {},
                },
                8 => switch (@as(u64, @bitCast(name[0..8].*))) {
                    asUint("contains") => _method = .CONTAINS,
                    asUint("removeAt") => _method = .REMOVE_AT,
                    else => {},
                },
                else => {},
            }

            const m = _method orelse {
                try self.setErrorFmt("'{s}' is not a valid method", .{name});
                return error.UnknownMethod;
            };

            try self.verifyMethodArity(m, arity);

            var buf: [3]u8 = undefined;
            const method_id: u16 = @intFromEnum(m);
            buf[0] = arity;
            @memcpy(buf[1..3], std.mem.asBytes(&method_id));
            return self.writer.opWithData(.METHOD, &buf);
        }

        fn verifyMethodArity(self: *Self, m: Method, arity: u8) !void {
            const expected: u8 = switch (m) {
                .POP => 0,
                .LAST => 0,
                .FIRST => 0,
                .APPEND => 1,
                .REMOVE => 1,
                .REMOVE_AT => 1,
                .CONTAINS => 1,
                .INDEX_OF => 1,
                .SORT => 0,
                .CONCAT => 1,
            };

            if (expected != arity) {
                try self.setErrorWrongArity(m.name(), expected, arity);
                return error.WrongParameterCount;
            }
        }

        fn property(self: *Self, name: []const u8) CompilerError!void {
            var _property: ?Property = null;
            switch (name.len) {
                3 => switch (@as(u24, @bitCast(name[0..3].*))) {
                    asUint("len") => _property = .LEN,
                    asUint("key") => _property = .KEY,
                    else => {},
                },
                5 => switch (@as(u40, @bitCast(name[0..5].*))) {
                    asUint("value") => _property = .VALUE,
                    else => {},
                },
                else => {},
            }

            const p = _property orelse {
                try self.setErrorFmt("'{s}' is not a valid field", .{name});
                return error.UnknownField;
            };

            const property_id: u16 = @intFromEnum(p);
            return self.writer.opWithData(.PROPERTY_GET, std.mem.asBytes(&property_id));
        }

        fn @"and"(self: *Self, _: bool) CompilerError!void {
            // shortcircuit, the left side is already executed, if it's false, we
            // can skip the rest.
            const jump_if_false = try self.jumper.forward(self, .JUMP_IF_FALSE);
            try self.parsePrecedence(.AND);
            try jump_if_false.goto();
        }

        fn @"or"(self: *Self, _: bool) CompilerError!void {
            const bc = &self.writer;
            // Rather than add a new op (JUMP_IF_TRUE), we can simulate this by
            // combining JUMP_IF_FALSE to jump over a JUMP. IF the left side (
            // which we've already executed) is true, the JUMP_IF_FALSE will
            // be skipped, and we'll execute the JUMP, which will skip the
            // right of the conditions. In other words, the JUMP_IS_FALSE only
            // exists to jump over the JUMP, which exists to shortcircuit on true.
            const jump_if_false = try self.jumper.forward(self, .JUMP_IF_FALSE);

            // the above jump_if_false only exists to skip over this jump
            // and this jump only exists to shortcircuit the condition because the condition is true
            const jump_if_true = try self.jumper.forward(self, .JUMP);
            try jump_if_false.goto();
            try bc.pop();
            try self.parsePrecedence(.OR);
            try jump_if_true.goto();
        }

        fn @"orelse"(self: *Self, _: bool) CompilerError!void {
            const bc = &self.writer;
            try bc.op(.PUSH);
            try bc.null();
            try bc.op(.EQUAL);

            const jump_if_false = try self.jumper.forward(self, .JUMP_IF_FALSE_POP);
            try bc.pop(); // pop off the left hand side (which was false)
            try self.parsePrecedence(.OR);
            try jump_if_false.goto();
        }

        fn ternary(self: *Self, _: bool) CompilerError!void {
            const jump_if_false = try self.jumper.forward(self, .JUMP_IF_FALSE_POP);
            try self.expression();
            try self.consume(.COLON, "colon (':')");

            const jump_if_true = try self.jumper.forward(self, .JUMP);
            try jump_if_false.goto();
            try self.expression();
            try jump_if_true.goto();
        }

        fn localVariableIndex(self: *const Self, name: []const u8) ?usize {
            const locals = self.locals.items;
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

        fn newFunction(self: *Self, name: []const u8) CompilerError!Function {
            const data_pos = try self.writer.newFunction(name);
            return .{
                .arity = null,
                .code_pos = null,
                .data_pos = data_pos,
            };
        }

        fn scopeDepth(self: *const Self) usize {
            return self.scopes.items.len;
        }

        fn currentScope(self: *const Self) *Scope {
            return &self.scopes.items[self.scopes.items.len - 1];
        }

        fn beginScope(self: *Self, inherit_locals: bool) !void {
            try self.scopes.append(self.arena, .{
                .has_return = false,
                .local_start = if (inherit_locals) self.currentScope().local_start else self.locals.items.len,
            });
        }

        // Fast_pop is true when we're returning from a function.
        // In this case, locals added by the functions don't need to be
        // individually popped, the VM can simply restore the stack back to
        // the caller's previous state.
        fn endScope(self: *Self, comptime fast_pop: bool) !void {
            const scope_pop_count = self.scopePopCount(self.scopeDepth() - 1);
            _ = self.scopes.pop();

            self.locals.items.len -= scope_pop_count;
            if (fast_pop) {
                return;
            }
            var bc = &self.writer;
            for (0..scope_pop_count) |_| {
                try bc.pop();
            }
        }

        // Returns the number of variables declared, up to this point, in the
        // current scope. Used in endScope to pop off all block-scoped variables.
        // Used in break & continue, which both must pop off any block-scoped variables.
        fn scopePopCount(self: *Self, depth: usize) usize {
            const locals = self.locals.items;

            var i = locals.len;
            while (i > 0) {
                i -= 1;
                if (locals[i].depth.? <= depth) {
                    return locals.len - i - 1;
                }
            }
            return locals.len;
        }

        fn parameterList(self: *Self, max_arity: u8) !u8 {
            if (try self.match(.RIGHT_PARENTHESIS)) {
                return 0;
            }
            var arity: u8 = 0;
            while (true) {
                if (arity == max_arity) {
                    try self.setErrorFmt("call supports up to {d} parameters, got {d}", .{ max_arity, arity });
                    return error.WrongParameterCount;
                }
                arity += 1;
                try self.expression();
                if (try self.match(.RIGHT_PARENTHESIS)) {
                    break;
                }
                try self.consume(.COMMA, "parameter separator (',')");
            }
            return arity;
        }

        fn setExpectationError(self: *Self, comptime message: []const u8) CompilerError!void {
            const current_token = self.current;
            return self.setErrorFmt("Expected " ++ message ++ ", got {} ({s})", .{ current_token, @tagName(current_token) });
        }

        fn setErrorMaxArity(self: *Self, name: []const u8) !void {
            return self.setErrorFmt("Function '{s}' has more than 255 parameters", .{name});
        }

        fn setErrorWrongArity(self: *Self, name: []const u8, expected: u8, actual: u8) !void {
            return self.setErrorFmt("Function '{s}' expects {d} parameter{s}, but called with {d}", .{ name, expected, if (expected == 1) "" else "s", actual });
        }

        fn setErrorFmt(self: *Self, comptime fmt: []const u8, args: anytype) error{OutOfMemory}!void {
            self.err = try std.fmt.allocPrint(self.arena, fmt, args);
        }

        fn setError(self: *Self, desc: []const u8) void {
            self.err = desc;
        }
    };
}

pub const CompilerError = error{
    UnexpectedToken,
    UnknownFunction,
    WrongParameterCount,
    ReservedFunction,
    FunctionRedeclared,
    MaximumLocalsDeclared,
    VariableRedeclared,
    UnreachableCode,
    InvalidContinue,
    InvalidContinueCount,
    InvalidBreak,
    InvalidBreakCount,
    UnknownVariable,
    VaraibleNotInitialized,
    InvalidMapKeyType,
    UnknownMethod,
    UnknownField,
    JumpTooBig,
    InvalidIterableCount,
    UnexpectedEOF,
    Invalid,
} || @import("scanner.zig").Error;

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

    // used at the end of compilation to make sure all function calls had
    // the correct arity (we can't do this at the function call itself, since
    // we might not know the correct arity yet)
    const Call = struct {
        arity: u8,
        name: []const u8,
    };
};

const CustomFunctionMeta = struct {
    arity: u8,
    function_id: u16,
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
            return .{ .arena = arena, .pop_depths = .{}, .break_scopes = .{}, .continue_scopes = .{} };
        }

        fn forward(_: *const Self, compiler: *Compiler(App), op_code: OpCode) !Forward {
            const bc = &compiler.writer;

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
                .jump_to = compiler.writer.currentPos(),
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
                    compiler.setError("'" ++ op ++ "' cannot be used outside of loop");
                    return if (comptime std.mem.eql(u8, op, "continue")) error.InvalidBreak else error.InvalidContinue;
                }
                try compiler.setErrorFmt("'" ++ op ++ " {d}' is invalid (current loop nesting: {d})", .{ levels, pop_depths.len });
                return if (comptime std.mem.eql(u8, op, "continue")) error.InvalidBreakCount else error.InvalidContinueCount;
            }

            // so we want to revert the scope by N levels. To figure this out,
            // we look at what the recorded scope was N levels ago. Every time
            // we enter a breakable scope, we record its depth, and on break/continue
            // we can use that recorded depths to know how many values we need
            // to pop from the stack.
            const target_scope = pop_depths[pop_depths.len - levels];
            const pop_count = compiler.scopePopCount(target_scope);

            // TODO: Add POPN op?
            const bc = &compiler.writer;
            for (0..pop_count) |_| {
                try bc.pop();
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
                const bc = &self.compiler.writer;

                // this is where we're jumping from. It's the start of the 2-byte
                // address containing the jump delta.
                const jump_from = self.jump_from;

                // this is where we want to jump to
                const jump_to = bc.currentPos();

                std.debug.assert(jump_to > jump_from);

                // +2 because we need to jump over the jump_from location itself
                const relative: i64 = jump_to - @as(i64, jump_from);
                if (relative > 32_767) {
                    self.compiler.setError("Jump size exceeded maximum allowed value");
                    return error.JumpTooBig;
                }

                return bc.insertInt(i16, jump_from, @intCast(relative));
            }
        };

        const Backward = struct {
            jump_to: u32,
            compiler: *Compiler(App),

            pub fn goto(self: Backward) !void {
                const bc = &self.compiler.writer;

                // This is where we're jumping to
                const jump_to = self.jump_to;

                // This is where we're jumping from (+1 since we need to jump
                // back over the JUMP instruction we're writing).
                const jump_from = bc.currentPos() + 1;

                const relative: i64 = -(@as(i64, jump_from) - jump_to);
                if (relative < -32_768) {
                    self.compiler.setError("Jump size exceeded maximum allowed value");
                    return error.JumpTooBig;
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
                return self.insertJumps(self.jumper.break_scopes.getLast().items, self.compiler.writer.currentPos());
            }

            fn continueHere(self: BreakableScope) !void {
                return self.insertJumps(self.jumper.continue_scopes.getLast().items, self.compiler.writer.currentPos());
            }

            fn continueAt(self: BreakableScope, jump_to: u32) !void {
                return self.insertJumps(self.jumper.continue_scopes.getLast().items, jump_to);
            }

            fn insertJumps(self: BreakableScope, list: []u32, jump_to: u32) !void {
                var bc = &self.compiler.writer;

                for (list) |jump_from| {
                    const relative: i64 = @as(i64, jump_to) - jump_from;
                    if (relative > 32_767 or relative < -32_768) {
                        self.compiler.setError("Jump size exceeded maximum allowed value");
                        return error.JumpTooBig;
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
        infix: ?*const fn (*C, bool) CompilerError!void,
        prefix: ?*const fn (*C, bool) CompilerError!void,
        precedence: i32,

        const Self = @This();

        inline fn get(token_type: Token) *const Self {
            return &rules[@intFromEnum(token_type)];
        }

        const rules = buildParseRules(&.{
            .{ Token.AND, C.@"and", null, Precedence.AND },
            .{ Token.BANG, null, C.unary, Precedence.NONE },
            .{ Token.BANG_EQUAL, C.binary, null, Precedence.EQUALITY },
            .{ Token.BOOLEAN, null, C.boolean, Precedence.NONE },
            .{ Token.COMMA, null, null, Precedence.NONE },
            .{ Token.DOT, C.dot, null, Precedence.CALL },
            .{ Token.ELSE, null, null, Precedence.NONE },
            .{ Token.EOF, null, null, Precedence.NONE },
            .{ Token.EQUAL, null, null, Precedence.NONE },
            .{ Token.EQUAL_EQUAL, C.binary, null, Precedence.EQUALITY },
            .{ Token.FLOAT, null, C.number, Precedence.NONE },
            .{ Token.GREATER, C.binary, null, Precedence.COMPARISON },
            .{ Token.GREATER_EQUAL, C.binary, null, Precedence.COMPARISON },
            .{ Token.IDENTIFIER, null, C.identifier, Precedence.NONE },
            .{ Token.IF, null, null, Precedence.NONE },
            .{ Token.INTEGER, null, C.number, Precedence.NONE },
            .{ Token.LEFT_BRACE, null, null, Precedence.NONE },
            .{ Token.LEFT_BRACKET, C.arrayIndex, C.array, Precedence.CALL },
            .{ Token.LEFT_PARENTHESIS, null, C.grouping, Precedence.NONE },
            .{ Token.LESSER, C.binary, null, Precedence.COMPARISON },
            .{ Token.LESSER_EQUAL, C.binary, null, Precedence.COMPARISON },
            .{ Token.MINUS, C.binary, C.unary, Precedence.TERM },
            .{ Token.NULL, null, C.null, Precedence.NONE },
            .{ Token.OR, C.@"or", null, Precedence.OR },
            .{ Token.ORELSE, C.@"orelse", null, Precedence.OR },
            .{ Token.PERCENT, C.binary, null, Precedence.FACTOR },
            .{ Token.PERCENT_BRACE, null, C.map, Precedence.CALL },
            .{ Token.PLUS, C.binary, null, Precedence.TERM },
            .{ Token.QUESTION_MARK, C.ternary, null, Precedence.OR },
            .{ Token.RETURN, null, null, Precedence.NONE },
            .{ Token.RIGHT_BRACE, null, null, Precedence.NONE },
            .{ Token.RIGHT_PARENTHESIS, null, null, Precedence.NONE },
            .{ Token.SEMICOLON, null, null, Precedence.NONE },
            .{ Token.SLASH, C.binary, null, Precedence.FACTOR },
            .{ Token.STAR, C.binary, null, Precedence.FACTOR },
            .{ Token.STRING, null, C.string, Precedence.NONE },
            .{ Token.VAR, null, null, Precedence.NONE },
        });

        fn buildParseRules(definitions: anytype) [maxRuleIndex(Token)]Self {
            var _rules: [maxRuleIndex(Token)]Self = undefined;
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
    for (@typeInfo(@typeInfo(E).@"union".tag_type.?).@"enum".fields) |f| {
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
