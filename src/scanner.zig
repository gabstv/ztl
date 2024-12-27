const std = @import("std");
const ztl = @import("ztl.zig");

const Allocator = std.mem.Allocator;

const Position = ztl.Position;

const ScanError = error{
    ScanError,
    OutOfMemory,
};

pub const Scanner = struct {
    // currently only used for unescaping strings
    scratch: std.ArrayList(u8),

    // current line in src
    line: u32 = 1,

    // position where the current line starts at
    // pos - line_start tell us where in the line we currently are
    line_start: u32 = 0,

    // where in src we are
    pos: u32 = 0,

    // the source that we're scanning
    src: []const u8,

    arena: Allocator,

    err: ?[]const u8 = null,

    // arena is an ArenaAllocator managed by the caller
    pub fn init(arena: Allocator, src: []const u8) Scanner {
        return .{
            .src = src,
            .arena = arena,
            .scratch = std.ArrayList(u8).init(arena),
        };
    }

    pub fn reset(self: *Scanner, src: []const u8) void {
        self.src = src;
        self.pos = 0;
        self.line = 0;
        self.err = null;
        self.line_start = 0;
        self.scratch.clearRetainingCapacity();
    }

    pub fn next(self: *Scanner) ScanError!Token {
        var pos = self.pos;
        const src = self.src;
        defer self.pos = pos;

        while (pos < src.len) {
            const b = src[pos];
            const start = pos;
            pos += 1;
            switch (b) {
                '{' => return self.createSimpleToken("LEFT_BRACE", "{"),
                '}' => return self.createSimpleToken("RIGHT_BRACE", "}"),
                '[' => return self.createSimpleToken("LEFT_BRACKET", "["),
                ']' => return self.createSimpleToken("RIGHT_BRACKET", "]"),
                '(' => return self.createSimpleToken("LEFT_PARENTHESIS", "("),
                ')' => return self.createSimpleToken("RIGHT_PARENTHESIS", ")"),
                '|' => return self.createSimpleToken("PIPE", ")"),
                ',' => return self.createSimpleToken("COMMA", ","),
                '.' => return self.createSimpleToken("DOT", "."),
                '$' => return self.createSimpleToken("DOLLAR", "$"),
                '?' => return self.createSimpleToken("QUESTION_MARK", "?"),
                '%' => {
                    if (self.at(pos) == '{') {
                        pos += 1;
                        return self.createSimpleToken("PERCENT_BRACE", "%{");
                    }
                    return self.createSimpleToken("PERCENT", "%");
                },
                '+' => {
                    if (self.at(pos) == '+') {
                        pos += 1;
                        return self.createSimpleToken("PLUS_PLUS", "++");
                    }
                    if (self.at(pos) == '=') {
                        pos += 1;
                        return self.createSimpleToken("PLUS_EQUAL", "+=");
                    }
                    return self.createSimpleToken("PLUS", "+");
                },
                '-' => {
                    if (self.at(pos) == '-') {
                        pos += 1;
                        return self.createSimpleToken("MINUS_MINUS", "--");
                    }
                    if (self.at(pos) == '=') {
                        pos += 1;
                        return self.createSimpleToken("MINUS_EQUAL", "-=");
                    }
                    return self.createSimpleToken("MINUS", "-");
                },
                ':' => return self.createSimpleToken("COLON", ":"),
                '*' => return self.createSimpleToken("STAR", "*"),
                ';' => return self.createSimpleToken("SEMICOLON", ";"),
                '/' => {
                    if (self.at(pos) != '/') {
                        return self.createSimpleToken("SLASH", "/");
                    }
                    while (pos < src.len) {
                        if (src[pos] != '\n') {
                            pos += 1;
                        } else {
                            pos += 1;
                            self.pos = pos;
                            self.line += 1;
                            self.line_start = pos;
                            break;
                        }
                    }
                },
                '!' => {
                    if (self.at(pos) == '=') {
                        pos += 1;
                        return self.createSimpleToken("BANG_EQUAL", "!=");
                    }
                    return self.createSimpleToken("BANG", "!");
                },
                '=' => {
                    if (self.at(pos) == '=') {
                        pos += 1;
                        return self.createSimpleToken("EQUAL_EQUAL", "==");
                    }
                    return self.createSimpleToken("EQUAL", "=");
                },
                '>' => {
                    if (self.at(pos) == '=') {
                        pos += 1;
                        return self.createSimpleToken("GREATER_EQUAL", ">=");
                    }
                    return self.createSimpleToken("GREATER", ">");
                },
                '<' => {
                    if (self.at(pos) == '=') {
                        pos += 1;
                        return self.createSimpleToken("LESSER_EQUAL", "<=");
                    }
                    return self.createSimpleToken("LESSER", "<");
                },
                '0'...'9' => return self.number(&pos),
                '"' => return self.string(&pos),
                '`' => {
                    const end = std.mem.indexOfScalarPos(u8, src, pos, '`') orelse {
                        self.err = "unterminated string literal";
                        pos = @intCast(src.len);
                        break;
                    };
                    pos = @intCast(end + 1);
                    return .{
                        .src = src[start..pos],
                        .value = .{ .STRING = src[start + 1 .. end] },
                        .position = self.position(start),
                    };
                },
                ' ', '\t', '\r' => self.pos = pos, // set this now so pos is right for any setError
                '\n' => {
                    self.line += 1;
                    self.line_start = pos;
                },
                'a'...'z', 'A'...'Z', '_', '@' => {
                    if (try self.identifier(&pos)) |token| {
                        return token;
                    }
                    // else an error was recorded, keep parsing
                },
                else => {
                    try self.setErrorFmt("Unexpected character: '{c}'", .{b});
                    return error.ScanError;
                },
            }
        }

        return .{
            .src = "<EOF>",
            .value = .{ .EOF = {} },
            .position = .{
                .pos = @intCast(self.src.len),
                .line = self.line,
                .line_start = self.line_start,
            },
        };
    }

    pub fn peek(self: *Scanner, needles: []const Token.Type) bool {
        const pos = self.pos;
        defer self.pos = pos;
        for (needles) |n| {
            const token = self.next() catch return false;
            if (token.value != n) {
                return false;
            }
        }
        return true;
    }

    fn at(self: *Scanner, pos: usize) u8 {
        const src = self.src;
        return if (pos < src.len) src[pos] else 0;
    }

    // TODO: Newline
    // TODO: optimize unescaping (maybe keep an array of N escape index so that
    // we can quickly copy inbetween without re-checking for \ again)
    // scanner_pos points to the first byte after the opening quote
    fn string(self: *Scanner, scanner_pos: *u32) error{ ScanError, OutOfMemory }!Token {
        var pos = scanner_pos.*;

        const start = pos;
        const src = self.src;

        var escape_count: usize = 0;

        blk: while (pos < src.len) {
            switch (src[pos]) {
                '"' => break :blk,
                '\\' => {
                    escape_count += 1;
                    pos += 1;
                },
                else => {},
            }
            pos += 1;
        }

        if (pos == src.len) {
            scanner_pos.* = pos;
            self.err = "unterminated string literal";
            return error.ScanError;
        }

        scanner_pos.* = pos + 1;

        var literal = src[start..pos];
        if (escape_count > 0) {
            var scratch = &self.scratch;
            scratch.clearRetainingCapacity();

            var i: u32 = 0;
            while (i < literal.len - 1) : (i += 1) {
                switch (literal[i]) {
                    '\\' => {
                        i += 1; // safe because our while loop is going to: i < literal.len - 1
                        switch (literal[i]) {
                            'n' => try scratch.append('\n'),
                            'r' => try scratch.append('\r'),
                            't' => try scratch.append('\t'),
                            '"' => try scratch.append('\"'),
                            '\\' => try scratch.append('\\'),
                            '\'' => try scratch.append('\''),
                            else => |b| {
                                self.pos = start + i;
                                try self.setErrorFmt("invalid escape character: '{c}'", .{b});
                                return error.ScanError;
                            },
                        }
                    },
                    else => |b| try scratch.append(b),
                }
            }

            if (i < literal.len) {
                // copy the last character
                try scratch.append(literal[i]);
            }
            literal = try self.arena.dupe(u8, scratch.items);
        }
        return .{
            .value = .{ .STRING = literal },
            .src = src[start..scanner_pos.*],
            .position = self.position(start),
        };
    }

    // scanner_pos points to the first byte after whatever byte triggered this
    fn number(self: *Scanner, scanner_pos: *u32) error{ ScanError, OutOfMemory }!Token {
        var pos = scanner_pos.*;
        const src = self.src;

        var float = false;
        const start = pos - 1;

        blk: while (pos < src.len) {
            switch (src[pos]) {
                '0'...'9' => {},
                '.' => float = true,
                else => break :blk,
            }
            pos += 1;
        }

        scanner_pos.* = pos;
        const buf = src[start..pos];

        if (float) {
            const value = std.fmt.parseFloat(f64, buf) catch |err| {
                try self.setErrorFmt("invalid float: {s}", .{@errorName(err)});
                return error.ScanError;
            };

            return .{
                .src = buf,
                .value = .{ .FLOAT = value },
                .position = self.position(start),
            };
        }

        const value = std.fmt.parseInt(i64, buf, 10) catch |err| {
            try self.setErrorFmt("invalid integer: {s}", .{@errorName(err)});
            return error.ScanError;
        };

        return .{ .src = buf, .value = .{ .INTEGER = value }, .position = self.position(start) };
    }

    // scanner_pos points to the first byte after whatever byte triggered this
    fn identifier(self: *Scanner, scanner_pos: *u32) !?Token {
        var pos = scanner_pos.*;

        const start = pos - 1;
        const src = self.src;
        blk: while (pos < src.len) {
            switch (src[pos]) {
                'a'...'z', 'A'...'Z', '_', '0'...'9' => pos += 1,
                else => break :blk,
            }
        }
        scanner_pos.* = pos;

        const value = src[start..pos];

        switch (value.len) {
            2 => switch (@as(u16, @bitCast(value[0..2].*))) {
                asUint("fn") => return self.createSimpleToken("FN", value),
                asUint("if") => return self.createSimpleToken("IF", value),
                asUint("or") => return self.createSimpleToken("OR", value),
                else => {},
            },
            3 => switch (@as(u24, @bitCast(value[0..3].*))) {
                asUint("and") => return self.createSimpleToken("AND", value),
                asUint("var") => return self.createSimpleToken("VAR", value),
                asUint("for") => return self.createSimpleToken("FOR", value),
                else => {},
            },
            4 => switch (@as(u32, @bitCast(value[0..4].*))) {
                asUint("else") => return self.createSimpleToken("ELSE", value),
                asUint("null") => return self.createSimpleToken("NULL", value),
                asUint("true") => return self.createToken(.{ .BOOLEAN = true }, value),
                else => {},
            },
            5 => switch (@as(u40, @bitCast(value[0..5].*))) {
                asUint("false") => return self.createToken(.{ .BOOLEAN = false }, value),
                asUint("while") => return self.createSimpleToken("WHILE", value),
                asUint("print") => return self.createSimpleToken("PRINT", value),
                asUint("break") => return self.createSimpleToken("BREAK", value),
                else => {},
            },
            6 => switch (@as(u48, @bitCast(value[0..6].*))) {
                asUint("return") => return self.createSimpleToken("RETURN", value),
                asUint("orelse") => return self.createSimpleToken("ORELSE", value),
                else => {},
            },
            7 => switch (@as(u56, @bitCast(value[0..7].*))) {
                asUint("foreach") => return self.createSimpleToken("FOREACH", value),
                else => {},
            },
            8 => switch (@as(u64, @bitCast(value[0..8].*))) {
                asUint("continue") => return self.createSimpleToken("CONTINUE", value),
                else => {},
            },
            else => {},
        }

        if (value.len > 127) {
            // would be better in the compiler, but so much easier to do here.
            try self.setErrorFmt("Identifier \"{s}\" exceeds the character limit of 127", .{value});
            return error.ScanError;
        }

        return .{
            .src = value,
            .value = .{ .IDENTIFIER = value },
            .position = self.position(start),
        };
    }

    fn createSimpleToken(self: *Scanner, comptime token_type: []const u8, src: []const u8) Token {
        return self.createToken(@unionInit(Token.Value, token_type, {}), src);
    }

    fn createToken(self: *Scanner, value: Token.Value, src: []const u8) Token {
        return .{
            .src = src,
            .value = value,
            .position = self.position(null),
        };
    }

    pub fn position(self: *const Scanner, start: ?u32) Position {
        return .{
            .pos = start orelse self.pos,
            .line = self.line,
            .line_start = self.line_start,
        };
    }

    pub fn srcAt(self: *const Scanner, p: Position) []const u8 {
        return self.src[p.pos..self.pos];
    }

    fn setErrorFmt(self: *Scanner, comptime fmt: []const u8, args: anytype) error{OutOfMemory}!void {
        self.err = try std.fmt.allocPrint(self.arena, fmt, args);
    }
};

pub const Token = struct {
    value: Value,
    src: []const u8,
    position: Position,

    pub const Value = union(Type) {
        AND,
        BANG,
        BANG_EQUAL,
        BOOLEAN: bool,
        BREAK,
        COLON,
        COMMA,
        CONTINUE,
        DOLLAR,
        DOT,
        ELSE,
        EOF,
        EQUAL,
        EQUAL_EQUAL,
        FLOAT: f64,
        FN,
        FOR,
        FOREACH,
        GREATER,
        GREATER_EQUAL,
        IDENTIFIER: []const u8,
        IF,
        INTEGER: i64,
        LEFT_BRACE,
        LEFT_BRACKET,
        LEFT_PARENTHESIS,
        LESSER,
        LESSER_EQUAL,
        MINUS,
        MINUS_EQUAL,
        MINUS_MINUS,
        NULL,
        OR,
        ORELSE,
        PERCENT,
        PERCENT_BRACE,
        PIPE,
        PLUS,
        PLUS_EQUAL,
        PLUS_PLUS,
        PRINT,
        QUESTION_MARK,
        RETURN,
        RIGHT_BRACE,
        RIGHT_BRACKET,
        RIGHT_PARENTHESIS,
        SEMICOLON,
        SLASH,
        STAR,
        START,
        STRING: []const u8,
        VAR,
        WHILE,
    };

    pub const Type = enum {
        AND,
        BANG,
        BANG_EQUAL,
        BOOLEAN,
        BREAK,
        COLON,
        COMMA,
        CONTINUE,
        DOLLAR,
        DOT,
        ELSE,
        EOF,
        EQUAL,
        EQUAL_EQUAL,
        FLOAT,
        FN,
        FOR,
        FOREACH,
        GREATER,
        GREATER_EQUAL,
        IDENTIFIER,
        IF,
        INTEGER,
        LEFT_BRACE,
        LEFT_BRACKET,
        LEFT_PARENTHESIS,
        LESSER,
        LESSER_EQUAL,
        MINUS,
        MINUS_EQUAL,
        MINUS_MINUS,
        NULL,
        OR,
        ORELSE,
        PERCENT,
        PERCENT_BRACE,
        PIPE,
        PLUS,
        PLUS_EQUAL,
        PLUS_PLUS,
        PRINT,
        QUESTION_MARK,
        RETURN,
        RIGHT_BRACE,
        RIGHT_BRACKET,
        RIGHT_PARENTHESIS,
        SEMICOLON,
        SLASH,
        STAR,
        START,
        STRING,
        VAR,
        WHILE,
    };
};

pub fn asUint(comptime string: anytype) @Type(std.builtin.Type{
    .int = .{
        .bits = @bitSizeOf(@TypeOf(string.*)) - 8, // (- 8) to exclude sentinel 0
        .signedness = .unsigned,
    },
}) {
    const byteLength = @bitSizeOf(@TypeOf(string.*)) / 8 - 1;
    const expectedType = *const [byteLength:0]u8;
    if (@TypeOf(string) != expectedType) {
        @compileError("expected : " ++ @typeName(expectedType) ++ ", got: " ++ @typeName(@TypeOf(string)));
    }

    return @bitCast(@as(*const [byteLength]u8, string).*);
}

const t = @import("t.zig");
test "scanner: empty" {
    try expectTokens("", &.{});
    try expectTokens("  ", &.{});
    try expectTokens("//", &.{});
    try expectTokens("// hi", &.{});
}

test "scanner: simple tokens" {
    try expectTokens(" { /  } [ ]\t (\t\t\r),.-+*;", &.{
        .{ .LEFT_BRACE = {} },
        .{ .SLASH = {} },
        .{ .RIGHT_BRACE = {} },
        .{ .LEFT_BRACKET = {} },
        .{ .RIGHT_BRACKET = {} },
        .{ .LEFT_PARENTHESIS = {} },
        .{ .RIGHT_PARENTHESIS = {} },
        .{ .COMMA = {} },
        .{ .DOT = {} },
        .{ .MINUS = {} },
        .{ .PLUS = {} },
        .{ .STAR = {} },
        .{ .SEMICOLON = {} },
    });
}

test "scanner: multibyte tokens" {
    try expectTokens("><! = == != >=\t<= ", &.{
        .{ .GREATER = {} },
        .{ .LESSER = {} },
        .{ .BANG = {} },
        .{ .EQUAL = {} },
        .{ .EQUAL_EQUAL = {} },
        .{ .BANG_EQUAL = {} },
        .{ .GREATER_EQUAL = {} },
        .{ .LESSER_EQUAL = {} },
    });
}

test "scanner: string literals" {
    try expectTokens("\"\"", &.{.{ .STRING = "" }});

    try expectTokens("\"hello world\" == \"Goodbye moon\"", &.{
        .{ .STRING = "hello world" },
        .{ .EQUAL_EQUAL = {} },
        .{ .STRING = "Goodbye moon" },
    });

    try expectTokens("\" \\n \\r \\t \\\" \\' \\\\ \"", &.{.{ .STRING = " \n \r \t \" ' \\ " }});

    try expectTokens("\"\\'\"", &.{.{ .STRING = "'" }});

    try expectTokens("\"abc\"  +  \"123\\'x\"", &.{
        .{ .STRING = "abc" },
        .{ .PLUS = {} },
        .{ .STRING = "123'x" },
    });

    {
        try expectError(" \"abc 123", "unterminated string literal");
        try expectError("\"ab\\\"", "unterminated string literal");
        try expectError(" \"   \\a \" ", "invalid escape character: 'a'");
    }

    try expectTokens("``", &.{.{ .STRING = "" }});

    try expectTokens("`hello world`", &.{.{ .STRING = "hello world" }});

    try expectTokens("`hello\"world`", &.{.{ .STRING = "hello\"world" }});

    try expectTokens("`hel\\nlo` `world`", &.{
        .{ .STRING = "hel\\nlo" },
        .{ .STRING = "world" },
    });
}

test "scanner: numeric literals" {
    try expectTokens("0 1 84 581 12348 893819838298 377178209854757", &.{
        .{ .INTEGER = 0 },
        .{ .INTEGER = 1 },
        .{ .INTEGER = 84 },
        .{ .INTEGER = 581 },
        .{ .INTEGER = 12348 },
        .{ .INTEGER = 893819838298 },
        .{ .INTEGER = 377178209854757 },
    });

    try expectTokens("-581", &.{
        .{ .MINUS = {} },
        .{ .INTEGER = 581 },
    });

    try expectTokens("1.1 3.14159 0.399132785 -49.2291", &.{
        .{ .FLOAT = 1.1 },
        .{ .FLOAT = 3.14159 },
        .{ .FLOAT = 0.399132785 },
        .{ .MINUS = {} },
        .{ .FLOAT = 49.2291 },
    });

    try expectError(" \n 1.2.3", "invalid float: InvalidCharacter");
}

test "scanner: identifier" {
    try expectTokens("cat _VAR hello_world ice9 I9c__302_nadudDD___", &.{
        .{ .IDENTIFIER = "cat" },
        .{ .IDENTIFIER = "_VAR" },
        .{ .IDENTIFIER = "hello_world" },
        .{ .IDENTIFIER = "ice9" },
        .{ .IDENTIFIER = "I9c__302_nadudDD___" },
    });
}

test "scanner: keyword" {
    try expectTokens("and else false fn if null or return true var while", &.{
        .{ .AND = {} },
        .{ .ELSE = {} },
        .{ .BOOLEAN = false },
        .{ .FN = {} },
        .{ .IF = {} },
        .{ .NULL = {} },
        .{ .OR = {} },
        .{ .RETURN = {} },
        .{ .BOOLEAN = true },
        .{ .VAR = {} },
        .{ .WHILE = {} },
    });
}

test "scanner: comments" {
    {
        try expectTokens(
            \\// this might not work\n
            \\ 1.2
            \\ >  // should this be >= ?
            \\ 1.1
        , &.{
            .{ .FLOAT = 1.2 },
            .{ .GREATER = {} },
            .{ .FLOAT = 1.1 },
        });
    }
}

test "scanner: misc" {
    try expectTokens("cat == 9", &.{
        .{ .IDENTIFIER = "cat" },
        .{ .EQUAL_EQUAL = {} },
        .{ .INTEGER = 9 },
    });
}

test "scanner: errors" {
    try expectError("~", "Unexpected character: '~'");
}

fn expectTokens(src: []const u8, expected: []const Token.Value) !void {
    defer t.reset();
    var scanner = Scanner.init(t.arena.allocator(), src);

    for (expected) |e| {
        const token = try scanner.next();
        try t.expectString(@tagName(e), @tagName(token.value));
        switch (e) {
            .STRING => |str| try t.expectString(str, token.value.STRING),
            .IDENTIFIER => |str| try t.expectString(str, token.value.IDENTIFIER),
            else => try t.expectEqual(e, token.value),
        }
    }
    const last = try scanner.next();
    try t.expectEqual(.EOF, last.value);
}

fn expectError(src: []const u8, expected: []const u8) !void {
    defer t.reset();
    var scanner = Scanner.init(t.arena.allocator(), src);

    while (true) {
        const token = scanner.next() catch break;
        if (token.value == .EOF) break;
    }

    const se = scanner.err orelse return error.NoError;
    try t.expectString(expected, se);
}
