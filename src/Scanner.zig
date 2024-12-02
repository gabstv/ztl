const std = @import("std");
const lib = @import("lib.zig");

const Token = lib.Token;
const Position = lib.Position;
const Compiler = lib.Compiler;

const MAX_ERRORS = 10;
const Allocator = std.mem.Allocator;

const AND_BIT = @as(u24, @bitCast([3]u8{ 'a', 'n', 'd' }));
const ELSE_BIT = @as(u32, @bitCast([4]u8{ 'e', 'l', 's', 'e' }));
const FALSE_BIT = @as(u40, @bitCast([5]u8{ 'f', 'a', 'l', 's', 'e' }));
const FN_BIT = @as(u16, @bitCast([2]u8{ 'f', 'n' }));
const IF_BIT = @as(u16, @bitCast([2]u8{ 'i', 'f' }));
const NULL_BIT = @as(u32, @bitCast([4]u8{ 'n', 'u', 'l', 'l' }));
const OR_BIT = @as(u16, @bitCast([2]u8{ 'o', 'r' }));
const RETURN_BIT = @as(u48, @bitCast([6]u8{ 'r', 'e', 't', 'u', 'r', 'n' }));
const TRUE_BIT = @as(u32, @bitCast([4]u8{ 't', 'r', 'u', 'e' }));
const VAR_BIT = @as(u24, @bitCast([3]u8{ 'v', 'a', 'r' }));
const VOID_BIT = @as(u32, @bitCast([4]u8{ 'v', 'o', 'i', 'd' }));
const WHILE_BIT = @as(u40, @bitCast([5]u8{ 'w', 'h', 'i', 'l', 'e' }));
const PRINT_BIT = @as(u40, @bitCast([5]u8{ 'p', 'r', 'i', 'n', 't' }));

const Scanner = @This();

// right now, only used for unescaping strings
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

// We only have this so that we can call compiler.setError.
// There's probably a better design than referencing the whole compiler, but
// this is all internal details and it works, so...
compiler: *Compiler,

pub fn init(compiler: *Compiler, src: []const u8) Scanner {
    return .{
        .src = src,
        .compiler = compiler,
        .scratch = std.ArrayList(u8).init(compiler._arena),
    };
}

pub fn next(self: *Scanner) !Token {
    var pos = self.pos;
    const src = self.src;
    defer self.pos = pos;

    while (pos < src.len) {
        const c = src[pos];
        pos += 1;
        switch (c) {
            '{' => return self.createSimpleToken("LEFT_BRACE"),
            '}' => return self.createSimpleToken("RIGHT_BRACE"),
            '[' => return self.createSimpleToken("LEFT_BRACKET"),
            ']' => return self.createSimpleToken("RIGHT_BRACKET"),
            '(' => return self.createSimpleToken("LEFT_PARENTHESIS"),
            ')' => return self.createSimpleToken("RIGHT_PARENTHESIS"),
            ',' => return self.createSimpleToken("COMMA"),
            '.' => return self.createSimpleToken("DOT"),
            '+' => return self.createSimpleToken("PLUS"),
            '-' => return self.createSimpleToken("MINUS"),
            '*' => return self.createSimpleToken("STAR"),
            ';' => return self.createSimpleToken("SEMICOLON"),
            '/' => {
                if (self.at(pos) != '/') {
                    return self.createSimpleToken("SLASH");
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
                    return self.createSimpleToken("BANG_EQUAL");
                }
                return self.createSimpleToken("BANG");
            },
            '=' => {
                if (self.at(pos) == '=') {
                    pos += 1;
                    return self.createSimpleToken("EQUAL_EQUAL");
                }
                return self.createSimpleToken("EQUAL");
            },
            '>' => {
                if (self.at(pos) == '=') {
                    pos += 1;
                    return self.createSimpleToken("GREATER_EQUAL");
                }
                return self.createSimpleToken("GREATER");
            },
            '<' => {
                if (self.at(pos) == '=') {
                    pos += 1;
                    return self.createSimpleToken("LESSER_EQUAL");
                }
                return self.createSimpleToken("LESSER");
            },
            '0'...'9' => {
                if (try self.number(&pos)) |token| {
                    return token;
                }
                // else error was recorded, keep parsing
            },
            '"' => {
                if (try self.string(&pos)) |token| {
                    return token;
                }
                // else an error was recorded, keep parsing
            },
            '`' => {
                const end = std.mem.indexOfScalarPos(u8, src, pos, '`') orelse {
                    try self.setError(error.InvalidString, "unterminated string literal");
                    pos = @intCast(src.len);
                    break;
                };
                const start = pos;
                pos = @intCast(end + 1);
                return .{
                    .value = .{ .STRING = src[start..end] },
                    .position = self.position(start - 1),
                };
            },
            ' ', '\t', '\r' => self.pos = pos, // set this now so pos is right for any setError
            '\n' => {
                self.line += 1;
                self.line_start = pos;
            },
            'a'...'z', 'A'...'Z', '_' => {
                if (self.identifier(&pos)) |token| {
                    return token;
                }
                // else an error was recorded, keep parsing
            },
            else => try self.setErrorFmt(error.UnexpectedCharacter, "Unexpected character: '{c}'", .{c}),
        }
    }

    return .{
        .value = .{ .EOF = {} },
        .position = .{
            .pos = @intCast(self.src.len),
            .line = self.line,
            .line_start = self.line_start,
        },
    };
}

fn at(self: *Scanner, pos: usize) u8 {
    const src = self.src;
    return if (pos < src.len) src[pos] else 0;
}

// TODO: Newline
// TODO: optimize unescaping (maybe keep an array of N escape index so that
// we can quickly copy inbetween without re-checking for \ again)
// scanner_pos points to the first byte after the opening quote
fn string(self: *Scanner, scanner_pos: *u32) !?Token {
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
        try self.setError(error.InvalidString, "unterminated string literal");
        return null;
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
                            try self.setErrorFmt(error.InvalidEscapeSequence, "invalid escape character: '{c}'", .{b});
                            return null;
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
        literal = scratch.items;
    }

    return .{
        .value = .{ .STRING = literal },
        .position = self.position(start),
    };
}

// scanner_pos points to the first byte after whatever byte triggered this
fn number(self: *Scanner, scanner_pos: *u32) !?Token {
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
            try self.setErrorFmt(error.InvalidFloat, "invalid float: {s}", .{@errorName(err)});
            return null;
        };

        return .{
            .value = .{ .FLOAT = value },
            .position = self.position(start),
        };
    }

    const value = std.fmt.parseInt(i64, buf, 10) catch |err| {
        try self.setErrorFmt(error.InvalidInteger, "invalid integer: {s}", .{@errorName(err)});
        return null;
    };

    return .{ .value = .{ .INTEGER = value }, .position = self.position(start) };
}

// scanner_pos points to the first byte after whatever byte triggered this
fn identifier(self: *Scanner, scanner_pos: *u32) ?Token {
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
        2 => {
            switch (@as(u16, @bitCast(value[0..2].*))) {
                FN_BIT => return self.createSimpleToken("FN"),
                IF_BIT => return self.createSimpleToken("IF"),
                OR_BIT => return self.createSimpleToken("OR"),
                else => {},
            }
        },
        3 => {
            switch (@as(u24, @bitCast(value[0..3].*))) {
                AND_BIT => return self.createSimpleToken("AND"),
                VAR_BIT => return self.createSimpleToken("VAR"),
                else => {},
            }
        },
        4 => {
            switch (@as(u32, @bitCast(value[0..4].*))) {
                ELSE_BIT => return self.createSimpleToken("ELSE"),
                NULL_BIT => return self.createSimpleToken("NULL"),
                TRUE_BIT => return self.createToken(.{ .BOOLEAN = true }),
                VOID_BIT => return self.createSimpleToken("VOID"),
                else => {},
            }
        },
        5 => {
            switch (@as(u40, @bitCast(value[0..5].*))) {
                FALSE_BIT => return self.createToken(.{ .BOOLEAN = false }),
                WHILE_BIT => return self.createSimpleToken("WHILE"),
                PRINT_BIT => return self.createSimpleToken("PRINT"),
                else => {},
            }
        },
        6 => {
            switch (@as(u48, @bitCast(value[0..6].*))) {
                RETURN_BIT => return self.createSimpleToken("RETURN"),
                else => {},
            }
        },
        else => {},
    }

    return .{
        .value = .{ .IDENTIFIER = value },
        .position = self.position(start),
    };
}

fn createSimpleToken(self: *Scanner, comptime token_type: []const u8) Token {
    return self.createToken(@unionInit(Token.Value, token_type, {}));
}

fn createToken(self: *Scanner, value: Token.Value) Token {
    return .{
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

fn setErrorFmt(self: *Scanner, err: ScanError, comptime fmt: []const u8, args: anytype) ScanError!void {
    return self.compiler.setErrorFmt(err, fmt, args, self.position(null)) catch |ce| switch (ce) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return err,
    };
}

fn setError(self: *Scanner, err: ScanError, desc: []const u8) !void {
    self.compiler.setError(err, desc, self.position(null)) catch {};
    return err;
}

pub const ScanError = error{
    OutOfMemory,
    InvalidString,
    InvalidEscapeSequence,
    InvalidFloat,
    InvalidInteger,
    UnexpectedCharacter,
};

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
        try expectError(" \"abc 123", .{ .err = error.InvalidString, .pos = 1, .line = 1, .line_start = 0, .desc = "unterminated string literal" });
        try expectError("\"ab\\\"", .{ .err = error.InvalidString, .pos = 0, .line = 1, .line_start = 0, .desc = "unterminated string literal" });
        try expectError(" \"   \\a \" ", .{ .err = error.InvalidEscapeSequence, .pos = 6, .line = 1, .line_start = 0, .desc = "invalid escape character: 'a'" });
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

    try expectError(" \n 1.2.3", .{ .err = error.InvalidFloat, .pos = 3, .line = 2, .line_start = 2, .desc = "invalid float: InvalidCharacter" });
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
    try expectTokens("and else false fn if null or return true var void while", &.{
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
        .{ .VOID = {} },
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
    try expectError("~", .{ .err = error.UnexpectedCharacter, .pos = 0, .line = 1, .line_start = 0, .desc = "Unexpected character: '~'" });
}

fn expectTokens(src: []const u8, expected: []const Token.Value) !void {
    var compiler = try Compiler.init(t.allocator);
    defer compiler.deinit();

    var scanner = Scanner.init(&compiler, src);

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

fn expectError(src: []const u8, expected: ExpectedError) !void {
    var compiler = try Compiler.init(t.allocator);
    defer compiler.deinit();

    var scanner = Scanner.init(&compiler, src);

    while (true) {
        const token = scanner.next() catch |err| {
            try t.expectEqual(expected.err, err);
            break;
        };
        if (token.value == .EOF) break;
    }

    const ce = compiler.err orelse return error.NoError;
    try t.expectEqual(expected.err, ce.err);
    try t.expectString(expected.desc, ce.desc);

    try t.expectEqual(expected.pos, ce.position.pos);
    try t.expectEqual(expected.line, ce.position.line);
    try t.expectEqual(expected.line_start, ce.position.line_start);
}

const ExpectedError = struct {
    err: anyerror,
    desc: []const u8,
    pos: u32,
    line: u32,
    line_start: u32,
};
