const std = @import("std");

pub const VM = @import("vm.zig").VM;
pub const Value = @import("value.zig").Value;
pub const DebugMode = @import("config.zig").DebugMode;
pub const Compiler = @import("compiler.zig").Compiler;
pub const Template = @import("template.zig").Template;
pub const disassemble = @import("byte_code.zig").disassemble;

pub const Error = struct {
    desc: []const u8,
    position: ?Position = null,

    pub fn format(self: Error, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        return writer.print("{s}", .{self.desc});
    }
};

pub const Position = struct {
    // the byte in src this token starts at
    pos: u32 = 0,

    // the line this token is on (1-based)
    line: u32 = 0,

    // the byte in src of the line this token is on, the actual token position on
    // the line is at pos - line_start
    line_start: u32 = 0,

    pub const ZERO: Position = .{};
};

const t = @import("t.zig");

test {
    std.testing.refAllDecls(@This());
}

test "ztl: local limit" {
    blk: {
        var c = Compiler(struct {
            pub const ztl_max_locals = 3;
        }).init(t.allocator) catch unreachable;
        defer c.deinit();

        c.compile(
            \\ var a = 1;
            \\ var b = 1;
            \\ var c = 1;
            \\ var d = 1;
        , .{}) catch {
            try t.expectString("Maximum number of local variable (3) exceeded", c.err.?.desc);
            break :blk;
        };
        return error.NoError;
    }

    {
        var c = Compiler(struct {
            pub const ztl_max_locals = 3;
        }).init(t.allocator) catch unreachable;
        defer c.deinit();
        try c.compile(
            \\ var a = 1;
            \\ var b = 1;
            \\ var c = 1;
        , .{});
    }
}

test "ztl: arithmetic" {
    try testReturnValue(.{ .i64 = 9 }, "return 1 + 8;");
    try testReturnValue(.{ .i64 = -1 }, "return 10 - 11;");
    try testReturnValue(.{ .i64 = 14 }, "return 2 * 7;");
    try testReturnValue(.{ .i64 = 2 }, "return 18 / 9;");

    try testReturnValue(.{ .i64 = 17 }, "return 2 + 5 * 3;");
    try testReturnValue(.{ .i64 = 21 }, "return (2 + 5) * 3;");
    try testReturnValue(.{ .i64 = 13 }, "return 2 * 5 + 3;");

    try testReturnValue(.{ .f64 = 4.5 }, "return 1.2 + 3.3;");
    try testReturnValue(.{ .f64 = 5.3 }, "return 2 + 3.3;");
    try testReturnValue(.{ .f64 = 5.3 }, "return 3.3 + 2;");
    try testReturnValue(.{ .f64 = 1.0 }, "return 1.1 - 0.1;");
    try testReturnValue(.{ .f64 = -1.2999999999999998 }, "return 2 - 3.3;");
    try testReturnValue(.{ .f64 = 1.2999999999999998 }, "return 3.3 - 2;");
    try testReturnValue(.{ .f64 = 3.9599999999999995 }, "return 1.2 * 3.3;");
    try testReturnValue(.{ .f64 = 20.4 }, "return 5.1 * 4;");
    try testReturnValue(.{ .f64 = 20.4 }, "return 4 * 5.1;");
    try testReturnValue(.{ .f64 = 0.36363636363636365 }, "return 1.2 / 3.3;");
    try testReturnValue(.{ .f64 = 1.275 }, "return 5.1 / 4;");
    try testReturnValue(.{ .f64 = 0.7843137254901962 }, "return 4 / 5.1;");
}

test "ztl: not" {
    try testReturnValue(.{ .bool = true }, "return !false;");
    try testReturnValue(.{ .bool = false }, "return !true;");
}

test "ztl: comparison int" {
    try testReturnValue(.{ .bool = true }, "return 1 == 1;");
    try testReturnValue(.{ .bool = false }, "return 1 == 2;");
    try testReturnValue(.{ .bool = false }, "return 1 != 1;");
    try testReturnValue(.{ .bool = true }, "return 1 != 2;");

    try testReturnValue(.{ .bool = false }, "return 1 > 1;");
    try testReturnValue(.{ .bool = false }, "return 1 > 2;");
    try testReturnValue(.{ .bool = true }, "return 2 > 1;");

    try testReturnValue(.{ .bool = true }, "return 1 >= 1;");
    try testReturnValue(.{ .bool = false }, "return 1 >= 2;");
    try testReturnValue(.{ .bool = true }, "return 2 >= 1;");

    try testReturnValue(.{ .bool = false }, "return 1 < 1;");
    try testReturnValue(.{ .bool = true }, "return 1 < 2;");
    try testReturnValue(.{ .bool = false }, "return 2 < 1;");

    try testReturnValue(.{ .bool = true }, "return 1 <= 1;");
    try testReturnValue(.{ .bool = true }, "return 1 <= 2;");
    try testReturnValue(.{ .bool = false }, "return 2 <= 1;");
}

test "ztl: comparison float" {
    try testReturnValue(.{ .bool = true }, "return 1.13 == 1.13;");
    try testReturnValue(.{ .bool = false }, "return 1.13 == 2.08;");
    try testReturnValue(.{ .bool = false }, "return 1.13 != 1.13;");
    try testReturnValue(.{ .bool = true }, "return 1.13 != 2.08;");

    try testReturnValue(.{ .bool = false }, "return 1.13 > 1.13;");
    try testReturnValue(.{ .bool = false }, "return 1.13 > 2.08;");
    try testReturnValue(.{ .bool = true }, "return 2.08 > 1.13;");

    try testReturnValue(.{ .bool = true }, "return 1.13 >= 1.13;");
    try testReturnValue(.{ .bool = false }, "return 1.13 >= 2.08;");
    try testReturnValue(.{ .bool = true }, "return 2.08 >= 1.13;");

    try testReturnValue(.{ .bool = false }, "return 1.13 < 1.13;");
    try testReturnValue(.{ .bool = true }, "return 1.13 < 2.08;");
    try testReturnValue(.{ .bool = false }, "return 2.08 < 1.13;");

    try testReturnValue(.{ .bool = true }, "return 1.13 <= 1.13;");
    try testReturnValue(.{ .bool = true }, "return 1.13 <= 2.08;");
    try testReturnValue(.{ .bool = false }, "return 2.08 <= 1.13;");
}

test "ztl: comparison int - float" {
    try testReturnValue(.{ .bool = true }, "return 1 == 1.0;");
    try testReturnValue(.{ .bool = false }, "return 1 == 1.1;");
    try testReturnValue(.{ .bool = false }, "return 1 != 1.0;");
    try testReturnValue(.{ .bool = true }, "return 1 != 1.1;");

    try testReturnValue(.{ .bool = false }, "return 1 > 1.0;");
    try testReturnValue(.{ .bool = false }, "return 1 > 2.0;");
    try testReturnValue(.{ .bool = true }, "return 2 > 1.9;");

    try testReturnValue(.{ .bool = true }, "return 1 >= 1.0;");
    try testReturnValue(.{ .bool = false }, "return 1 >= 2.0;");
    try testReturnValue(.{ .bool = true }, "return 2 >= 1.98;");

    try testReturnValue(.{ .bool = false }, "return 1 < 1.0;");
    try testReturnValue(.{ .bool = true }, "return 1 < 1.01;");
    try testReturnValue(.{ .bool = false }, "return 2 < 1.99;");

    try testReturnValue(.{ .bool = true }, "return 1 <= 1.0;");
    try testReturnValue(.{ .bool = true }, "return 1 <= 1.01;");
    try testReturnValue(.{ .bool = false }, "return 2 <= 1.99;");
}

test "ztl: comparison float - int" {
    try testReturnValue(.{ .bool = true }, "return 1.0 == 1;");
    try testReturnValue(.{ .bool = false }, "return 1.1 == 1;");
    try testReturnValue(.{ .bool = false }, "return 1.0 != 1;");
    try testReturnValue(.{ .bool = true }, "return 1.1 != 1;");

    try testReturnValue(.{ .bool = false }, "return 1.0 > 1;");
    try testReturnValue(.{ .bool = false }, "return 1.9 > 2;");
    try testReturnValue(.{ .bool = true }, "return 2.1 > 2;");

    try testReturnValue(.{ .bool = true }, "return 1.0 >= 1;");
    try testReturnValue(.{ .bool = false }, "return 1.9 >= 2;");
    try testReturnValue(.{ .bool = true }, "return 2.1 >= 2;");

    try testReturnValue(.{ .bool = false }, "return 1.0 < 1;");
    try testReturnValue(.{ .bool = true }, "return 0.99 < 1;");
    try testReturnValue(.{ .bool = false }, "return 2.1 < 2;");

    try testReturnValue(.{ .bool = true }, "return 1.0 <= 1;");
    try testReturnValue(.{ .bool = true }, "return 3.99 <= 4;");
    try testReturnValue(.{ .bool = false }, "return 10.1 <= 10;");
}

test "ztl: comparison bool" {
    try testReturnValue(.{ .bool = true }, "return true == true;");
    try testReturnValue(.{ .bool = true }, "return false == false;");
    try testReturnValue(.{ .bool = false }, "return true == false;");
    try testReturnValue(.{ .bool = false }, "return false == true;");

    try testReturnValue(.{ .bool = false }, "return true != true;");
    try testReturnValue(.{ .bool = false }, "return false != false;");
    try testReturnValue(.{ .bool = true }, "return true != false;");
    try testReturnValue(.{ .bool = true }, "return false != true;");
}

test "ztl: comparison null" {
    try testReturnValue(.{ .bool = true }, "return null == null;");
    try testReturnValue(.{ .bool = false }, "return null != null;");

    try testReturnValue(.{ .bool = false }, "return 0 == null;");
    try testReturnValue(.{ .bool = false }, "return 0.0 == null;");
    try testReturnValue(.{ .bool = false }, "return 1 == null;");
    try testReturnValue(.{ .bool = false }, "return 1.1 == null;");
    try testReturnValue(.{ .bool = false }, "return `` == null;");
    try testReturnValue(.{ .bool = false }, "return `abc` == null;");
    try testReturnValue(.{ .bool = false }, "return true == null;");
    try testReturnValue(.{ .bool = false }, "return false == null;");

    try testReturnValue(.{ .bool = true }, "return 0 != null;");
    try testReturnValue(.{ .bool = true }, "return 0.0 != null;");
    try testReturnValue(.{ .bool = true }, "return 1 != null;");
    try testReturnValue(.{ .bool = true }, "return 1.1 != null;");
    try testReturnValue(.{ .bool = true }, "return `` != null;");
    try testReturnValue(.{ .bool = true }, "return `abc` != null;");
    try testReturnValue(.{ .bool = true }, "return true != null;");
    try testReturnValue(.{ .bool = true }, "return false != null;");
}

test "ztl: comparison string" {
    try testReturnValue(.{ .bool = true }, "return `abc` == `abc`;");
    try testReturnValue(.{ .bool = false }, "return `abc` == `123`;");
    try testReturnValue(.{ .bool = false }, "return `abc` == `ABC`;");

    try testReturnValue(.{ .bool = false }, "return `abc` != `abc`;");
    try testReturnValue(.{ .bool = true }, "return `abc` != `123`;");
    try testReturnValue(.{ .bool = true }, "return `abc` != `ABC`;");

    try testReturnValue(.{ .bool = false }, "return `abc` < `abc`;");
    try testReturnValue(.{ .bool = false }, "return `abc` > `abc`;");
    try testReturnValue(.{ .bool = true }, "return `abc` <= `abc`;");
    try testReturnValue(.{ .bool = true }, "return `abc` >= `abc`;");

    try testReturnValue(.{ .bool = false }, "return `abc` < `ABC`;");
    try testReturnValue(.{ .bool = false }, "return `abc` <= `ABC`;");
    try testReturnValue(.{ .bool = true }, "return `ABC` <= `abc`;");
    try testReturnValue(.{ .bool = true }, "return `ABC` <= `abc`;");

    try testReturnValue(.{ .bool = true }, "return `abc` > `ABC`;");
    try testReturnValue(.{ .bool = true }, "return `abc` >= `ABC`;");
    try testReturnValue(.{ .bool = false }, "return `ABC` >= `abc`;");
    try testReturnValue(.{ .bool = false }, "return `ABC` >= `abc`;");
}

test "ztl: increment/decrement" {
    try testReturnValue(.{ .i64 = 4 },
        \\ var i = 0;
        \\ i++;
        \\ i++;
        \\ i++;
        \\ i--;
        \\ i++;
        \\ return i++;
    );

    try testReturnValue(.{ .i64 = 6 },
        \\ var x = 2;
        \\ x += 4;
        \\ return x ;
    );

    try testReturnValue(.{ .i64 = 6 },
        \\ var x = 2;
        \\ x += 4;
        \\ return x ;
    );

    try testReturnValue(.{ .i64 = -2 },
        \\ var x = 2;
        \\ x -= 4;
        \\ return x ;
    );

    // -1, 1...10 have special treatement, so test a range around there
    inline for (0..20) |i| {
        const src_pos = std.fmt.comptimePrint("var x = 0;\nx += {d};return x;", .{i});
        try testReturnValue(.{ .i64 = i }, src_pos);

        const signed: i64 = @intCast(i);
        const src_neg = std.fmt.comptimePrint("var x = 0;\nx += {d};return x;", .{signed});
        try testReturnValue(.{ .i64 = signed }, src_neg);
    }

    try testError("Expected semicolon (';'), got '++' (PLUS_PLUS)", "return 100++;");
}

test "ztl: variables" {
    defer t.reset();

    try testReturnValue(.{.string = "Leto"},
        \\ var name = `Leto`;
        \\ return name;
    );

    try testReturnValue(.{.string = "LONG"}, "var " ++ "l" ** 127 ++ " = `LONG`; return " ++ "l" ** 127 ++ ";");

    try testReturnValue(.{.string = "Leto"},
        \\ var name = `Leto`;
        \\ {
        \\    var name = "Ghanima" ;
        \\ }
        \\ return name;
    );

    try testReturnValue(.{.string = "other"},
        \\ var name = `Leto`;
        \\ {
        \\    var x = "Ghanima" ;
        \\ }
        \\ var x = "other";
        \\ return x;
    );

    try testReturnValue(.{.string = "Ghanima"},
        \\ var name = `Leto`;
        \\ {
        \\    var name = "Ghanima" ;
        \\    return name;
        \\ }
    );

    try testReturnValue(.{ .i64 = 4 },
        \\ var count = 3;
        \\ return count + 1;
    );

    try testError("Variable 'name' used before being initialized", "var name = name + 3;");
    try testError("Variable 'unknown' is unknown", "return unknown;");
    try testError("Expected assignment operator ('='), got '`hello`' (STRING)", "var x `hello`");

    try testError("Variable 'c' already declare",
        \\ var c = 3;
        \\ var c = 3;
    );

    try testError("Identifier \"" ++ "a" ** 128 ++ "\" exceeds the character limit of 127", "var " ++ "a" ** 128 ++ " = null;");
}

test "ztl: if" {
    // try testReturnValue(.{ .i64 = 1234 },
    //     \\ if (true) {
    //     \\   return 1234;
    //     \\ }
    //     \\ return 4321;
    // );

    try testReturnValue(.{ .i64 = 4321 },
        \\ if (false) {
        \\   return 1234;
        \\ }
        \\ return 4321;
    );

    try testReturnValue(.{ .i64 = 9 },
        \\ if (1 == 1) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );

    try testReturnValue(.{ .i64 = 10 },
        \\ if (1 != 1) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );

    try testReturnValue(.{ .i64 = 8 },
        \\ if (1 == 1) {
        \\   return 8;
        \\ } else if (2 == 2) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );

    try testReturnValue(.{ .i64 = 9 },
        \\ if (1 != 1) {
        \\   return 8;
        \\ } else if (2 == 2) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );

    try testReturnValue(.{ .i64 = 10 },
        \\ if (1 != 1) {
        \\   return 8;
        \\ } else if (2 != 2) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );
}

test "ztl: logical operators" {
    try testReturnValue(.{ .bool = false }, "return 1 == 1 and 3 == 2;");
    try testReturnValue(.{ .bool = false }, "return 0 == 1 and 3 == 2;");
    try testReturnValue(.{ .bool = false }, "return 1 == 3 or 3 == 4;");
    try testReturnValue(.{ .bool = true }, "return 1 == 1 and 2 == 2;");
    try testReturnValue(.{ .bool = true }, "return 1 == 1 or 3 == 2;");
    try testReturnValue(.{ .bool = true }, "return 1 == 3 or 3 == 3;");
    try testReturnValue(.{ .bool = false }, "return 1 == 3 and (3 == 4 or 4 == 4);");
}

test "ztl: while" {
    try testReturnValue(.{ .i64 = 10 },
        \\ var i = 0;
        \\ while (i < 10) {
        \\   i++;
        \\ }
        \\ return i;
    );

    try testReturnValue(.{ .i64 = 0 },
        \\ var i = 0;
        \\ while (false) {
        \\   i = i + 1;
        \\ }
        \\ return i;
    );
}

test "ztl: for" {
    try testReturnValue(.{ .i64 = 10 },
        \\ var i = 0;
        \\ for (var x = 0; x < 10; x = x + 1) {
        \\   i = i + 1;
        \\ }
        \\ return i;
    );

    try testReturnValue(.{ .i64 = 2 },
        \\ var i = 10;
        \\ for (var x = 10; x > 2; x = x - 1) {
        \\   i = i - 1;
        \\ }
        \\ return i;
    );

    // test various incerment/decrement while we're here (++ and --)
    try testReturnValue(.{ .i64 = 10 },
        \\ var i = 0;
        \\ for (var x = 0; x < 10; x++) {
        \\   i++;
        \\ }
        \\ return i;
    );

    try testReturnValue(.{ .i64 = 2 },
        \\ var i = 10;
        \\ for (var x = 10; x > 2; x--) {
        \\   i--;
        \\ }
        \\ return i;
    );

    // test various incerment/decrement while we're here (+= and -=)
    try testReturnValue(.{ .i64 = 8 },
        \\ var i = 0;
        \\ for (var x = 0; x < 10; x += 3) {
        \\   i += 2;
        \\ }
        \\ return i;
    );

    try testReturnValue(.{ .i64 = 4 },
        \\ var i = 10;
        \\ for (var x = 10; x > 2; x -= 3) {
        \\   i -= 2;
        \\ }
        \\ return i;
    );
}

test "ztl: empty scope" {
    try testReturnValue(.{ .null = {} }, "{} return null;"); // doesn't crash, yay!
}

test "ztl: functions" {
    try testReturnValue(.{ .i64 = 25 },
        \\ return value(3);
        \\
        \\ fn value(start) {
        \\   return start + 22;
        \\ }
    );

    // implicit return
    try testReturnValue(.{ .null = {} },
        \\ return value();
        \\
        \\ fn value() {
        \\ }
    );

    try testReturnValue(.{ .i64 = 26 },
        \\ var start = 4;
        \\ var noise = 99;
        \\ return value(start);
        \\
        \\ fn value(start) {
        \\   var noise = 100;
        \\   return start + 22;
        \\ }
    );

    try testReturnValue(.{ .i64 = 4 },
        \\ var x = 4;
        \\ var y = 6;
        \\ return sum(x, y);
        \\
        \\ fn sum(a, b) {
        \\    if (b == 0) {
        \\       return 5;
        \\    }
        \\    return a;
        \\ }
    );

    try testReturnValue(.{ .i64 = 10 },
        \\ fn first() {
        \\   var a = 1;
        \\   var c = second();
        \\   var d = 2;
        \\   return a + c + d;
        \\ }
        \\
        \\ fn second() {
        \\   var y = 3;
        \\   var z = 4;
        \\   return y + z;
        \\ }
        \\
        \\ return first();
    );

    try testReturnValue(.{ .i64 = 134 },
        \\ fn sum(a, b, count) {
        \\    if (count == 0) {
        \\       return magic(a) + b;
        \\    }
        \\    return sum(a + b, b, count - 1);
        \\ }
        \\
        \\ var x = 4;
        \\ var y = 6;
        \\ return sum(x, y, 10);
        \\
        \\
        \\ fn magic(a) {
        \\    if (a % 2 == 0) {
        \\      return a * 2;
        \\    }
        \\    return a;
        \\ }
    );

    try testError("Identifier \"" ++ "x" ** 128 ++ "\" exceeds the character limit of 127", "fn " ++ "x" ** 128 ++ "(){}");

    try testError("Unreachable code detected",
        \\ fn a() {
        \\  return "a";
        \\  return "b";
        \\ }
    );
}

test "ztl: variable scopes" {
    try testReturnValue(.{ .i64 = 100 },
        \\ var i = 0;
        \\ var count = 0;
        \\ while (i < 10) {
        \\   var j = 0;
        \\   while (j < 10) {
        \\      count += 1;
        \\      j += 1;
        \\   }
        \\   i += 1;
        \\ }
        \\ return count;
    );
}

test "ztl: list initialization" {
    defer t.reset();

    {
        try testReturnValue(t.createListRef(&.{}), "return [];");
    }

    {
        var arr = [_]Value{
            .{ .bool = true },
            .{ .f64 = 1.992 },
            .{.string = "over 9000!"},
        };
        try testReturnValue(t.createListRef(&arr), "return [true, 1.992, `over 9000!`];");
    }

    {
        var arr = [_]Value{
            .{ .null = {} },
            .{ .i64 = 1 },
            .{.string = "hello"},
        };
        try testReturnValue(t.createListRef(&arr),
            \\ var n = null;
            \\ var other = "hello";
            \\ return [n, 1, other];
        );
    }
}

test "ztl: list indexing" {
    try testReturnValue(.{ .i64 = 10 }, "return [10, 2002, 5][0];");
    try testReturnValue(.{ .i64 = 2002 }, "return [10, 2002, 5][1];");
    try testReturnValue(.{ .i64 = 5 }, "return [10, 2002, 5][2];");
    try testReturnValue(.{ .i64 = 5 }, "return [10, 2002, 5][-1];");
    try testReturnValue(.{ .i64 = 2002 }, "return [10, 2002, 5][-2];");
    try testReturnValue(.{ .i64 = 10 }, "return [10, 2002, 5][-3];");

    try testRuntimeError("Index out of range. Index: 0, Len: 0", "return [][0];");
    try testRuntimeError("Index out of range. Index: 1, Len: 0", "return [][1];");
    try testRuntimeError("Index out of range. Index: 1, Len: 1", "return [0][1];");
    try testRuntimeError("Index out of range. Index: -1, Len: 0", "return [][-1];");
    try testRuntimeError("Index out of range. Index: -3, Len: 2", "return [1,2][-3];");
}

test "ztl: list assignment" {
    defer t.reset();

    try testReturnValue(.{ .i64 = 10 },
        \\ var arr = [0];
        \\ arr[0] = 10;
        \\ return arr[0];
    );

    try testReturnValue(.{.string = "a"},
        \\ var arr = [0, 1, 2];
        \\ arr[2] = "a";
        \\ return arr[2];
    );

    try testReturnValue(.{ .bool = true },
        \\ var arr = [0, 1, 2];
        \\ arr[-1] = true;
        \\ return arr[2];
    );

    try testReturnValue(.{.string = "x"},
        \\ var arr = [0, 1, 2];
        \\ arr[-2] = "x";
        \\ return arr[1];
    );

    try testReturnValue(.{.string = "a"},
        \\ var arr = [0, 1, 2];
        \\ arr[-3] = "a";
        \\ return arr[0];
    );

    try testReturnValue(.{ .i64 = 11 },
        \\ var arr = [0, 5, 6];
        \\ arr[1] = arr[1] + arr[2];
        \\ return arr[1];
    );

    try testReturnValue(.{ .i64 = 6 },
        \\ var arr = [0, 5, 2];
        \\ arr[1]++;
        \\ return arr[1];
    );

    try testReturnValue(.{ .f64 = 2.2 },
        \\ var arr = [0, 5, 3.2];
        \\ var idx = 2;
        \\ arr[idx]--;
        \\ return arr[idx];
    );

    try testReturnValue(.{ .i64 = 13 },
        \\ var arr = [0, 5, 2];
        \\ arr[1] += 8;
        \\ return arr[1];
    );

    try testReturnValue(.{ .f64 = -7.7 },
        \\ var arr = [0, 5, 3.2];
        \\ var idx = 2;
        \\ arr[idx] -= 10.9;
        \\ return arr[idx];
    );

    try testReturnValue(.{ .f64 = 16.3 },
        \\ var arr = [0, 5, 3.2];
        \\ var idx = 2;
        \\ arr[idx] -= -13.1;
        \\ return arr[idx];
    );

    try testReturnValue(.{ .i64 = 8 },
        \\ var arr = [0, 7, 2];
        \\ return arr[1]++;
    );

    // important to test that the inner array is properly released
    try testReturnValue(.{ .i64 = 2 },
        \\ var arr = [[1]];
        \\ arr[0] = 2;
        \\ return arr[0];
    );

    try testRuntimeError("Index out of range. Index: 0, Len: 0", "[][0] = 1;");
    try testRuntimeError("Index out of range. Index: -1, Len: 0", "[][-1] = 1;");
    try testRuntimeError("Index out of range. Index: 1, Len: 1", "[1][1] = 1;");
    try testRuntimeError("Index out of range. Index: -2, Len: 1", "[1][-2] = 1;");
}

test "ztl: map initialization" {
    defer t.reset();

    {
        try testReturnValue(t.createMapRef(&.{}, &.{}), "return %{};");
    }

    {
        const expected = t.createMapRef(
            &.{"leto", "123", "a key"},
            &.{.{.bool = true}, .{.string = "hello"}, .{.f64 = -1.23}},
        );

        try testReturnValue(expected,
            \\ return %{
            \\   leto: true,
            \\   123: "hello",
            \\   `a key`: -1.23
            \\ };
        );

        // with trailing comma
        try testReturnValue(expected,
            \\ return %{
            \\   leto: true,
            \\   123: "hello",
            \\   `a key`: -1.23,
            \\ };
        );
    }
}

test "ztl: map indexing" {
    try testReturnValue(.{ .i64 = 1 }, "return %{a: 1}[`a`];");
    try testReturnValue(.{ .null = {} }, "return %{a: 1}[`b`];");
    try testReturnValue(.{ .null = {} }, "return %{a: 1}[123];");
    try testReturnValue(.{ .bool = true }, "return %{123: true}[123];");

    try testReturnValue(.{ .bool = true }, "return %{123: true}[123];");
    try testReturnValue(.{ .i64 = 2 },
        \\ var x = %{a: 2};
        \\ var key = "a";
        \\ return x[key];
    );
}

test "ztl: map assignment" {
    defer t.reset();

    try testReturnValue(.{ .i64 = 10 },
        \\ var map = %{0: 2};
        \\ map[0] = 10;
        \\ return map[0];
    );

    try testReturnValue(.{.string = "3"},
        \\ var map = %{"a": 1, "b": 2};
        \\ map["a"] = "3";
        \\ return map["a"];
    );

    try testReturnValue(.{ .i64 = 4 },
        \\ var map = %{"a": 1, "b": 2};
        \\ map["c"] = 4;
        \\ return map["c"];
    );

    try testReturnValue(.{ .i64 = 3 },
        \\ var map = %{"a": 1, "b": 2};
        \\ map["a"] = map["a"] + map["b"];
        \\ return map["a"];
    );

    try testReturnValue(.{ .i64 = -5 },
        \\ var map = %{1: 0, 2: -1, 3: -4};
        \\ map[3]--;
        \\ return map[3];
    );

    try testReturnValue(.{ .i64 = 13 },
        \\ var map = %{a: 1, `b`: 5, c: 2};
        \\ map["b"] += 8;
        \\ return map["b"];
    );

    try testReturnValue(.{ .i64 = 8 },
        \\ var map = %{"count": 7};
        \\ return map["count"]++;
    );

    // important to test that the inner array is properly released
    // with int key
    try testReturnValue(.{ .i64 = 5 },
        \\ var map = %{123: [2]};
        \\ map[123] = 5;
        \\ return map[123];
    );

    // important to test that the inner array is properly released
    // with string key
    try testReturnValue(.{ .i64 = 4 },
        \\ var map = %{"count": [2]};
        \\ map["count"] = 4;
        \\ return map["count"];
    );
}

test "ztl: list length" {
    try testReturnValue(.{ .i64 = 0 }, "return [].len;");
}

test "ztl: string indexing" {
    defer t.reset();

    try testReturnValue(.{.string = "a"}, "return `abc`[0];");
    try testReturnValue(.{.string = "b"}, "return `abc`[1];");
    try testReturnValue(.{.string = "c"}, "return `abc`[2];");
    try testReturnValue(.{.string = "c"}, "return `abc`[-1];");
    try testReturnValue(.{.string = "b"}, "return `abc`[-2];");
    try testReturnValue(.{.string = "a"}, "return `abc`[-3];");

    try testRuntimeError("Index out of range. Index: 0, Len: 0", "return ``[0];");
    try testRuntimeError("Index out of range. Index: 1, Len: 0", "return ``[1];");
    try testRuntimeError("Index out of range. Index: 1, Len: 1", "return `a`[1];");
    try testRuntimeError("Index out of range. Index: -1, Len: 0", "return ``[-1];");
    try testRuntimeError("Index out of range. Index: -3, Len: 2", "return `ab`[-3];");
}

test "ztl: invalid type indexing" {
    try testRuntimeError("Cannot index an integer", "return 0[0];");
    try testRuntimeError("Cannot index a float", "return 12.3[-1];");
    try testRuntimeError("Cannot index a boolean", "return true[0];");
    try testRuntimeError("Cannot index null", "return null[0];");

    try testRuntimeError("Invalid index or property type, got a boolean", "return [][true];");
    try testRuntimeError("Invalid index or property type, got null", "return [][null];");
    try testRuntimeError("Invalid index or property type, got a float", "return [][1.2];");
    try testRuntimeError("Cannot index a list with a string key", "return [][``];");
    try testRuntimeError("Invalid index or property type, got a list", "return [][[]];");
}

test "ztl: orelse" {
    defer t.reset();

    try testReturnValue(.{ .i64 = 4 }, "return 4 orelse 1;");
    try testReturnValue(.{ .i64 = 2 }, "return null orelse 2;");
    try testReturnValue(.{ .i64 = 3 }, "return null orelse 2+1;");
    try testReturnValue(.{.string = "hi"}, "return null orelse null orelse null orelse `hi`;");
    try testReturnValue(.{ .i64 = 1 }, "return 1 orelse null orelse null orelse `hi`;");
}

test "ztl: string dedupe" {
    defer t.reset();

    try testReturnValueWithApp(
        struct { pub const ztl_deduplicate_string_literals = true; },
        .{.string = "hello"},
        \\ var x = "hello";
        \\ var y = "hello";
        \\ return x;
    );

    try testReturnValueWithApp(
        struct { pub const ztl_deduplicate_string_literals = false; },
        .{.string = "hello"},
        \\ var x = "hello";
        \\ var y = "hello";
        \\ return x;
    );
}

test "ztl: break while" {
    try testReturnValue(.{ .i64 = 4 },
        \\ var i = 0;
        \\ while (i < 10) {
        \\   if (i == 4) break;
        \\   i += 1;
        \\ }
        \\ return i;
    );

    // Makes sure the stack is properly restored even on break
    try testReturnValue(.{ .i64 = 2 },
        \\ var i = 0;
        \\ while (i < 10) {
        \\   var noise = 3;
        \\   if (i == 4) break;
        \\   i += 1;
        \\ }
        \\ var y = 2;
        \\ return y;
    );

    try testReturnValue(.{ .i64 = 25 },
        \\ var i = 0;
        \\ var j = 0;
        \\ var x = 0;
        \\ while (i < 20) {
        \\   while (j < 10) {
        \\     if (j == 5) break;
        \\     j += 1;
        \\     x += 1;
        \\   }
        \\   i += 1;
        \\ }
        \\ return i + x;
    );

    try testReturnValue(.{ .i64 = 5 },
        \\ var i = 0;
        \\ var j = 0;
        \\ var x = 0;
        \\ while (i < 20) {
        \\   while ( j < 10) {
        \\     if (j == 5) break 2;
        \\     j += 1;
        \\     x += 1;
        \\   }
        \\   i += 1;
        \\ }
        \\ return i + x;
    );
}

test "ztl: break for" {
    try testReturnValue(.{ .i64 = 4 },
        \\ var i = 0;
        \\ for (; i < 10; i++) {
        \\   if (i == 4) break;
        \\   i += 1;
        \\ }
        \\ return i;
    );

    try testReturnValue(.{ .i64 = 25 },
        \\ var i = 0;
        \\ var j = 0;
        \\ var x = 0;
        \\ for (;i < 20; i++) {
        \\   for (;j < 10; j++) {
        \\     if (j == 5) break;
        \\     x += 1;
        \\   }
        \\   i += 1;
        \\ }
        \\ return i + x;
    );

    try testReturnValue(.{ .i64 = 5 },
        \\ var i = 0;
        \\ var j = 0;
        \\ var x = 0;
        \\ for (;i < 20; i++) {
        \\   for (;j < 10; j++) {
        \\     if (j == 5) break 2;
        \\     x += 1;
        \\   }
        \\ }
        \\ return i + x;
    );
}

test "ztl: continue while" {
    try testReturnValue(.{ .i64 = 5 },
        \\ var i = 0;
        \\ var count = 0;
        \\ while (i < 10) {
        \\   i += 1;
        \\   if (i % 2 == 0) continue;
        \\   count += 1;
        \\ }
        \\ return count;
    );

    try testReturnValue(.{ .i64 = 70 },
        \\ var i = 0;
        \\ var x = 0;
        \\ while (i < 20) {
        \\   var j = 0;
        \\   while (j < 10) {
        \\     j += 1;
        \\     if (i >= 5) continue;
        \\     x += 1;
        \\   }
        \\   i += 1;
        \\ }
        \\ return i + x;
    );

    try testReturnValue(.{ .i64 = 42 },
        \\ var i = 0;
        \\ var x = 0;
        \\ while (i < 20) {
        \\   var j = 0;
        \\   x += 1;
        \\   while (j < 10) {
        \\     j += 1;
        \\     i += 1;
        \\     if (i > 2) continue 2;
        \\     x += 2;
        \\   }
        \\   i += 1;
        \\ }
        \\ return i + x;
    );
}

test "ztl: continue for" {
    try testReturnValue(.{ .i64 = 15 },
        \\ var count = 0;
        \\ for (var i = 0; i < 10; i++) {
        \\   if (i % 2 == 0) {
        \\      count += 2;
        \\      continue;
        \\   }
        \\   count += 1;
        \\ }
        \\ return count;
    );

    try testReturnValue(.{ .i64 = 81 },
        \\ var count = 0;
        \\ for (var i = 0; i < 6; i++) {
        \\   for (var j = 0; j < 5; j++) {
        \\     if (i % 2 == 0) {
        \\        count += 2;
        \\        continue;
        \\     }
        \\     count += 3;
        \\   }
        \\   count += 1;
        \\ }
        \\ return count;
    );

    try testReturnValue(.{ .i64 = 54 },
        \\ var count = 0;
        \\ for (var i = 0; i < 6; i++) {
        \\   for (var j = 0; j < 5; j++) {
        \\     if (i % 2 == 0) {
        \\        count += 2;
        \\        continue 2;
        \\     }
        \\     count += 3;
        \\   }
        \\   count += 1;
        \\ }
        \\ return count;
    );
}

test "ztl: break invalid" {
    try testError("'break' cannot be used outside of loop", "break;");

    try testError("'break' cannot be used outside of loop",
        \\ for (var i = 0; i < 2; i++) {
        \\   add(i, i);
        \\ }
        \\
        \\ fn add(a, b) {
        \\   break;
        \\ }
    );

    try testError("'break 2' is invalid (current loop nesting: 1)",
        \\ for (var i = 0; i < 2; i++) {
        \\   break 2;
        \\ }
    );

    try testError("'break 2' is invalid (current loop nesting: 1)",
        \\ while (true) {
        \\   break 2;
        \\ }
    );
}

test "ztl: continue invalid" {
    try testError("'continue' cannot be used outside of loop", "continue;");

    try testError("'continue' cannot be used outside of loop",
        \\ for (var i = 0; i < 2; i++) {
        \\   add(i, i);
        \\ }
        \\
        \\ fn add(a, b) {
        \\   continue;
        \\ }
    );

    try testError("'continue 2' is invalid (current loop nesting: 1)",
        \\ for (var i = 0; i < 2; i++) {
        \\   continue 2;
        \\ }
    );

    try testError("'continue 2' is invalid (current loop nesting: 1)",
        \\ while (true) {
        \\   continue 2;
        \\ }
    );
}

test "ztl: ternary" {
    try testReturnValue(.{ .i64 = 1 }, "return true ? 1 : 10;");
    try testReturnValue(.{ .i64 = 10 }, "return false ? 1 : 10;");

    try testReturnValue(.{ .i64 = 3 },
        \\ var x = 2;
        \\ x += (x % 2 == 0) ? 1 : 2;
        \\ return x;
    );

    try testReturnValue(.{ .i64 = 5 },
        \\ var x = 3;
        \\ x += (x % 2 == 0) ? 1 : 2;
        \\ return x;
    );
}

test "ztl: list references" {
    try testReturnValue(.{ .i64 = 9 },
        \\ var total = [1];
        \\ {
        \\   var ref = total;
        \\   ref[0] = 9;
        \\ }
        \\ return total[0];
    );
}

test "ztl: foreach" {
    try testReturnValue(.{ .i64 = 5 },
        \\ var total = 5;
        \\ foreach([]) |item| {
        \\   total += item;
        \\ }
        \\ return total;
    );

    try testReturnValue(.{ .i64 = 15 },
        \\ var total = 1;
        \\ {
        \\   var arr = [2, 4, 8];
        \\   foreach(arr) |item| {
        \\    total += item;
        \\   }
        \\ }
        \\ return total;
    );

    try testReturnValue(.{ .i64 = 2 },
        \\ var total = 2;
        \\ {
        \\   foreach(%{}) |kv| {
        \\     total += kv.key + kv.value;
        \\   }
        \\ }
        \\ return total;
    );

    try testReturnValue(.{ .i64 = 627 },
        \\ var total = 1;
        \\ {
        \\   var map = %{10: 12, 300: 304};
        \\   foreach(map) |kv| {
        \\    total += kv.key + kv.value;
        \\   }
        \\ }
        \\ return total;
    );

    try testReturnValue(.{ .i64 = 3627 },
        \\ var total = 1;
        \\ {
        \\   var map = %{10: 12, 300: 304};
        \\   var arr = [1000, 2000];
        \\   foreach(map, arr) |kv, item| {
        \\    total += kv.key + kv.value + item;
        \\   }
        \\ }
        \\ return total;
    );

    // breaks on first short result
    try testReturnValue(.{ .i64 = 1023 },
        \\ var total = 1;
        \\ {
        \\   var map = %{10: 12, 300: 304};
        \\   var arr = [1000];
        \\   foreach(map, arr) |kv, item| {
        \\    total += kv.key + kv.value + item;
        \\   }
        \\ }
        \\ return total;
    );

    // breaks on first short result
    try testReturnValue(.{ .i64 = 1024 },
        \\ var total = 1;
        \\ {
        \\   var map = %{10: 13};
        \\   var arr = [1000, 2000];
        \\   foreach(map, arr) |kv, item| {
        \\    total += kv.key + kv.value + item;
        \\   }
        \\ }
        \\ return total;
    );
}

test "ztl: stack overflow" {
    try testRuntimeError("Maximum call depth (255) reached",
        \\ fn overflow() {
        \\   overflow();
        \\ }
        \\ overflow();
    );
}

fn testReturnValue(expected: Value, src: []const u8) !void {
    try testReturnValueWithApp(struct {
        pub const ztl_debug = DebugMode.full;
    }, expected, src);

    try testReturnValueWithApp(struct {
        pub const ztl_max_locals = 256;
    }, expected, src);

    try testReturnValueWithApp(struct {
        pub const ztl_max_locals = 300;
    }, expected, src);

    try testReturnValueWithApp(void, expected, src);
}

fn testReturnValueWithApp(comptime App: type, expected: Value, src: []const u8) !void {
    var c = try Compiler(App).init(t.allocator);
    defer c.deinit();

    c.compile(src, .{}) catch |err| {
        std.debug.print("Compilation error: {}\n", .{c.err.?});
        return err;
    };

    const byte_code = try c.byteCode(t.allocator);
    defer t.allocator.free(byte_code);
    // disassemble(App, t.allocator, byte_code, std.io.getStdErr().writer()) catch unreachable;

    var vm = VM(App).init(t.allocator);
    defer vm.deinit();

    var buf: std.ArrayListUnmanaged(u8) = .{};
    const value = vm.run(byte_code, buf.writer(t.allocator)) catch |err| {
        std.debug.print("{any}", .{err});
        if (vm.err) |e| {
            std.debug.print("{any} {s}\n", .{ e.err, e.desc });
        }
        disassemble(App, t.allocator, byte_code, std.io.getStdErr().writer()) catch unreachable;
        return err;
    };
    defer vm.release(value);

    const is_equal = expected.equal(value) catch false;
    if (is_equal == false) {
        std.debug.print("{any} != {any}\n", .{ expected, value });
        return error.NotEqual;
    }
}

fn testError(expected: []const u8, src: []const u8) !void {
    var c = Compiler(void).init(t.allocator) catch unreachable;
    defer c.deinit();

    c.compile(src, .{}) catch {
        const ce = c.err orelse unreachable;
        if (std.mem.indexOf(u8, ce.desc, expected) == null) {
            std.debug.print("Wrong error\nexpected: '{s}'\nactual:   '{s}'\n", .{ expected, ce.desc });
            return error.WrongError;
        }
        return;
    };

    return error.NoError;
}

fn testRuntimeError(expected: []const u8, src: []const u8) !void {
    var c = try Compiler(void).init(t.allocator);
    defer c.deinit();

    c.compile(src, .{}) catch |err| {
        std.debug.print("Compilation error: {}\n", .{c.err.?});
        return err;
    };

    const byte_code = try c.byteCode(t.allocator);
    defer t.allocator.free(byte_code);
    // disassemble({}, t.allocator, byte_code, std.io.getStdErr().writer()) catch unreachable;

    var vm = VM(void).init(t.allocator);
    defer vm.deinit();

    var buf: std.ArrayListUnmanaged(u8) = .{};
    _ = vm.run(byte_code, buf.writer(t.allocator)) catch {
        const ve = vm.err orelse unreachable;
        if (std.mem.indexOf(u8, ve.desc, expected) == null) {
            std.debug.print("Wrong error, expected: {s} but got:\n{s}\n", .{ expected, ve.desc });
            return error.WrongError;
        }
        return;
    };
    return error.NoError;
}
