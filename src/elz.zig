const std = @import("std");

const lib = @import("lib.zig");

pub const VM = lib.VM;
pub const Compiler = lib.Compiler;
pub const ByteCode = lib.ByteCode;

const t = @import("t.zig");

test "elz: arithmetic" {
    try t.expectEqual(9, testSimple("1 + 8").i64);
    try t.expectEqual(-1, testSimple("10 - 11").i64);
    try t.expectEqual(14, testSimple("2 * 7").i64);
    try t.expectEqual(2, testSimple("18 / 9").i64);

    try t.expectEqual(17, testSimple("2 + 5 * 3").i64);
    try t.expectEqual(21, testSimple("(2 + 5) * 3").i64);
    try t.expectEqual(13, testSimple("2 * 5 + 3").i64);

    try t.expectEqual(4.5, testSimple("1.2 + 3.3").f64);
    try t.expectEqual(5.3, testSimple("2 + 3.3").f64);
    try t.expectEqual(5.3, testSimple("3.3 + 2").f64);
    try t.expectEqual(1.0, testSimple("1.1 - 0.1").f64);
    try t.expectEqual(-1.2999999999999998, testSimple("2 - 3.3").f64);
    try t.expectEqual(1.2999999999999998, testSimple("3.3 - 2").f64);
    try t.expectEqual(3.9599999999999995, testSimple("1.2 * 3.3").f64);
    try t.expectEqual(20.4, testSimple("5.1 * 4").f64);
    try t.expectEqual(20.4, testSimple("4 * 5.1").f64);
    try t.expectEqual(0.36363636363636365, testSimple("1.2 / 3.3").f64);
    try t.expectEqual(1.275, testSimple("5.1 / 4").f64);
    try t.expectEqual(0.7843137254901962, testSimple("4 / 5.1").f64);
}

test "elz: not" {
    try t.expectEqual(true, testSimple("!false").bool);
    try t.expectEqual(false, testSimple("!true").bool);
}

test "elz: comparison int" {
    try t.expectEqual(true, testSimple("1 == 1").bool);
    try t.expectEqual(false, testSimple("1 == 2").bool);
    try t.expectEqual(false, testSimple("1 != 1").bool);
    try t.expectEqual(true, testSimple("1 != 2").bool);

    try t.expectEqual(false, testSimple("1 > 1").bool);
    try t.expectEqual(false, testSimple("1 > 2").bool);
    try t.expectEqual(true, testSimple("2 > 1").bool);

    try t.expectEqual(true, testSimple("1 >= 1").bool);
    try t.expectEqual(false, testSimple("1 >= 2").bool);
    try t.expectEqual(true, testSimple("2 >= 1").bool);

    try t.expectEqual(false, testSimple("1 < 1").bool);
    try t.expectEqual(true, testSimple("1 < 2").bool);
    try t.expectEqual(false, testSimple("2 < 1").bool);

    try t.expectEqual(true, testSimple("1 <= 1").bool);
    try t.expectEqual(true, testSimple("1 <= 2").bool);
    try t.expectEqual(false, testSimple("2 <= 1").bool);
}

test "elz: comparison float" {
    try t.expectEqual(true, testSimple("1.13 == 1.13").bool);
    try t.expectEqual(false, testSimple("1.13 == 2.08").bool);
    try t.expectEqual(false, testSimple("1.13 != 1.13").bool);
    try t.expectEqual(true, testSimple("1.13 != 2.08").bool);

    try t.expectEqual(false, testSimple("1.13 > 1.13").bool);
    try t.expectEqual(false, testSimple("1.13 > 2.08").bool);
    try t.expectEqual(true, testSimple("2.08 > 1.13").bool);

    try t.expectEqual(true, testSimple("1.13 >= 1.13").bool);
    try t.expectEqual(false, testSimple("1.13 >= 2.08").bool);
    try t.expectEqual(true, testSimple("2.08 >= 1.13").bool);

    try t.expectEqual(false, testSimple("1.13 < 1.13").bool);
    try t.expectEqual(true, testSimple("1.13 < 2.08").bool);
    try t.expectEqual(false, testSimple("2.08 < 1.13").bool);

    try t.expectEqual(true, testSimple("1.13 <= 1.13").bool);
    try t.expectEqual(true, testSimple("1.13 <= 2.08").bool);
    try t.expectEqual(false, testSimple("2.08 <= 1.13").bool);
}

test "elz: comparison int - float" {
    try t.expectEqual(true, testSimple("1 == 1.0").bool);
    try t.expectEqual(false, testSimple("1 == 1.1").bool);
    try t.expectEqual(false, testSimple("1 != 1.0").bool);
    try t.expectEqual(true, testSimple("1 != 1.1").bool);

    try t.expectEqual(false, testSimple("1 > 1.0").bool);
    try t.expectEqual(false, testSimple("1 > 2.0").bool);
    try t.expectEqual(true, testSimple("2 > 1.9").bool);

    try t.expectEqual(true, testSimple("1 >= 1.0").bool);
    try t.expectEqual(false, testSimple("1 >= 2.0").bool);
    try t.expectEqual(true, testSimple("2 >= 1.98").bool);

    try t.expectEqual(false, testSimple("1 < 1.0").bool);
    try t.expectEqual(true, testSimple("1 < 1.01").bool);
    try t.expectEqual(false, testSimple("2 < 1.99").bool);

    try t.expectEqual(true, testSimple("1 <= 1.0").bool);
    try t.expectEqual(true, testSimple("1 <= 1.01").bool);
    try t.expectEqual(false, testSimple("2 <= 1.99").bool);
}

test "elz: comparison float - int" {
    try t.expectEqual(true, testSimple("1.0 == 1").bool);
    try t.expectEqual(false, testSimple("1.1 == 1").bool);
    try t.expectEqual(false, testSimple("1.0 != 1").bool);
    try t.expectEqual(true, testSimple("1.1 != 1").bool);

    try t.expectEqual(false, testSimple("1.0 > 1").bool);
    try t.expectEqual(false, testSimple("1.9 > 2").bool);
    try t.expectEqual(true, testSimple("2.1 > 2").bool);

    try t.expectEqual(true, testSimple("1.0 >= 1").bool);
    try t.expectEqual(false, testSimple("1.9 >= 2").bool);
    try t.expectEqual(true, testSimple("2.1 >= 2").bool);

    try t.expectEqual(false, testSimple("1.0 < 1").bool);
    try t.expectEqual(true, testSimple("0.99 < 1").bool);
    try t.expectEqual(false, testSimple("2.1 < 2").bool);

    try t.expectEqual(true, testSimple("1.0 <= 1").bool);
    try t.expectEqual(true, testSimple("3.99 <= 4").bool);
    try t.expectEqual(false, testSimple("10.1 <= 10").bool);
}

test "elz: comparison bool" {
    try t.expectEqual(true, testSimple("true == true").bool);
    try t.expectEqual(true, testSimple("false == false").bool);
    try t.expectEqual(false, testSimple("true == false").bool);
    try t.expectEqual(false, testSimple("false == true").bool);

    try t.expectEqual(false, testSimple("true != true").bool);
    try t.expectEqual(false, testSimple("false != false").bool);
    try t.expectEqual(true, testSimple("true != false").bool);
    try t.expectEqual(true, testSimple("false != true").bool);
}

test "elz: comparison null" {
    try t.expectEqual(true, testSimple("null == null").bool);
    try t.expectEqual(false, testSimple("null != null").bool);

    try t.expectEqual(false, testSimple("0 == null").bool);
    try t.expectEqual(false, testSimple("0.0 == null").bool);
    try t.expectEqual(false, testSimple("1 == null").bool);
    try t.expectEqual(false, testSimple("1.1 == null").bool);
    try t.expectEqual(false, testSimple("`` == null").bool);
    try t.expectEqual(false, testSimple("`abc` == null").bool);
    try t.expectEqual(false, testSimple("true == null").bool);
    try t.expectEqual(false, testSimple("false == null").bool);

    try t.expectEqual(true, testSimple("0 != null").bool);
    try t.expectEqual(true, testSimple("0.0 != null").bool);
    try t.expectEqual(true, testSimple("1 != null").bool);
    try t.expectEqual(true, testSimple("1.1 != null").bool);
    try t.expectEqual(true, testSimple("`` != null").bool);
    try t.expectEqual(true, testSimple("`abc` != null").bool);
    try t.expectEqual(true, testSimple("true != null").bool);
    try t.expectEqual(true, testSimple("false != null").bool);
}

test "elz: comparison string" {
    try t.expectEqual(true, testSimple("`abc` == `abc`").bool);
    try t.expectEqual(false, testSimple("`abc` == `123`").bool);
    try t.expectEqual(false, testSimple("`abc` == `ABC`").bool);

    try t.expectEqual(false, testSimple("`abc` != `abc`").bool);
    try t.expectEqual(true, testSimple("`abc` != `123`").bool);
    try t.expectEqual(true, testSimple("`abc` != `ABC`").bool);

    try t.expectEqual(false, testSimple("`abc` < `abc`").bool);
    try t.expectEqual(false, testSimple("`abc` > `abc`").bool);
    try t.expectEqual(true, testSimple("`abc` <= `abc`").bool);
    try t.expectEqual(true, testSimple("`abc` >= `abc`").bool);

    try t.expectEqual(false, testSimple("`abc` < `ABC`").bool);
    try t.expectEqual(false, testSimple("`abc` <= `ABC`").bool);
    try t.expectEqual(true, testSimple("`ABC` <= `abc`").bool);
    try t.expectEqual(true, testSimple("`ABC` <= `abc`").bool);

    try t.expectEqual(true, testSimple("`abc` > `ABC`").bool);
    try t.expectEqual(true, testSimple("`abc` >= `ABC`").bool);
    try t.expectEqual(false, testSimple("`ABC` >= `abc`").bool);
    try t.expectEqual(false, testSimple("`ABC` >= `abc`").bool);
}

fn testSimple(src: []const u8) lib.Value {
    var c = Compiler.init(t.allocator) catch unreachable;
    defer c.deinit();

    c.compile(src) catch |err| {
        std.debug.print("==={any}===\n", .{err});
        for (c.errors()) |ce| {
            std.debug.print("{any} - {s}\n", .{ce.err, ce.desc});
        }
        unreachable;
    };

    const byte_code = c.byteCode(t.allocator) catch unreachable;
    defer t.allocator.free(byte_code);

    var vm = VM.init(t.allocator);
    defer vm.deinit();

    const value = vm.run(byte_code) catch |err| {
        std.debug.print("{any}", .{err});
        if (vm.err) |e| {
            std.debug.print("{any} {s}", .{e.err, e.desc});
        }
        unreachable;
    };
    return value;
}
