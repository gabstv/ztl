const std = @import("std");

pub const VM = @import("vm.zig").VM;
pub const Value = @import("value.zig").Value;
pub const Config = @import("config.zig").Config;
pub const Compiler = @import("compiler.zig").Compiler;
pub const disassemble = @import("byte_code.zig").disassemble;

pub const Preset = struct {
    pub const small = Config{
        .max_locals = 256,
        .initial_code_size = 256,
        .initial_data_size = 256,
    };
};

const t = @import("t.zig");

test "elz: local limit" {
    var c = Compiler(.{.max_locals = 3}).init(t.allocator) catch unreachable;
    defer c.deinit();

    blk: {
        c.compile(
            \\ var a = 1;
            \\ var b = 1;
            \\ var c = 1;
            \\ var d = 1;
        ) catch {
            try t.expectString("maximum number of local variable (3) exceeded", c.err.?.desc);
            break :blk;
        };
        return error.NoError;
    }

    {
        c.reset(4096);
        try c.compile(
            \\ var a = 1;
            \\ var b = 1;
            \\ var c = 1;
        );
    }
}

test "elz: arithmetic" {
    try t.expectEqual(9, testSimple("return 1 + 8;").i64);
    try t.expectEqual(-1, testSimple("return 10 - 11;").i64);
    try t.expectEqual(14, testSimple("return 2 * 7;").i64);
    try t.expectEqual(2, testSimple("return 18 / 9;").i64);

    try t.expectEqual(17, testSimple("return 2 + 5 * 3;").i64);
    try t.expectEqual(21, testSimple("return (2 + 5) * 3;").i64);
    try t.expectEqual(13, testSimple("return 2 * 5 + 3;").i64);

    try t.expectEqual(4.5, testSimple("return 1.2 + 3.3;").f64);
    try t.expectEqual(5.3, testSimple("return 2 + 3.3;").f64);
    try t.expectEqual(5.3, testSimple("return 3.3 + 2;").f64);
    try t.expectEqual(1.0, testSimple("return 1.1 - 0.1;").f64);
    try t.expectEqual(-1.2999999999999998, testSimple("return 2 - 3.3;").f64);
    try t.expectEqual(1.2999999999999998, testSimple("return 3.3 - 2;").f64);
    try t.expectEqual(3.9599999999999995, testSimple("return 1.2 * 3.3;").f64);
    try t.expectEqual(20.4, testSimple("return 5.1 * 4;").f64);
    try t.expectEqual(20.4, testSimple("return 4 * 5.1;").f64);
    try t.expectEqual(0.36363636363636365, testSimple("return 1.2 / 3.3;").f64);
    try t.expectEqual(1.275, testSimple("return 5.1 / 4;").f64);
    try t.expectEqual(0.7843137254901962, testSimple("return 4 / 5.1;").f64);
}

test "elz: not" {
    try t.expectEqual(true, testSimple("return !false;").bool);
    try t.expectEqual(false, testSimple("return !true;").bool);
}

test "elz: comparison int" {
    try t.expectEqual(true, testSimple("return 1 == 1;").bool);
    try t.expectEqual(false, testSimple("return 1 == 2;").bool);
    try t.expectEqual(false, testSimple("return 1 != 1;").bool);
    try t.expectEqual(true, testSimple("return 1 != 2;").bool);

    try t.expectEqual(false, testSimple("return 1 > 1;").bool);
    try t.expectEqual(false, testSimple("return 1 > 2;").bool);
    try t.expectEqual(true, testSimple("return 2 > 1;").bool);

    try t.expectEqual(true, testSimple("return 1 >= 1;").bool);
    try t.expectEqual(false, testSimple("return 1 >= 2;").bool);
    try t.expectEqual(true, testSimple("return 2 >= 1;").bool);

    try t.expectEqual(false, testSimple("return 1 < 1;").bool);
    try t.expectEqual(true, testSimple("return 1 < 2;").bool);
    try t.expectEqual(false, testSimple("return 2 < 1;").bool);

    try t.expectEqual(true, testSimple("return 1 <= 1;").bool);
    try t.expectEqual(true, testSimple("return 1 <= 2;").bool);
    try t.expectEqual(false, testSimple("return 2 <= 1;").bool);
}

test "elz: comparison float" {
    try t.expectEqual(true, testSimple("return 1.13 == 1.13;").bool);
    try t.expectEqual(false, testSimple("return 1.13 == 2.08;").bool);
    try t.expectEqual(false, testSimple("return 1.13 != 1.13;").bool);
    try t.expectEqual(true, testSimple("return 1.13 != 2.08;").bool);

    try t.expectEqual(false, testSimple("return 1.13 > 1.13;").bool);
    try t.expectEqual(false, testSimple("return 1.13 > 2.08;").bool);
    try t.expectEqual(true, testSimple("return 2.08 > 1.13;").bool);

    try t.expectEqual(true, testSimple("return 1.13 >= 1.13;").bool);
    try t.expectEqual(false, testSimple("return 1.13 >= 2.08;").bool);
    try t.expectEqual(true, testSimple("return 2.08 >= 1.13;").bool);

    try t.expectEqual(false, testSimple("return 1.13 < 1.13;").bool);
    try t.expectEqual(true, testSimple("return 1.13 < 2.08;").bool);
    try t.expectEqual(false, testSimple("return 2.08 < 1.13;").bool);

    try t.expectEqual(true, testSimple("return 1.13 <= 1.13;").bool);
    try t.expectEqual(true, testSimple("return 1.13 <= 2.08;").bool);
    try t.expectEqual(false, testSimple("return 2.08 <= 1.13;").bool);
}

test "elz: comparison int - float" {
    try t.expectEqual(true, testSimple("return 1 == 1.0;").bool);
    try t.expectEqual(false, testSimple("return 1 == 1.1;").bool);
    try t.expectEqual(false, testSimple("return 1 != 1.0;").bool);
    try t.expectEqual(true, testSimple("return 1 != 1.1;").bool);

    try t.expectEqual(false, testSimple("return 1 > 1.0;").bool);
    try t.expectEqual(false, testSimple("return 1 > 2.0;").bool);
    try t.expectEqual(true, testSimple("return 2 > 1.9;").bool);

    try t.expectEqual(true, testSimple("return 1 >= 1.0;").bool);
    try t.expectEqual(false, testSimple("return 1 >= 2.0;").bool);
    try t.expectEqual(true, testSimple("return 2 >= 1.98;").bool);

    try t.expectEqual(false, testSimple("return 1 < 1.0;").bool);
    try t.expectEqual(true, testSimple("return 1 < 1.01;").bool);
    try t.expectEqual(false, testSimple("return 2 < 1.99;").bool);

    try t.expectEqual(true, testSimple("return 1 <= 1.0;").bool);
    try t.expectEqual(true, testSimple("return 1 <= 1.01;").bool);
    try t.expectEqual(false, testSimple("return 2 <= 1.99;").bool);
}

test "elz: comparison float - int" {
    try t.expectEqual(true, testSimple("return 1.0 == 1;").bool);
    try t.expectEqual(false, testSimple("return 1.1 == 1;").bool);
    try t.expectEqual(false, testSimple("return 1.0 != 1;").bool);
    try t.expectEqual(true, testSimple("return 1.1 != 1;").bool);

    try t.expectEqual(false, testSimple("return 1.0 > 1;").bool);
    try t.expectEqual(false, testSimple("return 1.9 > 2;").bool);
    try t.expectEqual(true, testSimple("return 2.1 > 2;").bool);

    try t.expectEqual(true, testSimple("return 1.0 >= 1;").bool);
    try t.expectEqual(false, testSimple("return 1.9 >= 2;").bool);
    try t.expectEqual(true, testSimple("return 2.1 >= 2;").bool);

    try t.expectEqual(false, testSimple("return 1.0 < 1;").bool);
    try t.expectEqual(true, testSimple("return 0.99 < 1;").bool);
    try t.expectEqual(false, testSimple("return 2.1 < 2;").bool);

    try t.expectEqual(true, testSimple("return 1.0 <= 1;").bool);
    try t.expectEqual(true, testSimple("return 3.99 <= 4;").bool);
    try t.expectEqual(false, testSimple("return 10.1 <= 10;").bool);
}

test "elz: comparison bool" {
    try t.expectEqual(true, testSimple("return true == true;").bool);
    try t.expectEqual(true, testSimple("return false == false;").bool);
    try t.expectEqual(false, testSimple("return true == false;").bool);
    try t.expectEqual(false, testSimple("return false == true;").bool);

    try t.expectEqual(false, testSimple("return true != true;").bool);
    try t.expectEqual(false, testSimple("return false != false;").bool);
    try t.expectEqual(true, testSimple("return true != false;").bool);
    try t.expectEqual(true, testSimple("return false != true;").bool);
}

test "elz: comparison null" {
    try t.expectEqual(true, testSimple("return null == null;").bool);
    try t.expectEqual(false, testSimple("return null != null;").bool);

    try t.expectEqual(false, testSimple("return 0 == null;").bool);
    try t.expectEqual(false, testSimple("return 0.0 == null;").bool);
    try t.expectEqual(false, testSimple("return 1 == null;").bool);
    try t.expectEqual(false, testSimple("return 1.1 == null;").bool);
    try t.expectEqual(false, testSimple("return `` == null;").bool);
    try t.expectEqual(false, testSimple("return `abc` == null;").bool);
    try t.expectEqual(false, testSimple("return true == null;").bool);
    try t.expectEqual(false, testSimple("return false == null;").bool);

    try t.expectEqual(true, testSimple("return 0 != null;").bool);
    try t.expectEqual(true, testSimple("return 0.0 != null;").bool);
    try t.expectEqual(true, testSimple("return 1 != null;").bool);
    try t.expectEqual(true, testSimple("return 1.1 != null;").bool);
    try t.expectEqual(true, testSimple("return `` != null;").bool);
    try t.expectEqual(true, testSimple("return `abc` != null;").bool);
    try t.expectEqual(true, testSimple("return true != null;").bool);
    try t.expectEqual(true, testSimple("return false != null;").bool);
}

test "elz: comparison string" {
    try t.expectEqual(true, testSimple("return `abc` == `abc`;").bool);
    try t.expectEqual(false, testSimple("return `abc` == `123`;").bool);
    try t.expectEqual(false, testSimple("return `abc` == `ABC`;").bool);

    try t.expectEqual(false, testSimple("return `abc` != `abc`;").bool);
    try t.expectEqual(true, testSimple("return `abc` != `123`;").bool);
    try t.expectEqual(true, testSimple("return `abc` != `ABC`;").bool);

    try t.expectEqual(false, testSimple("return `abc` < `abc`;").bool);
    try t.expectEqual(false, testSimple("return `abc` > `abc`;").bool);
    try t.expectEqual(true, testSimple("return `abc` <= `abc`;").bool);
    try t.expectEqual(true, testSimple("return `abc` >= `abc`;").bool);

    try t.expectEqual(false, testSimple("return `abc` < `ABC`;").bool);
    try t.expectEqual(false, testSimple("return `abc` <= `ABC`;").bool);
    try t.expectEqual(true, testSimple("return `ABC` <= `abc`;").bool);
    try t.expectEqual(true, testSimple("return `ABC` <= `abc`;").bool);

    try t.expectEqual(true, testSimple("return `abc` > `ABC`;").bool);
    try t.expectEqual(true, testSimple("return `abc` >= `ABC`;").bool);
    try t.expectEqual(false, testSimple("return `ABC` >= `abc`;").bool);
    try t.expectEqual(false, testSimple("return `ABC` >= `abc`;").bool);
}

test "elz: increment/decrement" {
    try t.expectEqual(4, testSimple(
        \\ var i = 0;
        \\ i++;
        \\ i++;
        \\ i++;
        \\ i--;
        \\ i++;
        \\ return i++;
    ).i64);

    try t.expectEqual(6, testSimple(
        \\ var x = 2;
        \\ x += 4;
        \\ return x ;
    ).i64);

    try t.expectEqual(6, testSimple(
        \\ var x = 2;
        \\ x += 4;
        \\ return x ;
    ).i64);

    try t.expectEqual(-2, testSimple(
        \\ var x = 2;
        \\ x -= 4;
        \\ return x ;
    ).i64);

    try testError("Expected semicolon (';'), got '++' (PLUS_PLUS)", "return 100++;");
}

test "elz: variables" {
    defer t.reset();

    try t.expectString("Leto", testSimple(
        \\ var name = `Leto`;
        \\ return name;
    ).string);

    try t.expectString("Leto", testSimple(
        \\ var name = `Leto`;
        \\ {
        \\    var name = "Ghanima" ;
        \\ }
        \\ return name;
    ).string);

    try t.expectString("Ghanima", testSimple(
        \\ var name = `Leto`;
        \\ {
        \\    var name = "Ghanima" ;
        \\    return name;
        \\ }
    ).string);

    try t.expectEqual(4, testSimple(
        \\ var count = 3;
        \\ return count + 1;
    ).i64);

    try testError("Variable 'name' used before being initialized", "var name = name + 3;");
    try testError("Variable 'unknown' is unknown", "return unknown;");
    try testError("Expected assignment operator ('='), got '`hello`' (STRING)", "var x `hello`");
}

test "elz: if" {
    defer t.reset();

    try t.expectEqual(1234, testSimple(
        \\ if (true) {
        \\   return 1234;
        \\ }
        \\ return 4321;
    ).i64);

    try t.expectEqual(4321, testSimple(
        \\ if (false) {
        \\   return 1234;
        \\ }
        \\ return 4321;
    ).i64);

    try t.expectEqual(9, testSimple(
        \\ if (1 == 1) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    ).i64);

    try t.expectEqual(10, testSimple(
        \\ if (1 != 1) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    ).i64);

    try t.expectEqual(8, testSimple(
        \\ if (1 == 1) {
        \\   return 8;
        \\ } else if (2 == 2) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    ).i64);

    try t.expectEqual(9, testSimple(
        \\ if (1 != 1) {
        \\   return 8;
        \\ } else if (2 == 2) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    ).i64);

    try t.expectEqual(10, testSimple(
        \\ if (1 != 1) {
        \\   return 8;
        \\ } else if (2 != 2) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    ).i64);
}

test "elz: logical operators" {
    try t.expectEqual(false, testSimple("return 1 == 1 and 3 == 2;").bool);
    try t.expectEqual(false, testSimple("return 0 == 1 and 3 == 2;").bool);
    try t.expectEqual(false, testSimple("return 1 == 3 or 3 == 4;").bool);
    try t.expectEqual(true, testSimple("return 1 == 1 and 2 == 2;").bool);
    try t.expectEqual(true, testSimple("return 1 == 1 or 3 == 2;").bool);
    try t.expectEqual(true, testSimple("return 1 == 3 or 3 == 3;").bool);
    try t.expectEqual(false, testSimple("return 1 == 3 and (3 == 4 or 4 == 4);").bool);
}

test "elz: while" {
    try t.expectEqual(10, testSimple(
        \\ var i = 0;
        \\ while (i < 10) {
        \\   i++;
        \\ }
        \\ return i;
    ).i64);

    try t.expectEqual(0, testSimple(
        \\ var i = 0;
        \\ while (false) {
        \\   i = i + 1;
        \\ }
        \\ return i;
    ).i64);
}

test "elz: for" {
    try t.expectEqual(10, testSimple(
        \\ var i = 0;
        \\ for (var x = 0; x < 10; x = x + 1) {
        \\   i = i + 1;
        \\ }
        \\ return i;
    ).i64);

    try t.expectEqual(2, testSimple(
        \\ var i = 10;
        \\ for (var x = 10; x > 2; x = x - 1) {
        \\   i = i - 1;
        \\ }
        \\ return i;
    ).i64);

    // test various incerment/decrement while we're here (++ and --)
    try t.expectEqual(10, testSimple(
        \\ var i = 0;
        \\ for (var x = 0; x < 10; x++) {
        \\   i++;
        \\ }
        \\ return i;
    ).i64);

    try t.expectEqual(2, testSimple(
        \\ var i = 10;
        \\ for (var x = 10; x > 2; x--) {
        \\   i--;
        \\ }
        \\ return i;
    ).i64);

    // test various incerment/decrement while we're here (+= and -=)
    try t.expectEqual(8, testSimple(
        \\ var i = 0;
        \\ for (var x = 0; x < 10; x += 3) {
        \\   i += 2;
        \\ }
        \\ return i;
    ).i64);

    try t.expectEqual(4, testSimple(
        \\ var i = 10;
        \\ for (var x = 10; x > 2; x -= 3) {
        \\   i -= 2;
        \\ }
        \\ return i;
    ).i64);
}

test "elz: empty scope"  {
    _ = testSimple("{}"); // doesn't crash, yay!
}

fn testSimple(src: []const u8) Value {
    var c = Compiler(Preset.small).init(t.allocator) catch unreachable;
    defer c.deinit();

    c.compile(src) catch {
        std.debug.print("Compilation error: {}\n", .{c.err.?});
        unreachable;
    };

    const byte_code = c.byteCode(t.allocator) catch unreachable;
    defer t.allocator.free(byte_code);
    // disassemble(.{}, byte_code, std.io.getStdErr().writer()) catch unreachable;

    var vm = VM.init(t.allocator);
    defer vm.deinit();

    const value = vm.run(byte_code) catch |err| {
        std.debug.print("{any}", .{err});
        if (vm.err) |e| {
            std.debug.print("{any} {s}\n", .{ e.err, e.desc });
        }
        disassemble(.{}, byte_code, std.io.getStdErr().writer()) catch unreachable;
        unreachable;
    };

    // Values are tied to the VM, which this function will deinit
    // We need to dupe our strnig into our testing arena.
    return switch (value) {
        .string => |str| .{ .string = t.arena.allocator().dupe(u8, str) catch unreachable },
        else => value,
    };
}

fn testError(expected: []const u8, src: []const u8) !void {
    var c = Compiler(Preset.small).init(t.allocator) catch unreachable;
    defer c.deinit();

    c.compile(src) catch {
        const ce = c.err orelse unreachable;
        if (std.mem.indexOf(u8, ce.desc, expected) == null) {
            std.debug.print("Wrong error, expected: {s} but got:\n{}\n", .{ expected, ce });
            return error.WrongError;
        }
        return;
    };

    return error.NoError;
}
