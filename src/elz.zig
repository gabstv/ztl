const std = @import("std");

pub const VM = @import("vm.zig").VM;
pub const Value = @import("value.zig").Value;
pub const Config = @import("config.zig").Config;
pub const Compiler = @import("compiler.zig").Compiler;
pub const disassemble = @import("byte_code.zig").disassemble;

pub const Preset = struct {
    pub const small = Config{
        .debug = .none,
        .max_locals = 256,
        .initial_code_size = 256,
        .initial_data_size = 256,
    };

    pub const debugSmall = Config{
        .debug = .minimal,
        .max_locals = 256,
        .initial_code_size = 256,
        .initial_data_size = 256,
    };
};

const t = @import("t.zig");

test "elz: local limit" {
    blk: {
        var c = Compiler(.{.max_locals = 3}).init(t.allocator) catch unreachable;
        defer c.deinit();
        c.compile(
            \\ var a = 1;
            \\ var b = 1;
            \\ var c = 1;
            \\ var d = 1;
        ) catch {
            try t.expectString("Maximum number of local variable (3) exceeded", c.err.?.desc);
            break :blk;
        };
        return error.NoError;
    }

    {
        var c = Compiler(.{.max_locals = 3}).init(t.allocator) catch unreachable;
        defer c.deinit();
        try c.compile(
            \\ var a = 1;
            \\ var b = 1;
            \\ var c = 1;
        );
    }
}

test "elz: arithmetic" {
    try testReturnValue(.{.i64 = 9}, "return 1 + 8;");
    try testReturnValue(.{.i64 = -1}, "return 10 - 11;");
    try testReturnValue(.{.i64 = 14}, "return 2 * 7;");
    try testReturnValue(.{.i64 = 2}, "return 18 / 9;");

    try testReturnValue(.{.i64 = 17}, "return 2 + 5 * 3;");
    try testReturnValue(.{.i64 = 21}, "return (2 + 5) * 3;");
    try testReturnValue(.{.i64 = 13}, "return 2 * 5 + 3;");

    try testReturnValue(.{.f64 = 4.5}, "return 1.2 + 3.3;");
    try testReturnValue(.{.f64 = 5.3}, "return 2 + 3.3;");
    try testReturnValue(.{.f64 = 5.3}, "return 3.3 + 2;");
    try testReturnValue(.{.f64 = 1.0}, "return 1.1 - 0.1;");
    try testReturnValue(.{.f64 = -1.2999999999999998}, "return 2 - 3.3;");
    try testReturnValue(.{.f64 = 1.2999999999999998}, "return 3.3 - 2;");
    try testReturnValue(.{.f64 = 3.9599999999999995}, "return 1.2 * 3.3;");
    try testReturnValue(.{.f64 = 20.4}, "return 5.1 * 4;");
    try testReturnValue(.{.f64 = 20.4}, "return 4 * 5.1;");
    try testReturnValue(.{.f64 = 0.36363636363636365}, "return 1.2 / 3.3;");
    try testReturnValue(.{.f64 = 1.275}, "return 5.1 / 4;");
    try testReturnValue(.{.f64 = 0.7843137254901962}, "return 4 / 5.1;");
}

test "elz: not" {
    try testReturnValue(.{.bool = true}, "return !false;");
    try testReturnValue(.{.bool = false}, "return !true;");
}

test "elz: comparison int" {
    try testReturnValue(.{.bool = true}, "return 1 == 1;");
    try testReturnValue(.{.bool = false}, "return 1 == 2;");
    try testReturnValue(.{.bool = false}, "return 1 != 1;");
    try testReturnValue(.{.bool = true}, "return 1 != 2;");

    try testReturnValue(.{.bool = false}, "return 1 > 1;");
    try testReturnValue(.{.bool = false}, "return 1 > 2;");
    try testReturnValue(.{.bool = true}, "return 2 > 1;");

    try testReturnValue(.{.bool = true}, "return 1 >= 1;");
    try testReturnValue(.{.bool = false}, "return 1 >= 2;");
    try testReturnValue(.{.bool = true}, "return 2 >= 1;");

    try testReturnValue(.{.bool = false}, "return 1 < 1;");
    try testReturnValue(.{.bool = true}, "return 1 < 2;");
    try testReturnValue(.{.bool = false}, "return 2 < 1;");

    try testReturnValue(.{.bool = true}, "return 1 <= 1;");
    try testReturnValue(.{.bool = true}, "return 1 <= 2;");
    try testReturnValue(.{.bool = false}, "return 2 <= 1;");
}

test "elz: comparison float" {
    try testReturnValue(.{.bool = true}, "return 1.13 == 1.13;");
    try testReturnValue(.{.bool = false}, "return 1.13 == 2.08;");
    try testReturnValue(.{.bool = false}, "return 1.13 != 1.13;");
    try testReturnValue(.{.bool = true}, "return 1.13 != 2.08;");

    try testReturnValue(.{.bool = false}, "return 1.13 > 1.13;");
    try testReturnValue(.{.bool = false}, "return 1.13 > 2.08;");
    try testReturnValue(.{.bool = true}, "return 2.08 > 1.13;");

    try testReturnValue(.{.bool = true}, "return 1.13 >= 1.13;");
    try testReturnValue(.{.bool = false}, "return 1.13 >= 2.08;");
    try testReturnValue(.{.bool = true}, "return 2.08 >= 1.13;");

    try testReturnValue(.{.bool = false}, "return 1.13 < 1.13;");
    try testReturnValue(.{.bool = true}, "return 1.13 < 2.08;");
    try testReturnValue(.{.bool = false}, "return 2.08 < 1.13;");

    try testReturnValue(.{.bool = true}, "return 1.13 <= 1.13;");
    try testReturnValue(.{.bool = true}, "return 1.13 <= 2.08;");
    try testReturnValue(.{.bool = false}, "return 2.08 <= 1.13;");
}

test "elz: comparison int - float" {
    try testReturnValue(.{.bool = true}, "return 1 == 1.0;");
    try testReturnValue(.{.bool = false}, "return 1 == 1.1;");
    try testReturnValue(.{.bool = false}, "return 1 != 1.0;");
    try testReturnValue(.{.bool = true}, "return 1 != 1.1;");

    try testReturnValue(.{.bool = false}, "return 1 > 1.0;");
    try testReturnValue(.{.bool = false}, "return 1 > 2.0;");
    try testReturnValue(.{.bool = true}, "return 2 > 1.9;");

    try testReturnValue(.{.bool = true}, "return 1 >= 1.0;");
    try testReturnValue(.{.bool = false}, "return 1 >= 2.0;");
    try testReturnValue(.{.bool = true}, "return 2 >= 1.98;");

    try testReturnValue(.{.bool = false}, "return 1 < 1.0;");
    try testReturnValue(.{.bool = true}, "return 1 < 1.01;");
    try testReturnValue(.{.bool = false}, "return 2 < 1.99;");

    try testReturnValue(.{.bool = true}, "return 1 <= 1.0;");
    try testReturnValue(.{.bool = true}, "return 1 <= 1.01;");
    try testReturnValue(.{.bool = false}, "return 2 <= 1.99;");
}

test "elz: comparison float - int" {
    try testReturnValue(.{.bool = true}, "return 1.0 == 1;");
    try testReturnValue(.{.bool = false}, "return 1.1 == 1;");
    try testReturnValue(.{.bool = false}, "return 1.0 != 1;");
    try testReturnValue(.{.bool = true}, "return 1.1 != 1;");

    try testReturnValue(.{.bool = false}, "return 1.0 > 1;");
    try testReturnValue(.{.bool = false}, "return 1.9 > 2;");
    try testReturnValue(.{.bool = true}, "return 2.1 > 2;");

    try testReturnValue(.{.bool = true}, "return 1.0 >= 1;");
    try testReturnValue(.{.bool = false}, "return 1.9 >= 2;");
    try testReturnValue(.{.bool = true}, "return 2.1 >= 2;");

    try testReturnValue(.{.bool = false}, "return 1.0 < 1;");
    try testReturnValue(.{.bool = true}, "return 0.99 < 1;");
    try testReturnValue(.{.bool = false}, "return 2.1 < 2;");

    try testReturnValue(.{.bool = true}, "return 1.0 <= 1;");
    try testReturnValue(.{.bool = true}, "return 3.99 <= 4;");
    try testReturnValue(.{.bool = false}, "return 10.1 <= 10;");
}

test "elz: comparison bool" {
    try testReturnValue(.{.bool = true}, "return true == true;");
    try testReturnValue(.{.bool = true}, "return false == false;");
    try testReturnValue(.{.bool = false}, "return true == false;");
    try testReturnValue(.{.bool = false}, "return false == true;");

    try testReturnValue(.{.bool = false}, "return true != true;");
    try testReturnValue(.{.bool = false}, "return false != false;");
    try testReturnValue(.{.bool = true}, "return true != false;");
    try testReturnValue(.{.bool = true}, "return false != true;");
}

test "elz: comparison null" {
    try testReturnValue(.{.bool = true}, "return null == null;");
    try testReturnValue(.{.bool = false}, "return null != null;");

    try testReturnValue(.{.bool = false}, "return 0 == null;");
    try testReturnValue(.{.bool = false}, "return 0.0 == null;");
    try testReturnValue(.{.bool = false}, "return 1 == null;");
    try testReturnValue(.{.bool = false}, "return 1.1 == null;");
    try testReturnValue(.{.bool = false}, "return `` == null;");
    try testReturnValue(.{.bool = false}, "return `abc` == null;");
    try testReturnValue(.{.bool = false}, "return true == null;");
    try testReturnValue(.{.bool = false}, "return false == null;");

    try testReturnValue(.{.bool = true}, "return 0 != null;");
    try testReturnValue(.{.bool = true}, "return 0.0 != null;");
    try testReturnValue(.{.bool = true}, "return 1 != null;");
    try testReturnValue(.{.bool = true}, "return 1.1 != null;");
    try testReturnValue(.{.bool = true}, "return `` != null;");
    try testReturnValue(.{.bool = true}, "return `abc` != null;");
    try testReturnValue(.{.bool = true}, "return true != null;");
    try testReturnValue(.{.bool = true}, "return false != null;");
}

test "elz: comparison string" {
    try testReturnValue(.{.bool = true}, "return `abc` == `abc`;");
    try testReturnValue(.{.bool = false}, "return `abc` == `123`;");
    try testReturnValue(.{.bool = false}, "return `abc` == `ABC`;");

    try testReturnValue(.{.bool = false}, "return `abc` != `abc`;");
    try testReturnValue(.{.bool = true}, "return `abc` != `123`;");
    try testReturnValue(.{.bool = true}, "return `abc` != `ABC`;");

    try testReturnValue(.{.bool = false}, "return `abc` < `abc`;");
    try testReturnValue(.{.bool = false}, "return `abc` > `abc`;");
    try testReturnValue(.{.bool = true}, "return `abc` <= `abc`;");
    try testReturnValue(.{.bool = true}, "return `abc` >= `abc`;");

    try testReturnValue(.{.bool = false}, "return `abc` < `ABC`;");
    try testReturnValue(.{.bool = false}, "return `abc` <= `ABC`;");
    try testReturnValue(.{.bool = true}, "return `ABC` <= `abc`;");
    try testReturnValue(.{.bool = true}, "return `ABC` <= `abc`;");

    try testReturnValue(.{.bool = true}, "return `abc` > `ABC`;");
    try testReturnValue(.{.bool = true}, "return `abc` >= `ABC`;");
    try testReturnValue(.{.bool = false}, "return `ABC` >= `abc`;");
    try testReturnValue(.{.bool = false}, "return `ABC` >= `abc`;");
}

test "elz: increment/decrement" {
    try testReturnValue(.{.i64 = 4},
        \\ var i = 0;
        \\ i++;
        \\ i++;
        \\ i++;
        \\ i--;
        \\ i++;
        \\ return i++;
    );

    try testReturnValue(.{.i64 = 6},
        \\ var x = 2;
        \\ x += 4;
        \\ return x ;
    );

    try testReturnValue(.{.i64 = 6},
        \\ var x = 2;
        \\ x += 4;
        \\ return x ;
    );

    try testReturnValue(.{.i64 = -2},
        \\ var x = 2;
        \\ x -= 4;
        \\ return x ;
    );

    // -1, 1...10 have special treatement, so test a range around there
    inline for (0..20) |i| {
        const src_pos = std.fmt.comptimePrint("var x = 0;\nx += {d};return x;", .{i});
        try testReturnValue(.{.i64 = i}, src_pos);

        const signed: i64 = @intCast(i);
        const src_neg = std.fmt.comptimePrint("var x = 0;\nx += {d};return x;", .{signed});
        try testReturnValue(.{.i64 = signed}, src_neg);
    }


    try testError("Expected semicolon (';'), got '++' (PLUS_PLUS)", "return 100++;");
}

test "elz: variables" {
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

    try testReturnValue(.{.i64 = 4},
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

test "elz: if" {
    try testReturnValue(.{.i64 = 1234},
        \\ if (true) {
        \\   return 1234;
        \\ }
        \\ return 4321;
    );

    try testReturnValue(.{.i64 = 4321},
        \\ if (false) {
        \\   return 1234;
        \\ }
        \\ return 4321;
    );

    try testReturnValue(.{.i64 = 9},
        \\ if (1 == 1) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );

    try testReturnValue(.{.i64 = 10},
        \\ if (1 != 1) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );

    try testReturnValue(.{.i64 = 8},
        \\ if (1 == 1) {
        \\   return 8;
        \\ } else if (2 == 2) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );

    try testReturnValue(.{.i64 = 9},
        \\ if (1 != 1) {
        \\   return 8;
        \\ } else if (2 == 2) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );

    try testReturnValue(.{.i64 = 10},
        \\ if (1 != 1) {
        \\   return 8;
        \\ } else if (2 != 2) {
        \\   return 9;
        \\ } else {
        \\   return 10;
        \\ }
    );
}

test "elz: logical operators" {
    try testReturnValue(.{.bool = false}, "return 1 == 1 and 3 == 2;");
    try testReturnValue(.{.bool = false}, "return 0 == 1 and 3 == 2;");
    try testReturnValue(.{.bool = false}, "return 1 == 3 or 3 == 4;");
    try testReturnValue(.{.bool = true}, "return 1 == 1 and 2 == 2;");
    try testReturnValue(.{.bool = true}, "return 1 == 1 or 3 == 2;");
    try testReturnValue(.{.bool = true}, "return 1 == 3 or 3 == 3;");
    try testReturnValue(.{.bool = false}, "return 1 == 3 and (3 == 4 or 4 == 4);");
}

test "elz: while" {
    try testReturnValue(.{.i64 = 10},
        \\ var i = 0;
        \\ while (i < 10) {
        \\   i++;
        \\ }
        \\ return i;
    );

    try testReturnValue(.{.i64 = 0},
        \\ var i = 0;
        \\ while (false) {
        \\   i = i + 1;
        \\ }
        \\ return i;
    );
}

test "elz: for" {
    try testReturnValue(.{.i64 = 10},
        \\ var i = 0;
        \\ for (var x = 0; x < 10; x = x + 1) {
        \\   i = i + 1;
        \\ }
        \\ return i;
    );

    try testReturnValue(.{.i64 = 2},
        \\ var i = 10;
        \\ for (var x = 10; x > 2; x = x - 1) {
        \\   i = i - 1;
        \\ }
        \\ return i;
    );

    // test various incerment/decrement while we're here (++ and --)
    try testReturnValue(.{.i64 = 10},
        \\ var i = 0;
        \\ for (var x = 0; x < 10; x++) {
        \\   i++;
        \\ }
        \\ return i;
    );

    try testReturnValue(.{.i64 = 2},
        \\ var i = 10;
        \\ for (var x = 10; x > 2; x--) {
        \\   i--;
        \\ }
        \\ return i;
    );

    // test various incerment/decrement while we're here (+= and -=)
    try testReturnValue(.{.i64 = 8},
        \\ var i = 0;
        \\ for (var x = 0; x < 10; x += 3) {
        \\   i += 2;
        \\ }
        \\ return i;
    );

    try testReturnValue(.{.i64 = 4},
        \\ var i = 10;
        \\ for (var x = 10; x > 2; x -= 3) {
        \\   i -= 2;
        \\ }
        \\ return i;
    );
}

test "elz: empty scope" {
    try testReturnValue(.{.null = {}}, "{} return null;"); // doesn't crash, yay!
}

test "elz: functions" {
    try testReturnValue(.{.i64 = 25},
        \\ return value(3);
        \\
        \\ fn value(start) {
        \\   return start + 22;
        \\ }
    );

    // implicit return
    try testReturnValue(.{.null = {}},
        \\ return value();
        \\
        \\ fn value() {
        \\ }
    );

    try testReturnValue(.{.i64 = 26},
        \\ var start = 4;
        \\ var noise = 99;
        \\ return value(start);
        \\
        \\ fn value(start) {
        \\   var noise = 100;
        \\   return start + 22;
        \\ }
    );

    try testReturnValue(.{.i64 = 4},
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

    try testReturnValue(.{.i64 = 10},
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

    try testReturnValue(.{.i64 = 134},
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
}

fn testReturnValue(expected: Value, src: []const u8) !void {
    const configs = [_]Config{
        .{.debug = .full},
        Preset.small,
        .{.max_locals = 300}, // requires u16
    };

    inline for (configs) |config| {
        var c = try Compiler(config).init(t.allocator);
        defer c.deinit();

        c.compile(src) catch |err| {
            std.debug.print("Compilation error: {}\n", .{c.err.?});
            return err;
        };

        const byte_code = try c.byteCode(t.allocator);
        defer t.allocator.free(byte_code);
        // disassemble(config, byte_code, std.io.getStdErr().writer()) catch unreachable;

        var vm = VM(config).init(t.allocator);
        defer vm.deinit();

        const value = vm.run(byte_code) catch |err| {
            std.debug.print("{any}", .{err});
            if (vm.err) |e| {
                std.debug.print("{any} {s}\n", .{ e.err, e.desc });
            }
            disassemble(config, byte_code, std.io.getStdErr().writer()) catch unreachable;
            return err;
        };

        try t.expectString(@tagName(expected), @tagName(value));

        switch (value) {
            .string => |str| try t.expectString(expected.string, str),
            else => try t.expectEqual(expected, value),
        }
    }
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
