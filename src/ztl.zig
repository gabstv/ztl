const std = @import("std");

pub const VM = @import("vm.zig").VM;
pub const Value = @import("value.zig").Value;
pub const DebugMode = @import("config.zig").DebugMode;
pub const Global = @import("template.zig").Global;
pub const Template = @import("template.zig").Template;
pub const ErrorReport = @import("template.zig").ErrorReport;

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

pub fn Functions(comptime A: type) type {
    const App = switch (@typeInfo(A)) {
        .@"struct" => A,
        .pointer => |ptr| ptr.child,
        .void => void,
        else => @compileError("Template App must be a struct, got: " ++ @tagName(@typeInfo(A))),
    };

    if (App == void or @hasDecl(App, "ZtlFunctions") == false) {
        return @Type(.{.@"enum" = .{
            .decls = &.{},
            .tag_type = u8,
            .fields = &.{.{.name = "", .value = 0}}, // HACK, std.meta.stringToEnum doesn't work on an empty enum, lol what?
            .is_exhaustive = true,
        }});
    }
    const declarations = std.meta.declarations(App.ZtlFunctions);
    var fields: [declarations.len]std.builtin.Type.EnumField = undefined;

    for (declarations, 0..) |d, i| {
        fields[i] = .{.name = d.name, .value = i};
    }

    // the type of the @"enum" tag is std.builtin.Type.Enum
    // we use the type inference syntax, i.e. .{...}
    return @Type(.{.@"enum" = .{
        .decls = &.{},
        .tag_type = u16,
        .fields = &fields,
        .is_exhaustive = true,
    }});
}

const t = @import("t.zig");

test {
    std.testing.refAllDecls(@This());
}

const Compiler = @import("compiler.zig").Compiler;
const disassemble = @import("byte_code.zig").disassemble;

test "ztl: local limit" {
    blk: {
        var c = Compiler(struct {
            pub const ZtlConfig = struct {
                pub const max_locals = 3;
            };
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
            pub const ZtlConfig = struct {
                pub const max_locals = 3;
            };
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

test "ztl: comparison list" {
    try testReturnValue(.{ .bool = true }, "return [] == [];");
    try testReturnValue(.{ .bool = true }, "return [1] == [1];");
    try testReturnValue(.{ .bool = false }, "return [1] == [1,``];");
    try testReturnValue(.{ .bool = false }, "return [] == null;");
}

test "ztl: comparison map" {
    try testReturnValue(.{ .bool = true }, "return %{} == %{};");
    try testReturnValue(.{ .bool = true }, "return %{a:1} == %{a: 1};");
    try testReturnValue(.{ .bool = true }, "return %{a:1, 123: `a`} == %{123: `a`, a: 1};");
    try testReturnValue(.{ .bool = false }, "return %{a: 1} == %{a:1,b:2};");
    try testReturnValue(.{ .bool = false }, "return %{} == null;");
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

    try testReturnValueWithApp(struct {
        pub const ZtlConfig = struct {
            pub const deduplicate_string_literals = true;
        };
    },  .{}, .{.string = "hello"},
        \\ var x = "hello";
        \\ var y = "hello";
        \\ return x;
    );

    try testReturnValueWithApp(struct {
        pub const ZtlConfig = struct {
            pub const ztl_deduplicate_string_literals = false;
        };
    }, .{}, .{.string = "hello"},
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

test "ztl: break foreach" {
    try testReturnValue(.{ .i64 = 10 },
        \\ var count = 0;
        \\ var arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        \\ foreach (arr) |i| {
        \\   if (i == 5) break;
        \\   count += i;
        \\ }
        \\ return count;
    );

    try testReturnValue(.{ .i64 = 4010 },
        \\ var c1 = 0;
        \\ var c2 = 0;
        \\ foreach ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) |i| {
        \\   foreach ([10, 20, 30, 40, 50, 60, 70, 80, 90, 100]) |j| {
        \\     if (j == 50) break;
        \\     c2 += 100;
        \\   }
        \\   c1 += 1;
        \\ }
        \\ return c1 + c2;
    );

    try testReturnValue(.{ .i64 = 414 },
        \\ var c1 = 0;
        \\ var c2 = 0;
        \\ foreach ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], %{a: 1, b: 3}) |i, kv| {
        \\   foreach ([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], [1,2,3,4,5,6]) |j, k| {
        \\     if (j == 50) break 2;
        \\     c2 += 100 + kv.value + k;
        \\   }
        \\   c1 += 1;
        \\ }
        \\ return c1 + c2;
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

test "ztl: continue foreach" {
    try testReturnValue(.{ .i64 = 50 },
        \\ var count = 0;
        \\ var arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        \\ foreach (arr) |i| {
        \\   if (i == 5) continue;
        \\   count += i;
        \\ }
        \\ return count;
    );

    try testReturnValue(.{ .i64 = 6012 },
        \\ var c1 = 2;
        \\ var c2 = 0;
        \\ foreach ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) |i| {
        \\   foreach ([10, 20, 30, 40, 50, 60, 70]) |j| {
        \\     if (j == 50) continue;
        \\     c2 += 100;
        \\   }
        \\   c1 += 1;
        \\ }
        \\ return c1 + c2;
    );

    try testReturnValue(.{ .i64 = 1264 },
        \\ var c1 = 2;
        \\ var c2 = 0;
        \\ foreach ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], %{a: 1, b: 3, c: 4}) |i, kv| {
        \\   foreach ([10, 20, 30, 40, 50, 60, 70, 80, 90, 100], [1,2,3,4,5,6]) |j, k| {
        \\     if (j == 50) continue 2;
        \\     c2 += 100 + kv.value + k;
        \\   }
        \\   c1 += 10000;
        \\ }
        \\ return c1 + c2;
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

test "ztl: ztl functions" {
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

test "ztl: function custom" {
    const App = struct {
        id: i64,

        pub const ZtlFunctions = struct {
            pub const add = 2;
            pub const double = 1;
        };

        pub fn call(self: *@This(), vm: *VM(*@This()), function: Functions(@This()), values: []Value) !Value {
            _ = vm;
            switch (function) {
                .add => return .{.i64 = values[0].i64 + values[1].i64},
                .double => return .{.i64 = values[0].i64 * 2 + self.id},
            }
        }
    };

    var app = App{.id = 200};
    try testReturnValueWithApp(*App, &app, .{.i64 = 1204}, "return add(1000, double(2));");

    try testErrorWithApp(*App, "Function 'add' reserved by custom application function", "fn add(){}");
    try testErrorWithApp(*App, "Function 'add' expects 2 parameters, but called with 0", "return add());");
    try testErrorWithApp(*App, "Function 'double' expects 1 parameter, but called with 2", "return double(2, 4));");
}

test "ztl: function error" {
    try testError("Unknown function: 'flow'", "return flow();");
    try testError("Function 'print' reserved as built-in function", "fn print(){}");

    try testError("Function 'x' expects 0 parameters, but called with 1",
        \\ fn x() {}
        \\ x(23);
    );

    try testError("Function 'x' expects 1 parameter, but called with 3",
        \\ x(1,2,3);
        \\ fn x(a) {}
    );
}

test "ztl: properties" {
    try testReturnValue(.{ .i64 = 0 }, "return [].len;");
    try testReturnValue(.{ .i64 = 3 }, "return [1,10,100].len;");
    try testReturnValue(.{ .i64 = 0 }, "return %{}.len;");
    try testReturnValue(.{ .i64 = 1 }, "return %{a: 2}.len;");
}

test "ztl: method errors" {
    try testError("xx' is not a valid method", "return [].xx()");
}

test "ztl: method last" {
    try testError("Function 'last' expects 0 parameters, but called with 1", "return [].last(1)");

    try testReturnValue(.{ .null = {} }, "return [].last();");
    try testReturnValue(.{ .i64 = 20 }, "return [1,20].last();");
}

test "ztl: method first" {
    try testError("Function 'first' expects 0 parameters, but called with 2", "return [].first(`a`, true)");

    try testReturnValue(.{ .null = {} }, "return [].first();");
    try testReturnValue(.{ .i64 = 99 }, "return [99,2].first();");
}

test "ztl: method pop" {
    try testError("Function 'pop' expects 0 parameters, but called with 1", "return [].pop(null)");

    try testReturnValue(.{ .null = {} }, "return [].pop();");
    try testReturnValue(.{ .i64 = 132 },
        \\ var arr = [10, 20, 100];
        \\ var last = arr.pop();
        \\ return arr.len + arr[0] + arr[1] + last;
    );
}

test "ztl: method remove" {
    try testError("Function 'remove' expects 1 parameter, but called with 0", "return [].remove()");
    try testError("Function 'remove' expects 1 parameter, but called with 2", "return %{}.remove(true, false)");

    try testReturnValue(.{ .bool = false }, "return [].remove(`a`);");
    try testReturnValue(.{ .bool = true }, "return [[]].remove([]);");
    try testReturnValue(.{ .i64 = 1312 },
        \\ var arr = [10, 20, 300];
        \\ var removed = arr.remove(20) ? 1000 : 0;
        \\ return arr.len + arr[0] + arr[1] + removed;
    );

    try testReturnValue(.{ .null = {} }, "return %{}.remove(`a`);");
    try testReturnValue(.{ .null = {} }, "return %{}.remove(3);");
    try testReturnValue(.{ .i64 = 301 },
        \\ var map = %{a: 100, b: 200};
        \\ var removed = map.remove("b");
        \\ return map.len + map[`a`] + removed;
    );
    try testReturnValue(.{ .i64 = 301 },
        \\ var map = %{1: 100, 20: 200};
        \\ var removed = map.remove(20);
        \\ return map.len + map[1] + removed;
    );
}

test "ztl: method removeAt" {
    try testError("Function 'removeAt' expects 1 parameter, but called with 0", "return [].removeAt()");
    try testRuntimeError("Index out of range. Index: 0, Len: 0", "return [].removeAt(0);");

    try testReturnValue(.{ .i64 = 1057 },
        \\ var arr = [5, 25, 50];
        \\ var removed = arr.removeAt(1) == 25 ? 1000 : 0;
        \\ return arr.len + arr[0] + arr[1] + removed;
    );

    try testReturnValue(.{ .i64 = 2057 },
        \\ var arr = [5, 25, 50];
        \\ var removed = arr.removeAt(-2) == 25 ? 2000 : 0;
        \\ return arr.len + arr[0] + arr[1] + removed;
    );
}

test "ztl: method append" {
    try testError("Function 'append' expects 1 parameter, but called with 0", "return [].append()");

    try testReturnValue(.{ .i64 = 4 },
        \\ var arr = [];
        \\ arr.append(3);
        \\ return arr.len + arr[0];
    );

    try testReturnValue(.{ .i64 = 15 },
        \\ var arr = [];
        \\ arr.append(3);
        \\ arr.append(10);
        \\ return arr.len + arr[0] + arr[1];
    );

    {
        defer t.reset();
        var arr1 = [_]Value{
            .{ .i64 = 99 },
        };
        var arr2 = [_]Value{t.createListRef(&arr1)};
        try testReturnValue(t.createListRef(&arr2),
            \\ var arr = [];
            \\ {
            \\    var inner = [99];
            \\    arr.append(inner);
            \\ }
            \\ return arr;
        );
    }
}

test "ztl: method contains" {
    try testError("Function 'contains' expects 1 parameter, but called with 0", "return [].contains()");
    try testRuntimeError("Map key must be an integer or string, got a boolean", "return %{}.contains(true);");

    try testReturnValue(.{ .bool = false }, "return [].contains(true);");
    try testReturnValue(.{ .bool = false }, "return [].contains(32);");
    try testReturnValue(.{ .bool = false }, "return [1,2,3].contains(4);");
    try testReturnValue(.{ .bool = true }, "return [1,2,3].contains(3);");
    try testReturnValue(.{ .bool = true }, "return [`aa`, `BB`].contains(`aa`);");
    try testReturnValue(.{ .bool = true }, "return [`aa`, `BB`].contains(`BB`);");
    try testReturnValue(.{ .bool = false }, "return [`aa`, `BB`].contains(`AA`);");

    try testReturnValue(.{ .bool = false }, "return %{}.contains(123);");
    try testReturnValue(.{ .bool = false }, "return %{111: true}.contains(123);");
    try testReturnValue(.{ .bool = true }, "return %{123: 1.2}.contains(123);");
    try testReturnValue(.{ .bool = false }, "return %{abc: 1, def: 2}.contains(123);");
    try testReturnValue(.{ .bool = true }, "return %{abc: 1, def: 2}.contains(`abc`);");
    try testReturnValue(.{ .bool = true }, "return %{abc: 1, def: 2}.contains(`def`);");
    try testReturnValue(.{ .bool = false }, "return %{abc: 1, def: 2}.contains(`ABC`);");
}

test "ztl: method indexOf" {
    try testError("Function 'indexOf' expects 1 parameter, but called with 0", "return [].indexOf()");
    try testRuntimeError("Unknown method 'indexOf' for a map", "return %{}.indexOf(1);");

    try testReturnValue(.{ .null = {} }, "return [].indexOf(true);");
    try testReturnValue(.{ .null = {} }, "return [].indexOf(32);");
    try testReturnValue(.{ .null = {} }, "return [1,2,3].indexOf(4);");
    try testReturnValue(.{ .i64 = 2 }, "return [1,2,3].indexOf(3);");
    try testReturnValue(.{ .i64 = 0 }, "return [`aa`, `BB`].indexOf(`aa`);");
    try testReturnValue(.{ .i64 = 1 }, "return [`aa`, `BB`].indexOf(`BB`);");
    try testReturnValue(.{ .null = {} }, "return [`aa`, `BB`].indexOf(`AA`);");
}

test "ztl: method sort" {
    // try testError("Function 'sort' expects 0 parameters, but called with 2", "return [].sort(true, false)");
    // try testRuntimeError("Unknown method 'sort' for a boolean", "return true.sort();");

    // try testReturnValue(.{ .i64 = 5431 },
    //     \\ var arr = [4, 1, 3, 5];
    //     \\ arr.sort();
    //     \\ return arr[0] + (10 * arr[1]) + (100 * arr[2]) + (1000 * arr[3]);
    // );

    // try testReturnValue(.{ .f64 = 5431.2 },
    //     \\ var arr = [4, 1, 3.02, 5];
    //     \\ arr.sort();
    //     \\ return arr[0] + (10 * arr[1]) + (100 * arr[2]) + (1000 * arr[3]);
    // );

    {
        defer t.reset();
        var arr = [_]Value{
            .{ .string = "AZ" },
            .{ .string = "a" },
            .{ .string = "ab"},
        };
        try testReturnValue(t.createListRef(&arr), "return [`ab`, `a`, `AZ`].sort();");
    }
}

test "ztl: method concat" {
    defer t.reset();

    try testError("Function 'concat' expects 1 parameter, but called with 2", "return [].concat(true, false)");

    {
        var arr = [_]Value{
            .{ .i64 = 1 },
        };
        try testReturnValue(t.createListRef(&arr), "return [].concat(1);");
    }

    {
        var arr = [_]Value{
            .{ .i64 = 0 },
            .{ .i64 = 2 },
            .{ .i64 = 1 },
        };
        try testReturnValue(t.createListRef(&arr), "return [0,2].concat(1);");
    }

    {
        var arr = [_]Value{
            .{ .i64 = 0 },
            .{ .i64 = 2 },
        };
        try testReturnValue(t.createListRef(&arr), "return [].concat([0, 2]);");
    }

    {
        var arr = [_]Value{
            .{ .i64 = 1 },
            .{ .i64 = 3 },
            .{ .i64 = 0 },
            .{ .i64 = 2 },
        };
        try testReturnValue(t.createListRef(&arr), "return [1, 3].concat([0, 2]);");
    }

    {
        var arr = [_]Value{
            .{ .i64 = 1 },
            .{ .i64 = 3 },
            .{ .i64 = 0 },
            .{ .i64 = 2 },
        };
        try testReturnValue(t.createListRef(&arr), "return [1, 3].concat([0, 2]);");
    }

    {
        var arr = [_]Value{
            .{ .i64 = 1 },
            .{ .i64 = 3 },
            .{ .i64 = 0 },
            .{ .i64 = 2 },
        };
        try testReturnValue(t.createListRef(&arr),
            \\ var arr = [1, 3];
            \\ arr.concat([0, 2]);
            \\ return arr;
        );
    }

    {
        var arr = [_]Value{
            .{ .i64 = 1 },
            .{ .i64 = 3 },
            .{ .i64 = 2 },
            .{ .i64 = 0 },
        };
        try testReturnValue(t.createListRef(&arr),
            \\ var arr = [1, 3];
            \\ var other = [2, 0];
            \\ arr.concat(other);
            \\ return arr;
        );
    }

    {
        var arr1 = [_]Value{
            .{ .i64 = 1 },
            .{ .i64 = 2 },
        };
        var arr2 = [_]Value{t.createListRef(&arr1)};
        try testReturnValue(t.createListRef(&arr2),
            \\ var arr = [];
            \\ {
            \\   var other = [[1, 2]];
            \\   arr.concat(other);
            \\ }
            \\ return arr;
        );
    }
}

fn testReturnValue(expected: Value, src: []const u8) !void {
    try testReturnValueWithApp(struct {
        pub const ZtlConfig = struct {
            pub const debug = DebugMode.full;
            pub const allow_leaks = false;
        };
    }, .{}, expected, src);

    try testReturnValueWithApp(struct {
        pub const ZtlConfig = struct {
            pub const max_locals = 256;
            pub const allow_leaks = false;
        };
    }, .{}, expected, src);

    try testReturnValueWithApp(struct {
        pub const ZtlConfig = struct {
            pub const max_locals = 300;
            pub const allow_leaks = false;
        };
    }, .{}, expected, src);
}

fn testReturnValueWithApp(comptime App: type, app: App, expected: Value, src: []const u8) !void {
    var c = try Compiler(App).init(t.allocator);
    defer c.deinit();

    c.compile(src, .{}) catch |err| {
        std.debug.print("Compilation error: {}\n", .{c.err.?});
        return err;
    };

    const byte_code = try c.byteCode(t.allocator);
    defer t.allocator.free(byte_code);
    // disassemble(App, t.allocator, byte_code, std.io.getStdErr().writer()) catch unreachable;

    var arena = std.heap.ArenaAllocator.init(t.allocator);
    defer arena.deinit();
    var vm = VM(App).init(arena.allocator(), app);

    var buf: std.ArrayListUnmanaged(u8) = .{};
    const value = vm.run(byte_code, buf.writer(t.allocator)) catch |err| {
        std.debug.print("{any}", .{err});
        if (vm.err) |e| {
            std.debug.print("{any} {s}\n", .{ err, e });
        }
        disassemble(App, t.allocator, byte_code, std.io.getStdErr().writer()) catch unreachable;
        return err;
    };

    const is_equal = expected.equal(value) catch false;
    if (is_equal == false) {
        std.debug.print("{any} != {any}\n", .{ expected, value });
        return error.NotEqual;
    }
    vm.release(value);
    try checkVMForLeaks(&vm);
}

fn testError(expected: []const u8, src: []const u8) !void {
    return testErrorWithApp(void, expected, src);
}

fn testErrorWithApp(comptime App: type, expected: []const u8, src: []const u8) !void {
    var c = Compiler(App).init(t.allocator) catch unreachable;
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

    var arena = std.heap.ArenaAllocator.init(t.allocator);
    defer arena.deinit();
    var vm = VM(void).init(arena.allocator(), {});
    try checkVMForLeaks(&vm);

    var buf: std.ArrayListUnmanaged(u8) = .{};
    _ = vm.run(byte_code, buf.writer(t.allocator)) catch {
        if (std.mem.indexOf(u8, vm.err.?, expected) == null) {
            std.debug.print("Wrong error, expected: {s} but got:\n{s}\n", .{ expected, vm.err.? });
            return error.WrongError;
        }
        return;
    };
    return error.NoError;
}


fn checkVMForLeaks(vm: anytype) !void {
    if (vm._ref_pool.count == 0) {
        return;
    }
    std.debug.print("ref pool leak: {d}\n", .{vm._ref_pool.count});
    return error.MemoryLeak;
}
