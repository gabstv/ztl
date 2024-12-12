const std = @import("std");


pub const Value = union(enum) {
    f64: f64,
    i64: i64,
    bool: bool,
    null: void,
    array: List,
    string: []const u8,

    pub const List = std.ArrayListUnmanaged(Value);

    pub fn format(self: Value, _: []const u8, _: anytype, writer: anytype) !void {
        switch (self) {
            .i64 => |v| return std.fmt.format(writer, "{d}", .{v}),
            .string => |v| {
                try writer.writeByte('"');
                try writer.writeAll(v);
                return writer.writeByte('"');
            },
            .bool => |v| return writer.writeAll(if (v) "true" else "false"),
            .f64 => |v| return std.fmt.format(writer, "{d}", .{v}),
            .null => return writer.writeAll("null"),
            .array => |arr| {
                if (arr.items.len == 0) {
                    return writer.writeAll("[]");
                }
                try writer.writeByte('[');
                try std.fmt.format(writer, "{}", .{arr.items[0]});
                for (arr.items[1..]) |v| {
                    try writer.writeAll(", ");
                    try std.fmt.format(writer, "{}", .{v});
                }
                try writer.writeByte(']');
            }
        }
    }

    pub fn isTrue(self: Value) bool {
        return self == .bool and self.bool;
    }

    pub fn equal(self: Value, other: Value) error{Incompatible}!bool {
        switch (self) {
            .bool => |l| switch (other) {
                .bool => |r| return l == r,
                .null => return false,
                else => {},
            },
            .f64 => |l| switch (other) {
                .f64 => |r| return l == r,
                .i64 => |r| return l == @as(f64, @floatFromInt(r)),
                .null => return false,
                else => {},
            },
            .i64 => |l| switch (other) {
                .i64 => |r| return l == r,
                .f64 => |r| return @as(f64, @floatFromInt(l)) == r,
                .null => return false,
                else => {},
            },
            .null => return other == .null,
            .string => |l| switch (other) {
                .string => |r| return std.mem.eql(u8, l, r),
                .null => return false,
                else => {},
            },
            .array => |l| switch (other) {
                .array => |r| {
                    if (l.items.len != r.items.len) {
                        return false;
                    }
                    for (l.items, r.items) |ll, rr| {
                        const result = equal(ll, rr) catch return false;
                        if (result == false) {
                            return false;
                        }
                    }
                    return true;
                },
                .null => return false,
                else => {},
            }
        }
        return error.Incompatible;
    }

    pub fn friendlyName(self: Value) []const u8 {
        switch (self) {
            .i64 => return "integer",
            .f64 => return "float",
            .bool => return "boolean",
            .null => return "null",
            .string => return "string",
            .array => return "array",
        }
    }

    pub fn friendlyArticleName(self: Value) []const u8 {
        switch (self) {
            .i64 => return "an integer",
            .f64 => return "a float",
            .bool => return "a boolean",
            .null => return "null",
            .string => return "a string",
            .array => return "an array",
        }
    }
};

const t = @import("t.zig");
test "Value: isTrue" {
    try t.expectEqual(true, (Value{.bool = true}).isTrue());
    try t.expectEqual(false, (Value{.bool = false}).isTrue());

    try t.expectEqual(false, (Value{.i64 = 0}).isTrue());
    try t.expectEqual(false, (Value{.i64 = 100}).isTrue());
    try t.expectEqual(false, (Value{.f64 = 0}).isTrue());
    try t.expectEqual(false, (Value{.f64 = 100.00}).isTrue());
    try t.expectEqual(false, (Value{.string = ""}).isTrue());
    try t.expectEqual(false, (Value{.string = "true"}).isTrue());
    try t.expectEqual(false, (Value{.null = {}}).isTrue());
}

test "Value: format" {
    try assertFormat("0", .{.i64 = 0});
    try assertFormat("987654", .{.i64 = 987654});
    try assertFormat("-1234567", .{.i64 = -1234567});

    try assertFormat("1.2345678", .{.f64 = 1.2345678});
    try assertFormat("-0.000032", .{.f64 = -0.000032});

    try assertFormat("true", .{.bool = true});
    try assertFormat("false", .{.bool = false});

    try assertFormat("null", .{.null = {}});

    try assertFormat("\"\"", .{.string = ""});
    try assertFormat("\"hello world\"", .{.string = "hello world"});

    try assertFormat("[]", .{.array = .{}});

    var arr = [_]Value{
        .{.i64 = -3},
        .{.bool = true},
        .{.string = "over 9000"},
    };
    try assertFormat("[-3, true, \"over 9000\"]", .{.array = .{.items = &arr}});
}

test "Value: equal" {
    try assertEqual(true, .{.i64 = 0}, .{.i64 = 0});
    try assertEqual(true, .{.i64 = -10}, .{.i64 = -10});
    try assertEqual(true, .{.i64 = 99}, .{.f64 = 99.0});
    try assertEqual(false, .{.i64 = 0}, .{.i64 = 1});
    try assertEqual(false, .{.i64 = 99}, .{.f64 = 99.1});
    try assertEqual(false, .{.i64 = 0}, .{.null = {}});
    try assertEqual(false, .{.i64 = 94}, .{.null = {}});

    try assertEqual(true, .{.f64 = 0.32}, .{.f64 = 0.32});
    try assertEqual(true, .{.f64 = -102.32}, .{.f64 = -102.32});
    try assertEqual(true, .{.f64 = -942.0}, .{.i64 = -942});
    try assertEqual(false, .{.f64 = 0.32}, .{.f64 = 1.32});
    try assertEqual(false, .{.f64 = -942.1}, .{.i64 = -942});
    try assertEqual(false, .{.f64 = 0}, .{.null = {}});
    try assertEqual(false, .{.f64 = 1.32}, .{.null = {}});

    try assertEqual(true, .{.bool = true}, .{.bool = true});
    try assertEqual(true, .{.bool = false}, .{.bool = false});
    try assertEqual(false, .{.bool = true}, .{.bool = false});
    try assertEqual(false, .{.bool = false}, .{.bool = true});
    try assertEqual(false, .{.bool = true}, .{.null = {}});
    try assertEqual(false, .{.bool = false}, .{.null = {}});

    try assertEqual(true, .{.string = ""}, .{.string = ""});
    try assertEqual(true, .{.string = "abc123"}, .{.string = "abc123"});
    try assertEqual(false, .{.string = "abc123"}, .{.string = "ABC123"});
    try assertEqual(false, .{.string = "abc123"}, .{.null = {}});

    try assertEqual(true, .{.null = {}}, .{.null = {}});
    try assertEqual(false, .{.null = {}}, .{.i64 = 0});
    try assertEqual(false, .{.null = {}}, .{.i64 = 4});
    try assertEqual(false, .{.null = {}}, .{.bool = true});
    try assertEqual(false, .{.null = {}}, .{.bool = false});

    try assertEqual(true, .{.null = {}}, .{.null = {}});
    try assertEqual(false, .{.null = {}}, .{.i64 = 0});
    try assertEqual(false, .{.null = {}}, .{.i64 = 4});
    try assertEqual(false, .{.null = {}}, .{.bool = true});
    try assertEqual(false, .{.null = {}}, .{.bool = false});

    var arr1 = [_]Value{
        .{.i64 = -3},
        .{.bool = true},
        .{.string = "over 9000"},
    };

    var arr2 = [_]Value{
        .{.i64 = -3},
        .{.bool = true},
        .{.string = "over 9000!!"},
    };

    var arr3 = [_]Value{
        .{.i64 = -3},
        .{.bool = true},
    };
    try assertEqual(true, .{.array = .{.items = &arr1}}, .{.array = .{.items = &arr1}});
    try assertEqual(false, .{.array = .{.items = &arr1}}, .{.array = .{.items = &arr2}});
    try assertEqual(false, .{.array = .{.items = &arr1}}, .{.array = .{.items = &arr3}});
    try assertEqual(false, .{.array = .{.items = &arr2}}, .{.array = .{.items = &arr3}});
}

fn assertFormat(expected: []const u8, value: Value) !void {
    var arr = std.ArrayList(u8).init(t.allocator);
    defer arr.deinit();

    try std.fmt.format(arr.writer(), "{}", .{value});
    try t.expectString(expected, arr.items);
}


fn assertEqual(expected: bool, left: Value, right: Value) !void {
    try t.expectEqual(expected, left.equal(right));
}
