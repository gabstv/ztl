const std = @import("std");

const Allocator = std.mem.Allocator;

pub const Value = union(enum) {
    f64: f64,
    i64: i64,
    bool: bool,
    null: void,
    array: List,
    string: []const u8,
    property: i32,

    pub const List = std.ArrayListUnmanaged(Value);

    pub fn from(allocator: Allocator, zig: anytype) !Value {
        const T = @TypeOf(zig);
        switch (@typeInfo(T)) {
            .null => return .{ .null = {} },
            .int => |int| {
                if (int.signedness == .signed) {
                    switch (int.bits) {
                        1...64 => return .{ .i64 = zig },
                        else => {}
                    }
                } else {
                    switch (int.bits) {
                        1...63 => return .{ .i64 = zig },
                        else => {},
                    }
                }
                if (zig < std.math.minInt(i64) or zig > std.math.maxInt(i64)) {
                    return error.UnsupportedType;
                }
                return .{.i64 = @intCast(zig)};
            },
            .float => |float| {
                switch (float.bits) {
                    1...64 => return .{ .f64 = zig },
                    else => return .{ .f64 = @floatCast(zig) },
                }
            },
            .bool => return .{ .bool = zig },
            .comptime_int => return .{ .i64 = zig },
            .comptime_float => return .{ .f64 = zig },
            .pointer => |ptr| switch (ptr.size) {
                .One => switch (@typeInfo(ptr.child)) {
                    .array => {
                        const Slice = []const std.meta.Elem(ptr.child);
                        return from(allocator, @as(Slice, zig));
                    },
                    else => return from(allocator, zig.*),
                }
                .Many, .Slice => {
                    if (ptr.size == .Many and ptr.sentinel == null) {
                        return error.UnsupportedType;
                    }
                    const slice = if (ptr.size == .Many) std.mem.span(zig) else zig;
                    const child = ptr.child;
                    if (child == u8) {
                        return .{ .string = zig };
                    }

                    var arr: List = .{};
                    try arr.ensureTotalCapacity(allocator, slice.len);
                    for (slice) |v| {
                        arr.appendAssumeCapacity(try from(allocator, v));
                    }
                    return .{ .array = arr };
                },
                else => return error.UnsupportedType,
            },
            .array => |arr| {
                if (arr.child == u8) {
                    return .{.string = &zig};
                }
                return from(allocator, &zig);
            },
            .optional => |opt| {
                if (zig) |v| {
                    return from(allocator, @as(opt.child, v));
                }
                return .{ .null = {} };
            },
            .@"union" => {
                if (T == Value) {
                    return zig;
                }
                return error.UnsupportedType;
            },
            .@"struct" => {
                if (T == List) {
                    return .{.array = zig};
                }
                return error.UnsupportedType;
            },
            else => return error.UnsupportedType,
        }
    }
    pub fn deinit(self: *Value, allocator: Allocator) void {
        switch (self.*) {
            .array => |*arr| arr.deinit(allocator),
            else => {},
        }
    }

    pub fn format(self: Value, _: []const u8, _: anytype, writer: anytype) !void {
        switch (self) {
            .i64, .property => |v| return std.fmt.format(writer, "{d}", .{v}),
            .string => |v| try writer.writeAll(v),
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
            },
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
            },
            .property => return false,
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
            .property => return "property",
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
            .property => return "a property",
        }
    }
};

const t = @import("t.zig");
test "Value: isTrue" {
    try t.expectEqual(true, (Value{ .bool = true }).isTrue());
    try t.expectEqual(false, (Value{ .bool = false }).isTrue());

    try t.expectEqual(false, (Value{ .i64 = 0 }).isTrue());
    try t.expectEqual(false, (Value{ .i64 = 100 }).isTrue());
    try t.expectEqual(false, (Value{ .f64 = 0 }).isTrue());
    try t.expectEqual(false, (Value{ .f64 = 100.00 }).isTrue());
    try t.expectEqual(false, (Value{ .string = "" }).isTrue());
    try t.expectEqual(false, (Value{ .string = "true" }).isTrue());
    try t.expectEqual(false, (Value{ .null = {} }).isTrue());
}

test "Value: format" {
    try assertFormat("0", .{ .i64 = 0 });
    try assertFormat("987654", .{ .i64 = 987654 });
    try assertFormat("-1234567", .{ .i64 = -1234567 });

    try assertFormat("1.2345678", .{ .f64 = 1.2345678 });
    try assertFormat("-0.000032", .{ .f64 = -0.000032 });

    try assertFormat("true", .{ .bool = true });
    try assertFormat("false", .{ .bool = false });

    try assertFormat("null", .{ .null = {} });

    try assertFormat("", .{ .string = "" });
    try assertFormat("hello world", .{ .string = "hello world" });

    try assertFormat("[]", .{ .array = .{} });

    var arr = [_]Value{
        .{ .i64 = -3 },
        .{ .bool = true },
        .{ .string = "over 9000" },
    };
    try assertFormat("[-3, true, over 9000]", .{ .array = .{ .items = &arr } });
}

test "Value: equal" {
    try assertEqual(true, .{ .i64 = 0 }, .{ .i64 = 0 });
    try assertEqual(true, .{ .i64 = -10 }, .{ .i64 = -10 });
    try assertEqual(true, .{ .i64 = 99 }, .{ .f64 = 99.0 });
    try assertEqual(false, .{ .i64 = 0 }, .{ .i64 = 1 });
    try assertEqual(false, .{ .i64 = 99 }, .{ .f64 = 99.1 });
    try assertEqual(false, .{ .i64 = 0 }, .{ .null = {} });
    try assertEqual(false, .{ .i64 = 94 }, .{ .null = {} });

    try assertEqual(true, .{ .f64 = 0.32 }, .{ .f64 = 0.32 });
    try assertEqual(true, .{ .f64 = -102.32 }, .{ .f64 = -102.32 });
    try assertEqual(true, .{ .f64 = -942.0 }, .{ .i64 = -942 });
    try assertEqual(false, .{ .f64 = 0.32 }, .{ .f64 = 1.32 });
    try assertEqual(false, .{ .f64 = -942.1 }, .{ .i64 = -942 });
    try assertEqual(false, .{ .f64 = 0 }, .{ .null = {} });
    try assertEqual(false, .{ .f64 = 1.32 }, .{ .null = {} });

    try assertEqual(true, .{ .bool = true }, .{ .bool = true });
    try assertEqual(true, .{ .bool = false }, .{ .bool = false });
    try assertEqual(false, .{ .bool = true }, .{ .bool = false });
    try assertEqual(false, .{ .bool = false }, .{ .bool = true });
    try assertEqual(false, .{ .bool = true }, .{ .null = {} });
    try assertEqual(false, .{ .bool = false }, .{ .null = {} });

    try assertEqual(true, .{ .string = "" }, .{ .string = "" });
    try assertEqual(true, .{ .string = "abc123" }, .{ .string = "abc123" });
    try assertEqual(false, .{ .string = "abc123" }, .{ .string = "ABC123" });
    try assertEqual(false, .{ .string = "abc123" }, .{ .null = {} });

    try assertEqual(true, .{ .null = {} }, .{ .null = {} });
    try assertEqual(false, .{ .null = {} }, .{ .i64 = 0 });
    try assertEqual(false, .{ .null = {} }, .{ .i64 = 4 });
    try assertEqual(false, .{ .null = {} }, .{ .bool = true });
    try assertEqual(false, .{ .null = {} }, .{ .bool = false });

    try assertEqual(true, .{ .null = {} }, .{ .null = {} });
    try assertEqual(false, .{ .null = {} }, .{ .i64 = 0 });
    try assertEqual(false, .{ .null = {} }, .{ .i64 = 4 });
    try assertEqual(false, .{ .null = {} }, .{ .bool = true });
    try assertEqual(false, .{ .null = {} }, .{ .bool = false });

    var arr1 = [_]Value{
        .{ .i64 = -3 },
        .{ .bool = true },
        .{ .string = "over 9000" },
    };

    var arr2 = [_]Value{
        .{ .i64 = -3 },
        .{ .bool = true },
        .{ .string = "over 9000!!" },
    };

    var arr3 = [_]Value{
        .{ .i64 = -3 },
        .{ .bool = true },
    };
    try assertEqual(true, .{ .array = .{ .items = &arr1 } }, .{ .array = .{ .items = &arr1 } });
    try assertEqual(false, .{ .array = .{ .items = &arr1 } }, .{ .array = .{ .items = &arr2 } });
    try assertEqual(false, .{ .array = .{ .items = &arr1 } }, .{ .array = .{ .items = &arr3 } });
    try assertEqual(false, .{ .array = .{ .items = &arr2 } }, .{ .array = .{ .items = &arr3 } });
}

test "Value: from" {
    {
        // null
        try t.expectEqual({}, (try Value.from(undefined, null)).null);
        try t.expectEqual({}, (try Value.from(undefined, @as(?i32, null))).null);
    }

    {
        // bool
        try t.expectEqual(true, (try Value.from(undefined, true)).bool);
        try t.expectEqual(false, (try Value.from(undefined, false)).bool);
        try t.expectEqual(true, (try Value.from(undefined, @as(?bool, true))).bool);
        try t.expectEqual(false, (try Value.from(undefined, @as(?bool, false))).bool);
    }

    {
        // int
        try t.expectEqual(0, (try Value.from(undefined, 0)).i64);
        try t.expectEqual(33, (try Value.from(undefined, 33)).i64);
        try t.expectEqual(9_223_372_036_854_775_807, (try Value.from(undefined, @as(i64, 9_223_372_036_854_775_807))).i64);
        try t.expectEqual(-9_223_372_036_854_775_808, (try Value.from(undefined, @as(i64, -9_223_372_036_854_775_808))).i64);
        try t.expectEqual(495, (try Value.from(undefined, @as(i128, 495))).i64);
        try t.expectEqual(-949492, (try Value.from(undefined, @as(i128, -949492))).i64);
        try t.expectEqual(-88811123, (try Value.from(undefined, @as(i64, -88811123))).i64);

        try t.expectEqual(22, (try Value.from(undefined, @as(i32, 22))).i64);
        try t.expectEqual(1, (try Value.from(undefined, @as(u8, 1))).i64);
        try t.expectEqual(-5, (try Value.from(undefined, @as(i8, -5))).i64);

        try t.expectEqual(9592, (try Value.from(undefined, @as(?i16, 9592))).i64);
    }

    {
        // float
        try t.expectEqual(0.0, (try Value.from(undefined, 0.0)).f64);
        try t.expectEqual(-33.2, (try Value.from(undefined, -33.2)).f64);
        try t.expectEqual(0.01, (try Value.from(undefined, @as(f128, 0.01))).f64);
        try t.expectEqual(-2.9422100830078125e2, (try Value.from(undefined, @as(f32, -294.221))).f64);
        try t.expectEqual(99.123, (try Value.from(undefined, @as(f64, 99.123))).f64);
        try t.expectEqual(9.592e3, (try Value.from(undefined, @as(?f16, 9592.1))).f64);
    }

    {
        // string
        try t.expectString("", (try Value.from(undefined, "")).string);
        try t.expectString("abc", (try Value.from(undefined, "abc")).string);
        try t.expectString(&.{40,41,42}, (try Value.from(undefined, [_]u8{40,41,42})).string);

        const str = try t.allocator.dupe(u8, "hello");
        defer t.allocator.free(str);
        try t.expectString("hello", (try Value.from(undefined, str)).string);
    }

    {
        // arrays
        var arr1 = try Value.from(t.allocator, [3]i64{1, 99, -32});
        defer arr1.deinit(t.allocator);

        try t.expectEqual(3, arr1.array.items.len);
        try t.expectEqual(1, arr1.array.items[0].i64);
        try t.expectEqual(99, arr1.array.items[1].i64);
        try t.expectEqual(-32, arr1.array.items[2].i64);
    }

    {
        // value
        try t.expectEqual(123.456, (try Value.from(undefined, Value{.f64 = 123.456})).f64);
    }

    {
        // list of value
        var values: Value.List = .{};
        defer values.deinit(t.allocator);
        try values.append(t.allocator, .{.bool = false});
        try values.append(t.allocator, .{.null = {}});
        try values.append(t.allocator, .{.string = "Teg"});

        const from = try Value.from(undefined, values);
        try t.expectEqual(values.items.ptr, from.array.items.ptr);
    }

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
