const std = @import("std");

const Allocator = std.mem.Allocator;
const MemoryPool = std.heap.MemoryPool;

pub const Value = union(enum) {
    pub const List = std.ArrayListUnmanaged(Value);
    pub const Map = std.ArrayHashMapUnmanaged(KeyValue, Value, KeyValue.Context, true);

    i64: i64,
    f64: f64,
    bool: bool,
    null: void,
    ref: *Ref,
    property: i32,
    string: []const u8,

    pub const Ref = struct {
        count: u16 = 1,
        value: union(enum) {
            map_entry: Map.Entry,
            map: Value.Map,
            list: Value.List,
            map_iterator: MapIterator,
            list_iterator: ListIterator,
        },

        fn dereference(self: *Ref, ref_allocator: *MemoryPool(Value.Ref)) void {
            const count = self.count;
            if (count > 1) {
                self.count = count - 1;
                return;
            }

            switch (self.value) {
                .map_iterator => |it| it.ref.dereference(ref_allocator),
                .list_iterator => |it| it.ref.dereference(ref_allocator),
                else => {},
            }
            ref_allocator.destroy(self);
        }
    };

    pub fn format(self: Value, _: []const u8, _: anytype, writer: anytype) !void {
        return self.write(writer);
    }

    pub fn write(self: Value, writer: anytype) !void {
        switch (self) {
            .i64, .property => |v| return std.fmt.formatInt(v, 10, .lower, .{}, writer),
            .bool => |v| return writer.writeAll(if (v) "true" else "false"),
            .f64 => |v| return std.fmt.format(writer, "{d}", .{v}),
            .null => return writer.writeAll("null"),
            .string => |v| try writer.writeAll(v),
            .ref => |ref| switch (ref.value) {
                .list => |list| {
                    var items = list.items;
                    if (items.len == 0) {
                        return writer.writeAll("[]");
                    }
                    try writer.writeByte('[');
                    try items[0].write(writer);
                    for (items[1..]) |v| {
                        try writer.writeAll(", ");
                        try v.write(writer);
                    }
                    return writer.writeByte(']');
                },
                .map => |map| {
                    var it = map.iterator();
                    try writer.writeByte('{');
                    if (it.next()) |first| {
                        try first.key_ptr.*.write(writer);
                        try writer.writeAll(": ");
                        try first.value_ptr.*.write(writer);

                        while (it.next()) |kv| {
                            try writer.writeAll(", ");
                            try kv.key_ptr.*.write(writer);
                            try writer.writeAll(": ");
                            try kv.value_ptr.*.write(writer);
                        }
                    }
                    return writer.writeByte('}');
                },
                .list_iterator => return writer.writeAll("[...]"),
                .map_iterator => return writer.writeAll("{...}"),
                .map_entry => |entry| {
                    try entry.key_ptr.*.write(writer);
                    try writer.writeAll(": ");
                    return entry.value_ptr.*.write(writer);
                },
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
            .property => return false,
            .ref => |ref| {
                switch (other) {
                    .ref => {},
                    .null => return false,
                    else => return error.Incompatible,
                }

                switch (ref.value) {
                    .list => |l| switch (other.ref.value) {
                        .list => |r| {
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
                        else => {},
                    },
                    .map => |l| switch (other.ref.value) {
                        .map => |r| {
                            if (l.count() != r.count()) {
                                return false;
                            }
                            var it = l.iterator();
                            while (it.next()) |kv| {
                                const rv = r.get(kv.key_ptr.*) orelse return false;
                                if (try kv.value_ptr.equal(rv) == false) {
                                    return false;
                                }
                            }
                            return true;
                        },
                        else => {},
                    },
                    .map_entry => |l| switch (other.ref.value) {
                        .map_entry => |r| return l.key_ptr.equal(r.key_ptr.*) and try l.value_ptr.equal(r.value_ptr.*),
                        else => {},
                    },
                    .list_iterator, .map_iterator => return false,
                }
            },
        }
        return error.Incompatible;
    }

    pub fn reference(self: Value) Value {
        switch (self) {
            .ref => |ref| ref.count += 1,
            else => {},
        }
        return self;
    }

    pub fn dereference(self: Value, ref_allocator: *MemoryPool(Value.Ref)) void {
        if (self != .ref) {
            return;
        }
        self.ref.dereference(ref_allocator);
    }

    pub fn friendlyName(self: Value) []const u8 {
        switch (self) {
            .i64 => return "integer",
            .f64 => return "float",
            .bool => return "boolean",
            .null => return "null",
            .string => return "string",
            .property => return "property",
            .ref => |ref| switch (ref.value) {
                .list => return "list",
                .map => return "map",
                .map_entry => return "map entry",
                .map_iterator => return "map iterator",
                .list_iterator => return "list iterator",
            },
        }
    }

    pub fn friendlyArticleName(self: Value) []const u8 {
        switch (self) {
            .i64 => return "an integer",
            .f64 => return "a float",
            .bool => return "a boolean",
            .null => return "null",
            .property => return "a property",
            .string => return "a string",
            .ref => |ref| switch (ref.value) {
                .list => return "a list",
                .map => return "a map",
                .map_entry => return "a map entry",
                .map_iterator => return "a map iterator",
                .list_iterator => return "a list iterator",
            },
        }
    }
};

pub const KeyValue = union(enum) {
    i64: i64,
    string: []const u8,

    pub fn format(self: KeyValue, _: []const u8, _: anytype, writer: anytype) !void {
        return self.write(writer);
    }

    pub fn write(self: KeyValue, writer: anytype) !void {
        switch (self) {
            .i64, => |v| return std.fmt.formatInt(v, 10, .lower, .{}, writer),
            .string => |v| try writer.writeAll(v),
        }
    }


    pub fn toValue(self: KeyValue) Value {
        switch (self) {
            .i64, => |v| return .{.i64 = v},
            .string, => |v| return .{.string = v},
        }
    }

    pub fn equal(self: KeyValue, other: KeyValue) bool {
        switch (self) {
            .i64, => |l| switch (other) {
                .i64 => |r| return l == r,
                .string => return false,
            },
            .string => |l| switch (other) {
                .string => |r| return std.mem.eql(u8, l, r),
                .i64 => return false,
            },
        }
    }

    const Wyhash = std.hash.Wyhash;

    const Context = struct {
        pub fn hash(_: Context, key: KeyValue) u32 {
            switch (key) {
                .i64 => |v| return @as(u32, @truncate(Wyhash.hash(0, std.mem.asBytes(&v)))),
                .string => |v| return @as(u32, @truncate(Wyhash.hash(0, v))),
            }
        }

        pub fn eql(_: Context, a: KeyValue, b: KeyValue, _: usize) bool {
            switch (a) {
                .i64 => |av| switch (b) {
                    .i64 => |bv| return av == bv,
                    .string => return false,
                },
                .string => |av| switch (b) {
                    .string => |bv| return std.mem.eql(u8, av, bv),
                    .i64 => return false,
                },
            }
        }
    };
};

pub const ListIterator = struct {
    index: usize,
    list: *const Value.List,
    ref: *Value.Ref, // the Value.Ref of the list, which we need to deference when the iterator goes out of scope
};

pub const MapIterator = struct {
    inner: Value.Map.Iterator,
    ref: *Value.Ref, // the Value.Ref of the map, which we need to deference when the iterator goes out of scope
};

const t = @import("t.zig");
test "Value: isTrue" {
    try t.expectEqual(true, (Value{ .bool = true }).isTrue());
    try t.expectEqual(false, (Value{ .bool = false }).isTrue());

    try t.expectEqual(false, (Value{ .i64 = 0 }).isTrue());
    try t.expectEqual(false, (Value{ .i64 = 100 }).isTrue());
    try t.expectEqual(false, (Value{ .f64 = 0 }).isTrue());
    try t.expectEqual(false, (Value{ .f64 = 100.00 }).isTrue());
    try t.expectEqual(false, (Value{ .null = {} }).isTrue());
}

test "Value: format" {
    defer t.reset();

    try assertFormat("0", .{ .i64 = 0 });
    try assertFormat("987654", .{ .i64 = 987654 });
    try assertFormat("-1234567", .{ .i64 = -1234567 });

    try assertFormat("1.2345678", .{ .f64 = 1.2345678 });
    try assertFormat("-0.000032", .{ .f64 = -0.000032 });

    try assertFormat("true", .{ .bool = true });
    try assertFormat("false", .{ .bool = false });

    try assertFormat("null", .{ .null = {} });

    try assertFormat("", .{.string = ""});
    try assertFormat("hello world", .{.string = "hello world"});

    {
        var arr = [_]Value{};
        try assertFormat("[]", t.createListRef(&arr));
    }

    var arr = [_]Value{
        .{ .i64 = -3 },
        .{ .bool = true },
        .{.string = "over 9000"}
    };
    try assertFormat("[-3, true, over 9000]", t.createListRef(&arr));


    try assertFormat("{}", t.createMapRef(&.{}, &.{}));
    try assertFormat("{name: Leto, 123: true, arr: [-3, true, over 9000]}", t.createMapRef(
        &.{"name", "123", "arr"},
        &.{.{.string = "Leto"}, .{.bool = true}, t.createListRef(&arr)}
    ));
}

test "Value: equal" {
    defer t.reset();

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

    try assertEqual(true, .{.string = ""}, .{.string = ""});
     try assertEqual(true, .{.string = "abc123"}, .{.string = "abc123"});
    try assertEqual(false, .{.string = "abc123"}, .{.string = "ABC123"});
    try assertEqual(false, .{.string = "abc123"}, .{ .null = {} });

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
        .{.string = "over 9000"},
    };

    var arr2 = [_]Value{
        .{ .i64 = -3 },
        .{ .bool = true },
        .{.string = "over 9000!!"},
    };

    var arr3 = [_]Value{
        .{ .i64 = -3 },
        .{ .bool = true },
    };

    try assertEqual(true, t.createListRef(&arr1), t.createListRef(&arr1));
    try assertEqual(false, t.createListRef(&arr1), t.createListRef(&arr2));
    try assertEqual(false, t.createListRef(&arr1), t.createListRef(&arr3));
    try assertEqual(false, t.createListRef(&arr2), t.createListRef(&arr3));
    try assertEqual(false, t.createListRef(&arr2), .{ .null = {} });
    try assertEqualIncompatible(t.createListRef(&arr2), .{ .i64 = 2 });
    try assertEqualIncompatible(t.createListRef(&arr2), .{ .f64 = -95.11 });
    try assertEqualIncompatible(t.createListRef(&arr2), .{.string = "hello"});
    try assertEqualIncompatible(t.createListRef(&arr2), .{ .bool = true });

    const map1 = t.createMapRef(
        &.{"name", "123", "arr"},
        &.{.{.string = "Leto"}, .{.bool = true}, t.createListRef(&arr1)}
    );

    const map2 = t.createMapRef(
        &.{"name", "123", "arr"},
        &.{.{.string = "Leto"}, .{.bool = true}, t.createListRef(&arr1)}
    );

    // 122 is a different key
    const map3 = t.createMapRef(
        &.{"name", "122", "arr"},
        &.{.{.string = "Leto"}, .{.bool = true}, t.createListRef(&arr1)}
    );

    // LETO is a different value
    const map4 = t.createMapRef(
        &.{"name", "123", "arr"},
        &.{.{.string = "LETO"}, .{.bool = true}, t.createListRef(&arr1)}
    );

    // extra key
    const map5 = t.createMapRef(
        &.{"name", "123", "arr", "more"},
        &.{.{.string = "LETO"}, .{.bool = true}, t.createListRef(&arr1), .{.f64 = 1.344}},
    );

    try assertEqual(true, map1, map1);
    try assertEqual(true, map1, map2);
    try assertEqual(false, map1, map3);
    try assertEqual(false, map2, map3);
    try assertEqual(false, map1, map4);
    try assertEqual(false, map1, map5);
    try assertEqual(false, map1, .{ .null = {} });
    try assertEqualIncompatible(map1, .{ .i64 = 2 });
    try assertEqualIncompatible(map1, .{ .f64 = -95.11 });
    try assertEqualIncompatible(map1, .{.string = "hello"});
    try assertEqualIncompatible(map1, .{ .bool = true });
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

fn assertEqualIncompatible(left: Value, right: Value) !void {
    try t.expectError(error.Incompatible, left.equal(right));
}
