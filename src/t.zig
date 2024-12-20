const std = @import("std");

const Value = @import("value.zig").Value;
const KeyValue = @import("value.zig").KeyValue;

const Allocator = std.mem.Allocator;
pub const allocator = std.testing.allocator;

pub const expectEqual = std.testing.expectEqual;
pub const expectError = std.testing.expectError;
pub const expectSlice = std.testing.expectEqualSlices;
pub const expectString = std.testing.expectEqualStrings;

pub var arena = std.heap.ArenaAllocator.init(allocator);

pub fn reset() void {
    _ = arena.reset(.free_all);
}

// we can't use Value.from because it treats all the keys are strings (which
// makes sense when you're maping a struct -> map). But for testing, we want
// a quick way to create a Value.Map which can have either string or integer keys.
pub fn createMap(value: anytype) !Value {
    const T = @TypeOf(value);
    const fields = @typeInfo(T).@"struct".fields;

    var m: Value.Map = .{};
    try m.ensureTotalCapacity(allocator, fields.len);
    inline for (fields) |field| {
        var key: KeyValue = undefined;
        if (std.fmt.parseInt(i64, field.name, 10)) |v| {
            key = .{.i64 = v};
        } else |_| {
            key = .{.string = field.name};
        }
        m.putAssumeCapacity(key, try Value.from(allocator, @field(value, field.name)));
    }
    return .{ .map = m };
}
