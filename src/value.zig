const std = @import("std");

pub const Value = union(enum) {
    f64: f64,
    i64: i64,
    bool: bool,
    null: void,
    string: []const u8,

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
        }
    }

    pub fn isTrue(self: Value) bool {
        return self == .bool and self.bool;
    }
};
