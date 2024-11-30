const std = @import("std");

pub const VM = @import("VM.zig");
pub const Token = @import("Token.zig");
pub const Scanner = @import("Scanner.zig");
pub const Compiler = @import("Compiler.zig");
pub const ByteCode = @import("ByteCode.zig");

pub const Value = union(enum) {
    f64: f64,
    i64: i64,
    string: []const u8,
    bool: bool,
    null: void,

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

    // pub const Type = enum {
    //     FLOAT,
    //     INTEGER,
    //     STRING,
    //     BOOL,
    //     NULL,

    //     pub fn format(self: Type, _: []const u8, _: anytype, writer: anytype) !void {
    //         return writer.writeAll(switch (self) {
    //             .FLOAT => "float",
    //             .INTEGER => "int",
    //             .STRING => "string",
    //             .BOOL => "bool",
    //             .NULL => "null",
    //         });
    //     }
    // };
};

pub const Position = struct {
    // the byte in src this token starts at
    pos: u32,

    // the line this token is on (1-based)
    line: u32,

    // the byte in src of the line this token is on, the actual token position on
    // the line is at pos - line_start
    line_start: u32,

    pub const ZERO = Position{.pos = 0, .line = 0, .line_start = 0};
};

pub const OpCode = enum(u8) {
    ADD,
    CONSTANT_BOOL,
    CONSTANT_F64,
    CONSTANT_I64,
    CONSTANT_NULL,
    CONSTANT_STRING,
    DIVIDE,
    EQUAL,
    GREATER,
    LESSER,
    MULTIPLY,
    NEGATE,
    NOT,
    RETURN,
    SUBTRACT,
};

pub const testing = @import("t.zig");
