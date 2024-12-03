const std = @import("std");

const Allocator = std.mem.Allocator;
const Config = @import("config.zig").Config;

pub fn ByteCode(comptime config: Config) type {
    return struct {
        code: Buffer(config.initial_code_size),
        data: Buffer(config.initial_data_size),
        allocator: Allocator,

        const LocalIndex = config.LocalType();

        const Self = @This();

        pub fn init(allocator: Allocator) !Self {
            return .{
                .code = .{ },
                .data = .{ },
                .allocator = allocator,
            };
        }

        pub fn reset(self: *Self) void {
            self.code = .{};
            self.data = .{};
        }

        pub fn op(self: *Self, op_code: OpCode) !void {
            return self.code.write(self.allocator, &.{@intFromEnum(op_code)});
        }

        pub fn op2(self: *Self, op_code1: OpCode, op_code2: OpCode) !void {
            return self.code.write(self.allocator, &.{ @intFromEnum(op_code1), @intFromEnum(op_code2) });
        }

        pub fn @"i64"(self: *Self, value: i64) !void {
            var buf: [9]u8 = undefined;
            buf[0] = @intFromEnum(OpCode.CONSTANT_I64);
            @memcpy(buf[1..], std.mem.asBytes(&value));
            return self.code.write(self.allocator, &buf);
        }

        pub fn @"f64"(self: *Self, value: f64) !void {
            var buf: [9]u8 = undefined;
            buf[0] = @intFromEnum(OpCode.CONSTANT_F64);
            @memcpy(buf[1..], std.mem.asBytes(&value));
            return self.code.write(self.allocator, &buf);
        }

        pub fn @"bool"(self: *Self, value: bool) !void {
            var buf: [2]u8 = undefined;
            buf[0] = @intFromEnum(OpCode.CONSTANT_BOOL);
            buf[1] = if (value) 1 else 0;
            return self.code.write(self.allocator, &buf);
        }

        pub fn string(self: *Self, value: []const u8) !void {
            var buf: [5]u8 = undefined;
            const header = buf[0..4];
            const data_start: u32 = @intCast(self.data.pos);

            // Storing the end, rather than the length, means once less addition we
            // need to make when running the bytecode
            const data_end: u32 = @intCast(data_start + 4 + value.len);

            @memcpy(header, std.mem.asBytes(&data_end));
            try self.data.write(self.allocator, header);
            try self.data.write(self.allocator, value);

            buf[0] = @intFromEnum(OpCode.CONSTANT_STRING);
            @memcpy(buf[1..], std.mem.asBytes(&data_start));
            return self.code.write(self.allocator, &buf);
        }

        pub fn @"null"(self: *Self) !void {
            return self.op(OpCode.CONSTANT_NULL);
        }

        pub fn setLocal(self: *Self, local_index: LocalIndex) !void {
            var buf: [1 + @sizeOf(LocalIndex)]u8 = undefined;
            buf[0] = @intFromEnum(OpCode.SET_LOCAL);
            @memcpy(buf[1..], std.mem.asBytes(&local_index));
            return self.code.write(self.allocator, &buf);
        }

        pub fn getLocal(self: *Self, local_index: LocalIndex) !void {
            var buf: [1 + @sizeOf(LocalIndex)]u8 = undefined;
            buf[0] = @intFromEnum(OpCode.GET_LOCAL);
            @memcpy(buf[1..], std.mem.asBytes(&local_index));
            return self.code.write(self.allocator, &buf);
        }

        pub fn toBytes(self: *const Self, allocator: Allocator) ![]const u8 {
            const code = self.code;
            const data = self.data;

            const buf = try allocator.alloc(u8, 4 + code.pos + data.pos);

            const data_start = 4 + code.pos;
            const code_len: u32 = @intCast(code.pos);

            @memcpy(buf[0..4], std.mem.asBytes(&code_len));
            @memcpy(buf[4..data_start], code.buf[0..code.pos]);
            @memcpy(buf[data_start..], data.buf[0..data.pos]);

            return buf;
        }
    };
}

fn Buffer(comptime initial_size: usize) type {
    return struct {
        pos: usize = 0,
        buf: []u8 = &.{},

        const Self = @This();

        fn write(self: *Self, allocator: Allocator, data: []const u8) !void {
            var buf = self.buf;
            const pos = self.pos;
            const spare = buf.len - pos;

            if (spare < data.len) {
                if (buf.len == 0) {
                    buf = try allocator.alloc(u8, initial_size);
                    self.buf = buf;
                } else {
                    const new_capacity = buf.len * 2;
                    if (allocator.resize(buf, new_capacity)) {
                        self.buf = buf[0..new_capacity];
                    } else {
                        const new_buf = try allocator.alloc(u8, new_capacity);
                        @memcpy(new_buf[0..pos], buf[0..pos]);
                        allocator.free(buf);

                        buf = new_buf;
                        self.buf = new_buf;
                    }
                }
            }
            const end = pos + data.len;
            @memcpy(buf[pos..end], data);
            self.pos = end;
        }
    };
}

pub fn disassemble(byte_code: []const u8, writer: anytype) !void {
    var i: usize = 0;
    const code_length = @as(u32, @bitCast(byte_code[0..4].*));
    const code = byte_code[4 .. code_length + 4];

    while (i < code.len) {
        const op_code = std.meta.intToEnum(OpCode, code[i]) catch {
            try std.fmt.format(writer, "{x:0>4} ??? ({d})", .{ i, code[i] });
            return;
        };
        try std.fmt.format(writer, "{x:0>4} {s}", .{ i, @tagName(op_code) });
        i += 1;
        switch (op_code) {
            .CONSTANT_I64 => {
                const value = @as(*align(1) const i64, @ptrCast(code[i..(i + 8)])).*;
                try std.fmt.format(writer, " {d}\n", .{value});
                i += 8;
            },
            .CONSTANT_F64 => {
                const value = @as(*align(1) const f64, @ptrCast(code[i..(i + 8)])).*;
                try std.fmt.format(writer, " {d}\n", .{value});
                i += 8;
            },
            .CONSTANT_BOOL => {
                try std.fmt.format(writer, " {d}\n", .{code[i]});
                i += 1;
            },
            else => try writer.writeAll("\n"),
        }
    }
}

pub const OpCode = enum(u8) {
    ADD,
    CONSTANT_BOOL,
    CONSTANT_F64,
    CONSTANT_I64,
    CONSTANT_NULL,
    CONSTANT_STRING,
    DIVIDE,
    EQUAL,
    GET_LOCAL,
    GREATER,
    LESSER,
    MULTIPLY,
    NEGATE,
    NOT,
    POP,
    PRINT,
    RETURN,
    SET_LOCAL,
    SUBTRACT,
};

const t = @import("t.zig");
test "bytecode: write" {
    defer t.reset();
    var b = try ByteCode(.{}).init(t.arena.allocator());

    for (0..255) |i| {
        try b.code.write(b.allocator, &[_]u8{@intCast(i)});
    }

    for (b.code.buf[0..255], 0..) |data, i| {
        try t.expectEqual(@as(u8, @intCast(i)), data);
    }

    try b.code.write(b.allocator, "this is longer");
    try t.expectString("this is longer", b.code.buf[255..269]);
}

test "bytecode: write + disassemble" {
    defer t.reset();

    var b = try ByteCode(.{}).init(t.arena.allocator());
    try b.i64(-388491034);
    try b.f64(12.34567);
    try b.bool(true);
    try b.bool(false);
    try b.null();
    try b.op(OpCode.RETURN);
    try expectDisassemble(b,
        \\0000 CONSTANT_I64 -388491034
        \\0009 CONSTANT_F64 12.34567
        \\0012 CONSTANT_BOOL 1
        \\0014 CONSTANT_BOOL 0
        \\0016 CONSTANT_NULL
        \\0017 RETURN
        \\
    );
}

fn expectDisassemble(b: anytype, expected: []const u8) !void {
    var arr = std.ArrayList(u8).init(t.allocator);
    defer arr.deinit();

    const byte_code = try b.toBytes(t.allocator);
    defer t.allocator.free(byte_code);

    try disassemble(byte_code, arr.writer());
    try t.expectString(expected, arr.items);
}
