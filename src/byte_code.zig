const std = @import("std");

const Allocator = std.mem.Allocator;
const Config = @import("config.zig").Config;

pub fn ByteCode(comptime config: Config) type {
    return struct {
        allocator: Allocator,
        temp: Buffer(config.initial_code_size),
        code: Buffer(config.initial_code_size),
        data: Buffer(config.initial_data_size),

        const LocalIndex = config.LocalType();
        const SL = @sizeOf(LocalIndex);

        const Self = @This();

        pub fn init(allocator: Allocator) !Self {
            return .{
                .temp = .{ },
                .code = .{ },
                .data = .{ },
                .allocator = allocator,
            };
        }

        pub fn reset(self: *Self) void {
            self.temp = .{};
            self.code = .{};
            self.data = .{};
        }

        pub fn currentPos(self: *const Self) usize {
            return self.code.pos;
        }

        pub fn op(self: *Self, op_code: OpCode) !void {
            return self.code.write(self.allocator, &.{@intFromEnum(op_code)});
        }

        pub fn op2(self: *Self, op_code1: OpCode, op_code2: OpCode) !void {
            return self.code.write(self.allocator, &.{ @intFromEnum(op_code1), @intFromEnum(op_code2) });
        }

        // Pretty unsafe. If any jumps are issued, the positioning will be messed up
        // Currently only used to capture the increment portion of the FOR loop
        // and to then stick it at the end of the block
        pub fn beginCapture(self: *Self) void {
            std.debug.assert(self.temp.pos == 0);
            self.temp = self.code;
            self.code = .{};
        }

        pub fn endCapture(self: *Self) []const u8 {
            const code = self.code.buf[0..self.code.pos];
            self.code = self.temp;
            self.temp = .{};
            return code;
        }

        pub fn write(self: *Self, data: []const u8) !void {
            return self.code.write(self.allocator, data);
        }

        pub fn jump(self: *Self, to: usize) !void {
            if (to > 65536 ){
                return error.JumpTooBig;
            }

            var buf: [3]u8 = undefined;
            buf[0] = @intFromEnum(OpCode.JUMP);

            const u16_to: u16 = @intCast(to);
            @memcpy(buf[1..], std.mem.asBytes(&u16_to));
            try self.code.write(self.allocator, &buf);
        }

        pub fn prepareJump(self: *Self, op_code: OpCode) !usize {
            const buf: [3]u8 = [_]u8{@intFromEnum(op_code), 0, 0};
            try self.code.write(self.allocator, &buf);
            return self.code.pos - 2;
        }

        pub fn finalizeJump(self: *Self, jump_pos: usize) !void {
            const pos = self.code.pos;
            if (pos > 65536 ){
                return error.JumpTooBig;
            }

            const u16_pos: u16 = @intCast(pos);
            @memcpy(self.code.buf[jump_pos..jump_pos+2], std.mem.asBytes(&u16_pos));
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

            // Storing the end, rather than the length, means one less addition
            // the VM has to do. +4 to skip the data length header itself.
            const data_end: u32 = @intCast(4 + data_start + value.len);

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
            var buf: [1 + SL]u8 = undefined;
            buf[0] = @intFromEnum(OpCode.SET_LOCAL);
            @memcpy(buf[1..], std.mem.asBytes(&local_index));
            return self.code.write(self.allocator, &buf);
        }

        pub fn getLocal(self: *Self, local_index: LocalIndex) !void {
            var buf: [1 + SL]u8 = undefined;
            buf[0] = @intFromEnum(OpCode.GET_LOCAL);
            @memcpy(buf[1..], std.mem.asBytes(&local_index));
            return self.code.write(self.allocator, &buf);
        }

        pub fn incr(self: *Self, local_index: LocalIndex, value: u8) !void {
            var buf: [2 + SL]u8 = undefined;
            buf[0] = @intFromEnum(OpCode.INCR);
            buf[1] = value;
            @memcpy(buf[2..2 + SL], std.mem.asBytes(&local_index));
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

pub fn disassemble(comptime config: Config, byte_code: []const u8, writer: anytype) !void {
    const LocalIndex = config.LocalType();
    const SL = @sizeOf(LocalIndex);

    var i: usize = 0;
    const code_length = @as(u32, @bitCast(byte_code[0..4].*));
    const code = byte_code[4 .. code_length + 4];
    const data = byte_code[code_length + 4..];

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
                try std.fmt.format(writer, " {any}\n", .{code[i] == 1});
                i += 1;
            },
            .CONSTANT_STRING => {
                const header_start = @as(u32, @bitCast(code[i..i+4][0..4].*));
                i += 4;
                const header_end = header_start + 4;
                const string_end = @as(u32, @bitCast(data[header_start..header_end][0..4].*));
                try std.fmt.format(writer, " {s}\n", .{data[header_end..string_end]});
            },
            .JUMP => {
                const pos = @as(u16, @bitCast(code[i..i+2][0..2].*));
                try std.fmt.format(writer, " {x:0>4}\n", .{pos});
                i += 2;
            },
            .JUMP_IF_FALSE => {
                const pos = @as(u16, @bitCast(code[i..i+2][0..2].*));
                try std.fmt.format(writer, " {x:0>4}\n", .{pos});
                i += 2;
            },
            .SET_LOCAL => {
                const idx = @as(LocalIndex, @bitCast(code[i..i+SL][0..SL].*));
                try std.fmt.format(writer, " @{d}\n", .{idx});
                i += SL;
            },
            .GET_LOCAL => {
                const idx = @as(LocalIndex, @bitCast(code[i..i+SL][0..SL].*));
                try std.fmt.format(writer, " @{d}\n", .{idx});
                i += SL;
            },
            .INCR => {
                // i += 0 is pretty rare. So use value 0 for -1, which is more
                // common (i.e. i--)
                const value: i16 = if (code[i] == 0) -1 else code[i];
                i += 1;
                const idx = @as(LocalIndex, @bitCast(code[i..i+SL][0..SL].*));
                i += SL;
                try std.fmt.format(writer, " @{d} {d}\n", .{idx, value});
            },
            else => try writer.writeAll("\n"),
        }
    }
}

pub const OpCode = enum(u8) {
    ADD = 0,
    CONSTANT_BOOL = 1,
    CONSTANT_F64 = 2,
    CONSTANT_I64 = 3,
    CONSTANT_NULL = 4,
    CONSTANT_STRING = 5,
    DIVIDE = 6,
    EQUAL = 7,
    GET_LOCAL = 8,
    GREATER = 10,
    INCR = 11,
    JUMP = 12,
    JUMP_IF_FALSE = 13,
    LESSER = 14,
    MULTIPLY = 15,
    NEGATE = 16,
    NOT = 17,
    POP = 18,
    PRINT = 19,
    RETURN = 20,
    SET_LOCAL = 21,
    SUBTRACT = 22,
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
        \\0012 CONSTANT_BOOL true
        \\0014 CONSTANT_BOOL false
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

    try disassemble(.{}, byte_code, arr.writer());
    try t.expectString(expected, arr.items);
}
