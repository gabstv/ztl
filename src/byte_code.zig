const std = @import("std");

const Allocator = std.mem.Allocator;
const config = @import("config.zig");

pub const VERSION: u8 = 0;

pub fn ByteCode(comptime App: type) type {
    const MAX_CALL_FRAMES = config.extract(App, "zt_max_call_frames");
    const INITIAL_CODE_SIZE = config.extract(App, "zt_initial_code_size");
    const INITIAL_DATA_SIZE = config.extract(App, "zt_initial_data_size");

    return struct {
        allocator: Allocator,
        // We have a single-pass compiler. But in very simple cases, we'll do
        // something fancy that requires some lookahead. For example, the increment
        // part of a for loop (i.e. for (initial; condition; increment)) will be
        // read into "temp" and then glued to the end of the block.
        temp: Buffer(INITIAL_CODE_SIZE),

        data: Buffer(INITIAL_DATA_SIZE),

        frame_count: usize,
        frames: [MAX_CALL_FRAMES]Buffer(INITIAL_CODE_SIZE),

        // will reference frames[frame_count]
        frame: *Buffer(INITIAL_CODE_SIZE),

        // the full code (basically, merging of all the frames)
        code: Buffer(INITIAL_CODE_SIZE),

        const LocalIndex = config.LocalType(App);
        const SL = @sizeOf(LocalIndex);

        const Self = @This();

        pub fn init(allocator: Allocator) !Self {
            return .{
                .temp = .{},
                .data = .{},
                .code = .{},
                .frame = undefined,
                .frames = undefined,
                .frame_count = 0,
                .allocator = allocator,
            };
        }

        pub fn currentPos(self: *const Self) u32 {
            return self.frame.pos;
        }

        pub fn op(self: *Self, op_code: OpCode) !void {
            return self.frame.write(self.allocator, &.{@intFromEnum(op_code)});
        }

        pub fn op2(self: *Self, op_code1: OpCode, op_code2: OpCode) !void {
            return self.frame.write(self.allocator, &.{ @intFromEnum(op_code1), @intFromEnum(op_code2) });
        }

        pub fn beginScript(self: *Self) void {
            const frame_count = self.frame_count;
            std.debug.assert(frame_count == 0);

            self.frames[0] = .{};
            self.frame = &self.frames[0];
        }

        pub fn beginFunction(self: *Self, name: []const u8) !void {
            const fc = self.frame_count + 1;
            self.frames[fc] = .{};
            self.frame = &self.frames[fc];
            self.frame_count = fc;

            if (comptime config.shouldDebug(App, .full)) {
                try self.debug(.FUNCTION_NAME, 1 + @as(u8, @intCast(name.len)));
                try self.frame.write(self.allocator, &.{@intCast(name.len)});
                try self.frame.write(self.allocator, name);
            }
        }

        pub fn endFunction(self: *Self, data_pos: u32, arity: u8) !u32 {
            // this is where the function code starts
            const code_pos: u32 = @intCast(self.code.pos);

            // copy our function code from the frame into the final code
            try self.code.write(self.allocator, self.frame.buf[0..self.frame.pos]);

            // fill in the frame's data header (the arity and the position in code)
            self.data.buf[data_pos] = arity;
            @memcpy(self.data.buf[data_pos + 1 .. data_pos + 5], std.mem.asBytes(&code_pos));

            const frame_count = self.frame_count - 1;
            self.frame_count = frame_count;
            self.frame = &self.frames[frame_count];
            return code_pos;
        }

        pub fn newFunction(self: *Self, name: []const u8) !u32 {
            std.debug.assert(name.len <= 255);

            const pos = self.data.pos;

            // placeholder until endFunction is called
            try self.data.write(self.allocator, &.{
                0, //arity
                0, 0, 0, 0, // position in code
            });

            if (comptime config.shouldDebug(App, .minimal)) {
                try self.data.write(self.allocator, &.{@intCast(name.len)});
                try self.data.write(self.allocator, name);
            }

            return pos;
        }

        // Pretty unsafe. If any jumps are issued, the positioning will be messed up
        // Currently only used to capture the increment portion of the FOR loop
        // and to then stick it at the end of the block
        pub fn beginCapture(self: *Self) void {
            std.debug.assert(self.temp.pos == 0);
            self.frame = &self.temp;
        }

        pub fn endCapture(self: *Self) []const u8 {
            const code = self.frame.buf[0..self.frame.pos];
            self.frame = &self.frames[self.frame_count];
            self.temp.pos = 0;
            return code;
        }

        pub fn write(self: *Self, data: []const u8) !void {
            return self.frame.write(self.allocator, data);
        }

        pub fn jump(self: *Self, jump_pos: u32) !void {
            // jump is only ever used when we're jumping backwards. If we were
            // jumping forwards, we'd need to use prepareJump + finalizeJump
            // since the final jump location wouldn't be known yet.
            std.debug.assert(self.frame.pos > jump_pos);

            // -1 because when the VM executes this, it'll alread have read
            // the JUMP opcode, and we want our backwards jump to skip that
            // also
            const relative: i64 = @as(i64, jump_pos) - self.frame.pos - 1;
            if (relative < -32_768) {
                return error.JumpTooLarge;
            }

            try self.op(.JUMP);
            var relative_i16: i16 = @intCast(relative);
            try self.frame.write(self.allocator, std.mem.asBytes(&relative_i16));
        }

        pub fn prepareJump(self: *Self, op_code: OpCode) !u32 {
            try self.op(op_code);
            // create placeholder for jump address (which finalizeJump will fill)
            try self.frame.write(self.allocator, &.{ 0, 0 });
            return @intCast(self.frame.pos);
        }

        pub fn finalizeJump(self: *Self, jump_pos: u32) !void {
            // finalize jump is only ever used when we're jumping forward
            // (we use prepareJump + finalizeJump because the address to jump
            // to isn't known yet, so it must be ahead);
            std.debug.assert(jump_pos < self.frame.pos);

            // +2 because when the VM executes this, it'll be at the 2 byte offset
            // and we want it to jump that as well.
            const relative: i64 = 2 + self.frame.pos - @as(i64, jump_pos);
            if (relative > 32_767) {
                return error.JumpTooLarge;
            }

            const relative_i16: i16 = @intCast(relative);
            @memcpy(self.frame.buf[jump_pos - 2 .. jump_pos], std.mem.asBytes(&relative_i16));
        }

        pub fn call(self: *Self, data_pos: u32) !void {
            try self.op(.CALL);
            return self.frame.write(self.allocator, std.mem.asBytes(&data_pos));
        }

        pub fn @"i64"(self: *Self, value: i64) !void {
            try self.op(.CONSTANT_I64);
            return self.frame.write(self.allocator, std.mem.asBytes(&value));
        }

        pub fn @"f64"(self: *Self, value: f64) !void {
            try self.op(.CONSTANT_F64);
            return self.frame.write(self.allocator, std.mem.asBytes(&value));
        }

        pub fn @"bool"(self: *Self, value: bool) !void {
            try self.op(.CONSTANT_BOOL);
            return self.frame.write(self.allocator, &.{if (value) 1 else 0});
        }

        pub fn string(self: *Self, value: []const u8) !u32 {
            const data_start = self.data.pos;

            // Storing the end, rather than the length, means one less addition
            // the VM has to do. +4 to skip the data length header itself.
            const data_end: u32 = @intCast(4 + data_start + value.len);

            try self.data.write(self.allocator, std.mem.asBytes(&data_end));
            try self.data.write(self.allocator, value);

            try self.stringRef(data_start);
            return data_start;
        }

        pub fn stringRef(self: *Self, data_start: u32) !void {
            try self.op(.CONSTANT_STRING);
            try self.frame.write(self.allocator, std.mem.asBytes(&data_start));
        }

        pub fn @"null"(self: *Self) !void {
            return self.op(OpCode.CONSTANT_NULL);
        }

        pub fn property(self: *Self, value: i32) !void {
            try self.op(.CONSTANT_PROPERTY);
            return self.frame.write(self.allocator, std.mem.asBytes(&value));
        }

        pub fn initializeArray(self: *Self, value_count: u32) !void {
            try self.op(.INITIALIZE_ARRAY);
            return self.frame.write(self.allocator, std.mem.asBytes(&value_count));
        }

        pub fn setLocal(self: *Self, local_index: LocalIndex) !void {
            try self.op(.SET_LOCAL);
            return self.frame.write(self.allocator, std.mem.asBytes(&local_index));
        }

        pub fn getLocal(self: *Self, local_index: LocalIndex) !void {
            try self.op(.GET_LOCAL);
            return self.frame.write(self.allocator, std.mem.asBytes(&local_index));
        }

        pub fn incr(self: *Self, local_index: LocalIndex, value: u8) !void {
            try self.op(.INCR);
            try self.frame.write(self.allocator, &.{value});
            return self.frame.write(self.allocator, std.mem.asBytes(&local_index));
        }

        pub fn debug(self: *Self, debug_code: OpCode.Debug, len: u16) !void {
            try self.op(.DEBUG);

            // +1 for the OpCode.Debug
            // +2 for the length prefix itself
            // adding them here means the VM doesn't have to add + 3 to the value
            const total_len: u16 = len + 3;
            try self.frame.write(self.allocator, std.mem.asBytes(&total_len));

            return self.frame.write(self.allocator, &.{@intFromEnum(debug_code)});
        }

        pub fn toBytes(self: *const Self, allocator: Allocator) ![]const u8 {
            std.debug.assert(self.frame_count == 0);

            const code = self.code;
            const data = self.data;
            const script = self.frames[0];

            const script_start = self.code.pos;
            const code_len = script_start + script.pos;

            const buf = try allocator.alloc(u8, 9 + code.pos + script.pos + data.pos);

            buf[0] = VERSION;
            @memcpy(buf[1..5], std.mem.asBytes(&code_len));
            @memcpy(buf[5..9], std.mem.asBytes(&script_start));

            const code_end = 9 + self.code.pos;
            @memcpy(buf[9..code_end], code.buf[0..code.pos]);

            const script_end = code_end + script.pos;
            @memcpy(buf[code_end..script_end], script.buf[0..script.pos]);
            @memcpy(buf[script_end..], data.buf[0..data.pos]);

            return buf;
        }
    };
}

fn Buffer(comptime initial_size: u32) type {
    return struct {
        pos: u32 = 0,
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
            self.pos = @intCast(end);
        }
    };
}

pub fn disassemble(comptime App: type, byte_code: []const u8, writer: anytype) !void {
    const LocalIndex = config.LocalType(App);
    const SL = @sizeOf(LocalIndex);

    var i: usize = 0;
    const code_length = @as(u32, @bitCast(byte_code[1..5].*));
    const script_start = @as(u32, @bitCast(byte_code[5..9].*));

    const code = byte_code[9 .. code_length + 9];
    const data = byte_code[9 + code_length ..];
    try std.fmt.format(writer, "// Version: {d}\n", .{byte_code[0]});

    while (i < code.len) {
        if (i == script_start) {
            if (i != 0) {
                try writer.writeAll("\n");
            }
            try writer.writeAll("<main>:\n");
        }
        const op_code_pos = i;
        const op_code = std.meta.intToEnum(OpCode, code[i]) catch {
            try std.fmt.format(writer, "{x:0>4} ??? ({d})", .{ i, code[i] });
            return;
        };
        if (op_code != .DEBUG) {
            try std.fmt.format(writer, "{x:0>4} {s}", .{ i, @tagName(op_code) });
        }

        i += 1;
        switch (op_code) {
            .DEBUG => {
                i += 2; // skip the length, it's only used by the VM to skip over the entire debug op
                const debug_code: OpCode.Debug = @enumFromInt(code[i]);
                i += 1;
                switch (debug_code) {
                    .FUNCTION_NAME => {
                        if (i != 4) {
                            // At this point, if i == 4, then this would be the
                            // first instruction, and we don't want to prepend a \n
                            try writer.writeAll("\n");
                        }
                        const function_name_len = code[i];
                        i += 1;
                        try std.fmt.format(writer, "{x:0>4} fn {s}:\n", .{ op_code_pos, code[i .. i + function_name_len] });
                        i += function_name_len;
                    },
                }
            },
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
                const header_start = @as(u32, @bitCast(code[i .. i + 4][0..4].*));
                i += 4;
                const header_end = header_start + 4;
                const string_end = @as(u32, @bitCast(data[header_start..header_end][0..4].*));
                try std.fmt.format(writer, " {s}\n", .{data[header_end..string_end]});
            },
            .CONSTANT_PROPERTY => {
                const value = @as(*align(1) const i32, @ptrCast(code[i..(i + 4)])).*;
                try std.fmt.format(writer, " {d}\n", .{value});
                i += 4;
            },
            .JUMP => {
                const relative = @as(i16, @bitCast(code[i .. i + 2][0..2].*));
                const target: u32 = @intCast(@as(i32, @intCast(i)) + relative);
                try std.fmt.format(writer, " {x:0>4}\n", .{target});
                i += 2;
            },
            .JUMP_IF_FALSE, .JUMP_IF_FALSE_POP => {
                const relative = @as(i16, @bitCast(code[i .. i + 2][0..2].*));
                const target: u32 = @intCast(@as(i32, @intCast(i)) + relative);
                try std.fmt.format(writer, " {x:0>4}\n", .{target});
                i += 2;
            },
            .SET_LOCAL => {
                const idx = @as(LocalIndex, @bitCast(code[i .. i + SL][0..SL].*));
                try std.fmt.format(writer, " @{d}\n", .{idx});
                i += SL;
            },
            .GET_LOCAL => {
                const idx = @as(LocalIndex, @bitCast(code[i .. i + SL][0..SL].*));
                try std.fmt.format(writer, " @{d}\n", .{idx});
                i += SL;
            },
            .INCR => {
                // i += 0 is pretty rare. So use value 0 for -1, which is more
                // common (i.e. i--)
                const value: i16 = if (code[i] == 0) -1 else code[i];
                i += 1;
                const idx = @as(LocalIndex, @bitCast(code[i .. i + SL][0..SL].*));
                i += SL;
                try std.fmt.format(writer, " @{d} {d}\n", .{ idx, value });
            },
            .INITIALIZE_ARRAY => {
                const value_count: u32 = @bitCast(code[i .. i + 4][0..4].*);
                try std.fmt.format(writer, " {d}\n", .{value_count});
                i += 4;
            },
            .CALL => {
                const header_start = @as(u32, @bitCast(code[i .. i + 4][0..4].*));
                i += 4;

                const arity = data[header_start];
                var d = data[header_start + 1 ..];
                const jump: u32 = @bitCast(d[0..4].*);
                try std.fmt.format(writer, " {d} {x:0>4}", .{ arity, jump });
                if (comptime config.shouldDebug(App, .minimal)) {
                    d = d[4..];
                    const name_length = d[0];
                    try std.fmt.format(writer, " ({s})", .{d[1 .. name_length + 1]});
                }
                try std.fmt.format(writer, "\n", .{});
            },
            else => try writer.writeAll("\n"),
        }
    }
}

pub const OpCode = enum(u8) {
    DEBUG = 0,
    ADD,
    CALL,
    CONSTANT_BOOL,
    CONSTANT_F64,
    CONSTANT_I64,
    CONSTANT_NULL,
    CONSTANT_PROPERTY,
    CONSTANT_STRING,
    DIVIDE,
    EQUAL,
    GET_LOCAL,
    GREATER,
    INCR,
    INCR_REF,
    INDEX_GET,
    INDEX_SET,
    INITIALIZE_ARRAY,
    JUMP,
    JUMP_IF_FALSE,
    JUMP_IF_FALSE_POP,
    LESSER,
    MODULUS,
    MULTIPLY,
    NEGATE,
    NOT,
    OUTPUT,
    POP,
    PRINT,
    PUSH,
    RETURN,
    SET_LOCAL,
    SUBTRACT,

    const Debug = enum {
        FUNCTION_NAME,
    };
};

const t = @import("t.zig");
test "bytecode: write" {
    defer t.reset();
    var b = try ByteCode(void).init(t.arena.allocator());

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

    var b = try ByteCode(void).init(t.arena.allocator());
    b.beginScript();
    try b.i64(-388491034);
    try b.f64(12.34567);
    try b.bool(true);
    try b.bool(false);
    try b.null();
    try b.op(OpCode.RETURN);
    try expectDisassemble(void, b,
        \\// Version: 0
        \\<main>:
        \\0000 CONSTANT_I64 -388491034
        \\0009 CONSTANT_F64 12.34567
        \\0012 CONSTANT_BOOL true
        \\0014 CONSTANT_BOOL false
        \\0016 CONSTANT_NULL
        \\0017 RETURN
        \\
    );
}

test "bytecode: functions debug none" {
    defer t.reset();
    const App = struct {
        pub const zt_debug = config.DebugMode.none;
    };

    var b = try ByteCode(App).init(t.arena.allocator());
    b.beginScript();

    const data_pos = try b.newFunction("sum");
    try b.beginFunction("sum");
    try b.f64(44);
    try b.op(.RETURN);

    _ = try b.endFunction(data_pos, 2);
    try b.call(data_pos);
    try b.op(.RETURN);

    try expectDisassemble(App, b,
        \\// Version: 0
        \\0000 CONSTANT_F64 44
        \\0009 RETURN
        \\
        \\<main>:
        \\000a CALL 2 0000
        \\000f RETURN
        \\
    );
}

test "bytecode: functions debug minimal" {
    defer t.reset();
    const App = struct {
        pub const zt_debug = config.DebugMode.minimal;
    };

    var b = try ByteCode(App).init(t.arena.allocator());
    b.beginScript();

    const data_pos = try b.newFunction("sum");
    try b.beginFunction("sum");
    try b.f64(44);
    try b.op(.RETURN);

    _ = try b.endFunction(data_pos, 2);
    try b.call(data_pos);
    try b.op(.RETURN);

    try expectDisassemble(App, b,
        \\// Version: 0
        \\0000 CONSTANT_F64 44
        \\0009 RETURN
        \\
        \\<main>:
        \\000a CALL 2 0000 (sum)
        \\000f RETURN
        \\
    );
}

test "bytecode: functions debug full" {
    defer t.reset();
    const App = struct {
        pub const zt_debug = config.DebugMode.full;
    };

    var b = try ByteCode(App).init(t.arena.allocator());
    b.beginScript();

    const data_pos = try b.newFunction("sum");
    try b.beginFunction("sum");
    try b.f64(44);
    try b.op(.RETURN);

    _ = try b.endFunction(data_pos, 2);
    try b.call(data_pos);
    try b.op(.RETURN);

    try expectDisassemble(App, b,
        \\// Version: 0
        \\0000 fn sum:
        \\0008 CONSTANT_F64 44
        \\0011 RETURN
        \\
        \\<main>:
        \\0012 CALL 2 0000 (sum)
        \\0017 RETURN
        \\
    );
}

fn expectDisassemble(comptime App: type, b: anytype, expected: []const u8) !void {
    var arr = std.ArrayList(u8).init(t.allocator);
    defer arr.deinit();

    const byte_code = try b.toBytes(t.allocator);
    defer t.allocator.free(byte_code);

    try disassemble(App, byte_code, arr.writer());
    try t.expectString(expected, arr.items);
}
