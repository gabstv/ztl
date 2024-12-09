const std = @import("std");

const Value = @import("value.zig").Value;
const Config = @import("config.zig").Config;
const OpCode = @import("byte_code.zig").OpCode;

const Allocator = std.mem.Allocator;

const Stack = std.ArrayListUnmanaged(Value);

pub fn VM(comptime config: Config) type {
    return struct {
        _arena: std.heap.ArenaAllocator,

        _stack: Stack = .{},

        _frames: [config.max_call_frames]Frame(config) = undefined,

        // execution error
        err: ?Error = null,

        const LocalIndex = config.LocalType();
        const SL = @sizeOf(LocalIndex);

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            return .{
                ._arena = std.heap.ArenaAllocator.init(allocator),
            };
        }

        pub fn deinit(self: Self) void {
            self._arena.deinit();
        }

        pub fn run(self: *Self, byte_code: []const u8) !Value {
            var ip = byte_code.ptr;
            const code_end = 8 + @as(u32, @bitCast(ip[0..4].*));

            if (code_end == 8) {
                return .{.null = {}};
            }

            const code = byte_code[8..code_end];
            const data = byte_code[code_end..];

            // goto to the main script
            ip += 8 + @as(u32, @bitCast(ip[4..8].*));

            const allocator = self._arena.allocator();

            var frames = &self._frames;
            frames[0] = .{
                .ip = ip,
                .frame_pointer = 0,
            };
            var frame_count: usize = 0;

            var stack = &self._stack;
            var frame_pointer: usize = 0;

            while (true) {
                const op_code: OpCode = @enumFromInt(ip[0]);
                ip += 1;
                switch (op_code) {
                    .CONSTANT_I64 => {
                        const value = @as(i64, @bitCast(ip[0..8].*));
                        try stack.append(allocator, .{ .i64 = value });
                        ip += 8;
                    },
                    .CONSTANT_F64 => {
                        const value = @as(f64, @bitCast(ip[0..8].*));
                        try stack.append(allocator, .{ .f64 = value });
                        ip += 8;
                    },
                    .CONSTANT_BOOL => {
                        try stack.append(allocator, .{ .bool = ip[0] == 1 });
                        ip += 1;
                    },
                    .CONSTANT_STRING => {
                        const data_start = @as(u32, @bitCast(ip[0..4].*));
                        ip += 4;

                        const string_start = data_start + 4;
                        const string_end = @as(u32, @bitCast(data[data_start..string_start][0..4].*));
                        try stack.append(allocator, .{ .string = data[string_start..string_end] });
                    },
                    .CONSTANT_NULL => try stack.append(allocator, .{ .null = {} }),
                    .GET_LOCAL => {
                        const idx = if (comptime SL == 1) ip[0] else @as(u16, @bitCast(ip[0..2].*));
                        try stack.append(allocator, stack.items[frame_pointer + idx]);
                        ip += SL;
                    },
                    .SET_LOCAL => {
                        const idx = if (comptime SL == 1) ip[0] else @as(u16, @bitCast(ip[0..2].*));
                        stack.items[frame_pointer + idx] = stack.getLast();
                        ip += SL;
                    },
                    .ADD => try self.arithmetic(stack, &add),
                    .SUBTRACT => try self.arithmetic(stack, &subtract),
                    .MULTIPLY => try self.arithmetic(stack, &multiply),
                    .DIVIDE => try self.arithmetic(stack, &divide),
                    .MODULUS => try self.arithmetic(stack, &modulus),
                    .NEGATE => {
                        var v = &stack.items[stack.items.len - 1];
                        switch (v.*) {
                            .i64 => |n| v.i64 = -n,
                            .f64 => |n| v.f64 = -n,
                            else => return self.setErrorFmt(error.TypeError, "Cannot negate non-numeric value: -{s}", .{v.*}),
                        }
                    },
                    .NOT => {
                        var v = &stack.items[stack.items.len - 1];
                        switch (v.*) {
                            .bool => |b| v.bool = !b,
                            else => return self.setErrorFmt(error.TypeError, "Cannot inverse non-boolean value: !{s}", .{v.*}),
                        }
                    },
                    .EQUAL => try self.comparison(stack, &equal),
                    .GREATER => try self.comparison(stack, &greater),
                    .LESSER => try self.comparison(stack, &lesser),
                    .JUMP => {
                        // really??
                        const relative: i16 = @bitCast(ip[0..2].*);
                        if (relative >= 0) {
                            ip += @intCast(relative);
                        } else {
                            ip = ip - @abs(relative);
                        }
                    },
                    .JUMP_IF_FALSE => {
                        if (stack.items[stack.items.len - 1].isTrue()) {
                            // just skip the jump address
                            ip += 2;
                        } else {
                            // really??
                           const relative: i16 = @bitCast(ip[0..2].*);
                            if (relative >= 0) {
                                ip += @intCast(relative);
                            } else {
                                ip = ip - @abs(relative);
                            }
                        }
                    },
                    .INCR => {
                        const incr: i64 = if (ip[0] == 0) -1 else ip[0];
                        ip += 1;

                        const idx = if (comptime SL == 1) ip[0] else @as(u16, @bitCast(ip[0..2].*));
                        ip += SL;

                        const adjusted_idx = frame_pointer + idx;
                        const v = try self.add(stack.items[adjusted_idx], .{.i64 = incr});
                        try stack.append(allocator, v);
                        stack.items[adjusted_idx] = v;
                    },
                    .CALL => {
                        const data_start = @as(u32, @bitCast(ip[0..4].*));
                        ip += 4;

                        const meta = data[data_start..data_start + 5];

                        const arity = meta[0];
                        const code_pos = @as(u32, @bitCast(meta[1..5][0..4].*));

                        // Capture the state of our current frame. This is what
                        // we'll return to.
                        frames[frame_count].ip = ip;

                        // jump to the function code
                        ip = code[code_pos..].ptr;

                        // adjust our frame pointer
                        frame_pointer = stack.items.len - arity;

                        // Push a new frame. This is the functiont that we're
                        // going to be executing.
                        frame_count += 1;
                        frames[frame_count] = .{
                            .ip = ip,
                            .frame_pointer = frame_pointer,
                        };
                    },
                    .PRINT => std.debug.print("{}\n", .{stack.pop()}),
                    .POP => _ = stack.pop(),
                    .RETURN => {
                        const value = stack.pop();
                        if (frame_count == 0) {
                            return value;
                        }
                        stack.items.len = frames[frame_count].frame_pointer;

                        frame_count -= 1;
                        const frame = frames[frame_count];
                        ip = frame.ip;
                        frame_pointer = frame.frame_pointer;
                        try stack.append(allocator, value);
                    },
                    .DEBUG => {
                        // debug information always contains a 2 byte length
                        // prefix (including the lenght prefix itself) to make
                        // it quick for the VM to skip.
                        ip += @as(u16, @bitCast(ip[0..2].*));
                    }
                }
            }
        }


        fn arithmetic(self: *Self, stack: *Stack, operation: *const fn (self: *Self, left: Value, right: Value) anyerror!Value) !void {
            var values = stack.items;
            const right_index = values.len - 1;
            std.debug.assert(right_index >= 1);

            const left_index = right_index - 1;
            values[left_index] = try operation(self, values[left_index], values[right_index]);
            stack.items.len = right_index;
        }

        fn add(self: *Self, left: Value, right: Value) anyerror!Value {
            switch (left) {
                .i64 => |l| switch (right) {
                    .i64 => |r| return .{ .i64 = l + r },
                    .f64 => |r| return .{ .f64 = @as(f64, @floatFromInt(l)) + r },
                    else => {},
                },
                .f64 => |l| switch (right) {
                    .f64 => |r| return .{ .f64 = l + r },
                    .i64 => |r| return .{ .f64 = l + @as(f64, @floatFromInt(r)) },
                    else => {},
                },
                else => {},
            }
            return self.setErrorFmt(error.TypeError, "Cannot add non-numeric value: {s} + {s}", .{ left, right });
        }

        fn subtract(self: *Self, left: Value, right: Value) anyerror!Value {
            switch (left) {
                .i64 => |l| switch (right) {
                    .i64 => |r| return .{ .i64 = l - r },
                    .f64 => |r| return .{ .f64 = @as(f64, @floatFromInt(l)) - r },
                    else => {},
                },
                .f64 => |l| switch (right) {
                    .f64 => |r| return .{ .f64 = l - r },
                    .i64 => |r| return .{ .f64 = l - @as(f64, @floatFromInt(r)) },
                    else => {},
                },
                else => {},
            }
            return self.setErrorFmt(error.TypeError, "Cannot subtract non-numeric value: {s} - {s}", .{ left, right });
        }

        fn multiply(self: *Self, left: Value, right: Value) anyerror!Value {
            switch (left) {
                .i64 => |l| switch (right) {
                    .i64 => |r| return .{ .i64 = l * r },
                    .f64 => |r| return .{ .f64 = @as(f64, @floatFromInt(l)) * r },
                    else => {},
                },
                .f64 => |l| switch (right) {
                    .f64 => |r| return .{ .f64 = l * r },
                    .i64 => |r| return .{ .f64 = l * @as(f64, @floatFromInt(r)) },
                    else => {},
                },
                else => {},
            }
            return self.setErrorFmt(error.TypeError, "Cannot multiply non-numeric value: {s} - {s}", .{ left, right });
        }

        fn divide(self: *Self, left: Value, right: Value) anyerror!Value {
            switch (left) {
                .i64 => |l| switch (right) {
                    .i64 => |r| return .{ .i64 = @divTrunc(l, r) },
                    .f64 => |r| return .{ .f64 = @as(f64, @floatFromInt(l)) / r },
                    else => {},
                },
                .f64 => |l| switch (right) {
                    .f64 => |r| return .{ .f64 = l / r },
                    .i64 => |r| return .{ .f64 = l / @as(f64, @floatFromInt(r)) },
                    else => {},
                },
                else => {},
            }
            return self.setErrorFmt(error.TypeError, "Cannot divide non-numeric value: {s} - {s}", .{ left, right });
        }

        fn modulus(self: *Self, left: Value, right: Value) anyerror!Value {
            switch (left) {
                .i64 => |l| switch (right) {
                    .i64 => |r| return .{ .i64 = @mod(l, r) },
                    else => {},
                },
                else => {},
            }
            return self.setErrorFmt(error.TypeError, "Cannot take remainder of non-integer value: {s} - {s}", .{ left, right });
        }

        fn comparison(self: *Self, stack: *Stack, operation: *const fn (self: *Self, left: Value, right: Value) anyerror!bool) !void {
            var values = stack.items;
            const right_index = values.len - 1;
            std.debug.assert(right_index >= 1);

            const left_index = right_index - 1;
            const result = try operation(self, values[left_index], values[right_index]);
            values[left_index] = .{.bool = result};
            stack.items.len = right_index;
        }

        fn equal(self: *Self, left: Value, right: Value) anyerror!bool {
            switch (left) {
                .bool => |l| switch (right) {
                    .bool => |r| return l == r,
                    .null => return false,
                    else => {},
                },
                .f64 => |l| switch (right) {
                    .f64 => |r| return l == r,
                    .i64 => |r| return l == @as(f64, @floatFromInt(r)),
                    .null => return false,
                    else => {},
                },
                .i64 => |l| switch (right) {
                    .i64 => |r| return l == r,
                    .f64 => |r| return @as(f64, @floatFromInt(l)) == r,
                    .null => return false,
                    else => {},
                },
                .null => return right == .null,
                .string => |l| switch (right) {
                    .string => |r| return std.mem.eql(u8, l, r),
                    .null => return false,
                    else => {},
                },
            }
            return self.setErrorFmt(error.TypeError, "Incompatible type comparison: {s} == {s} ({s}, {s})", .{ left, right, @tagName(left), @tagName(right) });
        }

        fn greater(self: *Self, left: Value, right: Value) anyerror!bool {
            switch (left) {
                .f64 => |l| switch (right) {
                    .f64 => |r| return l > r,
                    .i64 => |r| return l > @as(f64, @floatFromInt(r)),
                    else => {},
                },
                .i64 => |l| switch (right) {
                    .i64 => |r| return l > r,
                    .f64 => |r| return @as(f64, @floatFromInt(l)) > r,
                    else => {},
                },
                .string => |l| switch (right) {
                    .string => |r| return std.mem.order(u8, l, r) == .gt,
                    else => {},
                },
                else => {},
            }
            return self.setErrorFmt(error.TypeError, "Incompatible type comparison: {s} > {s} ({s}, {s})", .{ left, right, @tagName(left), @tagName(right) });
        }

        fn lesser(self: *Self, left: Value, right: Value) anyerror!bool {
            switch (left) {
                .f64 => |l| switch (right) {
                    .f64 => |r| return l < r,
                    .i64 => |r| return l < @as(f64, @floatFromInt(r)),
                    else => {},
                },
                .i64 => |l| switch (right) {
                    .i64 => |r| return l < r,
                    .f64 => |r| return @as(f64, @floatFromInt(l)) < r,
                    else => {},
                },
                .string => |l| switch (right) {
                    .string => |r| return std.mem.order(u8, l, r) == .lt,
                    else => {},
                },
                else => {},
            }
            return self.setErrorFmt(error.TypeError, "Incompatible type comparison: {s} < {s} ({s}, {s})", .{ left, right, @tagName(left), @tagName(right) });
        }

        // inline fn values(self: *const Self) [*]Value {
        //     return @ptrFromInt(@intFromPtr(self._frame_pointer) - @sizeOf(Value));
        // }

        fn setErrorFmt(self: *Self, err: anyerror, comptime fmt: []const u8, args: anytype) anyerror {
            self.err = .{
                .err = err,
                .desc = try std.fmt.allocPrint(self._arena.allocator(), fmt, args),
            };
            return err;
        }

        fn setError(self: *Self, err: anyerror, desc: []const u8) anyerror {
            self.err = .{
                .err = err,
                .desc = desc,
            };
            return err;
        }
    };
}

fn Frame(comptime config: Config) type {
    _ = config;

    return struct {
        ip: [*]const u8,
        frame_pointer: usize = undefined,
    };
}

pub const Error = struct {
    err: anyerror,
    desc: []const u8,
};
