const std = @import("std");

const Value = @import("value.zig").Value;
const Config = @import("config.zig").Config;
const OpCode = @import("byte_code.zig").OpCode;

const Allocator = std.mem.Allocator;

pub fn VM(comptime config: Config) type {
    return struct {
        _allocator: Allocator,

        _stack: [10]Value = undefined,
        _stack_pointer: [*]Value = undefined,

        // whether we own err.?.desc or not
        _own_error: bool = false,

        // execution error
        err: ?Error = null,

        const LocalIndex = config.LocalType();
        const SL = @sizeOf(LocalIndex);

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            return .{
                ._allocator = allocator,
            };
        }

        pub fn deinit(self: Self) void {
            if (self._own_error) {
                self._allocator.free(self.err.?.desc);
            }
        }

        pub fn run(self: *Self, byte_code: []const u8) !Value {

            self._stack_pointer = &self._stack;

            var ip = byte_code.ptr;
            const code_length = @as(u32, @bitCast(ip[0..4].*));
            ip += 4;

            if (code_length == 0) {
                return .{.null = {}};
            }

            const start = ip;
            const data = ip[code_length..];

            while (true) {
                const op_code: OpCode = @enumFromInt(ip[0]);
                ip += 1;
                switch (op_code) {
                    .CONSTANT_I64 => {
                        const value = @as(i64, @bitCast(ip[0..8].*));
                        self.push(.{ .i64 = value });
                        ip += 8;
                    },
                    .CONSTANT_F64 => {
                        const value = @as(f64, @bitCast(ip[0..8].*));
                        self.push(.{ .f64 = value });
                        ip += 8;
                    },
                    .CONSTANT_BOOL => {
                        self.push(.{ .bool = ip[0] == 1 });
                        ip += 1;
                    },
                    .CONSTANT_STRING => {
                        const data_start = @as(u32, @bitCast(ip[0..4].*));
                        ip += 4;

                        const string_start = data_start + 4;
                        const string_end = @as(u32, @bitCast(data[data_start..string_start][0..4].*));
                        self.push(.{ .string = data[string_start..string_end] });
                    },
                    .CONSTANT_NULL => self.push(.{ .null = {} }),
                    .GET_LOCAL => {
                        const idx = if (comptime SL == 1) ip[0] else @as(u16, @bitCast(ip[0..2].*));
                        ip += SL;
                        self.push(self._stack[idx]);
                    },
                    .SET_LOCAL => {
                        const idx = if (comptime SL == 1) ip[0] else @as(u16, @bitCast(ip[0..2].*));
                        ip += SL;
                        self._stack[idx] = self.peek();
                    },
                    .ADD => try self.arithmetic(&add),
                    .SUBTRACT => try self.arithmetic(&subtract),
                    .MULTIPLY => try self.arithmetic(&multiply),
                    .DIVIDE => try self.arithmetic(&divide),
                    .NEGATE => {
                        const v = self.values();
                        switch (v[0]) {
                            .i64 => |n| v[0].i64 = -n,
                            .f64 => |n| v[0].f64 = -n,
                            else => return self.setErrorFmt(error.TypeError, "Cannot negate non-numeric value: -{s}", .{v[0]}),
                        }
                    },
                    .NOT => {
                        const v = self.values();
                        switch (v[0]) {
                            .bool => |b| v[0].bool = !b,
                            else => return self.setErrorFmt(error.TypeError, "Cannot not non-booleaning value: !{s}", .{v[0]}),
                        }
                    },
                    .EQUAL => try self.comparison(&equal),
                    .GREATER => try self.comparison(&greater),
                    .LESSER => try self.comparison(&lesser),
                    .JUMP => ip = start + @as(u16, @bitCast(ip[0..2].*)),
                    .JUMP_IF_FALSE => {
                        if (self.peek().isTrue()) {
                            // just skip the jump address
                            ip += 2;
                        } else {
                            // skip the jump address + the nested code
                            ip = start + @as(u16, @bitCast(ip[0..2].*));
                        }
                    },
                    .INCR => {
                        const incr: i64 = if (ip[0] == 0) -1 else ip[0];
                        ip += 1;

                        const idx = if (comptime SL == 1) ip[0] else @as(u16, @bitCast(ip[0..2].*));
                        ip += SL;

                        const v = try self.add(self._stack[idx], .{.i64 = incr});
                        self.push(v);
                        self._stack[idx] = v;
                    },
                    .PRINT => std.debug.print("{}\n", .{self.pop()}),
                    .POP => _ = self.pop(),
                    .RETURN => return self.pop(),
                }
            }
        }

        fn push(self: *Self, literal: Value) void {
            self._stack_pointer[0] = literal;
            self._stack_pointer += 1;
        }

        fn pop(self: *Self) Value {
            self._stack_pointer -= 1;
            return self._stack_pointer[0];
        }

        fn peek(self: *Self) Value {
            return (self._stack_pointer - 1)[0];
        }

        fn arithmetic(self: *Self, operation: *const fn (self: *Self, left: Value, right: Value) anyerror!Value) !void {
            // This is a pop(), pop() which brings sp -= 2;
            // Followed by a push() with brings sp += 1;
            // The final result being sp -= 1;
            // We can jump immediately to sp - 1, and grab sp[-1] (left) and sp[0] (right)
            // which puts our result in sp[0].
            self._stack_pointer -= 1;
            const ptr = self.values();
            const result = try operation(self, ptr[0], ptr[1]);
            ptr[0] = result;
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

        fn comparison(self: *Self, operation: *const fn (self: *Self, left: Value, right: Value) anyerror!bool) !void {
            // This is a pop(), pop() which brings sp -= 2;
            // Followed by a push() with brings sp += 1;
            // The final result being sp -= 1;
            // We can jump immediately to sp - 1, and grab sp[-1] (left) and sp[0] (right)
            // which puts our result in sp[0].
            // (Zig doesn't yet support negative indexes, so we use this ptrFromInt stuff)
            self._stack_pointer -= 1;
            const ptr = self.values();
            const result = try operation(self, ptr[0], ptr[1]);
            ptr[0] = .{ .bool = result };
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

        inline fn values(self: *const Self) [*]Value {
            return @ptrFromInt(@intFromPtr(self._stack_pointer) - @sizeOf(Value));
        }

        fn setErrorFmt(self: *Self, err: anyerror, comptime fmt: []const u8, args: anytype) anyerror {
            self.err = .{
                .err = err,
                .desc = try std.fmt.allocPrint(self._allocator, fmt, args),
            };
            self._own_error = true;
            return err;
        }

        fn setError(self: *Self, err: anyerror, desc: []const u8) anyerror {
            self.err = .{
                .err = err,
                .desc = desc,
            };
            self._own_error = false;
            return err;
        }
    };
}

pub const Error = struct {
    err: anyerror,
    desc: []const u8,
};
