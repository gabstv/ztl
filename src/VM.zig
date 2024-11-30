const std = @import("std");
const lib = @import("lib.zig");

const OpCode = lib.OpCode;
const Value = lib.Value;
const ByteCode = lib.ByteCode;
const Allocator = std.mem.Allocator;

const VM = @This();
_allocator: Allocator,

_stack: [10]Value = undefined,
_stack_pointer: [*]Value = undefined,

// whether we own err.?.desc or not
_own_error: bool = false,

// execution error
err: ?Error = null,

pub fn init(allocator: Allocator) VM {
    return .{
        ._allocator = allocator,
    };
}

pub fn deinit(self: VM) void {
    if (self._own_error) {
        self._allocator.free(self.err.?.desc);
    }
}

pub fn run(self: *VM, byte_code: []const u8) !Value {
    var ip = byte_code.ptr;
    self._stack_pointer = &self._stack;

    const code_length = @as(u32, @bitCast(ip[0..4].*));
    ip += 4;

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
            .ADD => try self.arithmetic(&add),
            .SUBTRACT => try self.arithmetic(&subtract),
            .MULTIPLY => try self.arithmetic(&multiply),
            .DIVIDE => try self.arithmetic(&divide),
            .NEGATE => {
                const ptr = self.values();
                switch (ptr[0]) {
                    .i64 => |n| ptr[0].i64 = -n,
                    .f64 => |n| ptr[0].f64 = -n,
                    else => return self.setErrorFmt(error.TypeError, "Cannot negate non-numeric value: -{s}", .{ptr[0]}),
                }
            },
            .NOT => {
                const ptr = self.values();
                switch (ptr[0]) {
                    .bool => |b| ptr[0].bool = !b,
                    else => return self.setErrorFmt(error.TypeError, "Cannot not non-booleaning value: !{s}", .{ptr[0]}),
                }
            },
            .EQUAL => try self.comparison(&equal),
            .GREATER => try self.comparison(&greater),
            .LESSER => try self.comparison(&lesser),
            .RETURN => return self.pop(),
        }
    }
}

fn push(self: *VM, literal: Value) void {
    self._stack_pointer[0] = literal;
    self._stack_pointer += 1;
}

fn pop(self: *VM) Value {
    self._stack_pointer -= 1;
    return self._stack_pointer[0];
}

fn arithmetic(self: *VM, operation: *const fn (self: *VM, left: Value, right: Value) anyerror!Value) !void {
    // This is a pop(), pop() which brings sp -= 2;
    // Followed by a push() with brings sp += 1;
    // The final result being sp -= 1;
    // We can jump immediately to sp - 1, and grab sp[-1] (left) and sp[0] (right)
    // which puts our result in sp[0].
    // (Zig doesn't yet support negative indexes, so we use this ptrFromInt stuff)
    self._stack_pointer -= 1;
    const ptr = self.values();
    const result = try operation(self, ptr[0], ptr[1]);
    ptr[0] = result;
}

fn add(self: *VM, left: Value, right: Value) anyerror!Value {
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

fn subtract(self: *VM, left: Value, right: Value) anyerror!Value {
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

fn multiply(self: *VM, left: Value, right: Value) anyerror!Value {
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

fn divide(self: *VM, left: Value, right: Value) anyerror!Value {
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

fn comparison(self: *VM, operation: *const fn (self: *VM, left: Value, right: Value) anyerror!bool) !void {
    // This is a pop(), pop() which brings sp -= 2;
    // Followed by a push() with brings sp += 1;
    // The final result being sp -= 1;
    // We can jump immediately to sp - 1, and grab sp[-1] (left) and sp[0] (right)
    // which puts our result in sp[0].
    // (Zig doesn't yet support negative indexes, so we use this ptrFromInt stuff)
    self._stack_pointer -= 1;
    const ptr = self.values();
    const result = try operation(self, ptr[0], ptr[1]);
    ptr[0] = .{.bool = result};
}

fn equal(self: *VM, left: Value, right: Value) anyerror!bool {
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

fn greater(self: *VM, left: Value, right: Value) anyerror!bool {
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

fn lesser(self: *VM, left: Value, right: Value) anyerror!bool {
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

inline fn values(self: *const VM) [*]Value {
    return @ptrFromInt(@intFromPtr(self._stack_pointer) - @sizeOf(Value));
}

fn setErrorFmt(self: *VM, err: anyerror, comptime fmt: []const u8, args: anytype) anyerror {
    self.err = .{
        .err = err,
        .desc = try std.fmt.allocPrint(self._allocator, fmt, args),
    };
    self._own_error = true;
    return err;
}

fn setError(self: *VM, err: anyerror, desc: []const u8) anyerror {
    self.err = .{
        .err = err,
        .desc = desc,
    };
    self._own_error = false;
    return err;
}

pub const Error = struct {
    err: anyerror,
    desc: []const u8,
};
