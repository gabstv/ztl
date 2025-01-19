const std = @import("std");

pub const VM = @import("vm.zig").VM;
pub const Value = @import("value.zig").Value;
pub const Global = @import("template.zig").Global;
pub const Template = @import("template.zig").Template;
pub const DebugMode = @import("config.zig").DebugMode;
pub const PartialResult = @import("compiler.zig").PartialResult;

pub const RenderErrorReport = @import("error_report.zig").Render;
pub const CompileErrorReport = @import("error_report.zig").Compile;

pub fn Functions(comptime A: type) type {
    const App = switch (@typeInfo(A)) {
        .@"struct" => A,
        .pointer => |ptr| ptr.child,
        .void => void,
        else => @compileError("Template App must be a struct, got: " ++ @tagName(@typeInfo(A))),
    };

    if (App == void or @hasDecl(App, "ZtlFunctions") == false) {
        return @Type(.{
            .@"enum" = .{
                .decls = &.{},
                .tag_type = u8,
                .fields = &.{.{ .name = "", .value = 0 }}, // HACK, std.meta.stringToEnum doesn't work on an empty enum, lol what?
                .is_exhaustive = true,
            },
        });
    }
    const declarations = std.meta.declarations(App.ZtlFunctions);
    var fields: [declarations.len]std.builtin.Type.EnumField = undefined;

    for (declarations, 0..) |d, i| {
        fields[i] = .{ .name = d.name, .value = i };
    }

    // the type of the @"enum" tag is std.builtin.Type.Enum
    // we use the type inference syntax, i.e. .{...}
    return @Type(.{ .@"enum" = .{
        .decls = &.{},
        .tag_type = u16,
        .fields = &fields,
        .is_exhaustive = true,
    } });
}

const t = @import("t.zig");

test {
    std.testing.refAllDecls(@This());
}
