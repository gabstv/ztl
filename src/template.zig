const std = @import("std");
const ztl = @import("ztl.zig");

const Value = ztl.Value;

const VM = @import("vm.zig").VM;
const Compiler = @import("compiler.zig").Compiler;

const RenderErrorReport = ztl.RenderErrorReport;
const CompileErrorReport = ztl.CompileErrorReport;

const Allocator = std.mem.Allocator;
const MemoryPool = std.heap.MemoryPool;
const ArenaAllocator = std.heap.ArenaAllocator;

pub fn Template(comptime App: type) type {
    return struct {
        app: App,
        allocator: Allocator,
        arena: ArenaAllocator,
        globals: [][]const u8,
        byte_code: []const u8,

        const Self = @This();

        pub fn init(allocator: Allocator, app: App) Self {
            return .{
                .app = app,
                .globals = &.{},
                .byte_code = "",
                .allocator = allocator,
                .arena = ArenaAllocator.init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        pub fn compile(self: *Self, src: []const u8, opts: CompilerOpts) !void {
            var build_arena = ArenaAllocator.init(self.allocator);
            defer build_arena.deinit();

            const template_arena = self.arena.allocator();
            const build_allocator = build_arena.allocator();

            var compiler = try Compiler(App).init(build_allocator);
            compiler.compile(src, .{.error_report = opts.error_report}) catch |err| {
                if (opts.error_report) |er| {
                    er.message = try template_arena.dupe(u8, er.message);
                }
                return err;
            };

            self.byte_code = try compiler.writer.toBytes(template_arena);

            {
                const globals = compiler.globals;
                self.globals = try template_arena.alloc([]const u8, globals.count());
                var it = globals.iterator();
                while (it.next()) |kv| {
                    // slice the leading '@'
                    self.globals[kv.value_ptr.*] = try template_arena.dupe(u8, kv.key_ptr.*[1..]);
                }
            }
        }

        pub fn render(self: *Self, writer: anytype, globals: anytype, opts: RenderOpts) !void {
            const allocator = opts.allocator orelse self.allocator;
            var arena = ArenaAllocator.init(allocator);
            defer arena.deinit();

            var vm = VM(App).init(arena.allocator(), self.app);
            try vm.prepareForGlobals(self.globals.len);

            const T = @TypeOf(globals);
            if (T == []Global) {
                for (globals) |g| {
                    if (self.getIndex(g.@"0")) |index| {
                        vm.injectGlobal(g.@"1", index);
                    }
                }
            } else switch (@typeInfo(T)) {
                .@"struct" => |s| inline for (s.fields) |f| {
                    const value = vm.createValue(@field(globals, f.name)) catch {
                        if (opts.error_report) |er| {
                            er.message = "Unsupported argument type: " ++ @typeName(@TypeOf(@field(globals, f.name)));
                        }
                        return error.InvalidArgument;
                    };
                    if (self.getIndex(f.name)) |index| {
                        vm.injectGlobal(value, index);
                    }
                },
                else => @compileError("globals must be a struct or an []ztl.Global, got: " ++ @typeName(T)),
            }

            _ = vm.run(self.byte_code, writer) catch |runtime_err| {
                if (opts.error_report) |er| {
                    if (vm.err) |vm_err| {
                        er.* = .{
                            .allocator = allocator,
                            .message = try allocator.dupe(u8, vm_err),
                        };
                    }
                }
                return runtime_err;
            };
        }

        pub fn disassemble(self: *const Self, writer: anytype) !void {
            return @import("byte_code.zig").disassemble(App, self.arena.child_allocator, self.byte_code, writer);
        }

        fn getIndex(self: *const Self, needle: []const u8) ?usize {
            for (self.globals, 0..) |g, i| {
                if (std.mem.eql(u8, g, needle)) {
                    return i;
                }
            }
            return null;
        }
    };
}

pub const CompilerOpts = struct {
    error_report: ?*CompileErrorReport = null,
};

pub const RenderOpts = struct {
    allocator: ?Allocator = null,
    error_report: ?*RenderErrorReport = null,
};

pub const Global = struct {
    []const u8,
    Value,
};

const t = @import("t.zig");
test "Template: simple" {
    try testTemplate("Simple", "Simple", .{});
    try testTemplate("Simple", "<%= `Simple` %>", .{});
    try testTemplate("Simple", "<%= `Simple`; %>", .{});
    try testTemplate("Simple", "<%=`Simple`%>", .{});
    try testTemplate("Simple", "<%=`Simple`;%>", .{});
    try testTemplate("93", "<%= (1+2)  * 31;%>", .{});
    try testTemplate("  Simple  ", "  <%= `Simple` %>  ", .{});
}

test "Template: edge cases" {
    try testTemplate("", "<%= %>", .{});

    try testTemplate("hello %", "hello %", .{});
    try testTemplate("hello % ", "hello % ", .{});
    try testTemplate("hello %%", "hello %%", .{});
    try testTemplate("hello <%", "hello <%%", .{});
    try testTemplate("hello <%=", "hello <%%=", .{});
    try testTemplate("hello <% world", "hello <%% world", .{});
    try testTemplate("%", "%", .{});
    try testTemplate("%>a", "%>a", .{});
    try testTemplate("%-Hi", "%-Hi", .{});
    try testTemplate("<%-", "<%%-", .{});

    try testTemplate("`Hello World`", "`Hello World`", .{});
    try testTemplate("`Hello\"World`", "`Hello\"World`", .{});
    try testTemplate("`Hello\"Wor\nl`d`", "`Hello\"Wor\nl`d`", .{});
}

test "Template: output literals" {
    try testTemplate("123", "<%= 123 %>", .{});
    try testTemplate("-1.23492", "<%= -1.23492 %>", .{});
    try testTemplate("null", "<%= null %>", .{});
    try testTemplate("", "<%= null orelse `` %>", .{});
    try testTemplate("true", "<%= true %>", .{});
    try testTemplate("false", "<%= false %>", .{});
    try testTemplate("[1, 2, true]", "<%= [1 ,2,  true] %>", .{});
}

test "Template: output variable" {
    try testTemplate("Leto", "<%= @name %>", .{ .name = "Leto" });
    try testTemplate("7", "<%= @x + 1 %>", .{ .x = 6 });
}

test "Template: space stripping" {
    try testTemplate("  Leto   ", "  <%= @name %>   ", .{ .name = "Leto" });
    try testTemplate("Leto   ", "  <%-= @name %>   ", .{ .name = "Leto" });
    try testTemplate("  Leto", "  <%= @name -%>  ", .{ .name = "Leto" });
    try testTemplate("Leto", "  <%-= @name -%>  ", .{ .name = "Leto" });
}

test "Template: escaping" {
    try testTemplate("<h1>hello</h1>", "<%= @name %>", .{ .name = "<h1>hello</h1>" });
    try testTemplate("&lt;h1&gt;hello&lt;/h1&gt;", "<%= escape @name %>", .{ .name = "<h1>hello</h1>" });

    try testTemplateWithApp(struct {
        pub const ZtlConfig = struct {
            pub const escape_by_default: bool = true;
        };
    }, "&lt;h1&gt;hello&lt;/h1&gt;", "<%= @name %>", .{ .name = "<h1>hello</h1>" });

    try testTemplateWithApp(struct {
        pub const ZtlConfig = struct {
            pub const escape_by_default: bool = true;
        };
    }, "<h1>hello</h1>", "<%= safe @name %>", .{ .name = "<h1>hello</h1>" });
}

test "Template: local and global" {
    try testTemplate("12",
        \\
        \\ <%- var x = 2; -%>
        \\ <%-= x + @count -%>
    , .{ .count = 10 });
}

test "Template: for loop" {
    try testTemplate(
        \\<h2>Products</h2>
        \\  product: 10
        \\  product: 22
        \\  product: 33
    ,
        \\<h2>Products</h2>
        \\<%- for (var i = 0; i < @products.len; i++) { %>
        \\  product: <%= @products[i] -%>
        \\<%- } %>
    , .{ .products = [_]i64{ 10, 22, 33 } });
}

test "Template: error" {
    try testTemplateError("Missing expected end tag, '%>'", "<%=");

    try testTemplateFullError(
        \\UnexpectedCharacter - ('\') - line 1:
        \\<%= \ %>
        \\----^
    , "<%= \\ %>");

    try testTemplateFullError(
        \\Invalid - Expected Closing output tag '%>', got integer '999' (INTEGER) - line 1:
        \\<%= 123 ; 999 %>
        \\----------^
    , "<%= 123 ; 999 %>");

    try testTemplateFullError(
        \\Invalid - Expected Closing output tag '%>', got integer '999' (INTEGER) - line 2:
        \\hello
        \\<%= 123 ; 999 %>
        \\----------^
    , "hello\n<%= 123 ; 999 %>");

    try testTemplateFullError(
        \\UnexpectedCharacter - ('\') - line 3:
        \\ <h2>Products</h2>
        \\ <% foreach (@products) |p| { %>
        \\    name: <%= p["name"] \ %>
        \\------------------------^
        \\ <% } %>
    ,
        \\ <h2>Products</h2>
        \\ <% foreach (@products) |p| { %>
        \\    name: <%= p["name"] \ %>
        \\ <% } %>
    );

    // above is a scanner error, this is a compiler erro
    try testTemplateFullError(
        \\UnexpectedToken - Expected left bracket (']'), got } (RIGHT_BRACE) - line 3:
        \\ <h2>Products</h2>
        \\ <% foreach (@products) |p| { %>
        \\    name: <%= p["name"} %>
        \\----------------------^
        \\ <% } %>
    ,
        \\ <h2>Products</h2>
        \\ <% foreach (@products) |p| { %>
        \\    name: <%= p["name"} %>
        \\ <% } %>
    );
}

test "Template: runtime error" {
    try testTemplateRenderError("Unknown method 'pop' for an integer",
        \\ <%
        \\      var value = 1;
        \\      value.pop();
        \\ %>
    );
}

test "Template: map global" {
    const globals = try t.allocator.alloc(Global, 2);
    defer t.allocator.free(globals);
    globals[0] = .{ "key_2", .{ .i64 = 123 } };
    globals[1] = .{ "a", .{ .string = "hello" } };
    try testTemplate("hello 123", "<%= @a %> <%= @key_2 %>", globals);

    globals[1] = .{ "key_2", .{ .i64 = 123 } };
    globals[0] = .{ "a", .{ .string = "hello" } };
    try testTemplate("hello 123", "<%= @a %> <%= @key_2 %>", globals);
}

fn testTemplate(expected: []const u8, template: []const u8, args: anytype) !void {
    return testTemplateWithApp(void, expected, template, args);
}

fn testTemplateWithApp(comptime App: type, expected: []const u8, template: []const u8, args: anytype) !void {
    var tmpl = Template(App).init(t.allocator, App{});
    defer tmpl.deinit();

    var error_report = CompileErrorReport{};
    tmpl.compile(template, .{.error_report = &error_report}) catch |err| {
        std.debug.print("Compile template error:\n{}\n", .{error_report});
        return err;
    };
    // try tmpl.disassemble(std.io.getStdErr().writer());

    var buf = std.ArrayList(u8).init(t.allocator);
    defer buf.deinit();
    tmpl.render(buf.writer(), args, .{}) catch |err| {
        std.debug.print("==disassemble==\n", .{});
        try tmpl.disassemble(std.io.getStdErr().writer());
        return err;
    };
    try t.expectString(expected, buf.items);
}

fn testTemplateError(expected: []const u8, template: []const u8) !void {
    var tmpl = Template(void).init(t.allocator, {});
    defer tmpl.deinit();

    var error_report = CompileErrorReport{};
    tmpl.compile(template, .{.error_report = &error_report}) catch {
        try t.expectString(expected, error_report.message);
        return;
    };
    return error.NoError;
}

fn testTemplateFullError(expected: []const u8, template: []const u8) !void {
    var tmpl = Template(void).init(t.allocator, {});
    defer tmpl.deinit();

    var error_report = CompileErrorReport{};
    tmpl.compile(template, .{.error_report = &error_report}) catch {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(t.allocator);

        try std.fmt.format(buf.writer(t.allocator), "{}", .{error_report});
        try t.expectString(expected, buf.items);
        return;
    };
    return error.NoError;
}

fn testTemplateRenderError(expected: []const u8, template: []const u8) !void {
    var tmpl = Template(void).init(t.allocator, {});
    defer tmpl.deinit();
    try tmpl.compile(template, .{});

    var buf = std.ArrayList(u8).init(t.allocator);
    defer buf.deinit();

var report = RenderErrorReport{};
    tmpl.render(buf.writer(), .{}, .{ .error_report = &report }) catch {
        defer report.deinit();
        try t.expectString(expected, report.message);
        return;
    };
    return error.NoError;
}
