const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const ztl_mod = b.addModule("ztl", .{
        .root_source_file = b.path("src/ztl.zig"),
        .target = target,
        .optimize = optimize,
    });

    // setup tests
    const tests = b.addTest(.{
        .root_module = ztl_mod,
        .test_runner = .{
            .path = b.path("test_runner.zig"),
            .mode = .simple,
        },
    });

    const run_test = b.addRunArtifact(tests);
    run_test.has_side_effects = true;

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_test.step);
}
