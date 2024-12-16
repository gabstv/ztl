const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("zt", .{
        .root_source_file = b.path("src/zt.zig"),
    });

    // setup tests
    const tests = b.addTest(.{
        .root_source_file = b.path("src/zt.zig"),
        .target = target,
        .optimize = optimize,
        .test_runner = b.path("test_runner.zig"),
    });

    const run_test = b.addRunArtifact(tests);
    run_test.has_side_effects = true;

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_test.step);
}
