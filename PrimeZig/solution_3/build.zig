const std = @import("std");

pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "PrimeZig",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    const options = b.addOptions();

    const all = b.option(bool, "all", "should run uncurated examples") orelse false;
    options.addOption(bool, "all", all);

    const arm_is_rpi4 = b.option(bool, "arm-is-rpi4", "treat arm architecture as 8GB rpi4") orelse false;
    options.addOption(bool, "arm_is_rpi4", arm_is_rpi4);

    const debug = b.option(bool, "debug", "debug mode: no selections are built") orelse false;
    options.addOption(bool, "selected", !debug);

    exe.root_module.addOptions("build_options", options);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
