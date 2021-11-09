const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("PrimeZig", "src/main.zig");
    exe.linkLibC();
    exe.setTarget(target);
    exe.setBuildMode(mode);

    const all = b.option(bool, "all", "should run uncurated examples") orelse false;
    exe.addBuildOption(bool, "all", all);

    const arm_is_rpi4 = b.option(bool, "arm-is-rpi4", "treat arm architecture as 8GB rpi4") orelse false;
    exe.addBuildOption(bool, "arm_is_rpi4", arm_is_rpi4);

    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
