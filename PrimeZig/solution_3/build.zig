const std = @import("std");
const Builder = std.build.Builder;

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

    const debug = b.option(bool, "debug", "debug mode: no selections are built") orelse false;

    exe.addBuildOption(bool, "selected", !debug);

    const tracy = b.option([]const u8, "tracy", "Enable Tracy integration. Supply path to Tracy source");

    if (tracy) |tracy_path| {
        const client_cpp = std.fs.path.join(
            b.allocator,
            &[_][]const u8{ tracy_path, "TracyClient.cpp" },
        ) catch unreachable;

        exe.addBuildOption(bool, "use_tracy", true);

        // On mingw, we need to opt into windows 7+ to get some features required by tracy.
        const tracy_c_flags: []const []const u8 = if (target.isWindows() and target.getAbi() == .gnu)
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined", "-D_WIN32_WINNT=0x601" }
        else
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined" };

        exe.addIncludeDir(tracy_path);
        exe.addCSourceFile(client_cpp, tracy_c_flags);
        exe.linkSystemLibraryName("c++");
        exe.linkLibC();

        if (target.isWindows()) {
            exe.linkSystemLibrary("dbghelp");
            exe.linkSystemLibrary("ws2_32");
        }

        exe.want_lto = false;
    } else {
        exe.addBuildOption(bool, "use_tracy", true);
    }

    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
