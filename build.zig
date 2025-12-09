const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // const is_dynamic_dispatch =
    //     b.option(bool, "dynamic_dispatch", "Enable Dynamic Function Dispatch") orelse false;
    // const options = b.addOptions();
    // options.addOption(bool, "is_dynamic_dispatch", is_dynamic_dispatch);

    const stringzilla_dep = b.lazyDependency("stringzilla", .{
        .target = target,
        .optimize = optimize,
    }) orelse return;

    const stringzilla_mod = b.addModule("stringzilla", .{
        .root_source_file = b.path("src/stringzilla.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    //stringzilla_mod.addOptions("config", options);

    stringzilla_mod.addCSourceFile(.{
        .file = stringzilla_dep.path("c/stringzilla.c"),
        .flags = &[_][]const u8{
            "-DSZ_DYNAMIC_DISPATCH=1",
            "-fno-sanitize=alignment",
        },
    });
    stringzilla_mod.addIncludePath(stringzilla_dep.path("include"));

    // const stringzilla_lib = b.addLibrary(.{
    //     .name = "stringzilla",
    //     .root_module = stringzilla_mod,
    // });
    // stringzilla_lib.installHeadersDirectory(stringzilla_dep.path("include/"), "", .{});
    // b.installArtifact(stringzilla_lib);

    const exe_root_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "stringzilla_example",
        .root_module = exe_root_module,
    });
    exe_root_module.addImport("stringzilla", stringzilla_mod);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the test application");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_module = stringzilla_mod,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    const exe_check = b.addExecutable(.{
        .name = "lsp check",
        .root_module = exe_root_module,
    });
    const check = b.step("check", "LSP Check that module compiles");
    check.dependOn(&exe_check.step);
}
