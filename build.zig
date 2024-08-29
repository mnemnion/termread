// Build script for termread
const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    
    const termread_module = b.addModule("termread", .{
        .root_source_file = b.path("src/termread.zig"),
        .target = target,
        .optimize = optimize,
    });

    _ = termread_module; // autofix
          
    const exe = b.addExecutable(.{
        .name = "termread",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
            run_cmd.addArgs(args);
        }
        
    const test_filters = b.option(
        []const []const u8,
        "test-filter",
        "Skip tests that do not match any filter",
    ) orelse &[0][]const u8{};

    
    const module_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/termread.zig"),
        .target = target,
        .optimize = optimize,
        .filters = test_filters,
    });

    const run_module_unit_tests = b.addRunArtifact(module_unit_tests);
          
    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .filters = test_filters,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
        
    const test_step = b.step("test", "Run unit tests");
    
    test_step.dependOn(&run_module_unit_tests.step);
        
    test_step.dependOn(&run_exe_unit_tests.step);
        

    const addOutputDirectoryArg = comptime if (@import("builtin").zig_version.order(.{ .major = 0, .minor = 13, .patch = 0 }) == .lt)
        std.Build.Step.Run.addOutputFileArg
    else
        std.Build.S tep.Run.addOutputDirectoryArg;

    const run_kcov = b.addSystemCommand(&.{
        "kcov",
        "--clean",
        "--exclude-line=unreachable,expect(false)",
    });
    run_kcov.addPrefixedDirectoryArg("--include-pattern=", b.path("."));
    const coverage_output = addOutputDirectoryArg(run_kcov, ".");

    // Pick your coverage entry point here:
    run_kcov.addArtifactArg(module_unit_tests);

    run_kcov.enableTestRunnerMode();

    const install_coverage = b.addInstallDirectory(.{
        .source_dir = coverage_output,
        .install_dir = .{ .custom = "coverage" },
        .install_subdir = "",
    });

    const coverage_step = b.step("coverage", "Generate coverage (kcov must be installed)");
    coverage_step.dependOn(&install_coverage.step); 
}
