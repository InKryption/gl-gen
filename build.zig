const std = @import("std");
const Build = std.Build;
const FileSource = Build.FileSource;
const util = @import("src/util.zig");
const gl_targets = @import("src/opengl-targets.zig");

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    _ = optimize;

    // the user may wish to use the generator executable directly;
    // specificially, this enables the use case where the 'gl.xml'
    // registry file cannot be supplied directly as a path, but instead
    // exists as a `std.Bulid.FileSource` in the user's build script.
    const generator_exe = b.addExecutable(.{
        .name = "generator-exe",
        .root_source_file = FileSource.relative("src/generate-gl.zig"),
        .target = target,
    });
    generator_exe.install();
    generator_exe.linkLibC(); // TODO: stop using c allocator to avoid C dependency

    const generate_bindings = b.addRunArtifact(generator_exe);

    if (b.option([]const u8, "registry", "Path to the gl.xml registry file")) |xml_registry_path| {
        // this is just so that a build.zig using this script as a dependency doesn't
        // pass a path that we interpret as relative to this build.zig file.
        if (!std.fs.path.isAbsolute(xml_registry_path)) @panic("Must specify an absolute path to the gl.xml registry file");
        generate_bindings.addArg("--registry");
        generate_bindings.addArg(xml_registry_path);
    }

    // Should be something like `GL_VERSION_4_6`, `GL_VERSION_ES_CM_1_0`, `GL_ES_VERSION_2_0`, or `GL_SC_VERSION_2_0`
    if (b.option([]const u8, "version", "Version of OpenGL to target")) |version| {
        generate_bindings.addArg("--api-version");
        generate_bindings.addArg(version);
    }

    if (b.option(gl_targets.Profile, "profile", "OpenGL profile to target")) |profile| {
        generate_bindings.addArg("--api-profile");
        generate_bindings.addArg(@tagName(profile));
    }

    // conveniently export a source file if the relevant arguments are provided.
    generate_bindings.addArg("--out");
    const bindings_src = generate_bindings.addOutputFileArg("gl.zig");

    // make sure to add extension arguments afterwards if any are provided
    if (b.option([]const []const u8, "extensions", "List of extension names to filter for")) |extensions| {
        generate_bindings.addArg("--");
        generate_bindings.addArgs(extensions);
    }

    // with this, the user can just obtain the opengl bindings as a module
    // through `glgen_dep.module("opengl_bindings")`
    _ = b.addModule("opengl-bindings", .{
        .source_file = bindings_src,
        .dependencies = &[_]Build.ModuleDependency{},
    });

    // local testing & misc

    const install_file = b.addInstallFile(bindings_src, "gl.zig");
    bindings_src.addStepDependencies(&install_file.step);

    b.getInstallStep().dependOn(&install_file.step);
}
