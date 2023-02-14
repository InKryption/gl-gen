const std = @import("std");
const TargetVersion = @import("src/target-version.zig").TargetVersion;

pub fn build(b: *std.Build) void {
    const gen_gl = b.addExecutable(.{
        .name = "gen-gl",
        .root_source_file = .{ .path = "src/gen-gl.zig" },
        .optimize = .Debug,
    });
    gen_gl.install();
    gen_gl.linkLibC();

    const build_options = b.addOptions();
    gen_gl.addOptions("build_options", build_options);

    const target_version = b.option(TargetVersion, "version", "Version of OpenGL to target.\n") orelse
        @panic("Must specify target OpenGL version.\n");

    // this is a bit of a hack
    build_options.contents.writer().print(
        \\pub const target_version = .{s};
        \\
    , .{@tagName(target_version)}) catch unreachable;

    // the rest of this is just fluff to demo the cache-ability.

    const run_gen_gl = b.addRunArtifact(gen_gl);
    _ = run_gen_gl.addOutputFileArg("gl.zig"); // this returns a file source, would usually add it as a module or w/e
    run_gen_gl.addArg("gl.xml");

    run_gen_gl.step.dependOn(b.getInstallStep());

    const run_gen_gl_step = b.step("run", "Run the generator");
    run_gen_gl_step.dependOn(&run_gen_gl.step);
}
