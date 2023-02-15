const std = @import("std");
const Build = std.Build;
const gl_targets = @import("src/opengl-targets.zig");

pub fn build(b: *Build) void {
    const target_version: gl_targets.Version = b.option(gl_targets.Version, "version", "Version of OpenGL to target") orelse @panic("Must specify OpenGL target version.\n");
    const target_profile: gl_targets.Profile = b.option(gl_targets.Profile, "profile", "OpenGL profile to target") orelse @panic("Must specify OpenGL target profile.\n");
    const output_name: []const u8 = b.option([]const u8, "output-name", "Name of the file that the bindings will be written to [default: 'gl.zig']") orelse "gl.zig";
    const xml_registry_file: ?[]const u8 = b.option([]const u8, "registry", "Path to the gl.xml registry file");

    const generator_build_options = b.createModule(.{
        .dependencies = &[_]Build.ModuleDependency{
            .{ .name = "opengl-targets", .module = b.createModule(.{ .source_file = .{ .path = "src/opengl-targets.zig" } }) },
        },
        .source_file = blk: {
            const basename = "build-options.zig";
            const write_build_options = b.addWriteFile(basename, b.fmt(
                \\const gl_targets = @import("opengl-targets");
                \\
                \\pub const target_version: gl_targets.Version = .{s};
                \\pub const target_profile: gl_targets.Profile = .{s};
                \\
            , .{ @tagName(target_version), @tagName(target_profile) }));
            break :blk write_build_options.getFileSource(basename).?;
        },
    });

    // the user may wish to use the generator executable directly;
    // specificially, this enables the use case where the 'gl.xml'
    // registry file cannot be supplied directly as a path, but instead
    // exists as a `std.Bulid.FileSource` in the user's build script.
    const generator_exe = b.addExecutable(.{
        .name = "generator-exe",
        .root_source_file = .{ .path = "src/generate-gl.zig" },
        .optimize = .Debug,
    });
    generator_exe.install();
    generator_exe.linkLibC();
    generator_exe.addModule("build-options", generator_build_options);

    const generate_bindings = b.addRunArtifact(generator_exe);
    const bindings_src = generate_bindings.addOutputFileArg(output_name);
    if (xml_registry_file) |path| {
        generate_bindings.addFileSourceArg(.{ .path = path });
    }

    // with this, the user can just obtain the opengl bindings as a module
    // through `glgen_dep.module("opengl_bindings")`
    b.addModule(.{
        .name = "opengl-bindings",
        .source_file = bindings_src,
    });

    // Since we can't pass `std.Build.FileSource`s through `std.Build.dependency`,
    // the user has to use the generator executable directly in order to supply
    // the xml registry as a file source argument. But if they want to use the
    // module, we issue an error if they didn't supply the xml registry file path.
    if (xml_registry_file == null) {
        generate_bindings.step.dependOn(&errorLogStep(b, error.MissingOpenGlXmlRegistry,
            \\In order to use the 'opengl-bindings' module,
            \\please specify `registry` when calling `std.Build.dependency`.
            \\
        , .{}).step);
    }
}

fn errorLogStep(
    b: *Build,
    err_code: anyerror,
    comptime fmt_str: []const u8,
    args: anytype,
) *ErrorLogStep(fmt_str[0..].*, @TypeOf(args)) {
    const Result = ErrorLogStep(
        fmt_str[0..].*,
        @TypeOf(args),
    );
    return Result.create(b, err_code, args);
}
fn ErrorLogStep(
    comptime fmt_str: anytype,
    comptime ArgsTuple: type,
) type {
    std.debug.assert(@TypeOf(fmt_str) == [fmt_str.len]u8);
    return struct {
        const Self = @This();
        step: Build.Step,
        fmt_args: ArgsTuple,
        err_code: anyerror,

        fn create(b: *Build, err_code: anyerror, args: ArgsTuple) *Self {
            const self = b.allocator.create(Self) catch unreachable;
            self.* = .{
                .step = Build.Step.init(.custom, "error-log-step", b.allocator, Self.make),
                .fmt_args = args,
                .err_code = err_code,
            };
            return self;
        }

        fn make(step: *Build.Step) anyerror!void {
            const self = @fieldParentPtr(Self, "step", step);
            std.log.err(&fmt_str, self.fmt_args);
            return self.err_code;
        }
    };
}
