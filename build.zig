const std = @import("std");
const Build = std.Build;
const FileSource = Build.FileSource;
const util = @import("src/util.zig");
const gl_targets = @import("src/opengl-targets.zig");

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    _ = target;
    const optimize = b.standardOptimizeOption(.{});
    _ = optimize;
    const gl_version: gl_targets.Version = b.option(gl_targets.Version, "version", "Version of OpenGL to target") orelse @panic("Must specify OpenGL target version.\n");
    const gl_profile: gl_targets.Profile = b.option(gl_targets.Profile, "profile", "OpenGL profile to target") orelse @panic("Must specify OpenGL target profile.\n");
    const xml_registry_file: ?[]const u8 = b.option([]const u8, "registry", "Path to the gl.xml registry file");
    if (xml_registry_file != null and !std.fs.path.isAbsolute(xml_registry_file.?)) {
        @panic("Must specify an absolute path to the gl.xml registry file");
    }

    const generator_build_options = b.createModule(.{
        .dependencies = &moduleDependencies(.{
            .@"opengl-targets" = b.createModule(.{ .source_file = FileSource.relative("src/opengl-targets.zig") }),
        }),
        .source_file = blk: {
            const basename = "build-options.zig";

            var contents = std.ArrayList(u8).init(b.allocator);
            defer contents.deinit();

            const writer = contents.writer();

            writer.print(
                \\const std = @import("std");
                \\const gl_targets = @import("opengl-targets");
                \\
                \\pub const gl_version: gl_targets.Version = .{s};
                \\pub const gl_profile: gl_targets.Profile = .{s};
                \\
            , .{
                @tagName(gl_version),
                @tagName(gl_profile),
            }) catch unreachable;

            const write_build_options = b.addWriteFile(basename, contents.items);
            break :blk write_build_options.getFileSource(basename).?;
        },
    });

    // the user may wish to use the generator executable directly;
    // specificially, this enables the use case where the 'gl.xml'
    // registry file cannot be supplied directly as a path, but instead
    // exists as a `std.Bulid.FileSource` in the user's build script.
    const generator_exe = b.addExecutable(.{
        .name = "generator-exe",
        .root_source_file = FileSource.relative("src/generate-gl.zig"),
    });
    generator_exe.install();
    generator_exe.linkLibC(); // TODO: stop using c allocator to avoid C dependency
    generator_exe.addModule("build-options", generator_build_options);

    const generate_bindings = b.addRunArtifact(generator_exe);
    const bindings_src = generate_bindings.addOutputFileArg("gl.zig");
    if (xml_registry_file) |path| {
        generate_bindings.addArg(path);
    }
    if (b.args) |args| {
        generate_bindings.addArgs(args);
    }

    // with this, the user can just obtain the opengl bindings as a module
    // through `glgen_dep.module("opengl_bindings")`
    b.addModule(.{
        .name = "opengl-bindings",
        .source_file = bindings_src,
        .dependencies = &moduleDependencies(.{}),
    });

    // Since we can't pass `std.FileSource`s through `std.Build.dependency`,
    // the user has to use the generator executable directly in order to supply
    // the xml registry as a file source argument. But if they want to use the
    // module, we issue an error if they didn't supply the xml registry file path.
    if (xml_registry_file == null) {
        const error_step = errorLogStep(b, error.MissingOpenGlXmlRegistry,
            \\In order to use the 'opengl-bindings' module,
            \\please specify `registry` when calling `std.Build.dependency`.
            \\
        , .{});
        generate_bindings.step.dependOn(&error_step.step);
    }

    // local testing & misc

    const install_file = b.addInstallFile(bindings_src, "gl.zig");
    bindings_src.addStepDependencies(&install_file.step);

    b.getInstallStep().dependOn(&install_file.step);
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

fn ModuleDependenciesArrayFromStruct(comptime Struct: type) type {
    const info = @typeInfo(Struct).Struct;
    std.debug.assert(!info.is_tuple or info.fields.len == 0);
    return [info.fields.len]Build.ModuleDependency;
}
inline fn moduleDependencies(
    dependency_struct: anytype,
) ModuleDependenciesArrayFromStruct(@TypeOf(dependency_struct)) {
    const DepStruct = @TypeOf(dependency_struct);
    const info = @typeInfo(DepStruct).Struct;

    var result: [info.fields.len]Build.ModuleDependency = undefined;
    inline for (info.fields) |field, i| {
        result[i] = .{
            .name = field.name,
            .module = @field(dependency_struct, field.name),
        };
    }
    return result;
}
