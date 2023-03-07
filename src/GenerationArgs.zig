const std = @import("std");
const util = @import("util.zig");
const gl_targets = @import("opengl-targets.zig");

const assert = std.debug.assert;

const GenerationArgs = @This();
output_file_path: []const u8,
gl_xml_file_path: []const u8,
c_scratch_file_path: []const u8,
zig_exe_path: []const u8,
api_version: gl_targets.Version,
api_profile: gl_targets.Profile,
extensions: []const []const u8,

const ArgName = enum {
    out,
    registry,
    @"c-scratch",
    @"zig-exe",
    @"api-version",
    @"api-profile",
};
comptime {
    assert(@typeInfo(ArgName).Enum.fields.len == @typeInfo(GenerationArgs).Struct.fields.len - 1);
}

pub fn deinit(args: GenerationArgs, allocator: std.mem.Allocator) void {
    for (args.extensions) |ext| allocator.free(ext);
    allocator.free(args.extensions);
    allocator.free(args.zig_exe_path);
    allocator.free(args.c_scratch_file_path);
    allocator.free(args.gl_xml_file_path);
    allocator.free(args.output_file_path);
}

pub fn parse(
    allocator: std.mem.Allocator,
    args_iter: anytype,
    comptime log_scope: @TypeOf(.enum_literal),
) !GenerationArgs {
    const log = std.log.scoped(log_scope);

    if (!args_iter.skip()) @panic("Tried to skip argv[0] (executable path), but argv is empty.\n");

    var output_file_path: ?[]u8 = null;
    errdefer allocator.free(output_file_path orelse "");

    var gl_xml_file_path: ?[]u8 = null;
    errdefer allocator.free(gl_xml_file_path orelse "");

    var c_scratch_file_path: ?[]u8 = null;
    errdefer allocator.free(c_scratch_file_path orelse "");

    var zig_exe_path: ?[]u8 = null;
    errdefer allocator.free(zig_exe_path orelse "");

    var api_version: ?gl_targets.Version = null;
    var api_profile: ?gl_targets.Profile = null;
    var present = std.EnumSet(ArgName).initEmpty();

    const whitespace_chars = [_]u8{ ' ', '\t', '\n', '\r' };
    while (true) {
        const arg_name: ArgName = blk: {
            const arg_start = std.mem.trim(u8, checkResult(args_iter.next()) orelse break, &whitespace_chars);
            if (std.mem.eql(u8, arg_start, "--")) break;

            if (!std.mem.startsWith(u8, arg_start, "--")) {
                log.err("Expected argument name to be preceeded by '--'.\n", .{});
                continue;
            }
            const tag = std.meta.stringToEnum(ArgName, arg_start["--".len..]) orelse {
                log.warn("Unrecognised argument name '{s}'. Valid argument names are:\n{s}\n", .{
                    arg_start["--".len..],
                    util.fmtMultiLineList(std.meta.fieldNames(ArgName), .{
                        .indent = &[_]u8{' '} ** 2,
                        .element_prefix = "\"",
                        .element_suffix = "\",",
                    }),
                });
                return error.UnrecognisedArgumentName;
            };

            break :blk tag;
        };
        const arg_val: []const u8 = if (checkResult(args_iter.next())) |arg_val| std.mem.trim(u8, arg_val, &whitespace_chars) else {
            log.err("Expected argument key value pairs of the form '--<name> <value>'.\n", .{});
            break;
        };

        if (present.contains(arg_name))
            log.warn("Specified '{s}' more than once.\n", .{@tagName(arg_name)});
        present.setPresent(arg_name, true);

        switch (arg_name) {
            .out => {
                output_file_path = try allocator.realloc(output_file_path orelse @as([]u8, ""), arg_val.len);
                std.mem.copy(u8, output_file_path.?, arg_val);
            },
            .registry => {
                gl_xml_file_path = try allocator.realloc(gl_xml_file_path orelse @as([]u8, ""), arg_val.len);
                std.mem.copy(u8, gl_xml_file_path.?, arg_val);
            },
            .@"zig-exe" => {
                zig_exe_path = try allocator.realloc(zig_exe_path orelse @as([]u8, ""), arg_val.len);
                std.mem.copy(u8, zig_exe_path.?, arg_val);
            },
            .@"c-scratch" => {
                c_scratch_file_path = try allocator.realloc(c_scratch_file_path orelse @as([]u8, ""), arg_val.len);
                std.mem.copy(u8, c_scratch_file_path.?, arg_val);
            },
            .@"api-version" => api_version = std.meta.stringToEnum(gl_targets.Version, arg_val) orelse {
                log.err("Expected api-version to be the target OpenGL API version. Should be one of:\n{s}\n", .{
                    util.fmtMultiLineList(std.meta.fieldNames(gl_targets.Version), .{
                        .indent = &[_]u8{' '} ** 2,
                        .element_prefix = "\"",
                        .element_suffix = "\",",
                    }),
                });

                return error.UnrecognisedApiVersion;
            },
            .@"api-profile" => api_profile = std.meta.stringToEnum(gl_targets.Profile, arg_val) orelse {
                log.err("Expected api-profile to be the target OpenGL API version. Should be one of:\n{s}\n", .{
                    util.fmtMultiLineList(std.meta.fieldNames(gl_targets.Profile), .{
                        .indent = &[_]u8{' '} ** 2,
                        .element_prefix = "\"",
                        .element_suffix = "\",",
                    }),
                });
                return error.UnrecognisedApiProfile;
            },
        }
    }

    { // check if any arguments are missing
        const missing = present.complement();
        var missing_iter = missing.iterator();
        while (missing_iter.next()) |missing_tag|
            log.err("Missing argument '{s}'.\n", .{@tagName(missing_tag)});
        if (missing.count() != 0) return error.MissingArguments;
    }

    const extensions: []const []const u8 = blk: {
        var extensions = std.ArrayList([]const u8).init(allocator);
        defer extensions.deinit();
        errdefer for (extensions.items) |ext_name| {
            allocator.free(ext_name);
        };

        while (checkResult(args_iter.next())) |ext_name| {
            try extensions.append(try allocator.dupe(u8, std.mem.trim(u8, ext_name, &whitespace_chars)));
        }

        break :blk try extensions.toOwnedSlice();
    };
    errdefer {
        for (extensions) |ext| allocator.free(ext);
        allocator.free(extensions);
    }

    return GenerationArgs{
        .output_file_path = output_file_path.?,
        .gl_xml_file_path = gl_xml_file_path.?,
        .c_scratch_file_path = c_scratch_file_path.?,
        .zig_exe_path = zig_exe_path.?,
        .api_version = api_version.?,
        .api_profile = api_profile.?,
        .extensions = extensions,
    };
}

/// Simple helper function that checks the type of `result`.
/// Returns `result` if it's a pointer to bytes with a known length,
/// although coerced to `?[]const u8`.
inline fn checkResult(result: anytype) ?[]const u8 {
    const Optional = @TypeOf(result);
    const T = switch (@typeInfo(Optional)) {
        .Optional => |optional| optional.child,
        else => @compileError("Expected optional pointer to byte array, but got `" ++ @typeName(Optional) ++ "` instead."),
    };
    return switch (@typeInfo(T)) {
        .Pointer => |pointer| switch (pointer.size) {
            .Slice => result,
            .One => switch (@typeInfo(pointer.child)) {
                .Array => result,
                else => @compileError("Expected pointer to byte array, but got `" ++ @typeName(T) ++ "` instead."),
            },
            else => @compileError("Pointer type `" ++ @typeName(T) ++ "` not supported."),
        },
        else => @compileError("Expected a pointer to bytes, but got `" ++ @typeName(T) ++ "` instead."),
    };
}
