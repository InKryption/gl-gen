const std = @import("std");
const util = @import("util.zig");
const gl_targets = @import("opengl-targets.zig");

const assert = std.debug.assert;

const GenerationArgs = @This();
output_file_path: []const u8,
gl_xml_file_path: []const u8,
api_version: []const u8,
api_profile: gl_targets.Profile,
extensions: std.StringArrayHashMapUnmanaged(void),

const ArgName = enum {
    out,
    registry,
    @"api-version",
    @"api-profile",

    inline fn isFlag(arg_name: @This()) bool {
        return switch (arg_name) {
            .out,
            .registry,
            .@"api-version",
            .@"api-profile",
            => false,
        };
    }
};

pub const ParseError = error{
    MissingDoubleDash,
    UnrecognisedArgumentName,
    NonFlagArgumentMissingValue,
    UnrecognisedApiProfile,
    MissingArguments,
};
pub fn parse(
    allocator: std.mem.Allocator,
    args_iter: anytype,
    comptime log_scope: @TypeOf(.enum_literal),
) (ParseError || std.mem.Allocator.Error)!GenerationArgs {
    const log = std.log.scoped(log_scope);

    if (!args_iter.skip()) @panic("Tried to skip argv[0] (executable path), but argv is empty.\n");

    var output_file_path: ?[]u8 = null;
    errdefer allocator.free(output_file_path orelse "");

    var gl_xml_file_path: ?[]u8 = null;
    errdefer allocator.free(gl_xml_file_path orelse "");

    var api_version: ?[]u8 = null;
    errdefer allocator.free(api_version orelse "");

    var api_profile: ?gl_targets.Profile = null;

    var present = std.EnumSet(ArgName).initEmpty();

    const found_extension_separator: bool = while (true) {
        const arg_name: ArgName = blk: {
            const arg_start_untrimmed = checkResult(args_iter.next()) orelse break false;
            const arg_start = std.mem.trim(u8, arg_start_untrimmed, &std.ascii.whitespace);
            if (!std.mem.startsWith(u8, arg_start, "--")) {
                log.err("Expected argument name to be preceeded by '--'.", .{});
                return error.MissingDoubleDash;
            }

            const arg_name_str: []const u8 = arg_start["--".len..];
            if (arg_name_str.len == 0) break true;
            const tag = std.meta.stringToEnum(ArgName, arg_start["--".len..]) orelse {
                log.warn("Unrecognised argument name '{s}'. Valid argument names are:\n{s}", .{
                    arg_start["--".len..],
                    util.fmtMultiLineList(comptime std.meta.fieldNames(ArgName), .{
                        .indent = &[_]u8{' '} ** 2,
                        .element_prefix = "\"",
                        .element_suffix = "\",",
                    }),
                });
                return error.UnrecognisedArgumentName;
            };

            break :blk tag;
        };
        const arg_val: ?[]const u8 = blk: {
            if (arg_name.isFlag()) break :blk null;
            const arg_val_untrimmed = checkResult(args_iter.next()) orelse {
                log.err("Expected argument key value pairs of the form '--{s} <value>'.", .{@tagName(arg_name)});
                return error.NonFlagArgumentMissingValue;
            };
            break :blk std.mem.trim(u8, arg_val_untrimmed, &std.ascii.whitespace);
        };

        const duplicate_arg = present.contains(arg_name);
        if (duplicate_arg)
            log.warn("Specified '{s}' more than once.\n", .{@tagName(arg_name)});
        present.setPresent(arg_name, true);

        switch (arg_name) {
            .out => output_file_path = try reallocAndCopy(allocator, output_file_path orelse @as([]u8, ""), arg_val.?),
            .registry => gl_xml_file_path = try reallocAndCopy(allocator, gl_xml_file_path orelse @as([]u8, ""), arg_val.?),
            .@"api-version" => api_version = try reallocAndCopy(allocator, api_version orelse @as([]u8, ""), arg_val.?),
            .@"api-profile" => api_profile = std.meta.stringToEnum(gl_targets.Profile, arg_val.?) orelse {
                log.err("Expected api-profile to be the target OpenGL API version. Should be one of:\n{s}", .{
                    util.fmtMultiLineList(comptime std.meta.fieldNames(gl_targets.Profile), .{
                        .indent = &[_]u8{' '} ** 2,
                        .element_prefix = "\"",
                        .element_suffix = "\",",
                    }),
                });
                return error.UnrecognisedApiProfile;
            },
        }
    };

    if (!present.eql(std.EnumSet(ArgName).initFull())) {
        var missing_list = std.BoundedArray([]const u8, @typeInfo(ArgName).Enum.fields.len){};

        var missing_iter = present.complement().iterator();
        while (missing_iter.next()) |missing|
            missing_list.appendAssumeCapacity(switch (missing) {
                inline else => |tag| "--" ++ @tagName(tag),
            });
        log.err("Missing arguments:\n{s}", .{util.fmtMultiLineList(missing_list.constSlice(), .{ .indent_level = 1, .element_suffix = "" })});

        return error.MissingArguments;
    }

    var extensions = std.StringArrayHashMapUnmanaged(void){};
    errdefer extensions.deinit(allocator);
    errdefer for (extensions.keys()) |ext| allocator.free(ext);

    if (found_extension_separator) {
        while (checkResult(args_iter.next())) |ext_name| {
            const gop = try extensions.getOrPut(allocator, ext_name);
            if (gop.found_existing) {
                log.warn("Specified extension '{s}' multiple times.", .{gop.key_ptr.*});
                continue;
            }
            gop.value_ptr.* = {};
            // this is fine, because the string contents are identical, so the hash should be identical
            gop.key_ptr.* = try allocator.dupe(u8, ext_name);
        }
    }

    return GenerationArgs{
        .output_file_path = output_file_path.?,
        .gl_xml_file_path = gl_xml_file_path.?,
        .api_version = api_version.?,
        .api_profile = api_profile.?,
        .extensions = extensions,
    };
}

pub fn deinit(const_args: GenerationArgs, allocator: std.mem.Allocator) void {
    var args = const_args;
    for (args.extensions.keys()) |ext| allocator.free(ext);
    args.extensions.deinit(allocator);
    allocator.free(args.api_version);
    allocator.free(args.gl_xml_file_path);
    allocator.free(args.output_file_path);
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

inline fn reallocAndCopy(allocator: std.mem.Allocator, alloc_dst: []u8, src: []const u8) std.mem.Allocator.Error![]u8 {
    const result: []u8 = switch (std.math.order(alloc_dst.len, src.len)) {
        .eq => alloc_dst,
        .lt, .gt => try allocator.realloc(alloc_dst, src.len),
    };
    std.mem.copy(u8, result, src);
    return result;
}
