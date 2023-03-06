const std = @import("std");
const xml = @import("xml.zig");
const util = @import("util.zig");
const gl_targets = @import("opengl-targets.zig");

const Registry = @import("Registry.zig");
const assert = std.debug.assert;

/// recognised khronos types
const KhronosType = enum {
    /// signed   8  bit
    khronos_int8_t,
    /// unsigned 8  bit
    khronos_uint8_t,
    /// signed   16 bit
    khronos_int16_t,
    /// unsigned 16 bit
    khronos_uint16_t,
    /// signed   32 bit
    khronos_int32_t,
    /// unsigned 32 bit
    khronos_uint32_t,
    /// signed   64 bit
    khronos_int64_t,
    /// unsigned 64 bit
    khronos_uint64_t,
    /// signed   same number of bits as a pointer
    khronos_intptr_t,
    /// unsigned same number of bits as a pointer
    khronos_uintptr_t,
    /// signed   size
    khronos_ssize_t,
    /// unsigned size
    khronos_usize_t,
    /// signed   32 bit floating point
    khronos_float_t,

    inline fn bitDepth(khronos_type: @This()) u16 {
        return switch (khronos_type) {
            .khronos_int8_t => 8,
            .khronos_uint8_t => 8,
            .khronos_int16_t => 16,
            .khronos_uint16_t => 16,
            .khronos_int32_t => 32,
            .khronos_uint32_t => 32,
            .khronos_int64_t => 64,
            .khronos_uint64_t => 64,
            .khronos_intptr_t => @bitSizeOf(*anyopaque),
            .khronos_uintptr_t => @bitSizeOf(*anyopaque),
            .khronos_ssize_t => @bitSizeOf(isize),
            .khronos_usize_t => @bitSizeOf(usize),
            .khronos_float_t => @bitSizeOf(f32),
        };
    }
};

/// recognised OpenGL types
const OpenGlType = enum {
    /// A boolean value, either GL_TRUE or GL_FALSE
    /// Bitdepth: 1+
    GLboolean,
    /// Signed, 2's complement binary integer
    /// Bitdepth: 8
    /// Common Enum: GL_BYTE
    GLbyte,
    /// Unsigned binary integer
    /// Common Enum: GL_UNSIGNED_BYTE
    /// Bitdepth: 8
    GLubyte,
    /// Signed, 2's complement binary integer
    /// Common Enum: GL_SHORT
    /// Bitdepth: 16
    GLshort,
    /// Unsigned binary integer
    /// Common Enum: GL_UNSIGNED_SHORT
    /// Bitdepth: 16
    GLushort,
    /// Signed, 2's complement binary integer
    /// Common Enum: GL_INT
    /// Bitdepth: 32
    GLint,
    /// Unsigned binary integer
    /// Common Enum: GL_UNSIGNED_INT
    /// Bitdepth: 32
    GLuint,
    /// Signed, 2's complement 16.16 integer
    /// Common Enum: GL_FIXED
    /// Bitdepth: 32
    GLfixed,
    /// Signed, 2's complement binary integer
    /// Bitdepth: 64
    GLint64,
    /// Unsigned binary integer
    /// Bitdepth: 64
    GLuint64,
    /// A non-negative binary integer, for sizes.
    /// Bitdepth: 32
    GLsizei,
    /// An OpenGL enumerator value
    /// Bitdepth: 32
    GLenum,
    /// Signed, 2's complement binary integer
    /// Bitdepth: ptrbits
    GLintptr,
    /// Non-negative binary integer size, for memory offsets and ranges
    /// Bitdepth: ptrbits
    GLsizeiptr,
    /// Sync Object handle
    /// Bitdepth: ptrbits
    GLsync,
    /// A bitfield value
    /// Bitdepth: 32
    GLbitfield,
    /// An IEEE-754 floating-point value
    /// Bitdepth: 16
    /// Common Enum: GL_HALF_FLOAT
    GLhalf,
    /// An IEEE-754 floating-point value
    /// Bitdepth: 32
    /// Common Enum: GL_FLOAT
    GLfloat,
    /// An IEEE-754 floating-point value, clamped to the range [0,1]
    /// Bitdepth: 32
    GLclampf,
    /// An IEEE-754 floating-point value
    /// Bitdepth: 64
    /// Common Enum: GL_DOUBLE
    GLdouble,
    /// An IEEE-754 floating-point value, clamped to the range [0,1]
    /// Bitdepth: 64
    GLclampd,

    inline fn bitDepth(gl_type: @This()) u16 {
        return switch (gl_type) {
            .GLboolean => 1,
            .GLbyte => 8,
            .GLubyte => 8,
            .GLshort => 16,
            .GLushort => 16,
            .GLint => 32,
            .GLuint => 32,
            .GLfixed => 32,
            .GLint64 => 64,
            .GLuint64 => 64,
            .GLsizei => 32,
            .GLenum => 32,
            .GLintptr => @bitSizeOf(*anyopaque),
            .GLsizeiptr => @bitSizeOf(*anyopaque),
            .GLsync => @bitSizeOf(*anyopaque),
            .GLbitfield => 32,
            .GLhalf => 16,
            .GLfloat => 32,
            .GLclampf => 32,
            .GLdouble => 64,
            .GLclampd => 64,
        };
    }
};

pub fn main() !void {
    const log = std.log.default;

    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 16 }){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();
    // const allocator = std.heap.c_allocator;

    const args: GenerationArgs = args: {
        var args_iter = try std.process.argsWithAllocator(allocator);
        defer args_iter.deinit();
        break :args try GenerationArgs.parse(allocator, &args_iter, .@"parse-gen-args");
    };
    defer args.deinit(allocator);

    const target_api_version: gl_targets.Version = args.api_version;
    const target_api_profile: gl_targets.Profile = args.api_profile;

    const registry: Registry = reg: {
        const tree: xml.Tree = tree: {
            const gl_xml_file = try std.fs.cwd().openFile(args.gl_xml_file_path, .{});
            defer gl_xml_file.close();

            var buffered_gl_xml = std.io.bufferedReaderSize(32_768, gl_xml_file.reader());
            const gl_xml_reader = buffered_gl_xml.reader();

            var error_line: u64 = undefined;
            var error_column: u64 = undefined;

            break :tree xml.parse(allocator, gl_xml_reader, .{
                .line = &error_line,
                .column = &error_column,
                .discard_comments = true,
                .discard_whitespace = true,
                .transform_entity_refs = true,
            }) catch |err| {
                log.err("Parsing error encountered at {d}:{d} (line:column) of the registry.\n", .{ error_line + 1, error_column + 1 });
                return err;
            };
        };
        defer tree.deinit(allocator);

        const registry = try Registry.parse(tree.root, allocator);
        errdefer registry.deinit(allocator);

        break :reg registry;
    };
    defer registry.deinit(allocator);

    if (false) { // debug print the type decls
        for (registry.types) |type_entry| {
            std.debug.print(
                \\{s}:
                \\  * requires: {?s}
                \\  * API: {?s}
                \\  * comment: {?s}
                \\  * name in body: {}
                \\  * C Definition: "
            , .{
                type_entry.name,
                type_entry.requires,
                if (type_entry.api) |tag| @tagName(tag) else null,
                type_entry.comment,
                type_entry.name_in_body,
            });
            switch (type_entry.type_def.apientry_indices) {
                .one => |index| {
                    std.debug.print("{s}<apientry/>{s}", .{
                        type_entry.type_def.text[0..index],
                        type_entry.type_def.text[index..],
                    });
                },
                .many => |indices| {
                    if (indices.len == 0) {
                        std.debug.print("{s}", .{type_entry.type_def.text});
                    } else {
                        for (indices) |index| {
                            std.debug.print("{s}<apientry/>", .{type_entry.type_def.text[0..index]});
                        }
                        std.debug.print("{s}", .{type_entry.type_def.text[indices[indices.len - 1]..]});
                    }
                },
            }
            std.debug.print("\"\n\n", .{});
        }
    }

    if (false) { // debug print the enum sets
        for (registry.enum_sets) |set| {
            std.debug.print(
                \\ * namespace: {s}
                \\ * group: {?s}
                \\ * type: {?s}
                \\ * vendor: {?s}
                \\ * range: 
            , .{
                set.namespace,
                set.group,
                set.type,
                set.vendor,
            });
            if (set.range) |range| {
                std.debug.print(
                    \\ {s}...{s}
                    \\
                , .{ range.start, range.end orelse range.start });
            } else {
                std.debug.print("null\n", .{});
            }

            std.debug.print(" * values:", .{});
            if (set.values.len == 0)
                std.debug.print(" (none)\n", .{})
            else
                std.debug.print("\n", .{});
            for (set.values) |val| {
                std.debug.print(
                    \\   + {s} = {s}{s} (alias={?s}, group={?s})
                    \\
                , .{
                    val.name,
                    val.value,
                    switch (val.type) {
                        .none => "",
                        inline else => |_, tag| @tagName(tag),
                    },
                    val.alias,
                    val.group,
                });
            }

            std.debug.print(" * unused ranges:", .{});
            if (set.unused_ranges.len == 0)
                std.debug.print(" (none)\n", .{})
            else
                std.debug.print("\n", .{});
            for (set.unused_ranges) |unused_range| {
                std.debug.print(
                    \\   + {s}...{s}
                , .{
                    unused_range.range.start,
                    unused_range.range.end orelse unused_range.range.start,
                });
                if (unused_range.vendor) |vendor| {
                    std.debug.print(
                        \\ ({s})
                    , .{vendor});
                }
                if (unused_range.comment) |comment| {
                    std.debug.print(
                        \\: "{s}"
                    , .{comment});
                }
                std.debug.print("\n", .{});
            }

            std.debug.print("\n", .{});
        }
    }

    if (false) { // debug print the commands
        std.debug.print("Commands (Namespace={s}):\n", .{registry.commands.namespace});
        for (registry.commands.entries) |cmd| {
            assert(!cmd.proto.def[cmd.proto.name_index].is_ptype);
            std.debug.print("   + \"", .{});
            for (cmd.proto.def, 0..) |component, i| {
                if (i != 0) std.debug.print(" ", .{});
                std.debug.print("{s}", .{std.mem.trim(u8, component.text, &[_]u8{ ' ', '\t', '\n', '\r' })});
            }
            std.debug.print("\":\n", .{});
            if (cmd.proto.group) |group| std.debug.print("     - group: {s}\n", .{group});

            if (cmd.comment) |comment| std.debug.print("     - comment: \"{s}\"\n", .{comment});
            if (cmd.alias) |alias| std.debug.print("     - alias: {s}\n", .{alias});
            if (cmd.vecequiv) |vecequiv| std.debug.print("     - vecequiv: {s}\n", .{vecequiv});

            std.debug.print("     - params:", .{});
            if (cmd.params.len == 0)
                std.debug.print(" (none)\n", .{})
            else
                std.debug.print("\n", .{});
            std.debug.print("", .{});
            for (cmd.params) |param| {
                std.debug.print("       º ", .{});
                for (param.def, 0..) |component, i| {
                    if (i != 0) std.debug.print(" ", .{});
                    std.debug.print("{s}", .{std.mem.trim(u8, component.text, &[_]u8{ ' ', '\t', '\n', '\r' })});
                }
                if (param.class) |class| std.debug.print(" (class=\"{s}\")", .{class});
                if (param.group) |group| std.debug.print(" (group=\"{s}\")", .{group});
                if (param.len) |len| std.debug.print(" (len=\"{s}\")", .{len});
                std.debug.print("\n", .{});
            }
            if (cmd.glx.len != 0) {
                std.debug.print("     - glx:\n", .{});
                for (cmd.glx) |info| {
                    std.debug.print("        º type={s}, opcode={s}", .{ info.type, info.opcode });
                    if (info.name) |name| std.debug.print(", name=\"{s}\"", .{name});
                    if (info.comment) |comment| std.debug.print(", comment=\"{s}\"", .{comment});
                    std.debug.print("\n", .{});
                }
            }
            std.debug.print("\n", .{});
        }
    }

    if (false) { // debug print the features
        for (registry.features) |feature| {
            std.debug.print(
                \\Feature ({s}):
                \\  * API: {s}
                \\  * Number: {d}.{d}
                \\
            , .{
                @tagName(feature.name),
                @tagName(feature.api),
                feature.number.major,
                feature.number.minor,
            });
            if (feature.comment) |comment| {
                std.debug.print(
                    \\  * Comment: "{s}"
                    \\
                , .{comment});
            }
            if (feature.protect) |protect| {
                std.debug.print(
                    \\  * Protect: {s}
                    \\
                , .{protect});
            }
            for ([_][]const Registry.FeatureSetGroup.FeatureSet{ feature.require_sets, feature.remove_sets }, 0..) |set_group, i| {
                std.debug.print(
                    \\  * {s} sets:
                , .{if (i == 0) "Require" else if (i == 1) "Remove" else unreachable});
                if (set_group.len == 0) std.debug.print(" (none)", .{});
                std.debug.print("\n", .{});

                for (set_group) |set| {
                    std.debug.print("    º Set:", .{});
                    if (set.profile) |profile| {
                        std.debug.print(" profile={s}", .{@tagName(profile)});
                    }
                    if (set.comment) |comment| {
                        if (set.profile != null) std.debug.print(",", .{});
                        std.debug.print(" comment=\"{s}\"", .{comment});
                    }
                    std.debug.print("\n", .{});

                    std.debug.print("      + Commands:", .{});
                    if (set.commands.len == 0) std.debug.print(" (none)", .{});
                    std.debug.print("\n", .{});
                    for (set.commands) |cmd| {
                        std.debug.print("        - {s}", .{cmd.name});
                        if (cmd.comment) |comment| std.debug.print(": \"{s}\"", .{comment});
                        std.debug.print("\n", .{});
                    }

                    std.debug.print("      + Enums:", .{});
                    if (set.enums.len == 0) std.debug.print(" (none)", .{});
                    std.debug.print("\n", .{});
                    for (set.enums) |enumerant| {
                        std.debug.print("        - {s}", .{enumerant.name});
                        if (enumerant.comment) |comment| std.debug.print(": \"{s}\"", .{comment});
                        std.debug.print("\n", .{});
                    }

                    std.debug.print("      + Types:", .{});
                    if (set.types.len == 0) std.debug.print(" (none)", .{});
                    std.debug.print("\n", .{});
                    for (set.types) |@"type"| {
                        std.debug.print("        - {s}", .{@"type".name});
                        if (@"type".comment) |comment| std.debug.print(": \"{s}\"", .{comment});
                        std.debug.print("\n", .{});
                    }

                    std.debug.print("\n", .{});
                }
                std.debug.print("\n", .{});
            }
        }
    }

    if (false) { // debug print the extensions
        for (registry.extensions) |extension| {
            std.debug.print(
                \\{s}:
                \\  + Supported: "{s}"
                \\
            , .{
                extension.name,
                extension.supported,
            });
            if (extension.protect) |protect| std.debug.print("  + Protect: \"{s}\"\n", .{protect});
            if (extension.comment) |comment| std.debug.print("  + Comment: \"{s}\"\n", .{comment});

            for ([_][]const Registry.Extension.FeatureSet{ extension.require_sets, extension.remove_sets }, 0..) |set_group, i| {
                std.debug.print(
                    \\  + {s} sets:
                , .{if (i == 0) "Require" else if (i == 1) "Remove" else unreachable});
                if (set_group.len == 0) std.debug.print(" (none)", .{});
                std.debug.print("\n", .{});

                for (set_group) |set| {
                    std.debug.print("    º Set:", .{});
                    if (set.api) |api| {
                        std.debug.print(" API=\"{s}\"", .{@tagName(api)});
                    }
                    if (set.profile) |profile| {
                        if (set.api != null) std.debug.print(",", .{});
                        std.debug.print(" profile={s}", .{@tagName(profile)});
                    }
                    if (set.comment) |comment| {
                        if (set.profile != null) std.debug.print(",", .{});
                        std.debug.print(" comment=\"{s}\"", .{comment});
                    }
                    std.debug.print("\n", .{});

                    std.debug.print("      + Commands:", .{});
                    if (set.commands.len == 0) std.debug.print(" (none)", .{});
                    std.debug.print("\n", .{});
                    for (set.commands) |cmd| {
                        std.debug.print("        - {s}", .{cmd.name});
                        if (cmd.comment) |comment| std.debug.print(": \"{s}\"", .{comment});
                        std.debug.print("\n", .{});
                    }

                    std.debug.print("      + Enums:", .{});
                    if (set.enums.len == 0) std.debug.print(" (none)", .{});
                    std.debug.print("\n", .{});
                    for (set.enums) |enumerant| {
                        std.debug.print("        - {s}", .{enumerant.name});
                        if (enumerant.comment) |comment| std.debug.print(": \"{s}\"", .{comment});
                        std.debug.print("\n", .{});
                    }

                    std.debug.print("      + Types:", .{});
                    if (set.types.len == 0) std.debug.print(" (none)", .{});
                    std.debug.print("\n", .{});
                    for (set.types) |@"type"| {
                        std.debug.print("        - {s}", .{@"type".name});
                        if (@"type".comment) |comment| std.debug.print(": \"{s}\"", .{comment});
                        std.debug.print("\n", .{});
                    }

                    std.debug.print("\n", .{});
                }
            }
            std.debug.print("\n", .{});
        }
    }

    const output_file = try std.fs.cwd().createFile(args.output_file_path, .{});
    defer output_file.close();

    var out_writer_buffered = std.io.bufferedWriter(output_file.writer());
    const out = out_writer_buffered.writer();

    var must_flush_out_writer_buffered = true;
    defer assert(!must_flush_out_writer_buffered or out_writer_buffered.end == 0);
    errdefer must_flush_out_writer_buffered = false;

    { // write the top level doc commment
        // describe generation parameters
        try out.print(
            \\//!
            \\//! Generation parameters:
            \\//! * API: {s}
            \\//! * Profile: {s}
            \\//! * Extensions: 
        , .{
            @tagName(target_api_version),
            @tagName(target_api_profile),
        });

        if (args.extensions.len == 0) {
            try out.writeAll("(none)\n");
        } else {
            for (args.extensions, 0..) |ext, i| {
                if (i != 0) try out.writeAll(", ");
                try out.print("\"{s}\"", .{ext});
            }
            try out.writeAll("\n");
        }

        if (registry.comment) |comment| { // write the comment element
            try out.writeAll(
                \\//!
                \\//! Registry comment:
                \\
            );
            var comment_line_iter = std.mem.split(u8, std.mem.trim(u8, comment, &.{ '\n', ' ' }), "\n");
            var longest_line_bytes: u32 = 0;
            while (comment_line_iter.next()) |line| {
                longest_line_bytes = @max(@intCast(u32, line.len), longest_line_bytes);
            }

            try out.writeAll("//! ");
            try out.writeByteNTimes('-', longest_line_bytes + 1);
            try out.writeByte('\n');
            comment_line_iter.reset();
            while (comment_line_iter.next()) |line| {
                try out.print("//! {s}\n", .{line});
            }
            try out.writeAll("//!");
            try out.writeByteNTimes('-', longest_line_bytes + 1);
            try out.writeAll("\n");
        }
        try out.writeAll(
            \\//!
            \\
        );
    }

    const FeatureSet = Registry.FeatureSetGroup.FeatureSet;

    const RequiredTypeCtx = util.AnyHashMapFieldContext(*const FeatureSet.Type, .name, std.array_hash_map.StringContext, u32, true);
    const RequiredEnumCtx = util.AnyHashMapFieldContext(*const FeatureSet.Enum, .name, std.array_hash_map.StringContext, u32, true);
    const RequiredCmdCtx = util.AnyHashMapFieldContext(*const FeatureSet.Command, .name, std.array_hash_map.StringContext, u32, true);

    var required_types = RequiredTypeCtx.ArrayHashMap(void, true).init(allocator);
    defer required_types.deinit();

    var required_enums = RequiredEnumCtx.ArrayHashMap(void, true).init(allocator);
    defer required_enums.deinit();

    var required_commands = RequiredCmdCtx.ArrayHashMap(void, true).init(allocator);
    defer required_commands.deinit();

    const target_feature_set_group: Registry.FeatureSetGroup = for (registry.features) |feature_set_group| {
        if (feature_set_group.name == target_api_version) break feature_set_group;
    } else return error.RegistryDoesntContainTargetFeatureSet;

    { // collect required stuff from target features
        var removed_types = RequiredTypeCtx.ArrayHashMap(void, true).init(allocator);
        defer {
            for (removed_types.keys()) |@"type"|
                _ = required_types.swapRemoveAdapted(@"type".name, RequiredTypeCtx.Adapted{ .inner = .{} });
            removed_types.deinit();
        }

        var removed_enums = RequiredEnumCtx.ArrayHashMap(void, true).init(allocator);
        defer {
            for (removed_enums.keys()) |enumerant|
                _ = required_enums.swapRemoveAdapted(enumerant.name, RequiredEnumCtx.Adapted{ .inner = .{} });
            removed_enums.deinit();
        }

        var removed_commands = RequiredCmdCtx.ArrayHashMap(void, true).init(allocator);
        defer {
            for (removed_commands.keys()) |cmd|
                _ = required_commands.swapRemoveAdapted(cmd.name, RequiredCmdCtx.Adapted{ .inner = .{} });
            removed_commands.deinit();
        }

        const OutSet = struct {
            types: *RequiredTypeCtx.ArrayHashMap(void, true),
            enums: *RequiredEnumCtx.ArrayHashMap(void, true),
            commands: *RequiredCmdCtx.ArrayHashMap(void, true),
        };
        for (registry.features) |feature_set_group| {
            if (feature_set_group.api != target_feature_set_group.api) {
                continue;
            }
            switch (feature_set_group.number.order(target_feature_set_group.number)) {
                .gt => continue,
                .lt, .eq => {},
            }
            _ = feature_set_group.protect; // TODO: Does this matter? Declaration order won't matter, but could it depend on something that isn't explicitly "required"?

            for ([_][]const Registry.FeatureSetGroup.FeatureSet{
                feature_set_group.require_sets,
                feature_set_group.remove_sets,
            }, [_]OutSet{
                OutSet{ .types = &required_types, .enums = &required_enums, .commands = &required_commands },
                OutSet{ .types = &removed_types, .enums = &removed_enums, .commands = &removed_commands },
            }) |feature_sets, out_set| {
                for (feature_sets) |set| {
                    if (set.profile != null and set.profile.? != target_api_profile) continue;

                    try out_set.types.ensureUnusedCapacity(set.types.len);
                    for (set.types) |*@"type"| out_set.types.putAssumeCapacity(@"type", {});

                    try out_set.enums.ensureUnusedCapacity(set.enums.len);
                    for (set.enums) |*enumerant| out_set.enums.putAssumeCapacity(enumerant, {});

                    try out_set.commands.ensureUnusedCapacity(set.commands.len);
                    for (set.commands) |*command| out_set.commands.putAssumeCapacity(command, {});
                }
            }
        }

        const desired_ext_set: std.StringHashMapUnmanaged(void) = blk: {
            var desired_ext_set = std.StringHashMapUnmanaged(void){};
            errdefer desired_ext_set.deinit(allocator);

            try desired_ext_set.ensureUnusedCapacity(allocator, std.math.lossyCast(u32, args.extensions.len));
            for (args.extensions) |ext_str| desired_ext_set.putAssumeCapacity(ext_str, {});

            break :blk desired_ext_set;
        };
        defer {
            var copy = desired_ext_set;
            copy.deinit(allocator);
        }

        for (registry.extensions) |extension| {
            if (!desired_ext_set.contains(extension.name)) continue;
            _ = extension.protect; // TODO: Does this matter? Declaration order won't matter, but could it depend on something that isn't explicitly "required"?

            {
                var support_iter = std.mem.split(u8, extension.supported, "|");
                while (support_iter.next()) |supported_str| {
                    const supported_api = std.meta.stringToEnum(gl_targets.Api, supported_str) orelse {
                        return error.UnrecognizedSupportedApi;
                    };
                    if (supported_api == target_feature_set_group.api) {
                        break;
                    }
                } else continue; // the support string doesn't contain the target API, discard it
            }

            if (extension.remove_sets.len != 0) {
                log.warn("Extension '{s}' removes features.", .{extension.name});
            }

            for ([_][]const Registry.Extension.FeatureSet{
                extension.require_sets,
                extension.remove_sets,
            }, [_]OutSet{
                OutSet{ .types = &required_types, .enums = &required_enums, .commands = &required_commands },
                OutSet{ .types = &removed_types, .enums = &removed_enums, .commands = &removed_commands },
            }) |feature_sets, out_set| {
                for (feature_sets) |set| {
                    if (set.profile != null and
                        set.profile.? != target_api_profile) continue;
                    if (set.api != null and
                        set.api.? != target_feature_set_group.api) continue;

                    try out_set.types.ensureUnusedCapacity(set.types.len);
                    for (set.types) |*@"type"| {
                        out_set.types.putAssumeCapacity(@"type", {});
                    }

                    try out_set.enums.ensureUnusedCapacity(set.enums.len);
                    for (set.enums) |*enumerant| {
                        out_set.enums.putAssumeCapacity(enumerant, {});
                    }

                    try out_set.commands.ensureUnusedCapacity(set.commands.len);
                    for (set.commands) |*command| {
                        out_set.commands.putAssumeCapacity(command, {});
                    }
                }
            }
        }
    }

    const EnumerantContext = util.AnyHashMapFieldContext(*const Registry.EnumsSet.Enumerant, .name, std.hash_map.StringContext, u64, false);
    const EnumerantSet = EnumerantContext.HashMap(void, std.hash_map.default_max_load_percentage);

    var all_enums = EnumerantSet.init(allocator);
    defer all_enums.deinit();

    var enum_groups = std.StringHashMap(EnumerantSet.Unmanaged).init(allocator);
    defer {
        var iter = enum_groups.valueIterator();
        while (iter.next()) |val| val.deinit(allocator);
        enum_groups.deinit();
    }

    // collect required enum values
    for (registry.enum_sets) |enum_set| {
        for (enum_set.enumerants) |*enumerant| {
            if (!required_enums.containsAdapted(enumerant.name, RequiredEnumCtx.Adapted{ .inner = .{} })) continue;
            try all_enums.putNoClobber(enumerant, {});

            var group_iter = std.mem.split(u8, enumerant.group orelse continue, ",");
            while (group_iter.next()) |group| {
                const gop_group = try enum_groups.getOrPut(group);
                if (!gop_group.found_existing) {
                    gop_group.value_ptr.* = .{};
                }

                const gop_enumerant = try gop_group.value_ptr.getOrPutAdapted(allocator, enumerant.name, EnumerantContext.Adapted{ .inner = .{} });
                // there should be no two distinct enumerants that have the same 'name'.
                // If there is, the registry is probably messed up.
                assert(!gop_enumerant.found_existing);
                gop_enumerant.key_ptr.* = enumerant;
            }
        }
    }

    {
        var lowercase_name_buf = std.ArrayList(u8).init(allocator);
        defer lowercase_name_buf.deinit();

        var ungrouped_iter = all_enums.keyIterator();
        while (ungrouped_iter.next()) |val_ptr| {
            const enumerant: Registry.EnumsSet.Enumerant = val_ptr.*.*;

            assert(std.mem.startsWith(u8, enumerant.name, "GL_"));
            const zig_name = try util.lowerStringArrayList(&lowercase_name_buf, enumerant.name["GL_".len..]);

            try out.print("pub const {s} = {s};\n", .{ util.fmtGlobalZigId(zig_name), enumerant.value });
        }

        var grouped_iter = enum_groups.iterator();
        while (grouped_iter.next()) |entry| {
            const group_name = entry.key_ptr.*;

            try out.print("pub const {s} = enum(u32) {{\n", .{group_name});
            var val_iter = entry.value_ptr.keyIterator();
            while (val_iter.next()) |val_ptr| {
                const enumerant: Registry.EnumsSet.Enumerant = val_ptr.*.*;

                assert(std.mem.startsWith(u8, enumerant.name, "GL_"));
                const zig_name = try util.lowerStringArrayList(&lowercase_name_buf, enumerant.name["GL_".len..]);

                try out.print("    {s} = {s},\n", .{ std.zig.fmtId(zig_name), enumerant.value });
            }
            try out.writeAll("};\n");
        }
    }

    try out_writer_buffered.flush();
}

const GenerationArgs = struct {
    output_file_path: []const u8,
    gl_xml_file_path: []const u8,
    api_version: gl_targets.Version,
    api_profile: gl_targets.Profile,
    extensions: []const []const u8,

    fn deinit(args: GenerationArgs, allocator: std.mem.Allocator) void {
        for (args.extensions) |ext| allocator.free(ext);
        allocator.free(args.extensions);
        allocator.free(args.output_file_path);
        allocator.free(args.gl_xml_file_path);
    }

    fn parse(
        allocator: std.mem.Allocator,
        args_iter: *std.process.ArgIterator,
        comptime log_scope: @TypeOf(.enum_literal),
    ) !GenerationArgs {
        const log = std.log.scoped(log_scope);

        if (!args_iter.skip()) @panic("Tried to skip argv[0] (executable path), but argv is empty.\n");

        var output_file_path: ?[]u8 = null;
        errdefer allocator.free(output_file_path orelse "");

        var gl_xml_file_path: ?[]u8 = null;
        errdefer allocator.free(gl_xml_file_path orelse "");

        var api_version: ?gl_targets.Version = null;
        var api_profile: ?gl_targets.Profile = null;

        const ArgName = enum {
            out,
            registry,
            @"api-version",
            @"api-profile",
        };
        var present = std.EnumSet(ArgName).initEmpty();

        const whitespace_chars = [_]u8{ ' ', '\t', '\n', '\r' };
        while (true) {
            const arg_name: ArgName = blk: {
                const arg_start = std.mem.trim(u8, args_iter.next() orelse break, &whitespace_chars);
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
            const arg_val: []const u8 = if (args_iter.next()) |arg_val| std.mem.trim(u8, arg_val, &whitespace_chars) else {
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

            while (args_iter.next()) |ext_name| {
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
            .api_version = api_version.?,
            .api_profile = api_profile.?,
            .extensions = extensions,
        };
    }
};
