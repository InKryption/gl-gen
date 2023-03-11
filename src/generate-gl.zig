const std = @import("std");
const builtin = @import("builtin");
const xml = @import("xml.zig");
const util = @import("util.zig");
const gl_targets = @import("opengl-targets.zig");

const GenerationArgs = @import("GenerationArgs.zig");
const Registry = @import("Registry.zig");
const assert = std.debug.assert;

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

    const target_api_version: []const u8 = args.api_version;
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
            target_api_version,
            @tagName(target_api_profile),
        });

        if (args.extensions.count() == 0) {
            try out.writeAll("(none)\n");
        } else {
            for (args.extensions.keys(), 0..) |ext, i| {
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
            \\
        );
    }

    const FeatureSetGroup = Registry.FeatureSetGroup;
    const target_feature_set_group = registry.features.getKeyAdapted(target_api_version, FeatureSetGroup.HashCtx.Adapted{}) orelse {
        const ListFeatureNamesFmt = struct {
            features: *const FeatureSetGroup.Set,
            pub fn format(
                self: @This(),
                comptime fmt_str: []const u8,
                options: std.fmt.FormatOptions,
                writer: anytype,
            ) @TypeOf(writer).Error!void {
                _ = fmt_str;
                _ = options;
                for (self.features.keys()) |entry| {
                    try writer.print("  '{s}'\n", .{entry.name});
                }
            }
        };
        log.err(
            "Couldn't find feature set group with name '{s}' in registry. Available feature set groups are:\n{}",
            .{ target_api_version, ListFeatureNamesFmt{ .features = &registry.features } },
        );
        return error.RegistryDoesntContainTargetFeatureSet;
    };

    var required_types = std.StringArrayHashMap(void).init(allocator);
    defer required_types.deinit();

    var required_enums = std.StringArrayHashMap(void).init(allocator);
    defer required_enums.deinit();

    var required_commands = std.StringArrayHashMap(void).init(allocator);
    defer required_commands.deinit();

    { // collect required stuff from target features and extension
        var removed_types = std.StringArrayHashMap(void).init(allocator);
        defer {
            for (removed_types.keys()) |@"type"|
                _ = required_types.swapRemove(@"type");
            removed_types.deinit();
        }

        var removed_enums = std.StringArrayHashMap(void).init(allocator);
        defer {
            for (removed_enums.keys()) |enumerant|
                _ = required_enums.swapRemove(enumerant);
            removed_enums.deinit();
        }

        var removed_commands = std.StringArrayHashMap(void).init(allocator);
        defer {
            for (removed_commands.keys()) |cmd|
                _ = required_commands.swapRemove(cmd);
            removed_commands.deinit();
        }

        const OutSet = struct {
            types: *std.StringArrayHashMap(void),
            enums: *std.StringArrayHashMap(void),
            commands: *std.StringArrayHashMap(void),
        };
        for (registry.features.keys()) |feature_set_group| {
            if (feature_set_group.api != target_feature_set_group.api) continue;
            switch (feature_set_group.number.order(target_feature_set_group.number)) {
                .gt => continue,
                .lt, .eq => {},
            }
            _ = feature_set_group.protect; // TODO: Does this matter? Declaration order won't matter, but could it depend on something that isn't explicitly "required"?

            for (
                [_][]const Registry.FeatureSetGroup.FeatureSet{
                    feature_set_group.require_sets,
                    feature_set_group.remove_sets,
                },
                [_]OutSet{
                    OutSet{ .types = &required_types, .enums = &required_enums, .commands = &required_commands },
                    OutSet{ .types = &removed_types, .enums = &removed_enums, .commands = &removed_commands },
                },
            ) |feature_sets, out_set| {
                for (feature_sets) |set| {
                    if (set.profile != null and set.profile.? != target_api_profile) continue;

                    try out_set.types.ensureUnusedCapacity(set.types.count());
                    for (set.types.keys()) |@"type"| {
                        out_set.types.putAssumeCapacity(@"type".name, {});
                    }

                    try out_set.enums.ensureUnusedCapacity(set.enums.count());
                    for (set.enums.keys()) |enumerant| {
                        out_set.enums.putAssumeCapacity(enumerant.name, {});
                    }

                    try out_set.commands.ensureUnusedCapacity(set.commands.count());
                    for (set.commands.keys()) |command| {
                        out_set.commands.putAssumeCapacity(command.name, {});
                    }
                }
            }
        }

        for (args.extensions.keys()) |required_ext_name| {
            const extension = registry.extensions.getKeyAdapted(required_ext_name, Registry.Extension.HashCtx.Adapted{}) orelse {
                log.err("The registry is missing the required extension '{s}'.", .{required_ext_name});
                return error.RegistryMissingRequiredExtension;
            };
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

                    try out_set.types.ensureUnusedCapacity(set.types.count());
                    for (set.types.keys()) |@"type"| {
                        out_set.types.putAssumeCapacity(@"type".name, {});
                    }

                    try out_set.enums.ensureUnusedCapacity(set.enums.count());
                    for (set.enums.keys()) |enumerant| {
                        out_set.enums.putAssumeCapacity(enumerant.name, {});
                    }

                    try out_set.commands.ensureUnusedCapacity(set.commands.count());
                    for (set.commands.keys()) |command| {
                        out_set.commands.putAssumeCapacity(command.name, {});
                    }
                }
            }
        }
    }

    const EnumerantCtx = struct {
        const KeyValue = Registry.EnumsSet.Enumerant;
        pub fn hash(ctx: @This(), key: *const KeyValue) u32 {
            _ = ctx;
            return Adapted.hash(.{}, key.name);
        }
        pub fn eql(ctx: @This(), a: *const KeyValue, b: *const KeyValue, b_index: usize) bool {
            _ = ctx;
            return Adapted.eql(.{}, a.name, b, b_index);
        }

        const Adapted = struct {
            pub fn hash(ctx: Adapted, key_name: []const u8) u32 {
                _ = ctx;
                return KeyValue.adaptedHashCtx().hash(key_name);
            }
            pub fn eql(ctx: Adapted, a_name: []const u8, b: *const KeyValue, b_index: usize) bool {
                _ = ctx;
                return KeyValue.adaptedHashCtx().eql(a_name, b.*, b_index);
            }
        };
    };
    const EnumerantSet = std.ArrayHashMap(*const Registry.EnumsSet.Enumerant, void, EnumerantCtx, true);

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
        for (enum_set.enumerants.keys()) |*enumerant| {
            if (!required_enums.contains(enumerant.name)) continue;
            try all_enums.putNoClobber(enumerant, {});

            var group_iter = std.mem.split(u8, enumerant.group orelse continue, ",");
            while (group_iter.next()) |group| {
                const gop_group = try enum_groups.getOrPut(group);
                if (!gop_group.found_existing) {
                    gop_group.value_ptr.* = .{};
                }

                const gop_enumerant = try gop_group.value_ptr.getOrPutAdapted(allocator, enumerant.name, EnumerantCtx.Adapted{});
                // there should be no two distinct enumerants that have the same 'name'.
                // If there is, the registry is probably messed up.
                assert(!gop_enumerant.found_existing);
                gop_enumerant.key_ptr.* = enumerant;
            }
        }
    }

    var lowercase_name_buf = std.ArrayList(u8).init(allocator);
    defer lowercase_name_buf.deinit();

    // don't need GLvoid
    _ = required_types.swapRemove("GLvoid");

    { // write enums
        for (all_enums.keys()) |enumerant| {
            assert(std.mem.startsWith(u8, enumerant.name, "GL_"));
            const zig_name = try util.lowerStringArrayList(&lowercase_name_buf, enumerant.name["GL_".len..]);

            try out.print("pub const {s} = {s};\n", .{ util.fmtGlobalZigId(zig_name), enumerant.value });
        }

        var grouped_iter = enum_groups.iterator();
        while (grouped_iter.next()) |entry| {
            const group_name = entry.key_ptr.*;

            try out.print("pub const {s} = enum(u32) {{\n", .{group_name});
            for (entry.value_ptr.keys()) |enumerant| {
                assert(std.mem.startsWith(u8, enumerant.name, "GL_"));
                const zig_name = try util.lowerStringArrayList(&lowercase_name_buf, enumerant.name["GL_".len..]);

                try out.print("    {s} = {s},\n", .{ std.zig.fmtId(zig_name), enumerant.value });
            }
            try out.writeAll("};\n");
        }
    }

    try out_writer_buffered.flush();
}

inline fn cIntTypeSignedness(c_type: std.Target.CType) ?std.builtin.Signedness {
    return switch (c_type) {
        .ushort, .uint, .ulong, .ulonglong => .unsigned,
        .short, .int, .long, .longlong => .signed,
        .float, .double, .longdouble => null,
    };
}

inline fn bitSizeToCIntType(
    target: std.Target,
    bit_size: u16,
    signedness: std.builtin.Signedness,
) ?std.Target.CType {
    for (comptime std.enums.values(std.Target.CType)) |c_type| {
        const c_type_signedness = cIntTypeSignedness(c_type) orelse continue; // non-integers don't count
        const c_type_bits = target.c_type_bit_size(c_type);

        if (c_type_signedness != signedness) continue;
        if (c_type_bits != bit_size) continue;
        return c_type;
    }
    return null;
}

inline fn fmtCType(c_type: std.Target.CType) FmtCType {
    return .{ .c_type = c_type };
}
const FmtCType = struct {
    c_type: std.Target.CType,

    pub fn format(
        self: FmtCType,
        comptime fmt_str: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = fmt_str;
        _ = options;
        try writer.writeAll(switch (self.c_type) {
            .short => "short",
            .ushort => "unsigned short",
            .int => "int",
            .uint => "unsigned",
            .long => "long",
            .ulong => "unsigned long",
            .longlong => "long long",
            .ulonglong => "unsigned long long",
            .float => "float",
            .double => "double",
            .longdouble => "long double",
        });
    }
};
