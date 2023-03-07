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

    // var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 16 }){};
    // defer _ = gpa.deinit();

    // const allocator = gpa.allocator();
    const allocator = std.heap.c_allocator;

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

    { // collect required stuff from target features and extension
        var removed_types = RequiredTypeCtx.ArrayHashMap(void, true).init(allocator);
        defer {
            for (removed_types.keys()) |@"type"|
                _ = required_types.swapRemove(@"type");
            removed_types.deinit();
        }

        var removed_enums = RequiredEnumCtx.ArrayHashMap(void, true).init(allocator);
        defer {
            for (removed_enums.keys()) |enumerant|
                _ = required_enums.swapRemove(enumerant);
            removed_enums.deinit();
        }

        var removed_commands = RequiredCmdCtx.ArrayHashMap(void, true).init(allocator);
        defer {
            for (removed_commands.keys()) |cmd|
                _ = required_commands.swapRemove(cmd);
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

    var lowercase_name_buf = std.ArrayList(u8).init(allocator);
    defer lowercase_name_buf.deinit();

    // don't need GLvoid
    _ = required_types.swapRemoveAdapted(@as([]const u8, "GLvoid"), RequiredTypeCtx.Adapted{ .inner = .{} });

    { // write types
        const target = builtin.target; // TODO: maybe make the target a build option through build.zig, separate from the native host?

        const c_scratch_file = try std.fs.cwd().createFile(args.c_scratch_file_path, .{});
        defer c_scratch_file.close();

        var c_scratch_buffered_writer = std.io.bufferedWriter(c_scratch_file.writer());
        const c_scratch_writer = c_scratch_buffered_writer.writer();

        const bits_per_byte = try std.math.divExact(u16, target.c_type_bit_size(.int), target.c_type_byte_size(.int));
        if (bits_per_byte != 8) {
            log.err("The target byte size is '{d}'.", .{bits_per_byte});
            return error.WeirdByteSize;
        }

        try c_scratch_writer.writeAll(
            \\typedef signed char int8_t;
            \\typedef unsigned char uint8_t;
            \\
        );

        for (1..4) |int_bit_size_log2| {
            for (comptime std.enums.values(std.builtin.Signedness)) |signedness| {
                const int_bit_size = @as(u16, 8) << @intCast(u2, int_bit_size_log2);
                const c_type = bitSizeToCIntType(target, int_bit_size, signedness) orelse continue;
                try c_scratch_writer.print("typedef {} {s}{d}_t;\n", .{
                    fmtCType(c_type),
                    switch (signedness) {
                        .signed => "int",
                        .unsigned => "uint",
                    },
                    target.c_type_bit_size(c_type),
                });
            }
        }

        try c_scratch_writer.print(
            \\typedef int{0d}_t intptr_t;
            \\typedef uint{0d}_t uintptr_t;
            \\
        , .{target.cpu.arch.ptrBitWidth()});
        try c_scratch_writer.writeAll("\n");

        // recognised khronos types
        const KhronosType = enum {
            khronos_int8_t,
            khronos_uint8_t,
            khronos_int16_t,
            khronos_uint16_t,
            khronos_int32_t,
            khronos_uint32_t,
            khronos_int64_t,
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
        };

        for (comptime std.enums.values(KhronosType)) |khronos_type| {
            try c_scratch_writer.print("#define {s} ", .{@tagName(khronos_type)});
            switch (khronos_type) {
                inline //
                .khronos_int8_t,
                .khronos_uint8_t,
                .khronos_int16_t,
                .khronos_uint16_t,
                .khronos_int32_t,
                .khronos_uint32_t,
                .khronos_int64_t,
                .khronos_uint64_t,
                .khronos_intptr_t,
                .khronos_uintptr_t,
                => |tag| try c_scratch_writer.writeAll(@tagName(tag)["khronos_".len..]),

                .khronos_ssize_t => try c_scratch_writer.print("int{d}_t", .{target.cpu.arch.ptrBitWidth()}), // NOTE: Zig assumes pointer bits == size bits,
                .khronos_usize_t => try c_scratch_writer.print("uint{d}_t", .{target.cpu.arch.ptrBitWidth()}), // and that's probably true for most targets that support OpenGL
                .khronos_float_t => {
                    assert(target.c_type_bit_size(.float) == 32); // TODO: Maybe add logic for this?
                    try c_scratch_writer.writeAll("float");
                },
            }
            try c_scratch_writer.writeAll("\n");
        }
        try c_scratch_writer.writeAll("\n");

        for (registry.types) |type_entry| {
            if (!required_types.containsAdapted(type_entry.name, RequiredTypeCtx.Adapted{ .inner = .{} })) continue;
            if (type_entry.type_def.apientry_indices.len() != 0) {
                log.err("Required type '{s}' contains API entry/entries - unhandled.\n", .{type_entry.name});
                continue;
            }

            try c_scratch_writer.writeAll(type_entry.type_def.text);
            try c_scratch_writer.writeAll("\n");
        }

        try c_scratch_buffered_writer.flush();

        const translated_c: [:0]const u8 = blk: {
            const exec_result = try std.ChildProcess.exec(.{
                .allocator = allocator,
                .argv = &[_][]const u8{ args.zig_exe_path, "translate-c", args.c_scratch_file_path },
            });
            defer allocator.free(exec_result.stderr);
            errdefer allocator.free(exec_result.stdout);

            var translated_c = try allocator.realloc(exec_result.stdout, exec_result.stdout.len + 1);
            translated_c[translated_c.len - 1] = 0;
            break :blk translated_c[0 .. translated_c.len - 1 :0];
        };
        defer allocator.free(translated_c);

        var zig_ast = try std.zig.Ast.parse(allocator, translated_c, .zig);
        defer zig_ast.deinit(allocator);

        const tokens_tags: []const std.zig.Token.Tag = zig_ast.tokens.items(.tag);
        for (zig_ast.rootDecls()) |root_decl_index| {
            const decl_info = zig_ast.fullVarDecl(root_decl_index) orelse continue;
            const first_tok_index = decl_info.firstToken();
            assert(tokens_tags[first_tok_index] == .keyword_pub); // this is a translated C file, all the decls should be `pub`.
            if (tokens_tags[first_tok_index + 1] != .keyword_const) continue;
            if (tokens_tags[first_tok_index + 2] != .identifier) continue;
            if (tokens_tags[first_tok_index + 3] != .equal) continue;

            const ident: []const u8 = zig_ast.tokenSlice(first_tok_index + 2);
            if (!required_types.containsAdapted(ident, RequiredTypeCtx.Adapted{ .inner = .{} })) continue;
            const type_entry: Registry.TypeEntry = for (registry.types) |type_entry| {
                if (std.mem.eql(u8, ident, type_entry.name)) break type_entry;
            } else unreachable;

            const def_node_index = decl_info.ast.init_node;
            const def_node_first_tok = first_tok_index + 4;
            const def_node_last_tok = zig_ast.lastToken(def_node_index);
            assert(def_node_first_tok == zig_ast.firstToken(def_node_index));

            if (type_entry.comment) |comment| {
                var comment_line_iter = std.mem.split(u8, comment, &std.ascii.whitespace);
                while (comment_line_iter.next()) |comment_line| {
                    try out.print("/// {s}\n", .{comment_line});
                }
            }

            try out.print("pub const {s} =", .{ident});
            var i = def_node_first_tok;
            while (i <= def_node_last_tok) : (i += 1) {
                const tok_slice = zig_ast.tokenSlice(i);
                try out.print(" {s}", .{tok_slice});
            }
            try out.writeAll(";\n");
        }
        try out.writeAll("\n");
    }

    { // write enums
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
