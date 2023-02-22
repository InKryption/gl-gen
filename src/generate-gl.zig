const std = @import("std");
const xml = @import("xml.zig");
const util = @import("util.zig");
const gl_targets = @import("opengl-targets.zig");
const assert = std.debug.assert;

pub fn main() !void {
    const allocator = std.heap.c_allocator;

    var stderr_buffered = std.io.bufferedWriter(std.io.getStdErr().writer());
    defer {
        var retries: u32 = 0;
        while (retries < 3) : (retries += 1) {
            stderr_buffered.flush() catch continue;
            break;
        }
    }
    const stderr = stderr_buffered.writer();

    const args: GenerationArgs = args: {
        var args_iter = try std.process.argsWithAllocator(allocator);
        defer args_iter.deinit();
        break :args try GenerationArgs.parse(allocator, &args_iter, stderr);
    };
    defer args.deinit(allocator);

    const api_version: gl_targets.Version = args.api_version;
    const api_profile: gl_targets.Profile = args.api_profile;

    const tree: xml.Tree = tree: {
        var buffered_gl_xml = std.io.bufferedReaderSize(32_768, args.gl_xml_file.reader());
        const gl_xml_reader = buffered_gl_xml.reader();

        var error_line: u64 = undefined;
        var error_column: u64 = undefined;

        break :tree xml.parse(allocator, gl_xml_reader, .{
            .line = &error_line,
            .column = &error_column,
            .discard_comments = true,
            .discard_whitespace = true,
        }) catch |err| {
            try stderr.print("Parsing error encountered at {d}:{d} (line:column) of the registry.\n", .{ error_line + 1, error_column + 1 });
            return err;
        };
    };
    defer tree.deinit(allocator);

    var out_writer_buffered = std.io.bufferedWriter(args.output_file.writer());
    const out = out_writer_buffered.writer();

    var must_flush_out_writer_buffered = true;
    defer assert(!must_flush_out_writer_buffered or out_writer_buffered.end == 0);
    errdefer must_flush_out_writer_buffered = false;

    if (!std.mem.eql(u8, tree.root.name, "registry")) {
        try stderr.print("Expected root element to be 'registry', found '{s}'.\n", .{tree.root.name});
        return error.InvalidRootElement;
    }

    // top level doc comment describing generation parameters
    try out.print(
        \\//!
        \\//! Generation parameters:
        \\//! * API: {s}
        \\//! * Profile: {s}
        \\//! * Extensions: 
    , .{
        api_version.stringWithGlPrefix(),
        @tagName(api_profile),
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

    { // write the comment element
        const name = "comment";
        const elem_index: usize = tree.root.getChildElementIndexPos(name, 0) orelse {
            try stderr.print("Missing element '{s}'.\n", .{name});
            return error.MissingCommentElement;
        };
        if (tree.root.getChildElementIndexPos(name, elem_index + 1) != null) {
            try stderr.print("Too many elements with name '{s}'.\n", .{name});
            return error.TooManyCommentElements;
        }
        const element = tree.root.children[elem_index].element;

        if (element.children.len != 1 or element.children[0] != .text) {
            try stderr.print("Expected only text in {s} element.\n", .{name});
            return error.TooManyCommentElementChildren;
        }

        try out.writeAll(
            \\//! Registry comment:
            \\
        );
        var comment_line_iter = std.mem.split(u8, std.mem.trim(u8, element.children[0].text, &.{ '\n', ' ' }), "\n");
        var longest_line_bytes: u32 = 0;
        while (comment_line_iter.next()) |line| {
            longest_line_bytes = @max(@intCast(u32, line.len), longest_line_bytes);
        }

        try out.writeAll("//! ");
        try out.writeByteNTimes('-', longest_line_bytes);
        try out.writeByte('\n');
        comment_line_iter.reset();
        while (comment_line_iter.next()) |line| {
            try out.print("//! {s}\n", .{line});
        }
        try out.writeAll("//! ");
        try out.writeByteNTimes('-', longest_line_bytes);
        try out.writeAll(
            \\
            \\//!
            \\
        );
    }

    try out.writeAll(
        \\const gl = @This();
        \\
        \\pub const Enum = u32;
        \\
        \\
    );

    { // opengl type definitions

        const elem_name = "types";

        const elem_index = tree.root.getChildElementIndexPos(elem_name, 0) orelse {
            try stderr.print("Missing element '{s}'.\n", .{elem_name});
            return error.MissingTypesElement;
        };
        if (tree.root.getChildElementIndexPos(elem_name, elem_index + 1) != null) {
            try stderr.print("Too many elements with name '{s}'.\n", .{elem_name});
            return error.TooManyTypesElements;
        }
    }

    blk: { // check for groups element, but ignore it.
        const elem_name = "groups";

        const elem_index = tree.root.getChildElementIndexPos(elem_name, 0) orelse {
            try stderr.print("Missing element '{s}'.\n", .{elem_name});
            break :blk;
        };
        if (tree.root.getChildElementIndexPos(elem_name, elem_index + 1) != null) {
            try stderr.print("Too many elements with name '{s}'.\n", .{elem_name});
            return error.TooManyTypesElements;
        }
    }

    var required_enums = std.StringHashMap(void).init(allocator);
    defer required_enums.deinit();

    var required_commands = std.StringHashMap(void).init(allocator);
    defer required_commands.deinit();

    var required_types = std.StringHashMap(void).init(allocator);
    defer required_types.deinit();

    { // iterate feature & extension elements
        var removed_enums = std.StringHashMap(void).init(allocator);
        defer {
            var iter = removed_enums.keyIterator();
            while (iter.next()) |removed| {
                _ = required_enums.remove(removed.*);
            }
            removed_enums.deinit();
        }

        var removed_commands = std.StringHashMap(void).init(allocator);
        defer {
            var iter = removed_commands.keyIterator();
            while (iter.next()) |removed| {
                _ = required_commands.remove(removed.*);
            }
            removed_commands.deinit();
        }

        var removed_types = std.StringHashMap(void).init(allocator);
        defer {
            var iter = removed_types.keyIterator();
            while (iter.next()) |removed| {
                _ = required_types.remove(removed.*);
            }
            removed_types.deinit();
        }

        // iterate feature elements
        var feature_group_elem_iter = tree.root.childElementIterator("feature");
        const target_feature_element: xml.Element = blk: {
            while (feature_group_elem_iter.next()) |feature_elem| {
                const feature_elem_index = feature_group_elem_iter.index - 1;
                const name_val = feature_elem.getAttributeValue("name") orelse {
                    try stderr.print("Feature element index {d} missing name.\n", .{feature_elem_index});
                    return error.FeatureMissingNameAttribute;
                };
                if (std.mem.eql(u8, api_version.stringWithGlPrefix(), name_val)) {
                    break :blk feature_elem;
                }
            }
            try stderr.print("Registry contains no feature element with a 'name' attribute of value '{s}'.\n", .{api_version.stringWithGlPrefix()});
            return error.MissingRequestedFeature;
        };

        const target_api_val: []const u8 = target_feature_element.getAttributeValue("api") orelse {
            try stderr.print("Feature element of name '{s}' missing 'api' attribute.\n", .{api_version.stringWithGlPrefix()});
            return error.MissingRequestedFeatureApi;
        };

        const target_number_val: []const u8 = target_feature_element.getAttributeValue("number") orelse {
            try stderr.print("Feature element of name '{s}' missing 'number' attribute.\n", .{api_version.stringWithGlPrefix()});
            return error.MissingRequestedFeatureNumber;
        };

        const FeatureSetType = enum { require, remove };
        const FeatureRefType = enum { command, type, @"enum" };

        const Number = struct {
            major: u8,
            minor: u8,

            inline fn compare(a: @This(), b: @This()) std.math.Order {
                return switch (std.math.order(a.major, b.major)) {
                    .eq => std.math.order(a.minor, b.minor),
                    .lt => .lt,
                    .gt => .gt,
                };
            }

            inline fn parseNumber(str: []const u8) !@This() {
                var iter = std.mem.split(u8, str, ".");
                const major_str = try (iter.next() orelse error.EmptyString);
                const minor_str = try (iter.next() orelse error.MissingMinorVersion);
                return @This(){
                    .major = try std.fmt.parseUnsigned(u8, major_str, 10),
                    .minor = try std.fmt.parseUnsigned(u8, minor_str, 10),
                };
            }
        };
        const target_number: Number = Number.parseNumber(target_number_val) catch |err| {
            try stderr.print("Failed to parse number attribute of feature with name '{s}'.", .{api_version.stringWithGlPrefix()});
            return err;
        };

        feature_group_elem_iter.reset();
        while (feature_group_elem_iter.next()) |feature_group_elem| {
            const feature_group_elem_index = feature_group_elem_iter.index - 1;
            const api_val: []const u8 = feature_group_elem.getAttributeValue("api") orelse {
                try stderr.print("Feature element index {d} missing api attribute.\n", .{feature_group_elem_index});
                continue;
            };
            const name_val: []const u8 = feature_group_elem.getAttributeValue("name") orelse {
                try stderr.print("Feature element index {d} missing name attribute.\n", .{feature_group_elem_index});
                continue;
            };
            const number_val: []const u8 = feature_group_elem.getAttributeValue("number") orelse {
                try stderr.print("Feature element index {d} missing number attribute.\n", .{feature_group_elem_index});
                continue;
            };

            if (!std.mem.eql(u8, api_val, target_api_val)) continue;
            const number = Number.parseNumber(number_val) catch |err| {
                try stderr.print("Failed to parse number attribute of feature with name '{s}' (parsing error: {s}).", .{ api_version.stringWithGlPrefix(), @errorName(err) });
                continue;
            };
            switch (number.compare(target_number)) {
                .gt => continue,
                .lt, .eq => {},
            }

            for (feature_group_elem.children) |feature_group_child| {
                const feature_set_elem: xml.Element = switch (feature_group_child) {
                    .element => |elem| elem,
                    .comment => continue,
                    .text => {
                        try stderr.print("Encountered unexpected text in feature feature element index {d}.\n", .{feature_group_elem_index});
                        continue;
                    },
                };
                const feature_set_type: FeatureSetType = std.meta.stringToEnum(FeatureSetType, feature_set_elem.name) orelse {
                    try stderr.print(
                        "Encountered unrecognized feature set type '{s}' in feature element index {d}.\n",
                        .{ feature_set_elem.name, feature_group_elem_index },
                    );
                    continue;
                };

                // some 'require'/'remove' elements have a 'profile' element.
                // none that I've seen have an 'api' element, unlike in the case of 'extension'
                // elements, dealt with further down, which is the main reason this might
                // seem duplicated.
                if (feature_set_elem.getAttributeValue("profile")) |feature_set_profile_str| {
                    const feature_set_profile = std.meta.stringToEnum(gl_targets.Profile, feature_set_profile_str) orelse {
                        try stderr.print("Feature set in group '{s}' has unrecognised profile '{s}'.\n", .{ name_val, feature_set_profile_str });
                        continue;
                    };
                    if (feature_set_profile != api_profile) {
                        continue;
                    }
                }

                for (feature_set_elem.children) |feature_set_child| {
                    const feature_ref_elem: xml.Element = switch (feature_set_child) {
                        .element => |elem| elem,
                        .comment => continue,
                        .text => |text| {
                            try stderr.print("Encountered unexpected text in feature set. Unexpected text: '{s}'.\n", .{text});
                            continue;
                        },
                    };
                    const feature_ref_type = std.meta.stringToEnum(FeatureRefType, feature_ref_elem.name) orelse {
                        try stderr.print("Encountered unrecognized feature ref type '{s}'.\n", .{feature_ref_elem.name});
                        continue;
                    };
                    const feature_ref_name = feature_ref_elem.getAttributeValue("name") orelse {
                        try stderr.print("Missing 'name' attribute in feature reference.\n", .{});
                        continue;
                    };

                    switch (feature_ref_type) {
                        .command => switch (feature_set_type) {
                            .require => try required_commands.put(feature_ref_name, {}),
                            .remove => try removed_commands.put(feature_ref_name, {}),
                        },
                        .type => switch (feature_set_type) {
                            .require => try required_commands.put(feature_ref_name, {}),
                            .remove => try removed_commands.put(feature_ref_name, {}),
                        },
                        .@"enum" => switch (feature_set_type) {
                            .require => try required_enums.put(feature_ref_name, {}),
                            .remove => try removed_enums.put(feature_ref_name, {}),
                        },
                    }
                }
            }
        }

        // iterate the extension elements inside the "extension" element
        // get requested extensions as a set for quick lookup.
        const requested_extension_set: std.StringHashMap(void) = blk: {
            var requested_extension_set = std.StringHashMap(void).init(allocator);
            try requested_extension_set.ensureTotalCapacity(std.math.lossyCast(u32, args.extensions.len));
            for (args.extensions) |ext| {
                const gop = try requested_extension_set.getOrPut(ext);
                if (gop.found_existing)
                    try stderr.print("Extension '{s}' specified multiple times.\n", .{ext});
            }
            break :blk requested_extension_set;
        };
        defer {
            var copy = requested_extension_set;
            copy.deinit();
        }

        const extensions_element: xml.Element = blk: {
            const extensions_element_index = tree.root.getChildElementIndexPos("extensions", 0) orelse {
                try stderr.writeAll("Missing element 'extensions'.\n");
                return error.MissingExtensionsElement;
            };
            if (tree.root.getChildElementIndexPos("extensions", extensions_element_index + 1) != null) {
                try stderr.writeAll("Found more than one 'extensions' element.");
                return error.TooManyExtensionsElements;
            }
            break :blk tree.root.children[extensions_element_index].element;
        };

        for (extensions_element.children, 0..) |extensions_child, extensions_child_index| {
            const ext_elem: xml.Element = switch (extensions_child) {
                .comment => continue,
                .text => |text| {
                    try stderr.print("Unexpected text found in extension element. Unexpected text: '{s}'.\n", .{text});
                    continue;
                },
                .element => |elem| blk: {
                    if (std.mem.eql(u8, elem.name, "extension")) break :blk elem;
                    try stderr.print("Unexpected non-extension child element in extensions element '{s}'.", .{elem.name});
                    continue;
                },
            };
            const ext_name = ext_elem.getAttributeValue("name") orelse {
                try stderr.print("Extension element index {d} missing 'name' attribute.\n", .{extensions_child_index});
                continue;
            };
            if (!requested_extension_set.contains(ext_name)) continue;

            { // check whether the extension supports/is supported by the target API.
                const ext_supported_apis_str = ext_elem.getAttributeValue("supported") orelse {
                    try stderr.print("Extension element '{s}' missing 'supported' attribute.\n", .{ext_name});
                    continue;
                };
                var ext_supported_apis_iter = std.mem.split(u8, ext_supported_apis_str, "|");
                while (ext_supported_apis_iter.next()) |supported_api| {
                    if (std.mem.eql(u8, target_api_val, supported_api)) break;
                } else {
                    try stderr.print(
                        "Requested extension '{s}' does not support target API '{s}' (specified by '{s}').\n",
                        .{ ext_name, target_api_val, api_version.stringWithGlPrefix() },
                    );
                    continue;
                }
            }

            for (ext_elem.children) |ext_elem_child| {
                const feature_set_elem: xml.Element = switch (ext_elem_child) {
                    .element => |elem| elem,
                    .comment => continue,
                    .text => |text| {
                        try stderr.print("Unexpected text in extension element '{s}'. Unexpected text: '{s}'.\n", .{ ext_name, text });
                        continue;
                    },
                };
                const feature_set_type: FeatureSetType = std.meta.stringToEnum(FeatureSetType, feature_set_elem.name) orelse {
                    try stderr.print("Encountered unrecognized feature set type '{s}' in extension element '{s}'.\n", .{ feature_set_elem.name, ext_name });
                    continue;
                };

                if (feature_set_elem.getAttributeValue("api")) |feature_set_api_str| {
                    if (!std.mem.eql(u8, target_api_val, feature_set_api_str)) continue;
                }
                if (feature_set_elem.getAttributeValue("profile")) |feature_set_profile_str| {
                    const feature_set_profile = std.meta.stringToEnum(gl_targets.Profile, feature_set_profile_str) orelse {
                        try stderr.print(
                            "Feature set in extension '{s}' has unrecognized profile type '{s}'.\n",
                            .{ ext_name, feature_set_profile_str },
                        );
                        continue;
                    };
                    if (feature_set_profile != api_profile) continue;
                }

                for (feature_set_elem.children) |feature_set_child| {
                    const feature_ref_elem: xml.Element = switch (feature_set_child) {
                        .element => |elem| elem,
                        .comment => continue,
                        .text => |text| {
                            try stderr.print("Encountered unexpected text in feature set. Unexpected text: '{s}'.\n", .{text});
                            continue;
                        },
                    };
                    const feature_ref_type = std.meta.stringToEnum(FeatureRefType, feature_ref_elem.name) orelse {
                        try stderr.print("Encountered unrecognized feature ref type '{s}'.\n", .{feature_ref_elem.name});
                        continue;
                    };
                    const feature_ref_name = feature_ref_elem.getAttributeValue("name") orelse {
                        try stderr.print("Missing 'name' attribute in feature reference.\n", .{});
                        continue;
                    };

                    switch (feature_ref_type) {
                        .command => switch (feature_set_type) {
                            .require => try required_commands.put(feature_ref_name, {}),
                            .remove => try removed_commands.put(feature_ref_name, {}),
                        },
                        .type => switch (feature_set_type) {
                            .require => try required_commands.put(feature_ref_name, {}),
                            .remove => try removed_commands.put(feature_ref_name, {}),
                        },
                        .@"enum" => switch (feature_set_type) {
                            .require => try required_enums.put(feature_ref_name, {}),
                            .remove => try removed_enums.put(feature_ref_name, {}),
                        },
                    }
                }
            }
        }
    }

    // represents the (name)-(integer value) mapping of enum values,
    // except with the integers still in string form, so as to note lose
    // information, like the base it was written in.
    var enum_values = std.StringHashMap([]const u8).init(allocator);
    defer enum_values.deinit();

    var enum_groups = std.StringHashMap([]const []const u8).init(allocator);
    defer {
        var iter = enum_groups.valueIterator(); // only free the slices of strings, and not the strings themselves (owned by the XML tree).
        while (iter.next()) |slice| allocator.free(slice.*);
        enum_groups.deinit();
    }

    {
        var growable_enum_groups = std.StringHashMap(std.ArrayListUnmanaged([]const u8)).init(allocator);
        defer {
            var iter = growable_enum_groups.valueIterator();
            while (iter.next()) |arraylist| arraylist.deinit(allocator);
            growable_enum_groups.deinit();
        }

        var enums_elem_iter = tree.root.childElementIterator("enums");
        var elem_index: usize = 0;
        while (enums_elem_iter.next()) |enum_collection_element| : (elem_index += 1) {
            const enum_collection_namespace = enum_collection_element.getAttribute("namespace") orelse {
                try stderr.print("Enums element index {d} missing 'namespace' attribute.\n", .{elem_index});
                continue;
            };
            _ = enum_collection_namespace;

            for (enum_collection_element.children) |child| {
                const enum_element: xml.Element = switch (child) {
                    .element => |elem| elem,
                    .comment => continue,
                    .text => |text| {
                        try stderr.print("Encountered unexpected text in enums element index {d}. Text: '{s}'.\n", .{ elem_index, text });
                        continue;
                    },
                };
                if (enum_element.children.len != 0) {
                    try stderr.print("Enum contains unexpected children in enums element index {d}.\n", .{elem_index});
                }
                if (std.mem.eql(u8, enum_element.name, "unused")) {
                    continue;
                }

                const value_str = enum_element.getAttributeValue("value") orelse {
                    try stderr.print("Enum missing value attribute in enums element index {d}.\n", .{elem_index});
                    return error.MalformedEnum;
                };
                const name_str = enum_element.getAttributeValue("name") orelse {
                    try stderr.print("Enum missing name attribute in enums element index {d}.\n", .{elem_index});
                    return error.MalformedEnum;
                };

                if (!required_enums.contains(name_str)) continue;
                _ = std.fmt.parseInt(i128, value_str, 0) catch |err| {
                    try stderr.print("Failed to parse value of enum with name '{s}' (value string: '{s}').\n", .{ name_str, value_str });
                    return err;
                };
                try enum_values.putNoClobber(name_str, value_str);

                const enum_groups_str = enum_element.getAttributeValue("group") orelse {
                    const lower_cased_name = try std.ascii.allocLowerString(allocator, name_str["GL_".len..]);
                    defer allocator.free(lower_cased_name);
                    try out.print("pub const {s} = {s};\n", .{ std.zig.fmtId(lower_cased_name), value_str });
                    continue;
                };

                var enum_group_iter = std.mem.split(u8, enum_groups_str, ",");
                while (enum_group_iter.next()) |enum_group_name| {
                    const gop = try growable_enum_groups.getOrPut(enum_group_name);
                    if (!gop.found_existing) {
                        gop.value_ptr.* = .{};
                    }
                    try gop.value_ptr.append(allocator, name_str);
                }
            }
        }

        var growable_egs_iter = growable_enum_groups.iterator();
        while (growable_egs_iter.next()) |entry| {
            const SortNamesByValueContext = struct {
                enum_names: [][]const u8,
                enum_values: *const std.StringHashMap([]const u8),

                pub inline fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                    const a_val_str = ctx.enum_values.get(ctx.enum_names[a]).?;
                    const b_val_str = ctx.enum_values.get(ctx.enum_names[b]).?;

                    const a_val = std.fmt.parseInt(i128, a_val_str, 0) catch unreachable;
                    const b_val = std.fmt.parseInt(i128, b_val_str, 0) catch unreachable;

                    return a_val < b_val;
                }
                pub inline fn swap(ctx: @This(), a: usize, b: usize) void {
                    std.mem.swap([]const u8, &ctx.enum_names[a], &ctx.enum_names[b]);
                }
            };

            const enum_names_slice: [][]const u8 = try entry.value_ptr.toOwnedSlice(allocator);
            std.sort.sortContext(enum_names_slice.len, SortNamesByValueContext{
                .enum_names = enum_names_slice,
                .enum_values = &enum_values,
            });
            try enum_groups.putNoClobber(entry.key_ptr.*, enum_names_slice);
        }
    }

    var group_iter = enum_groups.iterator();
    while (group_iter.next()) |entry| {
        try out.print("pub const {s} = enum(gl.Enum) {{\n", .{entry.key_ptr.*});
        for (entry.value_ptr.*) |name| {
            const value = enum_values.get(name).?;

            const lower_cased_name = try std.ascii.allocLowerString(allocator, name["GL_".len..]);
            defer allocator.free(lower_cased_name);

            try out.print("    {s} = {s},\n", .{ std.zig.fmtId(lower_cased_name), value });
        }
        try out.writeAll("};\n");
    }

    try out_writer_buffered.flush();
}

const GenerationArgs = struct {
    output_file: std.fs.File,
    gl_xml_file: std.fs.File,
    api_version: gl_targets.Version,
    api_profile: gl_targets.Profile,
    extensions: []const []const u8,

    fn deinit(args: GenerationArgs, allocator: std.mem.Allocator) void {
        for (args.extensions) |ext| allocator.free(ext);
        allocator.free(args.extensions);
        args.output_file.close();
        args.gl_xml_file.close();
    }

    fn parse(
        allocator: std.mem.Allocator,
        args_iter: *std.process.ArgIterator,
        stderr: anytype,
    ) !GenerationArgs {
        if (!args_iter.skip()) @panic("Tried to skip argv[0] (executable path), but argv is empty.\n");

        var output_file_path: ?[]u8 = null;
        defer allocator.free(output_file_path orelse "");

        var gl_xml_file_path: ?[]u8 = null;
        defer allocator.free(gl_xml_file_path orelse "");

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
                    try stderr.print("Expected argument name to be preceeded by '--'.\n", .{});
                    continue;
                }
                const tag = std.meta.stringToEnum(ArgName, arg_start["--".len..]) orelse {
                    try stderr.print("Unrecognised argument name '{s}'. Valid argument names are:\n", .{arg_start["--".len..]});
                    inline for (@typeInfo(ArgName).Enum.fields) |field| try stderr.print("  \"{s}\",\n", .{field.name});
                    continue;
                };

                break :blk tag;
            };
            const arg_val: []const u8 = if (args_iter.next()) |arg_val| std.mem.trim(u8, arg_val, &whitespace_chars) else {
                try stderr.writeAll("Expected argument key value pairs of the form '--<name> <value>'.\n");
                break;
            };

            if (present.contains(arg_name))
                try stderr.print("Specified '{s}' more than once.\n", .{@tagName(arg_name)});
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
                    try stderr.writeAll(comptime err_msg: {
                        var err_str: []const u8 = "Expected api-version to be the target OpenGL API version. Should be one of:\n";
                        for (@typeInfo(gl_targets.Version).Enum.fields) |field| err_str = err_str ++ "  " ++ field.name ++ ",\n";
                        break :err_msg err_str;
                    });
                    return error.UnrecognisedApiVersion;
                },
                .@"api-profile" => api_profile = std.meta.stringToEnum(gl_targets.Profile, arg_val) orelse {
                    try stderr.writeAll(comptime err_msg: {
                        var err_str: []const u8 = "Expected api-profile to be the target OpenGL API version. Should be one of:\n";
                        for (@typeInfo(gl_targets.Version).Enum.fields) |field| err_str = err_str ++ "  " ++ field.name ++ ",\n";
                        break :err_msg err_str;
                    });
                    return error.UnrecognisedApiProfile;
                },
            }
        }

        { // check if any arguments are missing
            const missing = present.complement();
            var missing_iter = missing.iterator();
            while (missing_iter.next()) |missing_tag|
                try stderr.print("Missing argument '{s}'.\n", .{@tagName(missing_tag)});
            if (missing.count() != 0) return error.MissingArguments;
        }

        const output_file = std.fs.cwd().createFile(output_file_path.?, .{}) catch |err| {
            try stderr.print("Failed to create/open output file '{s}'.\n", .{output_file_path.?});
            return err;
        };
        errdefer output_file.close();

        const gl_xml_file = std.fs.cwd().openFile(gl_xml_file_path.?, .{}) catch |err| {
            try stderr.print("Failed to open registry file '{s}'.\n", .{gl_xml_file_path.?});
            return err;
        };
        errdefer gl_xml_file.close();

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
            .output_file = output_file,
            .gl_xml_file = gl_xml_file,
            .api_version = api_version.?,
            .api_profile = api_profile.?,
            .extensions = extensions,
        };
    }
};
