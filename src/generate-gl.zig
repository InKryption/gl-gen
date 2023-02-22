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

    const Args = struct {
        output_file: std.fs.File,
        gl_xml_file: std.fs.File,
        api_version: gl_targets.Version,
        api_profile: gl_targets.Profile,
        extensions: []const []const u8,
    };
    const args: Args = args: {
        var args_iter = try std.process.argsWithAllocator(allocator);
        defer args_iter.deinit();

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
                .@"api-version" => api_version = std.meta.stringToEnum(gl_targets.Version, arg_val) orelse return stderr.writeAll(comptime err_msg: {
                    var err_str: []const u8 = "Expected api-version to be the target OpenGL API version. Should be one of:\n";
                    for (@typeInfo(gl_targets.Version).Enum.fields) |field| err_str = err_str ++ "  " ++ field.name ++ ",\n";
                    break :err_msg err_str;
                }),
                .@"api-profile" => api_profile = std.meta.stringToEnum(gl_targets.Profile, arg_val) orelse return stderr.writeAll(comptime err_msg: {
                    var err_str: []const u8 = "Expected api-profile to be the target OpenGL API version. Should be one of:\n";
                    for (@typeInfo(gl_targets.Version).Enum.fields) |field| err_str = err_str ++ "  " ++ field.name ++ ",\n";
                    break :err_msg err_str;
                }),
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

        break :args Args{
            .output_file = output_file,
            .gl_xml_file = gl_xml_file,
            .api_version = api_version.?,
            .api_profile = api_profile.?,
            .extensions = extensions,
        };
    };
    defer {
        for (args.extensions) |ext| allocator.free(ext);
        allocator.free(args.extensions);
        args.output_file.close();
        args.gl_xml_file.close();
    }

    const api_version: gl_targets.Version = args.api_version;
    const api_profile: gl_targets.Profile = args.api_profile;

    const tree: xml.Tree = tree: {
        var buffered_gl_xml = std.io.bufferedReaderSize(4096, args.gl_xml_file.reader());
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

    { // check for groups element, but ignore it.
        const elem_name = "groups";

        const elem_index = tree.root.getChildElementIndexPos(elem_name, 0) orelse {
            try stderr.print("Missing element '{s}'.\n", .{elem_name});
            return error.MissingTypesElement;
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

    { // iterate feature elements
        var feature_group_elem_iter = tree.root.childElementIterator("feature");
        const target_name = api_version.stringWithGlPrefix();

        var removed_enums = std.StringHashMap(void).init(allocator);
        defer {
            var removed_iter = removed_enums.keyIterator();
            while (removed_iter.next()) |removed_enum| {
                _ = required_enums.remove(removed_enum.*);
            }
            removed_enums.deinit();
        }

        var removed_commands = std.StringHashMap(void).init(allocator);
        defer {
            var removed_iter = removed_commands.keyIterator();
            while (removed_iter.next()) |removed_command| {
                _ = required_commands.remove(removed_command.*);
            }
            removed_commands.deinit();
        }

        var removed_types = std.StringHashMap(void).init(allocator);
        defer {
            var removed_iter = removed_types.keyIterator();
            while (removed_iter.next()) |removed_type| {
                _ = required_types.remove(removed_type.*);
            }
            removed_types.deinit();
        }

        const target_feature_element: xml.Element = while (feature_group_elem_iter.next()) |feature_elem| {
            const feature_elem_index = feature_group_elem_iter.index - 1;
            const name_val = feature_elem.getAttributeValue("name") orelse {
                try stderr.print("Feature element index {d} missing name.\n", .{feature_elem_index});
                continue;
            };
            if (std.mem.eql(u8, target_name, name_val)) {
                break feature_elem;
            }
        } else {
            try stderr.print("Registry contains no feature element with a 'name' attribute of value '{s}'.\n", .{target_name});
            return error.MissingRequestedFeature;
        };

        const target_api_val: []const u8 = target_feature_element.getAttributeValue("api") orelse {
            try stderr.print("Feature element of name '{s}' missing 'api' attribute.\n", .{target_name});
            return error.MissingRequestedFeatureApi;
        };

        const target_number_val: []const u8 = target_feature_element.getAttributeValue("number") orelse {
            try stderr.print("Feature element of name '{s}' missing 'number' attribute.\n", .{target_name});
            return error.MissingRequestedFeatureNumber;
        };

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
            try stderr.print("Failed to parse number attribute of feature with name '{s}'.", .{target_name});
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
                try stderr.print("Failed to parse number attribute of feature with name '{s}' (parsing error: {s}).", .{ target_name, @errorName(err) });
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
                const FeatureSetType = enum { require, remove };
                const feature_set_type = std.meta.stringToEnum(FeatureSetType, feature_set_elem.name) orelse {
                    try stderr.print(
                        "Encountered unrecognized feature set type '{s}' in feature element index {d}.\n",
                        .{ feature_set_elem.name, feature_group_elem_index },
                    );
                    continue;
                };

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
                    const FeatureRefType = enum { command, type, @"enum" };
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

    { // iterate the many "enums" elements
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
                    .comment => continue,
                    .text => |text| {
                        try stderr.print("Encountered unexpected text in enums element index {d}. Text: '{s}'.\n", .{ elem_index, text });
                        continue;
                    },
                    .element => |elem| elem,
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
                    try stderr.print("Failed to parse value of enum with name '{s}'.\n", .{name_str});
                    return err;
                };

                try out.print("pub const {s} = {s};\n", .{ name_str, value_str });
            }
        }
    }

    try out_writer_buffered.flush();
}
