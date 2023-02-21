const std = @import("std");
const xml = @import("xml.zig");
const util = @import("util.zig");
const build_options = @import("build-options");
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
        extensions: []const []const u8,
    };
    const args: Args = args: {
        var output_file: ?std.fs.File = null;
        var gl_xml_file: ?std.fs.File = null;
        var extensions: ?[]const []const u8 = null;

        var args_iter = try std.process.argsWithAllocator(allocator);
        defer args_iter.deinit();

        if (!args_iter.skip()) @panic("Tried to skip argv[0] (executable path), but argv is empty.\n");

        if (args_iter.next()) |output_file_path| {
            output_file = std.fs.cwd().createFile(output_file_path, .{}) catch |err| {
                try stderr.print("Failed to create/open output file '{s}'. Error: '{s}'.\n", .{ output_file_path, @errorName(err) });
                return;
            };
        } else {
            try stderr.writeAll("Expected first argument to be a path to write the bindings to.\n");
            return;
        }
        errdefer output_file.?.close();

        if (args_iter.next()) |gl_xml_file_path| {
            gl_xml_file = std.fs.cwd().openFile(gl_xml_file_path, std.fs.File.OpenFlags{}) catch |err| {
                try stderr.print("Failed to open xml registry '{s}'. Error: '{s}'.\n", .{ gl_xml_file_path, @errorName(err) });
                return;
            };
        } else {
            try stderr.writeAll("Expected third argument to be a path to 'gl.xml'.\n");
            return;
        }
        errdefer gl_xml_file.?.close();

        var extensions_list = std.ArrayList([]const u8).init(allocator);
        defer extensions_list.deinit();
        while (args_iter.next()) |ext| {
            errdefer for (extensions_list.items) |prev_ext| allocator.free(prev_ext);
            const duped = try allocator.dupe(u8, ext);
            errdefer allocator.free(duped);
            try extensions_list.append(duped);
        }
        extensions = try extensions_list.toOwnedSlice();
        errdefer {
            for (extensions) |ext| allocator.free(ext);
            allocator.free(extensions);
        }

        break :args Args{
            .output_file = output_file.?,
            .gl_xml_file = gl_xml_file.?,
            .extensions = extensions.?,
        };
    };
    defer {
        for (args.extensions) |ext| allocator.free(ext);
        allocator.free(args.extensions);
        args.output_file.close();
        args.gl_xml_file.close();
    }

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
        build_options.gl_version.stringWithGlPrefix(),
        @tagName(build_options.gl_profile),
    });

    if (args.extensions.len == 0) {
        try out.writeAll("(none)\n");
    } else {
        for (args.extensions) |ext, i| {
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

    { // iterate the many "enums" elements
        const elem_name = "enums";

        var maybe_next_elem_index: ?usize = tree.root.getChildElementIndexPos(elem_name, 0) orelse {
            try stderr.print("Missing at least one element named '{s}'.\n", .{elem_name});
            return error.MissingEnumElements;
        };
        while (maybe_next_elem_index) |elem_index| {
            const enum_collection_element = tree.root.getChildElementPos(elem_name, elem_index).?;
            maybe_next_elem_index = tree.root.getChildElementIndexPos(elem_name, elem_index + 1);

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
