const std = @import("std");
const xml = @import("xml.zig");
const util = @import("util.zig");
const gl_targets = @import("opengl-targets.zig");
const assert = std.debug.assert;

pub fn main() !void {
    const log = std.log.default;

    const allocator = std.heap.c_allocator;

    const args: GenerationArgs = args: {
        var args_iter = try std.process.argsWithAllocator(allocator);
        defer args_iter.deinit();
        break :args try GenerationArgs.parse(allocator, &args_iter, .@"parse-gen-args");
    };
    defer args.deinit(allocator);

    const api_version: gl_targets.Version = args.api_version;
    const api_profile: gl_targets.Profile = args.api_profile;

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
    defer registry.deinit(allocator);

    if (false) { // debug print the type decls in a nice format to stderr
        for (registry.types) |type_entry| {
            std.debug.print(
                \\ * {s}:
                \\  + requires: {?s}
                \\  + API: {?s}
                \\  + comment: {?s}
                \\  + name in body: {}
                \\  + C Definition: "
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

    if (false) { // debug print the enum sets in a nice format to stderr
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

    const output_file = try std.fs.cwd().createFile(args.output_file_path, .{});
    defer output_file.close();

    var out_writer_buffered = std.io.bufferedWriter(output_file.writer());
    const out = out_writer_buffered.writer();

    if (true) return;

    var must_flush_out_writer_buffered = true;
    defer assert(!must_flush_out_writer_buffered or out_writer_buffered.end == 0);
    errdefer must_flush_out_writer_buffered = false;

    if (!std.mem.eql(u8, tree.root.name, "registry")) {
        log.err("Expected root element to be 'registry', found '{s}'.\n", .{tree.root.name});
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
            log.err("Missing element '{s}'.\n", .{name});
            return error.MissingCommentElement;
        };
        if (tree.root.getChildElementIndexPos(name, elem_index + 1) != null) {
            log.err("Too many elements with name '{s}'.\n", .{name});
            return error.TooManyCommentElements;
        }
        const element = tree.root.children[elem_index].element;

        if (element.children.len != 1 or element.children[0] != .text) {
            log.err("Expected only text in {s} element.\n", .{name});
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
}

const Registry = struct {
    /// Text content of the '<comment>' element (presumably only one).
    comment: ?[]const u8,
    /// '<type>' entries from the '<types>' element (presumably there is usually only one).
    types: []const TypeEntry,
    /// true if registry contains '<groups>' element.
    groups_elem: bool,
    enum_sets: []const EnumsSet,
    /// '<commands>' element (presumably there is usually only one).
    commands: Commands,
    features: []const FeatureSetGroup,
    /// '<extension>' elements from the '<extensions>' element (presumably there is usually only one).
    extensions: []const Extension,

    pub fn parse(tree: xml.Element, allocator: std.mem.Allocator) !Registry {
        const top_level_comment: ?[]const u8 = blk: {
            if (tree.getChildElementIndexPos("comment", 0)) |comment_elem_index| {
                if (tree.getChildElementIndexPos("comment", comment_elem_index + 1) != null) {
                    return error.TooManyTopLevelCommentElements;
                }
                const elem = tree.children[comment_elem_index].element;
                if (elem.attributes.len != 0)
                    return error.TopLevelCommentElementHasAttributes;
                if (elem.children.len == 0)
                    return error.EmptyTopLevelCommentElement;
                if (elem.children.len != 1 or elem.children[0] != .text)
                    return error.TopLevelCommentElementHasWeirdData;
                const duped_text = try allocator.dupe(u8, elem.children[0].text);
                errdefer allocator.free(duped_text);
                break :blk duped_text;
            }
            break :blk null;
        };
        errdefer allocator.free(top_level_comment orelse "");

        const types: []const TypeEntry = types: {
            var types = std.ArrayList(TypeEntry).init(allocator);
            defer types.deinit();
            errdefer for (types.items) |entry| entry.deinit(allocator);

            var c_def_text_buffer = std.ArrayList(u8).init(allocator);
            defer c_def_text_buffer.deinit();

            const types_elem_index = tree.getChildElementIndexPos("types", 0) orelse {
                return error.MissingTypesElement;
            };
            if (tree.getChildElementIndexPos("types", types_elem_index + 1) != null)
                return error.TooManyTypesElements;
            const types_elem = tree.children[types_elem_index].element;

            for (types_elem.children) |types_elem_child| {
                const type_elem: xml.Element = switch (types_elem_child) {
                    .element => |elem| elem,
                    .comment => continue,
                    .text => return error.UnexpectedTextInTypesElement,
                };
                if (!std.mem.eql(u8, type_elem.name, "type")) {
                    return error.UnexpectedNonTypeElementInTypesElement;
                }

                const requires: ?[]const u8 =
                    if (type_elem.getAttributeValue("requires")) |requires| try allocator.dupe(u8, requires) else null;
                errdefer allocator.free(requires orelse "");

                const api: ?gl_targets.Api = api: {
                    const api_str = type_elem.getAttributeValue("api") orelse break :api null;
                    const api = try (std.meta.stringToEnum(gl_targets.Api, api_str) orelse
                        error.UnrecognisedApiInTypeAttribute);
                    break :api api;
                };

                const comment: ?[]const u8 = comment: {
                    const comment = type_elem.getAttributeValue("comment") orelse break :comment null;
                    break :comment try allocator.dupe(u8, comment);
                };
                errdefer allocator.free(comment orelse "");

                const maybe_name_attribute: ?[]const u8 = type_elem.getAttributeValue("name");
                const maybe_name_elem_index: ?usize = type_elem.getChildElementIndexPos("name", 0);

                if (maybe_name_attribute == null and maybe_name_elem_index == null) {
                    return error.TypeMissingName;
                }
                if (maybe_name_attribute != null and maybe_name_elem_index != null) {
                    return error.RedundantTypeNames;
                }

                const type_name: []const u8 = name: {
                    if (maybe_name_attribute) |attr_val| {
                        break :name try allocator.dupe(u8, attr_val);
                    }
                    const type_name_elem = type_elem.children[maybe_name_elem_index.?].element;
                    if (type_name_elem.children.len == 0)
                        return error.TypeNameElementMissingText;
                    if (type_name_elem.children.len != 1 or type_name_elem.children[0] != .text)
                        return error.TypeNameElementHasTooManyChildren;
                    break :name try allocator.dupe(u8, type_name_elem.children[0].text);
                };
                errdefer allocator.free(type_name);

                const type_def: TypeEntry.TypeDef = def: {
                    var type_def_text = std.ArrayList(u8).init(allocator);
                    defer type_def_text.deinit();

                    var apientry_indices_list = try std.ArrayList(usize).initCapacity(allocator, 1);
                    defer apientry_indices_list.deinit();

                    for (type_elem.children, 0..) |type_elem_child, i| {
                        switch (type_elem_child) {
                            .comment => continue,
                            .text => |text| try type_def_text.appendSlice(text),
                            .element => |elem| {
                                const name_tag = std.meta.stringToEnum(enum { name, apientry }, elem.name) orelse {
                                    return error.UnrecognisedTypeDefElement;
                                };
                                switch (name_tag) {
                                    .apientry => try apientry_indices_list.append(type_def_text.items.len),
                                    .name => {
                                        assert(maybe_name_elem_index != null and i == maybe_name_elem_index.?);
                                        assert(elem.children.len == 1);
                                        try type_def_text.appendSlice(elem.children[0].text);
                                    },
                                }
                            },
                        }
                    }

                    const text = try type_def_text.toOwnedSlice();
                    errdefer allocator.free(text);

                    const apientry_indices = if (apientry_indices_list.items.len == 1)
                        TypeEntry.TypeDef.ApientryIndices{ .one = apientry_indices_list.items[0] }
                    else
                        TypeEntry.TypeDef.ApientryIndices{ .many = try apientry_indices_list.toOwnedSlice() };
                    errdefer apientry_indices;

                    break :def TypeEntry.TypeDef{
                        .text = text,
                        .apientry_indices = apientry_indices,
                    };
                };
                errdefer type_def.deinit(allocator);

                try types.append(TypeEntry{
                    .requires = requires,
                    .name = type_name,
                    .name_in_body = maybe_name_elem_index != null,
                    .api = api,
                    .comment = comment,
                    .type_def = type_def,
                });
            }

            break :types try types.toOwnedSlice();
        };
        errdefer {
            for (types) |entry| entry.deinit(allocator);
            allocator.free(types);
        }

        const enum_sets: []const EnumsSet = enum_sets: {
            var enum_sets = std.ArrayList(EnumsSet).init(allocator);
            defer enum_sets.deinit();
            errdefer for (enum_sets.items) |entry| entry.deinit(allocator);

            var enum_set_elem_iter = tree.childElementIterator("enums");
            while (enum_set_elem_iter.next()) |enum_set_elem| {
                const namespace = if (enum_set_elem.getAttributeValue("namespace")) |str|
                    try allocator.dupe(u8, str)
                else {
                    return error.EnumsGroupMissingNamespace;
                };
                errdefer allocator.free(namespace);

                const enum_set_type: ?[]const u8 = if (enum_set_elem.getAttributeValue("type")) |str| try allocator.dupe(u8, str) else null;
                errdefer allocator.free(enum_set_type orelse "");

                const enum_set_range: ?EnumsSet.ValueRange = blk: {
                    const maybe_start_attr = enum_set_elem.getAttributeValue("start");
                    const maybe_end_attr = enum_set_elem.getAttributeValue("end");

                    if (maybe_start_attr == null and maybe_end_attr == null) break :blk null;
                    if (maybe_start_attr != null and maybe_end_attr == null) return error.StartMissingEndAttribute;
                    if (maybe_start_attr == null and maybe_end_attr != null) return error.EndMissingStartAttribute;
                    assert(maybe_start_attr != null and maybe_end_attr != null);

                    const duped_start = try allocator.dupe(u8, maybe_start_attr.?);
                    errdefer allocator.free(duped_start);

                    const duped_end = try allocator.dupe(u8, maybe_end_attr.?);
                    errdefer allocator.free(duped_end);

                    break :blk EnumsSet.ValueRange{
                        .start = duped_start,
                        .end = duped_end,
                    };
                };
                errdefer if (enum_set_range) |unwrapped| unwrapped.deinit(allocator);

                const enum_set_vendor: ?[]const u8 = if (enum_set_elem.getAttributeValue("vendor")) |str| try allocator.dupe(u8, str) else null;
                errdefer allocator.free(enum_set_vendor orelse "");

                const enum_set_comment: ?[]const u8 = if (enum_set_elem.getAttributeValue("comment")) |str| try allocator.dupe(u8, str) else null;
                errdefer allocator.free(enum_set_comment orelse "");

                const enum_set_group: ?[]const u8 = if (enum_set_elem.getAttributeValue("group")) |str| try allocator.dupe(u8, str) else null;
                errdefer allocator.free(enum_set_group orelse "");

                var values_list = std.ArrayList(EnumsSet.Value).init(allocator);
                defer values_list.deinit();
                errdefer for (values_list.items) |val| val.deinit(allocator);

                var unused_ranges_list = std.ArrayList(EnumsSet.UnusedRange).init(allocator);
                defer unused_ranges_list.deinit();
                errdefer for (unused_ranges_list.items) |unused_range| unused_range.deinit(allocator);

                // iterate and collect elements
                for (enum_set_elem.children) |enums_group_child| {
                    const enum_entry: xml.Element = switch (enums_group_child) {
                        .comment => continue,
                        .element => |elem| elem,
                        .text => return error.UnexpectedTextInEnumsGroupElement,
                    };
                    const elem_tag = std.meta.stringToEnum(enum { @"enum", unused }, enum_entry.name) orelse {
                        return error.UnexpectedElementName;
                    };
                    switch (elem_tag) {
                        .@"enum" => {
                            const value_name: []const u8 = if (enum_entry.getAttributeValue("name")) |str|
                                try allocator.dupe(u8, str)
                            else {
                                return error.EnumEntryMissingName;
                            };
                            errdefer allocator.free(value_name);

                            const value_int: []const u8 = if (enum_entry.getAttributeValue("value")) |str|
                                try allocator.dupe(u8, str)
                            else {
                                return error.EnumEntryMissingValue;
                            };
                            errdefer allocator.free(value_int);

                            const value_alias: ?[]const u8 = if (enum_entry.getAttributeValue("alias")) |str| try allocator.dupe(u8, str) else null;
                            errdefer allocator.free(value_alias orelse "");

                            const value_group: ?[]const u8 = if (enum_entry.getAttributeValue("group")) |str| try allocator.dupe(u8, str) else null;
                            errdefer allocator.free(value_group orelse "");

                            const value_api: ?gl_targets.Api = blk: {
                                const str = enum_entry.getAttributeValue("api") orelse break :blk null;
                                break :blk try (std.meta.stringToEnum(gl_targets.Api, str) orelse error.UnrecognisedApi);
                            };

                            const ValueTag = enum {
                                u,
                                l,
                                ll,

                                lu,
                                ul,

                                llu,
                                ull,
                            };
                            const value_type: EnumsSet.Value.Type = blk: {
                                const str = enum_entry.getAttributeValue("type") orelse break :blk .none;
                                break :blk switch (try (std.meta.stringToEnum(ValueTag, str) orelse error.UnrecognizedEnumValueTypeSuffix)) {
                                    .u => .u,
                                    .l => .l,
                                    .ll => .ll,
                                    .lu, .ul => .lu,
                                    .llu, .ull => .llu,
                                };
                            };

                            try values_list.append(EnumsSet.Value{
                                .name = value_name,
                                .value = value_int,
                                .api = value_api,
                                .type = value_type,
                                .group = value_group,
                                .alias = value_alias,
                            });
                        },
                        .unused => {
                            const unused_range_vendor: ?[]const u8 = if (enum_entry.getAttributeValue("vendor")) |str| try allocator.dupe(u8, str) else null;
                            errdefer allocator.free(unused_range_vendor orelse "");

                            const unused_range_comment: ?[]const u8 = if (enum_entry.getAttributeValue("comment")) |str| try allocator.dupe(u8, str) else null;
                            errdefer allocator.free(unused_range_comment orelse "");

                            const unused_range_val: EnumsSet.ValueRange = blk: {
                                const start_attr: []const u8 = if (enum_entry.getAttributeValue("start")) |str|
                                    try allocator.dupe(u8, str)
                                else
                                    return error.MissingStartAttribute;
                                errdefer allocator.free(start_attr);

                                const end_attr: ?[]const u8 = if (enum_entry.getAttributeValue("end")) |str| try allocator.dupe(u8, str) else null;
                                errdefer allocator.free(end_attr orelse "");

                                break :blk EnumsSet.ValueRange{
                                    .start = start_attr,
                                    .end = end_attr,
                                };
                            };
                            errdefer unused_range_val.deinit(allocator);

                            try unused_ranges_list.append(EnumsSet.UnusedRange{
                                .range = unused_range_val,
                                .vendor = unused_range_vendor,
                                .comment = unused_range_comment,
                            });
                        },
                    }
                }

                const values = try values_list.toOwnedSlice();
                errdefer allocator.free(values);
                errdefer for (values) |val| val.deinit(allocator);
                assert(values_list.items.len == 0);

                const unused_ranges = try unused_ranges_list.toOwnedSlice();
                errdefer allocator.free(unused_ranges);
                errdefer for (unused_ranges) |unused_range| unused_range.deinit(allocator);
                assert(unused_ranges_list.items.len == 0);

                try enum_sets.append(EnumsSet{
                    .namespace = namespace,
                    .type = enum_set_type,
                    .group = enum_set_group,
                    .vendor = enum_set_vendor,
                    .range = enum_set_range,
                    .comment = enum_set_comment,

                    .values = values,
                    .unused_ranges = unused_ranges,
                });
            }

            break :enum_sets try enum_sets.toOwnedSlice();
        };
        errdefer {
            for (enum_sets) |set| set.deinit(allocator);
            allocator.free(enum_sets);
        }

        return std.mem.zeroInit(Registry, .{
            .comment = top_level_comment,
            .types = types,
            .groups_elem = blk: {
                const first_index = tree.getChildElementIndexPos("groups", 0);
                if (first_index != null and tree.getChildElementIndexPos("groups", first_index.? + 1) != null) return error.TooManyGroupElements;
                break :blk first_index != null;
            },
            .enum_sets = enum_sets,
        });
        // return Registry{
        //     .comment = top_level_comment,
        //     .types = types,
        //     .enum_sets = enum_sets,
        // };
    }

    pub fn deinit(reg: Registry, allocator: std.mem.Allocator) void {
        allocator.free(reg.comment orelse "");

        for (reg.types) |entry| entry.deinit(allocator);
        allocator.free(reg.types);

        for (reg.enum_sets) |enums_group| enums_group.deinit(allocator);
        allocator.free(reg.enum_sets);

        reg.commands.deinit(allocator);

        for (reg.features) |feature_set_group| feature_set_group.deinit(allocator);
        allocator.free(reg.features);

        for (reg.extensions) |extension| extension.deinit(allocator);
        allocator.free(reg.extensions);
    }

    const TypeEntry = struct {
        requires: ?[]const u8,
        name: []const u8,
        /// True if the name was found in the body of the type definition
        /// in a '<name>' element, instead of as an attribute
        name_in_body: bool,
        api: ?gl_targets.Api,
        comment: ?[]const u8,
        type_def: TypeDef,

        pub fn deinit(self: TypeEntry, allocator: std.mem.Allocator) void {
            allocator.free(self.requires orelse "");
            allocator.free(self.name);
            allocator.free(self.comment orelse "");
            self.type_def.deinit(allocator);
        }

        const TypeDef = struct {
            text: []const u8,
            /// indices into the `text` where <apientry/> elements would be.
            apientry_indices: ApientryIndices,

            pub fn deinit(self: TypeDef, allocator: std.mem.Allocator) void {
                allocator.free(self.text);
                self.apientry_indices.deinit(allocator);
            }

            const ApientryIndices = union(enum) {
                one: usize,
                many: []const usize,

                pub fn deinit(self: ApientryIndices, allocator: std.mem.Allocator) void {
                    switch (self) {
                        .one => {},
                        .many => |many| allocator.free(many),
                    }
                }
            };
        };
    };

    const EnumsSet = struct {
        namespace: []const u8,
        type: ?[]const u8,
        group: ?[]const u8,
        vendor: ?[]const u8,
        range: ?ValueRange,
        comment: ?[]const u8,

        values: []const Value,
        unused_ranges: []const UnusedRange,

        pub fn deinit(self: EnumsSet, allocator: std.mem.Allocator) void {
            allocator.free(self.namespace);
            allocator.free(self.type orelse "");
            allocator.free(self.group orelse "");
            allocator.free(self.vendor orelse "");
            if (self.range) |range| range.deinit(allocator);
            allocator.free(self.comment orelse "");

            for (self.values) |value| value.deinit(allocator);
            allocator.free(self.values);

            for (self.unused_ranges) |range| range.deinit(allocator);
            allocator.free(self.unused_ranges);
        }

        const Value = struct {
            name: []const u8,
            value: []const u8,
            api: ?gl_targets.Api,
            type: Type,
            group: ?[]const u8,
            alias: ?[]const u8,

            pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
                allocator.free(self.name);
                allocator.free(self.value);
                allocator.free(self.group orelse "");
                allocator.free(self.alias orelse "");
            }

            const Type = union(std.c.Token.NumSuffix) {
                none,
                f: noreturn,
                l,
                u,
                lu,
                ll,
                llu,
            };
        };
        const UnusedRange = struct {
            range: ValueRange,
            vendor: ?[]const u8,
            comment: ?[]const u8,

            pub fn deinit(self: UnusedRange, allocator: std.mem.Allocator) void {
                self.range.deinit(allocator);
                allocator.free(self.vendor orelse "");
                allocator.free(self.comment orelse "");
            }
        };
        const ValueRange = struct {
            start: []const u8,
            /// null indicates that 'start' is the only value within the range.
            end: ?[]const u8,

            pub fn deinit(self: ValueRange, allocator: std.mem.Allocator) void {
                allocator.free(self.start);
                allocator.free(self.end orelse "");
            }
        };
    };

    const Commands = struct {
        namespace: []const u8,
        /// list of the contained '<command>' element tags.
        entries: []const Entry,

        pub fn deinit(self: Commands, allocator: std.mem.Allocator) void {
            allocator.free(self.namespace);
            for (self.entries) |entry| entry.deinit(allocator);
            allocator.free(self.entries);
        }

        const Entry = struct {
            comment: ?[]const u8,
            proto: Proto,
            /// list of the <param>
            params: []const Param,
            /// the value of the 'name' attribute of the '<alias>' element.
            /// no documentation specifies, but I presume there should only ever be one of these at a time.
            alias: ?[]const u8,
            /// the value of the 'name' attribute of the '<vecequiv>' element.
            /// no documentation specifies, but I presume there should only ever be one of these at a time.
            vecequiv: ?[]const u8,
            /// the '<glx>' element.
            /// no documentation specifies, but I presume there should only ever be one of these at a time.
            glx: ?GlxInfo,

            pub fn deinit(self: Entry, allocator: std.mem.Allocator) void {
                allocator.free(self.comment orelse "");
                self.proto.deinit(allocator);

                for (self.params) |param| param.deinit(allocator);
                allocator.free(self.params);

                allocator.free(self.alias orelse "");
                allocator.free(self.vecequiv orelse "");
                if (self.glx) |glx| glx.deinit(allocator);
            }

            const Proto = struct {
                group: ?[]const u8,
                name_index: usize,
                def: []const Component,

                pub fn deinit(self: Proto, allocator: std.mem.Allocator) void {
                    allocator.free(self.group orelse "");

                    for (self.def) |component| component.deinit(allocator);
                    allocator.free(self.def);
                }

                const Component = struct {
                    /// true if the text is enclosed in a '<ptype>' element.
                    is_ptype: bool,
                    text: []const u8,

                    pub fn deinit(self: Component, allocator: std.mem.Allocator) void {
                        allocator.free(self.text);
                    }
                };
            };
            const Param = struct {
                group: ?[]const u8,
                len: ?[]const u8,
                class: ?[]const u8,

                name_index: usize,
                def: []const Component,

                pub fn deinit(self: Param, allocator: std.mem.Allocator) void {
                    allocator.free(self.group orelse "");
                    allocator.free(self.len orelse "");
                    allocator.free(self.class orelse "");
                    for (self.def) |component| component.deinit(allocator);
                    allocator.free(self.def);
                }

                const Component = struct {
                    /// true if the text is enclosed in a '<ptype>' element.
                    is_ptype: bool,
                    text: []const u8,

                    pub fn deinit(self: Component, allocator: std.mem.Allocator) void {
                        allocator.free(self.text);
                    }
                };
            };
            const GlxInfo = struct {
                //! The readme.pdf that lives with the official gl.xml doesn't seem to describe the '<glx>' tag
                //! directly, but every discoverable example of it I've seen has exactly these attributes.
                type: []const u8,
                opcode: []const u8,

                pub fn deinit(self: GlxInfo, allocator: std.mem.Allocator) void {
                    allocator.free(self.type);
                    allocator.free(self.opcode);
                }
            };
        };
    };

    const FeatureSetGroup = struct {
        api: gl_targets.Api,
        name: gl_targets.Version,
        number: Number,
        protect: ?[]const u8,
        comment: ?[]const u8,
        require_sets: []const FeatureSet,
        remove_sets: []const FeatureSet,

        pub fn deinit(self: FeatureSetGroup, allocator: std.mem.Allocator) void {
            allocator.free(self.protect orelse "");
            allocator.free(self.comment orelse "");

            for (self.require_sets) |set| set.deinit(allocator);
            allocator.free(self.require_sets);

            for (self.remove_sets) |set| set.deinit(allocator);
            allocator.free(self.remove_sets);
        }

        const FeatureSet = struct {
            profile: ?gl_targets.Profile,
            comment: ?[]const u8,

            commands: []const Command,
            enums: []const Enum,
            types: []const Type,

            pub fn deinit(self: FeatureSet, allocator: std.mem.Allocator) void {
                allocator.free(self.comment orelse "");

                for (self.commands) |command| command.deinit(allocator);
                allocator.free(self.commands);

                for (self.enums) |@"enum"| @"enum".deinit(allocator);
                allocator.free(self.enums);

                for (self.types) |@"type"| @"type".deinit(allocator);
                allocator.free(self.types);
            }

            const Command = struct {
                name: []const u8,
                comment: ?[]const u8,

                pub fn deinit(self: Command, allocator: std.mem.Allocator) void {
                    allocator.free(self.name);
                    allocator.free(self.comment orelse "");
                }
            };
            const Enum = struct {
                name: []const u8,
                comment: ?[]const u8,

                pub fn deinit(self: Enum, allocator: std.mem.Allocator) void {
                    allocator.free(self.name);
                    allocator.free(self.comment orelse "");
                }
            };
            const Type = struct {
                name: []const u8,
                comment: ?[]const u8,

                pub fn deinit(self: Type, allocator: std.mem.Allocator) void {
                    allocator.free(self.name);
                    allocator.free(self.comment orelse "");
                }
            };
        };

        const Number = struct {
            major: u32,
            minor: u32,

            pub fn parse(src: []const u8) error{
                NumberMissingSeparator,
                TooManySeparators,
                MajorOverflow,
                MajorInvalidCharacter,
                MinorOverflow,
                MinorInvalidCharacter,
            }!Number {
                const sep_index = std.mem.indexOfScalar(u8, src, '.') orelse return error.NumberMissingSeparator;
                if (std.mem.lastIndexOfScalar(u8, src, '.').? != sep_index) return error.TooManySeparators;

                const major_str = src[0..sep_index];
                const minor_str = src[sep_index + 1 ..];

                return Number{
                    .major = std.fmt.parseUnsigned(u32, major_str, 10) catch |err| return switch (err) {
                        inline else => |e| comptime @field(anyerror, "Major" ++ @errorName(e)),
                    },
                    .minor = std.fmt.parseUnsigned(u32, minor_str, 10) catch |err| return switch (err) {
                        inline else => |e| comptime @field(anyerror, "Minor" ++ @errorName(e)),
                    },
                };
            }
        };
    };

    const Extension = struct {
        name: []const u8,
        supported: []const u8,
        protect: ?[]const u8,
        comment: ?[]const u8,

        require_sets: []const FeatureSet,
        /// The readme.pdf next to gl.xml states:
        /// > A <remove> block defines a set of interfaces removed by a <feature> (this is primarily
        /// > useful for the OpenGL core profile, which removed many interfaces - extensions should
        /// > never remove interfaces, although this usage is allowed by the schema).
        remove_sets: []const FeatureSet,

        pub fn deinit(self: Extension, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
            allocator.free(self.supported);
            allocator.free(self.protect orelse "");
            allocator.free(self.comment orelse "");

            for (self.require_sets) |set| set.deinit(allocator);
            allocator.free(self.require_sets);

            for (self.remove_sets) |set| set.deinit(allocator);
            allocator.free(self.remove_sets);
        }

        const FeatureSet = struct {
            profile: ?gl_targets.Profile,
            comment: ?[]const u8,
            /// The readme.pdf next to gl.xml states:
            /// > The api attribute is only supported inside <extension> tags, since <feature>
            /// > tags already define a specific API.
            /// This is one of the reasons this is defined as a separate type to `FeatureSetGroup.FeatureSet`.
            api: ?gl_targets.Api,

            commands: []const Command,
            enums: []const Enum,
            types: []const Type,

            pub fn deinit(self: FeatureSet, allocator: std.mem.Allocator) void {
                allocator.free(self.comment orelse "");

                for (self.commands) |command| command.deinit(allocator);
                allocator.free(self.commands);

                for (self.enums) |@"enum"| @"enum".deinit(allocator);
                allocator.free(self.enums);

                for (self.types) |@"type"| @"type".deinit(allocator);
                allocator.free(self.types);
            }

            const Command = FeatureSetGroup.FeatureSet.Command;
            const Enum = FeatureSetGroup.FeatureSet.Enum;
            const Type = FeatureSetGroup.FeatureSet.Type;
        };
    };
};

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

        // const output_file = std.fs.cwd().createFile(output_file_path.?, .{}) catch |err| {
        //     log.err("Failed to create/open output file '{s}'.\n", .{output_file_path.?});
        //     return err;
        // };
        // errdefer output_file.close();

        // const gl_xml_file = std.fs.cwd().openFile(gl_xml_file_path.?, .{}) catch |err| {
        //     log.err("Failed to open registry file '{s}'.\n", .{gl_xml_file_path.?});
        //     return err;
        // };
        // errdefer gl_xml_file.close();

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
