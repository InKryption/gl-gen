const std = @import("std");
const util = @import("util.zig");
const assert = std.debug.assert;

pub const Tree = struct {
    arena_state: std.heap.ArenaAllocator.State,
    root: Element,

    pub fn deinit(tree: Tree, allocator: std.mem.Allocator) void {
        tree.arena_state.promote(allocator).deinit();
    }
};
pub const Element = struct {
    name: []const u8,
    attributes: Attribute.Set,
    children: []const Child,

    pub const Attribute = struct {
        name: []const u8,
        value: []const u8,
        quote: u8,

        pub const Set = std.ArrayHashMapUnmanaged(Attribute, void, HashCtx, true);
        pub const HashCtx = struct {
            pub fn hash(ctx: HashCtx, key: Attribute) u32 {
                _ = ctx;
                return Adapted.hash(.{}, key.name);
            }
            pub fn eql(ctx: HashCtx, a: Attribute, b: Attribute, b_index: usize) bool {
                _ = ctx;
                return Adapted.eql(.{}, a.name, b, b_index);
            }
            pub const Adapted = struct {
                pub fn hash(ctx: Adapted, key_name: []const u8) u32 {
                    _ = ctx;
                    return std.array_hash_map.hashString(key_name);
                }
                pub fn eql(ctx: Adapted, a_name: []const u8, b: Attribute, b_index: usize) bool {
                    _ = ctx;
                    _ = b_index;
                    return std.mem.eql(u8, a_name, b.name);
                }
            };
        };
    };
    pub const Child = union(enum) {
        element: Element,
        text: []const u8,
        comment: []const u8,
    };

    pub fn getAttributeValue(elem: Element, name: []const u8) ?[]const u8 {
        const attr = elem.getAttribute(name) orelse return null;
        return attr.value;
    }

    pub fn getAttribute(elem: Element, name: []const u8) ?Attribute {
        return elem.attributes.getKeyAdapted(name, Attribute.HashCtx.Adapted{});
    }

    pub fn getAttributeIndex(elem: Element, name: []const u8) ?usize {
        return elem.attributes.getIndexAdapted(name, Attribute.HashCtx.Adapted{});
    }

    /// Same as `getTextIndex`, but returns the text data directly.
    pub fn getText(elem: Element, index: usize) ?[]const u8 {
        const actual_index = elem.getTextIndex(index) orelse return null;
        return elem.children[actual_index].text;
    }

    /// If the child at index `index` is text data, returns `index`.
    /// Otherwise, searches for and returns the index of the first child
    /// which is text data.
    /// If the child at `index` is not text data, and there exist no children
    /// after `index` which are text data, returns null.
    /// Asserts `index < elem.children.len`.
    pub fn getTextIndex(elem: Element, index: usize) ?usize {
        assert(index < elem.children.len);
        for (elem.children[index..], index..) |child, offset| {
            switch (child) {
                .text => return offset,
                .element, .comment => continue,
            }
        }
        return null;
    }

    /// Get the first element with the specified name, which must either
    /// be at `start_pos`, or after the same.
    /// Returns `null` if such an element does not exist.
    pub fn getChildElementPos(elem: Element, name: []const u8, start_pos: usize) ?Element {
        const index = elem.getChildElementIndexPos(name, start_pos) orelse return null;
        return elem.children[index].element;
    }

    /// Get the index of the first element with the specified name,
    /// which is either at `start_pos`, or after `start_pos`.
    /// Returns `null` if such an element does not exist.
    pub fn getChildElementIndexPos(elem: Element, name: []const u8, start_pos: usize) ?usize {
        for (elem.children[start_pos..], start_pos..) |child, index| {
            const child_elem_name: []const u8 = switch (child) {
                .element => |child_elem| child_elem.name,
                .text, .comment => continue,
            };
            if (!std.mem.eql(u8, child_elem_name, name)) continue;
            return index;
        }
        return null;
    }

    /// Get the last element with the specified name.
    /// Returns `null` if such an element does not exist.
    pub fn getLastChildElement(elem: Element, name: []const u8) ?Element {
        return @call(.always_inline, getLastChildElementPos, .{ elem, name, elem.children.len });
    }

    /// Get the last element with the specified name, which
    /// exists before `limit_pos`.
    /// Returns `null` if such an element does not exist.
    /// Asserts `limit_pos <= elem.children.len`.
    /// Asserts `limit_pos > 0`.
    pub fn getLastChildElementPos(elem: Element, name: []const u8, limit_pos: usize) ?Element {
        const index = elem.getLastChildElementIndexPos(name, limit_pos) orelse return null;
        return elem.children[index].element;
    }

    /// Get the index of the last element with the specified name.
    /// Returns `null` if such an element does not exist.
    pub fn getLastChildElementIndex(elem: Element, name: []const u8) ?usize {
        return @call(.always_inline, getLastChildElementIndexPos, .{ elem, name, elem.children.len });
    }

    /// Get the index of the last element with the specified name,
    /// which exists before `limit_pos`.
    /// Returns `null` if such an element does not exist.
    /// Asserts `limit_pos <= elem.children.len`.
    /// Asserts `limit_pos > 0`.
    pub fn getLastChildElementIndexPos(elem: Element, name: []const u8, limit_pos: usize) ?usize {
        assert(limit_pos <= elem.children.len);
        assert(limit_pos > 0);
        var iter = std.mem.reverseIterator(elem.children[0..limit_pos]);
        var i: usize = limit_pos - 1;
        while (iter.next()) |child| : (i -= 1) {
            const child_elem_name: []const u8 = switch (child) {
                .element => |child_elem| child_elem.name,
                .text, .comment => continue,
            };
            if (std.mem.eql(u8, child_elem_name, name)) continue;
            return i;
        }
        return null;
    }

    /// Returns an iterator over child elements with the specified name.
    pub inline fn childElementIterator(elem: Element, name: []const u8) ChildElementIterator {
        return .{
            .root = elem,
            .target_name = name,
        };
    }
    pub const ChildElementIterator = struct {
        root: Element,
        target_name: []const u8,
        index: usize = 0,

        pub fn next(iter: *ChildElementIterator) ?Element {
            const new_index = iter.root.getChildElementIndexPos(iter.target_name, iter.index) orelse {
                iter.index = iter.root.children.len;
                return null;
            };
            iter.index = new_index + 1;
            return iter.root.children[new_index].element;
        }

        pub fn reset(iter: *ChildElementIterator) void {
            const refreshed = iter.root.childElementIterator(iter.target_name);
            iter.* = refreshed;
        }
    };

    pub inline fn fmt(elem: Element, options: struct { depth: u32 = 0 }) Formatter {
        return .{
            .elem = elem,
            .depth = options.depth,
        };
    }

    pub const Formatter = struct {
        elem: Element,
        depth: u32 = 0,

        pub fn format(
            formatter: Formatter,
            comptime fmt_str: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            _ = options;
            _ = fmt_str;
            const spaces_per_depth = 4;
            try writer.print("{} = .{{\n", .{std.zig.fmtId(formatter.elem.name)});

            try writer.writeByteNTimes(' ', (formatter.depth + 1) * spaces_per_depth);
            try writer.writeAll(".attributes = .{");
            if (formatter.elem.attributes.len != 0) {
                try writer.writeByte('\n');
                for (formatter.elem.attributes) |attr| {
                    try writer.writeByteNTimes(' ', (formatter.depth + 2) * spaces_per_depth);
                    try writer.print(".{0s} = \"\\{2c}{1s}\\{2c}\"", .{ attr.name, attr.value, attr.quote });
                    try writer.writeAll(",\n");
                }
                try writer.writeByteNTimes(' ', (formatter.depth + 1) * spaces_per_depth);
            }
            try writer.writeAll("},\n");

            try writer.writeByteNTimes(' ', (formatter.depth + 1) * spaces_per_depth);
            try writer.writeAll(".children = .{");
            if (formatter.elem.children.len != 0) {
                try writer.writeByte('\n');
                for (formatter.elem.children) |child| {
                    switch (child) {
                        .element => |element| {
                            try writer.writeByteNTimes(' ', (formatter.depth + 1) * spaces_per_depth);
                            try writer.print(".{},\n", .{element.fmt(.{ .depth = formatter.depth + 3 })});
                        },
                        .text, .comment => |contents| {
                            try writer.writeByteNTimes(' ', (formatter.depth + 1) * spaces_per_depth);
                            try writer.print(".@\"[{s}]\" =\n", .{switch (child) {
                                .text => "Text",
                                .comment => "Comment",
                                else => unreachable,
                            }});
                            var line_iter = std.mem.tokenize(u8, contents, "\n");
                            while (line_iter.next()) |line| {
                                try writer.writeByteNTimes(' ', (formatter.depth + 2) * spaces_per_depth);
                                try writer.writeAll("\\" ++ "\\");
                                try writer.writeAll(line);
                                try writer.writeAll("\n");
                            }
                            if (contents.len != 0 and contents[contents.len - 1] == '\n') {
                                try writer.writeByteNTimes(' ', (formatter.depth + 1) * spaces_per_depth);
                                try writer.writeAll("\\" ++ "\\" ++ "\n");
                            }
                            try writer.writeByteNTimes(' ', (formatter.depth + 1) * spaces_per_depth);
                            try writer.writeAll(",\n");
                        },
                    }
                }

                try writer.writeByteNTimes(' ', (formatter.depth + 1) * spaces_per_depth);
            }
            try writer.writeAll("},\n");

            try writer.writeByteNTimes(' ', (formatter.depth + 0) * spaces_per_depth);
            try writer.writeAll("}");
        }
    };
};

pub fn parse(
    child_allocator: std.mem.Allocator,
    xml_reader: anytype,
    args: struct {
        /// Only written to on error. Indicates the line on which the error occured (0 based index).
        line: *u64,
        /// Only written to on error. Indicates the column on which the error occured (0 based index).
        column: *u64,

        /// Discard comments
        discard_comments: bool = true,
        /// Discard any text that is only comprised of whitespace
        discard_whitespace: bool = true,

        /// Transform entity references to their textual equivalent, e.g. "&amp;" -> "&"
        transform_entity_refs: bool = true,
    },
) !Tree {
    var lct_reader = util.lineColumnTrackingReader(xml_reader);
    errdefer {
        args.line.* = lct_reader.line;
        args.column.* = lct_reader.col;
    }

    const reader = lct_reader.reader();

    var arena = std.heap.ArenaAllocator.init(child_allocator);
    errdefer arena.deinit();
    const allocator = arena.allocator();

    var string_intern = StringIntern.init(allocator);

    var state: enum {
        start,
        left_angle_bracket,
        left_angle_bracket_bang,
        content,
    } = .start;

    var element_stack = try std.ArrayList(Element).initCapacity(child_allocator, 8);
    defer element_stack.deinit();

    var attributes_buf = Element.Attribute.Set.Managed.init(child_allocator);
    defer attributes_buf.deinit();

    var element_name_buf = try std.ArrayList(u8).initCapacity(child_allocator, 64);
    defer element_name_buf.deinit();

    var attr_data_buf = try std.ArrayList(u8).initCapacity(child_allocator, 128);
    defer attr_data_buf.deinit();

    var content_buf = try std.ArrayList(u8).initCapacity(child_allocator, 128);
    defer content_buf.deinit();

    var entity_ref_buf = try std.ArrayList(u8).initCapacity(child_allocator, "&apos;".len);
    defer entity_ref_buf.deinit();

    if (!try reader.isBytes(
        \\<?xml version="1.0" encoding="UTF-8"?>
    )) return error.UnrecognizedPrologue;

    mainloop: while (try nextNonXmlWhitespaceChar(reader)) |iteration_cp| {
        switch (state) {
            .start => switch (iteration_cp) {
                '<' => state = .left_angle_bracket,
                else => if (!isXmlWhiteSpaceChar(iteration_cp)) return error.InvalidStart,
            },
            .left_angle_bracket => switch (iteration_cp) {
                '!' => state = .left_angle_bracket_bang,
                '/' => {
                    const last_element_open = element_stack.popOrNull() orelse return error.InvalidElementEndTagAtStart;

                    { // compare this closing tag with the last opening tag
                        var last_element_open_name_fbs = std.io.fixedBufferStream(last_element_open.name);
                        const last_element_open_name_reader = last_element_open_name_fbs.reader();

                        while (true) {
                            const expected_cp = (util.readCodepointUtf8(last_element_open_name_reader) catch unreachable) orelse break;
                            assert(isXmlNameChar(expected_cp));
                            const actual_cp = try ((try util.readCodepointUtf8(reader)) orelse error.OpenAndCloseTagsDontMatch);
                            if (actual_cp != expected_cp) return error.OpenAndCloseTagsDontMatch;
                        }
                    }

                    var last_cp = try ((try util.readCodepointUtf8(reader)) orelse error.EndOfStream);
                    if (isXmlWhiteSpaceChar(last_cp)) {
                        last_cp = try ((try nextNonXmlWhitespaceChar(reader)) orelse error.EndOfStream);
                    }
                    switch (last_cp) {
                        '>' => {},
                        else => {
                            if (!isXmlNameChar(last_cp)) return error.InvalidElementEndTagNameChar;
                            return error.OpenAndCloseTagsDontMatch;
                        },
                    }

                    if (element_stack.items.len == 0) return .{
                        .arena_state = arena.state,
                        .root = last_element_open,
                    };
                    const parent: *Element = &element_stack.items[element_stack.items.len - 1];
                    parent.children = try allocator.realloc(@constCast(parent.children), parent.children.len + 1);
                    @constCast(&parent.children[parent.children.len - 1]).* = .{
                        .element = last_element_open,
                    };
                    state = .content;
                    continue :mainloop;
                },
                else => {
                    if (!isXmlNameStartChar(iteration_cp)) return error.InvalidNode;

                    assert(element_name_buf.items.len == 0);
                    _ = try util.writeCodepointUtf8(element_name_buf.writer(), iteration_cp);

                    const elem_name_sentinel: u21 = while (try util.readCodepointUtf8(reader)) |subsequent_cp| {
                        if (isXmlNameChar(subsequent_cp)) {
                            _ = try util.writeCodepointUtf8(element_name_buf.writer(), subsequent_cp);
                            continue;
                        }
                        break subsequent_cp;
                    } else return error.EndOfStream;

                    assert(attributes_buf.count() == 0);

                    var latest_cp = elem_name_sentinel;
                    while (true) {
                        if (isXmlWhiteSpaceChar(latest_cp)) {
                            latest_cp = try ((try nextNonXmlWhitespaceChar(reader)) orelse error.EndOfStream);
                        }
                        switch (latest_cp) {
                            else => { // parse attributes
                                attr_data_buf.shrinkRetainingCapacity(0);
                                var attr_value_start: usize = 0;
                                if (!isXmlNameStartChar(latest_cp))
                                    return error.InvalidElementTagChar;
                                attr_value_start += try util.writeCodepointUtf8(attr_data_buf.writer(), latest_cp);

                                while (try util.readCodepointUtf8(reader)) |maybe_attr_name_char| {
                                    if (isXmlNameChar(maybe_attr_name_char)) {
                                        attr_value_start += try util.writeCodepointUtf8(attr_data_buf.writer(), maybe_attr_name_char);
                                        continue;
                                    }
                                    if (isXmlWhiteSpaceChar(maybe_attr_name_char)) {
                                        latest_cp = try ((try nextNonXmlWhitespaceChar(reader)) orelse error.EndOfStream);
                                    } else if (maybe_attr_name_char == '=') {
                                        latest_cp = maybe_attr_name_char;
                                        break;
                                    } else return error.MissingAttributeEquals;
                                } else return error.EndOfStream;

                                assert(latest_cp == '=');
                                latest_cp = try ((try nextNonXmlWhitespaceChar(reader)) orelse error.EndOfStream);

                                const quote_char: u8 = try switch (latest_cp) {
                                    inline '\"', '\'' => |quote_char| @as(u8, quote_char),
                                    else => error.MissingAttributeValueQuote,
                                };

                                while (try util.readCodepointUtf8(reader)) |str_cp| {
                                    if (str_cp == quote_char) break;
                                    _ = try util.writeCodepointUtf8(attr_data_buf.writer(), str_cp);
                                } else return error.EndOfStream;

                                const attr_data_dupe: []const u8 = try string_intern.get(attr_data_buf.items);
                                const attr_gop = try attributes_buf.getOrPutAdapted(attr_data_dupe[0..attr_value_start], Element.Attribute.HashCtx.Adapted{});
                                if (attr_gop.found_existing) return error.DuplicateAttribute;
                                attr_gop.key_ptr.* = Element.Attribute{
                                    .name = attr_data_dupe[0..attr_value_start],
                                    .value = attr_data_dupe[attr_value_start..],
                                    .quote = quote_char,
                                };
                                attr_gop.value_ptr.* = {};
                                latest_cp = try ((try util.readCodepointUtf8(reader)) orelse error.EndOfStream);
                            },
                            '/' => {
                                if (element_name_buf.items.len == 0) return error.ElementMissingName;
                                if (try util.readCodepointUtf8(reader)) |last_element_cp| {
                                    try switch (last_element_cp) {
                                        '>' => {},
                                        else => error.ExpectedRightAngleBracketAfterSlash,
                                    };

                                    const new_elem: Element = Element{
                                        .name = try string_intern.get(element_name_buf.items),
                                        .attributes = (try attributes_buf.cloneWithAllocator(allocator)).unmanaged,
                                        .children = &.{},
                                    };
                                    element_name_buf.shrinkRetainingCapacity(0);
                                    attributes_buf.shrinkRetainingCapacity(0);

                                    // no parent in the stack to unwind,
                                    // so this is the only element.
                                    if (element_stack.items.len == 0) return .{
                                        .arena_state = arena.state,
                                        .root = new_elem,
                                    };

                                    const parent: *Element = &element_stack.items[element_stack.items.len - 1];
                                    parent.children = try allocator.realloc(@constCast(parent.children), parent.children.len + 1);
                                    @constCast(&parent.children[parent.children.len - 1]).* = .{
                                        .element = new_elem,
                                    };
                                    state = .content;
                                    break;
                                } else return error.EndOfStream;
                                continue :mainloop;
                            },
                            '>' => {
                                if (element_name_buf.items.len == 0) return error.ElementMissingName;
                                const new_elem: Element = Element{
                                    .name = try string_intern.get(element_name_buf.items),
                                    .attributes = (try attributes_buf.cloneWithAllocator(allocator)).unmanaged,
                                    .children = &.{},
                                };
                                element_name_buf.shrinkRetainingCapacity(0);
                                attributes_buf.shrinkRetainingCapacity(0);
                                try element_stack.append(new_elem);
                                state = .content;
                                continue :mainloop;
                            },
                        }
                    }
                },
            },
            .left_angle_bracket_bang => switch (iteration_cp) {
                '-' => switch (try ((try util.readCodepointUtf8(reader)) orelse error.EndOfStream)) {
                    '-' => {
                        assert(content_buf.items.len == 0);
                        while (true) {
                            const comment_cp = try ((try util.readCodepointUtf8(reader)) orelse error.EndOfStream);
                            if (comment_cp != '-') {
                                if (!args.discard_comments) {
                                    _ = try util.writeCodepointUtf8(content_buf.writer(), comment_cp);
                                }
                                continue;
                            }

                            const comment_cp_2 = try ((try util.readCodepointUtf8(reader)) orelse error.EndOfStream);
                            if (comment_cp_2 != '-') {
                                if (!args.discard_comments) {
                                    _ = try util.writeCodepointUtf8(content_buf.writer(), comment_cp);
                                    _ = try util.writeCodepointUtf8(content_buf.writer(), comment_cp_2);
                                }
                                continue;
                            }

                            const comment_cp_3 = try ((try util.readCodepointUtf8(reader)) orelse error.EndOfStream);
                            if (comment_cp_3 != '>') {
                                return error.DoubleDashInComment;
                            }

                            break;
                        } else unreachable;

                        if (!args.discard_comments) {
                            const parent: *Element = &element_stack.items[element_stack.items.len - 1];
                            parent.children = try allocator.realloc(@constCast(parent.children), parent.children.len + 1);
                            @constCast(&parent.children[parent.children.len - 1]).* = .{
                                .comment = try string_intern.get(content_buf.items),
                            };
                            content_buf.shrinkRetainingCapacity(0);
                        }
                        state = .content;
                        continue :mainloop;
                    },
                    else => return error.ExpectedSecondSash,
                },
                '[' => @panic(
                    "TODO",
                ),
                else => return error.InvalidCharacter,
            },
            .content => {
                assert(content_buf.items.len == 0);
                var non_whitespace: bool = false;

                var cdata_cp = iteration_cp;
                while (true) : (cdata_cp = try ((try util.readCodepointUtf8(reader)) orelse error.EndOfStream)) {
                    switch (cdata_cp) {
                        '<' => {
                            if (content_buf.items.len != 0) blk: {
                                if (args.discard_whitespace and !non_whitespace) break :blk;
                                const parent: *Element = &element_stack.items[element_stack.items.len - 1];
                                parent.children = try allocator.realloc(@constCast(parent.children), parent.children.len + 1);
                                @constCast(&parent.children[parent.children.len - 1]).* = .{
                                    .text = try string_intern.get(content_buf.items),
                                };
                                content_buf.shrinkRetainingCapacity(0);
                            }
                            state = .left_angle_bracket;
                            continue :mainloop;
                        },
                        '&' => {
                            assert(entity_ref_buf.items.len == 0);
                            cdata_cp = try ((try util.readCodepointUtf8(reader)) orelse error.EndOfStream);
                            while (true) : (cdata_cp = try ((try util.readCodepointUtf8(reader)) orelse error.EndOfStream)) {
                                if (cdata_cp == ';') break;
                                if (cdata_cp != '#' and !isXmlNameChar(cdata_cp)) {
                                    return error.InvalidEntityReference;
                                }
                                _ = try util.writeCodepointUtf8(entity_ref_buf.writer(), cdata_cp);
                            }

                            if (entity_ref_buf.items.len == 0) {
                                return error.InvalidEmptyEntityRefName;
                            }
                            const entity_ref_cp: u21 = switch (entity_ref_buf.items[0]) {
                                ';' => return error.InvalidEmptyEntityRefName,
                                '#' => blk: {
                                    if (entity_ref_buf.items.len == 1)
                                        return error.MissingCharacterRefDigits;
                                    if (entity_ref_buf.items[1] == 'x') {
                                        const digits = entity_ref_buf.items[2..];
                                        if (digits.len == 2)
                                            return error.MissingCharacterRefHexDigits;
                                        break :blk std.fmt.parseUnsigned(u21, digits, 16) catch |err| return switch (err) {
                                            error.Overflow => error.InvalidCharacterRefValue,
                                            error.InvalidCharacter => error.InvalidCharacterRef,
                                        };
                                    }
                                    const digits = entity_ref_buf.items[1..];
                                    break :blk std.fmt.parseUnsigned(u21, digits, 10) catch |err| return switch (err) {
                                        error.Overflow => error.InvalidCharacterRefValue,
                                        error.InvalidCharacter => error.InvalidCharacterRef,
                                    };
                                },
                                else => blk: {
                                    var utf_8_view = std.unicode.Utf8View.init(entity_ref_buf.items) catch |err| return switch (err) {
                                        error.InvalidUtf8 => error.InvalidEntityRef,
                                    };
                                    var cp_iter = utf_8_view.iterator();
                                    if (!isXmlNameStartChar(cp_iter.nextCodepoint().?))
                                        return error.InvalidEntityRefNameStartChar;
                                    while (cp_iter.nextCodepoint()) |name_cp| {
                                        if (!isXmlNameChar(name_cp))
                                            return error.InvalidEntityRefNameChar;
                                    }

                                    if (std.meta.stringToEnum(enum { quot, amp, apos, lt, gt }, entity_ref_buf.items)) |predefined_tag| {
                                        break :blk switch (predefined_tag) {
                                            .quot => '\u{22}',
                                            .amp => '\u{26}',
                                            .apos => '\u{27}',
                                            .lt => '\u{3C}',
                                            .gt => '\u{3E}',
                                        };
                                    }
                                    @panic("TODO: Handle non-predefined entity references."); // I hate XML
                                },
                            };

                            if (!args.transform_entity_refs) {
                                try content_buf.append('&');
                                try content_buf.appendSlice(entity_ref_buf.items);
                                try content_buf.append(';');
                                entity_ref_buf.shrinkRetainingCapacity(0);
                                continue;
                            }

                            _ = try util.writeCodepointUtf8(content_buf.writer(), entity_ref_cp);
                            entity_ref_buf.shrinkRetainingCapacity(0);
                            continue;
                        },
                        else => {
                            non_whitespace = non_whitespace or !isXmlWhiteSpaceChar(cdata_cp);
                            _ = try util.writeCodepointUtf8(content_buf.writer(), cdata_cp);
                        },
                    }
                } else unreachable;
            },
        }
    } else return error.EndOfStream;
}

const StringIntern = struct {
    allocator: std.mem.Allocator,
    set: std.StringHashMapUnmanaged(void) = .{},

    inline fn init(allocator: std.mem.Allocator) StringIntern {
        return .{
            .allocator = allocator,
        };
    }

    fn deinit(self: *StringIntern) void {
        var iter = self.set.keyIterator();
        while (iter.next()) |str| {
            self.allocator.free(str.*);
        }
        self.set.deinit(self.allocator);
    }

    fn get(self: *StringIntern, str: []const u8) ![]const u8 {
        const gop = try self.set.getOrPut(self.allocator, str);
        if (!gop.found_existing) {
            // this is fine, because the string contents are identical,
            // so the hash will remain identical
            gop.key_ptr.* = try self.allocator.dupe(u8, str);
        }
        return gop.key_ptr.*;
    }
};

/// Skips codepoints considered whitespace in XML, returning
/// the first non-whitespace codepoint encountered (or null)
/// if the stream ends without returning a non-whitespace
/// codepoint.
fn nextNonXmlWhitespaceChar(reader: anytype) !?u21 {
    while (try util.readCodepointUtf8(reader)) |cp| {
        if (!isXmlWhiteSpaceChar(cp)) return cp;
    }
    return null;
}

inline fn isXmlWhiteSpaceChar(cp: u21) bool {
    return switch (cp) {
        '\u{20}',
        '\u{09}',
        '\u{0D}',
        '\u{0A}',
        => true,
        else => false,
    };
}

fn isXmlNameStartChar(cp: u21) bool {
    return switch (cp) {
        ':',
        'A'...'Z',
        '_',
        'a'...'z',
        '\u{C0}'...'\u{D6}',
        '\u{D8}'...'\u{F6}',
        '\u{F8}'...'\u{2FF}',
        '\u{370}'...'\u{37D}',
        '\u{37F}'...'\u{1FFF}',
        '\u{200C}'...'\u{200D}',
        '\u{2070}'...'\u{218F}',
        '\u{2C00}'...'\u{2FEF}',
        '\u{3001}'...'\u{D7FF}',
        '\u{F900}'...'\u{FDCF}',
        '\u{FDF0}'...'\u{FFFD}',
        '\u{10000}'...'\u{EFFFF}',
        => true,
        else => false,
    };
}

fn isXmlNameChar(cp: u21) bool {
    return @call(.always_inline, isXmlNameStartChar, .{cp}) or switch (cp) {
        '-',
        '.',
        '0'...'9',
        '\u{B7}',
        '\u{0300}'...'\u{036F}',
        '\u{203F}'...'\u{2040}',
        => true,
        else => false,
    };
}

fn isXmlValidChar(cp: u21) bool {
    return switch (cp) {
        '\u{9}',
        '\u{A}',
        '\u{D}',
        '\u{20}'...'\u{D7FF}',
        '\u{E000}'...'\u{FFFD}',
        '\u{10000}'...'\u{10FFFF}',
        => true,
        else => false,
    };
}
