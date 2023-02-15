//! Usage `<executable> <path/to/output.zig> <path/to/gl.xml> [<extensions>...]`
//!
const std = @import("std");
const assert = std.debug.assert;

const build_options = @import("build-options");

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
            try stderr.writeAll("Expected second argument to be a path to 'gl.xml'.\n");
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

    const data: OpenGlData = data: {
        var buffered_gl_xml = std.io.bufferedReaderSize(4096, args.gl_xml_file.reader());
        const gl_xml_reader = buffered_gl_xml.reader();

        var fallback_ally = FallbackBufferAllocator.init(&struct {
            var static_buffer: [512]u8 = undefined;
        }.static_buffer, allocator);

        var error_line: u64 = undefined;
        var error_column: u64 = undefined;
        const tree = parseXml(fallback_ally.allocator(), gl_xml_reader, .{
            .line = &error_line,
            .column = &error_column,
            .discard_comments = true,
            .discard_whitespace = true,
        }) catch |err|
            {
            try stderr.print("Parsing error encountered at {d}:{d} (line:column) of the registry.\n", .{ error_line + 1, error_column + 1 });
            return err;
        };
        defer tree.deinit(fallback_ally.allocator());

        break :data .{
            .comment = &.{},
        };
    };
    _ = data;

    var out_writer_buffered = std.io.bufferedWriter(args.output_file.writer());
    defer assert(out_writer_buffered.end == 0); // remember to flush
    const out = out_writer_buffered.writer();

    try out.print(
        \\//!
        \\//! Generation parameters:
        \\//! API: {s}
        \\//! Profile: {s}
        \\//! Extensions: 
    , .{
        build_options.target_version.stringWithGlPrefix(),
        @tagName(build_options.target_profile),
    });

    if (args.extensions.len == 0) {
        try out.writeAll("(none)\n");
    } else for (args.extensions) |ext, i| {
        if (i != 0) try out.writeAll(", ");
        try out.print("\"{s}\"", .{ext});
    }

    try out.writeAll(
        \\//!
        \\const preamble = @import("preamble");
        \\
        \\
    );

    try out_writer_buffered.flush();
}

const OpenGlData = struct {
    comment: []const u8,
};

const XmlTree = struct {
    arena_state: std.heap.ArenaAllocator.State,
    root: Element,

    const Element = struct {
        name: []const u8,
        attributes: []const Attribute,
        children: []const Child,

        const Attribute = struct {
            name: []const u8,
            value: []const u8,
            quote: u8,
        };
        const Child = union(enum) {
            element: Element,
            text: []const u8,
            comment: []const u8,
        };

        fn getAttributeValue(elem: Element, name: []const u8) ?[]const u8 {
            for (elem.attributes) |attr| {
                if (!std.mem.eql(u8, attr.name, name)) continue;
                return attr.value;
            }
            return null;
        }

        fn getChildElement(elem: Element, name: []const u8) ?Element {
            for (elem.children) |child| {
                const child_elem: Element = switch (child) {
                    .element => |child_elem| child_elem,
                    .text => continue,
                    .comment => continue,
                };
                if (!std.mem.eql(u8, child_elem.name, name)) continue;
                return child_elem;
            }
            return null;
        }
    };

    fn deinit(tree: XmlTree, allocator: std.mem.Allocator) void {
        tree.arena_state.promote(allocator).deinit();
    }
};
fn parseXml(
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
    },
) !XmlTree {
    var lct_reader = lineColumnTrackingReader(xml_reader);
    errdefer {
        args.line.* = lct_reader.line;
        args.column.* = lct_reader.col;
    }

    const reader = lct_reader.reader();

    var arena = std.heap.ArenaAllocator.init(child_allocator);
    errdefer arena.deinit();
    const allocator = arena.allocator();

    var state: enum {
        start,
        left_angle_bracket,
        left_angle_bracket_bang,
        content,
    } = .start;

    var element_stack = try std.ArrayList(XmlTree.Element).initCapacity(child_allocator, 8);
    defer element_stack.deinit();

    var element_name_buf = try std.ArrayList(u8).initCapacity(child_allocator, 64);
    defer element_name_buf.deinit();

    var attr_data_buf = try std.ArrayList(u8).initCapacity(child_allocator, 128);
    defer attr_data_buf.deinit();

    var attributes_buf = try std.ArrayList(XmlTree.Element.Attribute).initCapacity(child_allocator, 8);
    defer attributes_buf.deinit();

    var content_buf = try std.ArrayList(u8).initCapacity(child_allocator, 128);
    defer content_buf.deinit();

    if (!try reader.isBytes(
        \\<?xml version="1.0" encoding="UTF-8"?>
        \\
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
                            const expected_cp = (readCodepoint(last_element_open_name_reader) catch unreachable) orelse break;
                            assert(isXmlNameChar(expected_cp));
                            const actual_cp = try ((try readCodepoint(reader)) orelse error.OpenAndCloseTagsDontMatch);
                            if (actual_cp != expected_cp) return error.OpenAndCloseTagsDontMatch;
                        }
                    }

                    var last_cp = try ((try readCodepoint(reader)) orelse error.EndOfStream);
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
                    const parent: *XmlTree.Element = &element_stack.items[element_stack.items.len - 1];
                    parent.children = try allocator.realloc(@constCast(parent.children), parent.children.len + 1);
                    @constCast(&parent.children[parent.children.len - 1]).* = .{
                        .element = last_element_open,
                    };
                    state = .content;
                    continue :mainloop;
                },
                else => {
                    if (!isXmlNameStartChar(iteration_cp)) return error.InvalidNode;

                    element_name_buf.shrinkRetainingCapacity(0);
                    _ = try writeCodepoint(element_name_buf.writer(), iteration_cp);

                    const elem_name_sentinel: u21 = while (try readCodepoint(reader)) |subsequent_cp| {
                        if (isXmlNameChar(subsequent_cp)) {
                            _ = try writeCodepoint(element_name_buf.writer(), subsequent_cp);
                            continue;
                        }
                        break subsequent_cp;
                    } else return error.EndOfStream;

                    attributes_buf.shrinkRetainingCapacity(0);

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
                                attr_value_start += try writeCodepoint(attr_data_buf.writer(), latest_cp);

                                while (try readCodepoint(reader)) |maybe_attr_name_char| {
                                    if (isXmlNameChar(maybe_attr_name_char)) {
                                        attr_value_start += try writeCodepoint(attr_data_buf.writer(), maybe_attr_name_char);
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

                                while (try readCodepoint(reader)) |str_cp| {
                                    if (str_cp == quote_char) break;
                                    _ = try writeCodepoint(attr_data_buf.writer(), str_cp);
                                } else return error.EndOfStream;

                                const attr_data_dupe = try allocator.dupe(u8, attr_data_buf.items);
                                try attributes_buf.append(XmlTree.Element.Attribute{
                                    .name = attr_data_dupe[0..attr_value_start],
                                    .value = attr_data_dupe[attr_value_start..],
                                    .quote = quote_char,
                                });
                                latest_cp = try ((try readCodepoint(reader)) orelse error.EndOfStream);
                            },
                            '/' => {
                                if (element_name_buf.items.len == 0) return error.ElementMissingName;
                                if (try readCodepoint(reader)) |last_element_cp| {
                                    try switch (last_element_cp) {
                                        '>' => {},
                                        else => error.ExpectedRightAngleBracketAfterSlash,
                                    };

                                    const new_elem = try makeElementWithSharedAllocBetweenNameAndAttributes(
                                        allocator,
                                        element_name_buf.items,
                                        attributes_buf.items,
                                    );

                                    // no parent in the stack to unwind,
                                    // so this is the only element.
                                    if (element_stack.items.len == 0) return .{
                                        .arena_state = arena.state,
                                        .root = new_elem,
                                    };

                                    const parent: *XmlTree.Element = &element_stack.items[element_stack.items.len - 1];
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
                                const new_elem = try makeElementWithSharedAllocBetweenNameAndAttributes(
                                    allocator,
                                    element_name_buf.items,
                                    attributes_buf.items,
                                );
                                try element_stack.append(new_elem);
                                state = .content;
                                continue :mainloop;
                            },
                        }
                    }
                },
            },
            .left_angle_bracket_bang => switch (iteration_cp) {
                '-' => switch (try ((try readCodepoint(reader)) orelse error.EndOfStream)) {
                    '-' => {
                        content_buf.shrinkRetainingCapacity(0);
                        while (true) {
                            const comment_cp = try ((try readCodepoint(reader)) orelse error.EndOfStream);
                            if (comment_cp != '-') {
                                if (!args.discard_comments) {
                                    _ = try writeCodepoint(content_buf.writer(), comment_cp);
                                }
                                continue;
                            }

                            const comment_cp_2 = try ((try readCodepoint(reader)) orelse error.EndOfStream);
                            if (comment_cp_2 != '-') {
                                if (!args.discard_comments) {
                                    _ = try writeCodepoint(content_buf.writer(), comment_cp);
                                    _ = try writeCodepoint(content_buf.writer(), comment_cp_2);
                                }
                                continue;
                            }

                            const comment_cp_3 = try ((try readCodepoint(reader)) orelse error.EndOfStream);
                            if (comment_cp_3 != '>') {
                                return error.DoubleDashInComment;
                            }

                            break;
                        } else unreachable;

                        if (!args.discard_comments) {
                            const parent: *XmlTree.Element = &element_stack.items[element_stack.items.len - 1];
                            parent.children = try allocator.realloc(@constCast(parent.children), parent.children.len + 1);
                            @constCast(&parent.children[parent.children.len - 1]).* = .{
                                .comment = try allocator.dupe(u8, content_buf.items),
                            };
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
                content_buf.shrinkRetainingCapacity(0);
                var non_whitespace: bool = false;

                var cdata_cp = iteration_cp;
                while (true) : (cdata_cp = try ((try readCodepoint(reader)) orelse error.EndOfStream)) {
                    switch (cdata_cp) {
                        '<' => {
                            if (content_buf.items.len != 0) blk: {
                                if (args.discard_whitespace and !non_whitespace) break :blk;
                                const parent: *XmlTree.Element = &element_stack.items[element_stack.items.len - 1];
                                parent.children = try allocator.realloc(@constCast(parent.children), parent.children.len + 1);
                                @constCast(&parent.children[parent.children.len - 1]).* = .{
                                    .text = try allocator.dupe(u8, content_buf.items),
                                };
                            }
                            state = .left_angle_bracket;
                            continue :mainloop;
                        },
                        else => {
                            non_whitespace = non_whitespace or !isXmlWhiteSpaceChar(cdata_cp);
                            _ = try writeCodepoint(content_buf.writer(), cdata_cp);
                        },
                    }
                } else unreachable;
            },
        }
    } else return error.EndOfStream;
}
fn makeElementWithSharedAllocBetweenNameAndAttributes(
    allocator: std.mem.Allocator,
    name: []const u8,
    attributes: []const XmlTree.Element.Attribute,
) std.mem.Allocator.Error!XmlTree.Element {
    const Attribute = XmlTree.Element.Attribute;
    const elem_data_buf = try allocator.alignedAlloc(
        u8,
        @alignOf(Attribute),
        // zig fmt: off
            (@sizeOf(Attribute) * attributes.len) +
            (@sizeOf(u8)        * name.len),
        // zig fmt: on
    );
    errdefer allocator.free(elem_data_buf);

    const attributes_slice: []Attribute = std.mem.bytesAsSlice(Attribute, elem_data_buf[0 .. @sizeOf(Attribute) * attributes.len]);
    assert(attributes_slice.len == attributes.len);
    std.mem.copy(Attribute, attributes_slice, attributes);

    const name_slice: []u8 = elem_data_buf[attributes_slice.len * @sizeOf(Attribute) ..];
    assert(name_slice.len == name.len);
    std.mem.copy(u8, name_slice, name);

    return XmlTree.Element{
        .name = name_slice,
        .attributes = attributes_slice,
        .children = &[_]XmlTree.Element.Child{},
    };
}

inline fn readByteOrNull(reader: anytype) @TypeOf(reader).Error!?u8 {
    var byte: [1]u8 = undefined;
    const bytes_read = try reader.read(&byte);
    return switch (bytes_read) {
        0 => null,
        1 => @bitCast(u8, byte),
        else => unreachable,
    };
}

/// Read a codepoint from the stream, or return null
/// if the stream ends before returning the full codepoint.
fn readCodepoint(reader: anytype) !?u21 {
    const start_byte: u8 = (try readByteOrNull(reader)) orelse return null;
    switch (try std.unicode.utf8ByteSequenceLength(start_byte)) {
        1 => return start_byte,
        inline 2, 3, 4 => |cp_len| {
            var cp_bytes: std.BoundedArray(u8, cp_len) = .{};
            cp_bytes.appendAssumeCapacity(start_byte);
            try reader.readIntoBoundedBytes(cp_len, &cp_bytes);
            if (cp_bytes.len < cp_len) {
                return null;
            }
            assert(cp_bytes.len == cp_len);
            const decodeFunc = comptime switch (cp_len) {
                2 => std.unicode.utf8Decode2,
                3 => std.unicode.utf8Decode3,
                4 => std.unicode.utf8Decode4,
                else => unreachable,
            };
            return try decodeFunc(cp_bytes.constSlice());
        },
        else => unreachable,
    }
}

/// Encode and write the codepoint to the stream.
/// Returns the number of bytes written to the stream.
fn writeCodepoint(writer: anytype, codepoint: u21) @TypeOf(writer).Error!u3 {
    var buf: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(codepoint, &buf) catch |err| switch (err) {
        error.Utf8CannotEncodeSurrogateHalf,
        error.CodepointTooLarge,
        => std.unicode.utf8Encode(std.unicode.replacement_character, &buf) catch unreachable,
    };
    try writer.writeAll(buf[0..len]);
    return len;
}

/// Skips codepoints considered whitespace in XML, returning
/// the first non-whitespace codepoint encountered (or null)
/// if the stream ends without returning a non-whitespace
/// codepoint.
fn nextNonXmlWhitespaceChar(reader: anytype) !?u21 {
    while (try readCodepoint(reader)) |cp| {
        if (!isXmlWhiteSpaceChar(cp)) return cp;
    }
    return null;
}

fn expectCodepoints(reader: anytype, codepoints: []const u21) !bool {
    for (codepoints) |expected_cp| {
        const actual_cp = (try readCodepoint(reader)) orelse false;
        if (actual_cp != expected_cp) return false;
    }
    return true;
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

fn RemoveConst(comptime Ptr: type) type {
    var new_info = @typeInfo(Ptr).Pointer;
    new_info.is_const = false;
    return @Type(.{ .Pointer = new_info });
}

inline fn lineColumnTrackingReader(reader: anytype) LineColumnTrackingReader(@TypeOf(reader)) {
    return .{
        .inner = reader,
    };
}
fn LineColumnTrackingReader(comptime ReaderType: type) type {
    return struct {
        const Self = @This();
        inner: Inner,
        line: u64 = 0,
        col: u64 = 0,
        ignore_linefeed: bool = true,

        pub const Inner = ReaderType;

        pub const Reader = std.io.Reader(*Self, Inner.Error, Self.read);
        pub fn reader(self: *Self) Reader {
            return .{ .context = self };
        }

        fn read(self: *Self, buffer: []u8) Inner.Error!usize {
            const bytes_read = try self.inner.read(buffer);
            if (bytes_read == 0) return 0;

            for (buffer[0..bytes_read]) |byte| {
                switch (byte) {
                    '\n' => {
                        self.line += 1;
                        self.col = 0;
                    },
                    else => {
                        if (self.ignore_linefeed and byte == '\r') continue;
                        self.col += 1;
                    },
                }
            }

            return bytes_read;
        }
    };
}

const FallbackBufferAllocator = struct {
    fba: std.heap.FixedBufferAllocator,
    fallback_allocator: std.mem.Allocator,

    pub fn init(buffer: []u8, fallback: std.mem.Allocator) FallbackBufferAllocator {
        return .{
            .fba = std.heap.FixedBufferAllocator.init(buffer),
            .fallback_allocator = fallback,
        };
    }

    pub inline fn allocator(self: *FallbackBufferAllocator) std.mem.Allocator {
        return std.mem.Allocator{
            .ptr = self,
            .vtable = comptime &std.mem.Allocator.VTable{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }

    fn alloc(ptr: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self = @ptrCast(*align(1) FallbackBufferAllocator, ptr);
        return self.fba.allocator().rawAlloc(len, ptr_align, ret_addr) orelse
            self.fallback_allocator.rawAlloc(len, ptr_align, ret_addr);
    }

    fn resize(ptr: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self = @ptrCast(*align(1) FallbackBufferAllocator, ptr);
        if (self.fba.ownsPtr(buf.ptr)) {
            return self.fba.allocator().rawResize(buf, buf_align, new_len, ret_addr);
        } else {
            return self.fallback_allocator.rawResize(buf, buf_align, new_len, ret_addr);
        }
    }

    fn free(ptr: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self = @ptrCast(*align(1) FallbackBufferAllocator, ptr);
        if (self.fba.ownsPtr(buf.ptr)) {
            return self.fba.allocator().rawFree(buf, buf_align, ret_addr);
        } else {
            return self.fallback_allocator.rawFree(buf, buf_align, ret_addr);
        }
    }
};
