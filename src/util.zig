const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;

pub inline fn lineColumnTrackingReader(reader: anytype) LineColumnTrackingReader(@TypeOf(reader)) {
    return .{
        .inner = reader,
    };
}
pub fn LineColumnTrackingReader(comptime ReaderType: type) type {
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
                        self.col += @boolToInt(!(self.ignore_linefeed and byte == '\r'));
                    },
                }
            }

            return bytes_read;
        }
    };
}

pub inline fn readByteOrNull(reader: anytype) @TypeOf(reader).Error!?u8 {
    var byte: u8 = undefined;
    const bytes_read = try reader.read(@as(*[1]u8, &byte));
    return switch (bytes_read) {
        0 => null,
        1 => byte,
        else => unreachable,
    };
}

/// Read a codepoint from the stream, or return null
/// if the stream ends before returning the full codepoint.
pub fn readCodepointUtf8(reader: anytype) !?u21 {
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
pub fn writeCodepointUtf8(writer: anytype, codepoint: u21) @TypeOf(writer).Error!u3 {
    var buf: [4]u8 = undefined;
    const len = std.unicode.utf8Encode(codepoint, &buf) catch |err| switch (err) {
        error.Utf8CannotEncodeSurrogateHalf,
        error.CodepointTooLarge,
        => std.unicode.utf8Encode(std.unicode.replacement_character, &buf) catch unreachable,
    };
    try writer.writeAll(buf[0..len]);
    return len;
}

pub fn fmtMultiLineList(
    list: anytype,
    args: anytype,
) FmtMultiLineList(std.meta.Elem(@TypeOf(list))) {
    var formatter = FmtMultiLineList(std.meta.Elem(@TypeOf(list))){
        .list = list,
    };
    inline for (@typeInfo(@TypeOf(args)).Struct.fields) |field| {
        comptime assert(!std.mem.eql(u8, field.name, "list"));
        @field(formatter, field.name) = @field(args, field.name);
    }
    return formatter;
}

pub fn FmtMultiLineList(comptime T: type) type {
    return struct {
        const Self = @This();
        list: []align(1) const T,

        indent_level: u32 = 0,
        indent_first_elem: bool = true,

        indent_prefix: []const u8 = "",
        indent: []const u8 = " " ** 4,
        indent_suffix: []const u8 = "",

        element_prefix: []const u8 = "",
        element_suffix: []const u8 = ",",

        final_newline: bool = true,
        newline: []const u8 = "\n",

        pub fn format(
            self: Self,
            comptime fmt_str: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            if (self.indent_first_elem) {
                try self.writeIndent(writer);
            }
            for (self.list, 0..) |value, i| {
                if (i != 0) {
                    try writer.writeAll(self.newline);
                    try self.writeIndent(writer);
                }
                try writer.writeAll(self.element_prefix);
                try std.fmt.formatType(
                    value,
                    fmt_str,
                    options,
                    writer,
                    std.fmt.default_max_depth,
                );
                try writer.writeAll(self.element_suffix);
            }
            if (self.final_newline) {
                try writer.writeAll(self.newline);
            }
        }

        fn writeIndent(self: Self, writer: anytype) !void {
            try writer.writeAll(self.indent_prefix);
            for (0..self.indent_level) |_| try writer.writeAll(self.indent);
            try writer.writeAll(self.indent_suffix);
        }
    };
}

pub inline fn fmtGlobalZigId(string: []const u8) FmtGlobalZigId {
    return .{ .string = string };
}
pub const FmtGlobalZigId = struct {
    string: []const u8,

    pub fn format(
        self: FmtGlobalZigId,
        comptime fmt_str: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt_str;
        _ = options;
        const must_escape =
            !std.zig.isValidId(self.string) or
            std.zig.primitives.isPrimitive(self.string);
        if (!must_escape)
            try writer.writeAll(self.string)
        else
            try writer.print(
                \\@"{s}"
            , .{self.string});
    }
};

pub inline fn lowerStringArrayList(
    output: *std.ArrayList(u8),
    ascii_string: []const u8,
) std.mem.Allocator.Error![]u8 {
    try output.resize(ascii_string.len);
    return std.ascii.lowerString(output.items, ascii_string);
}

pub fn OneImplicitDeref(comptime T: type) type {
    switch (@typeInfo(T)) {
        .Pointer => |pointer| switch (pointer.size) {
            .One => return pointer.child,
            else => {},
        },
        else => {},
    }
    return T;
}

pub fn AnyHashMapFieldContext(
    comptime S: type,
    comptime field_id: std.meta.FieldEnum(OneImplicitDeref(S)),
    comptime InnerContext: type,
    comptime Hash: type,
    comptime is_array: bool,
) type {
    return struct {
        const Self = @This();
        inner: InnerContext,

        pub const Adapted = AnyHashMapFieldContextAdapted(S, field_id, InnerContext, Hash, is_array);
        comptime {
            _ = Adapted;
        }

        pub fn hash(self: Self, key: S) Hash {
            const key_field = @field(key, @tagName(field_id));
            return Adapted.hash(.{ .inner = self.inner }, key_field);
        }
        pub const eql = (if (is_array) struct {
            fn eql(self: Self, a: S, b: S, b_index: usize) bool {
                const a_field = @field(a, @tagName(field_id));
                return Adapted.eql(.{ .inner = self.inner }, a_field, b, b_index);
            }
        } else struct {
            fn eql(self: Self, a: S, b: S) bool {
                const a_field = @field(a, @tagName(field_id));
                return Adapted.eql(.{ .inner = self.inner }, a_field, b);
            }
        }).eql;
    };
}

fn AnyHashMapFieldContextAdapted(
    comptime S: type,
    comptime field_id: std.meta.FieldEnum(OneImplicitDeref(S)),
    comptime InnerContext: type,
    comptime Hash: type,
    comptime is_array: bool,
) type {
    return struct {
        const Self = @This();
        inner: InnerContext,

        const Field = std.meta.FieldType(OneImplicitDeref(S), field_id);
        comptime {
            std.hash_map.verifyContext(InnerContext, Field, Field, Hash, is_array);
        }

        pub fn hash(self: Self, key_field: Field) Hash {
            return self.inner.hash(key_field);
        }
        pub const eql = (if (is_array) struct {
            fn eql(self: Self, a_field: Field, b: S, b_index: usize) bool {
                const b_field = @field(b, @tagName(field_id));
                return self.inner.eql(a_field, b_field, b_index);
            }
        } else struct {
            fn eql(self: Self, a_field: Field, b: S) bool {
                const b_field = @field(b, @tagName(field_id));
                return self.inner.eql(a_field, b_field);
            }
        }).eql;
    };
}
