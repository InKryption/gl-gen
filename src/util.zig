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

/// Returns a stream that reads out the bytes that result when
/// formatting the given arguments with the format string.
/// The stream performs no allocations and is safe to copy by value.
pub fn formatReader(comptime fmt_str: []const u8, args: anytype) FormatReader(fmt_str, @TypeOf(args)) {
    return FormatReader(fmt_str, @TypeOf(args)).init(args);
}
pub fn FormatReader(
    comptime fmt_str: anytype,
    comptime ArgsTuple: type,
) type {
    if (@typeInfo(@TypeOf(fmt_str)) == .Pointer) return FormatReader(fmt_str[0..].*, ArgsTuple);
    if (@TypeOf(fmt_str) != [fmt_str.len]u8) return FormatReader(@as([fmt_str.len]u8, fmt_str), ArgsTuple);
    assert(@TypeOf(fmt_str) == [fmt_str.len]u8);

    return struct {
        const Self = @This();
        args: ArgsTuple,
        total_bytes: u64,
        bytes_read: u64 = 0,

        pub inline fn init(args: ArgsTuple) Self {
            return .{
                .args = args,
                .total_bytes = std.fmt.count(&fmt_str, args),
            };
        }

        pub const Reader = std.io.Reader(*Self, ReadError, Self.read);
        pub fn reader(self: *Self) Reader {
            return .{ .context = self };
        }

        pub const ReadError = error{};
        fn read(self: *Self, buffer: []u8) ReadError!usize {
            if (buffer.len == 0) return 0;
            if (self.bytes_read == self.total_bytes) return 0;

            const amt = @min(buffer.len, self.total_bytes - self.bytes_read);

            var fbs = std.io.fixedBufferStream(buffer[0..amt]);
            const fbw = fbs.writer();

            var limited_discard_stream = limitedDiscardWriter(fbw, self.bytes_read);
            const lds = limited_discard_stream.writer();

            lds.print(&fmt_str, self.args) catch |err| switch (err) {
                error.NoSpaceLeft => assert(fbs.pos == amt),
            };
            self.bytes_read += fbs.pos;
            return amt;
        }
    };
}

test formatReader {
    var fmt_stream = formatReader("{s} {s} 0x{x}", .{ "foo", "bar", 0xdeadbeef });
    const fmt_reader = fmt_stream.reader();

    const expected = try std.fmt.allocPrint(std.testing.allocator, "{s} {s} 0x{x}", .{ "foo", "bar", 0xdeadbeef });
    defer std.testing.allocator.free(expected);

    try std.testing.expect(fmt_reader.isBytes(expected) catch false);

    fmt_stream = formatReader("{s} {s} 0x{x}", .{ "foo", "bar", 0xdeadbeef });
    {
        const actual = try fmt_reader.readAllAlloc(std.testing.allocator, 128);
        defer std.testing.allocator.free(actual);
        try std.testing.expectEqualStrings(expected, actual);
    }
}

/// Returns a writer that is guaranteed to successfully "write" `count` bytes,
/// but without ever actually writing them to the underlying stream. After secretly
/// discarding `count` bytes, it will then go on to write any more bytes to the
/// given inner `writer` stream.
pub fn limitedDiscardWriter(writer: anytype, count: u64) LimitedDiscardWriter(@TypeOf(writer)) {
    return LimitedDiscardWriter(@TypeOf(writer)).init(writer, count);
}
pub fn LimitedDiscardWriter(comptime WriterType: type) type {
    return struct {
        const Self = @This();
        inner: Inner,
        ignore_count: u64,

        pub const Inner = WriterType;
        pub fn init(inner: Inner, count: u64) Self {
            return .{
                .inner = inner,
                .ignore_count = count,
            };
        }

        pub const Writer = std.io.Writer(*Self, Inner.Error, Self.write);
        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        fn write(self: *Self, bytes: []const u8) Inner.Error!usize {
            if (self.ignore_count >= bytes.len) {
                self.ignore_count -= bytes.len;
                return bytes.len;
            }
            if (self.ignore_count > 0) {
                const ignored = self.ignore_count;
                self.ignore_count = 0;
                return ignored;
            }
            return self.inner.write(bytes);
        }
    };
}

test limitedDiscardWriter {
    var buffer: [0]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buffer);

    var limited_discard_stream = limitedDiscardWriter(fbs.writer(), 32);
    const lds_writer = limited_discard_stream.writer();

    try std.testing.expectEqual(@as(error{NoSpaceLeft}!void, {}), lds_writer.writeAll(" " ** 4));
    try std.testing.expectEqual(@as(error{NoSpaceLeft}!usize, 0), lds_writer.write(""));
    try std.testing.expectEqual(@as(error{NoSpaceLeft}!usize, 4), lds_writer.write(" " ** 4));
    try std.testing.expectEqual(@as(error{NoSpaceLeft}!usize, 8), lds_writer.write(" " ** 8));
    try std.testing.expectEqual(@as(error{NoSpaceLeft}!usize, 16), lds_writer.write(" " ** 16));
    try std.testing.expectEqual(@as(error{NoSpaceLeft}!usize, 0), lds_writer.write(""));
    try std.testing.expectError(error.NoSpaceLeft, lds_writer.writeByte(' '));
    try std.testing.expectError(error.NoSpaceLeft, lds_writer.writeByte(' '));
}

pub fn fmtLiteral(
    value: anytype,
    options: struct {
        indent: u64 = 0,
    },
) FmtLiteral(@TypeOf(value)) {
    return .{
        .value = value,
        .indent = options.indent,
    };
}
pub fn FmtLiteral(comptime T: type) type {
    if (@typeInfo(T) == .ErrorSet) return FmtLiteral(anyerror);
    return struct {
        const Self = @This();
        value: T,
        indent: u64,

        pub fn format(
            self: Self,
            comptime fmt_str: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            _ = options;
            _ = fmt_str;
            switch (@typeInfo(T)) {
                .Type => @compileError("Can't print a type literal.\n"),
                .Void => try writer.writeAll("void{}"),
                .Bool => try writer.writeAll(if (self.value) "true" else "false"),
                .NoReturn => @compileError("Can't print a noreturn literal.\n"),
                .Int, .Float, .ComptimeInt, .ComptimeFloat => {
                    try writer.print("@as({s}, ", .{@typeName(T)});
                    try std.fmt.formatType(self.value, "", .{}, writer, undefined);
                    try writer.writeAll(")");
                },
                .Pointer => |pointer| {
                    if (!pointer.is_const) {
                        @compileError("Can't write a mutable pointer literal.\n");
                    }
                    try writer.writeAll("&");
                    switch (pointer.size) {
                        .One => {
                            try writer.print("{}", .{fmtLiteral(self.value.*, .{ .indent = self.indent + 1 })});
                        },
                        .Slice => {
                            if (typeNameRequiresNoContext(pointer.child)) {
                                try writer.print("[_]{s}{{", .{@typeName(pointer.child)});
                            } else {
                                try writer.writeAll(".{");
                            }
                            if (self.value.len != 0) {
                                try writer.writeAll("\n");
                                for (self.value) |elem| {
                                    try writer.writeByteNTimes(' ', (self.indent + 1) * 4);
                                    try writer.print("{},\n", .{fmtLiteral(elem, .{ .indent = self.indent + 1 })});
                                }
                                try writer.writeByteNTimes(' ', (self.indent + 0) * 4);
                            }
                            try writer.writeAll("}");
                        },
                        .Many => {
                            const sentinel = @ptrCast(*align(1) pointer.child, pointer.sentinel orelse @compileError("Can't print a non-sentinel terminated many item pointer literal")).*;
                            try writer.print("{}", .{fmtLiteral(std.mem.sliceTo(self.value, sentinel), .{ .indent = self.indent })});
                        },
                        .C => @compileError("C pointer literals aren't supported.\n"),
                    }
                },
                .Array => |array| {
                    if (typeNameRequiresNoContext(array.child)) {
                        try writer.print("[_]{s}{{", .{@typeName(array.child)});
                    } else {
                        try writer.writeAll(".{");
                    }
                    if (self.value.len != 0) {
                        try writer.writeAll("\n");
                        for (self.value) |elem| {
                            try writer.writeByteNTimes(' ', (self.indent + 1) * 4);
                            try writer.print("{},\n", .{fmtLiteral(elem, .{ .indent = self.indent + 1 })});
                        }
                        try writer.writeByteNTimes(' ', (self.indent + 0) * 4);
                    }
                    try writer.writeAll("}");
                },
                .Struct => |structure| {
                    try writer.writeAll(".{");
                    if (structure.fields.len != 0) {
                        try writer.writeAll("\n");
                        inline for (structure.fields) |field| {
                            const field_value = @field(self.value, field.name);

                            try writer.writeByteNTimes(' ', (self.indent + 1) * 4);
                            if (comptime structure.is_tuple) {
                                try writer.print("{},\n", .{fmtLiteral(field_value, .{ .indent = self.indent + 1 })});
                            } else {
                                try writer.print(".{s} = {},\n", .{ field.name, fmtLiteral(field_value, .{ .indent = self.indent + 1 }) });
                            }
                        }
                        try writer.writeByteNTimes(' ', (self.indent + 0) * 4);
                    }
                    try writer.writeAll("}");
                },
                .Undefined => try writer.writeAll("undefined"),
                .Null => try writer.writeAll("null"),
                .Optional => {
                    try if (self.value) |value|
                        writer.print("{}", .{fmtLiteral(value, .{ .indent = self.indent })})
                    else
                        writer.print("{}", .{fmtLiteral(null, .{ .indent = self.indent })});
                },
                .ErrorUnion => {
                    try if (self.value) |value|
                        writer.print("{}", .{fmtLiteral(value, .{ .indent = self.indent })})
                    else |err|
                        writer.print("{}", .{fmtLiteral(err, .{ .indent = self.indent })});
                },
                .ErrorSet => {
                    try writer.print("error.{s}", .{self.value});
                },
                .Enum => try writer.print(".{s}", .{@tagName(self.value)}),
                .Union => |@"union"| {
                    if (@"union".tag_type == null) {
                        @compileError("Can't print untagged union literal.\n");
                    }
                    switch (self.value) {
                        inline else => |value, tag| {
                            try writer.print(".{{ .{s} = {} }}", .{ @tagName(tag), fmtLiteral(value, .{ .indent = self.indent }) });
                        },
                    }
                },
                .Fn => @compileError("Can't print function literal.\n"),
                .Opaque => @compileError("Can't print opaque literal.\n"),
                .Frame => @compileError("Can't print frame literal.\n"),
                .AnyFrame => @compileError("Can't print anyframe literal.\n"),
                .Vector => |vector| {
                    try writer.print("", .{fmtLiteral(@as([vector.len]vector.child, self.value), .{ .indent = self.indent })});
                },
                .EnumLiteral => try writer.writeAll("." ++ @tagName(self.value)),
            }
        }
    };
}

inline fn typeNameRequiresNoContext(comptime T: type) bool {
    comptime return switch (@typeInfo(T)) {
        .Type,
        .Void,
        .Bool,
        .NoReturn,
        .Int,
        .Float,
        .ComptimeFloat,
        .ComptimeInt,
        .Undefined,
        .Null,
        .ErrorSet,
        .AnyFrame,
        .EnumLiteral,
        => true,

        .Enum,
        .Union,
        .Frame,
        => false,

        .Pointer => |pointer| typeNameRequiresNoContext(pointer.child),
        .Array => |array| typeNameRequiresNoContext(array.child),
        .Struct => |structure| structure.is_tuple and for (structure.fields) |field| {
            if (!typeNameRequiresNoContext(field.type)) break false;
        } else true,
        .Optional => |optional| typeNameRequiresNoContext(optional.child),
        .ErrorUnion => |error_union| typeNameRequiresNoContext(error_union.error_set) and typeNameRequiresNoContext(error_union.payload),
        .Fn => |function| blk: {
            std.builtin.Type.Fn;
            if (function.is_generic) break :blk false;
            for (function.params) |param| {
                const ParamType = param.type orelse break :blk false;
                if (!typeNameRequiresNoContext(ParamType)) break :blk false;
            }
            break :blk true;
        },
        .Opaque => T == anyopaque,
        .Vector => |vector| typeNameRequiresNoContext(vector.child),
    };
}
