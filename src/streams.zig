const std = @import("std");

pub const ReloadToken = struct {
    stream: *CharacterStream,
    index: usize,
    pub fn call(rt: ReloadToken) void {
        rt.stream.index = rt.index;
    }
};
pub const CharacterStream = struct {
    text: []const u8,
    index: usize,

    /// saves the position in the stream
    pub fn save(stream: *CharacterStream) ReloadToken {
        return .{ .stream = stream, .index = stream.index };
    }
    pub fn peek(stream: *CharacterStream) ?u8 {
        if (stream.index < stream.text.len) {
            return stream.text[stream.index];
        }
        return null;
    }
    pub fn undo(stream: *CharacterStream) void {
        stream.index -= 1;
    }
    pub fn next(stream: *CharacterStream) ?u8 {
        if (stream.peek()) |res| {
            stream.index += 1;
            return res;
        }
        return null;
    }

    pub fn startsWith(stream: *CharacterStream, text: []const u8) bool {
        return std.mem.startsWith(u8, stream.text[stream.index..], text);
    }
    pub fn startsWithTake(stream: *CharacterStream, text: []const u8) bool {
        if (stream.startsWith(text)) {
            stream.index += text.len;
            return true;
        }
        return false;
    }
};

pub const StringTokenStream = struct {
    stream: *CharacterStream,
    end_char: u8,
    pub fn wrap(end_char: u8, cs: *CharacterStream) StringTokenStream {
        return .{ .end_char = end_char, .stream = cs };
    }
    pub const StringToken = union(enum) {
        chunk: u8, // todo should probably be []const u8
        escape_start,
        end,
    };
    pub fn next(sts: StringTokenStream) StringToken {
        switch (sts.stream.next() orelse @panic("unexpected eof in str")) {
            '\n' => @panic("unexpected newline in str"),
            '\\' => @panic("TODO string escapes"),
            else => |char| {
                if (char == sts.end_char) return .end;
                return .{ .chunk = char };
            },
        }
    }
};

pub const TokenStream = struct {
    stream: *CharacterStream,
    pub fn wrap(cs: *CharacterStream) TokenStream {
        return .{ .stream = cs };
    }
    pub fn save(ts: TokenStream) ReloadToken {
        return ts.stream.save();
    }
    fn eatComment(ts: TokenStream) void {
        while (ts.stream.next()) |char| switch (char) {
            '\n' => break,
            else => {},
        };
    }
    fn eatSpaces(ts: TokenStream) void {
        while (ts.stream.next()) |char| switch (char) {
            ' ', '\r', '\n', '\t' => {},
            '/' => {
                ts.stream.undo();
                if (ts.stream.startsWithTake("//")) {
                    ts.eatComment();
                }
            },
            else => {
                ts.stream.undo();
                break;
            },
        };
    }
    pub const Gap = enum { gap, no_gap, req_gap };
    pub fn next(ts: TokenStream, gap: Gap) ?[]const u8 {
        const reload = ts.save();
        if (gap == .gap) ts.eatSpaces();
        if (gap == .req_gap and reload.index == ts.stream.index) {
            reload.call();
            @panic("todo error");
        }

        const start = ts.save();

        const char = ts.stream.next() orelse {
            reload.call();
            return null;
        };
        switch (char) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                while (ts.stream.next()) |c2| switch (c2) {
                    'a'...'z', 'A'...'Z', '0'...'9' => {},
                    else => {
                        ts.stream.undo();
                        break;
                    },
                };
            },
            '{', '}', '(', ')', '[', ']', '#', '@', ',', ';', '"' => {
                // do nothing
            },
            else => |bad| {
                reload.call();
                std.log.err("got '{c}'", .{bad});
                @panic("todo error");
            },
        }
        return ts.stream.text[start.index..ts.stream.index];
    }
    pub fn eat(ts: TokenStream, gap: Gap, expected: []const u8) bool {
        const reload = ts.save();
        if (ts.next(gap)) |value| {
            if (std.mem.eql(u8, value, expected)) {
                return true;
            }
            reload.call();
        }
        return false;
    }
};
