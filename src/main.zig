const std = @import("std");
const streams = @import("streams.zig");
const CharacterStream = streams.CharacterStream;
const TokenStream = streams.TokenStream;
const StringTokenStream = streams.StringTokenStream;

const Symbol = enum(usize) { _ };
const Expr = union(enum) {
    /// evaulates to the result of calling the function with the args
    call: Call,
    /// evaluates to the literal value
    literal: Literal,
    /// evaluates enter, next, then exit.
    /// enter/exit must return void. returns
    /// the result of evaluating next.
    /// note: could be fun if the return value
    /// of enter could be passed to exit somehow,
    /// to allow for const a = 5; defer something(a)
    /// as just 5, defer(#) or something
    block: Block,
    /// evaluates the contained expression,
    /// potential for early exit
    label: Label,
    /// evaluates the expresisons in order,
    /// returns an array of results
    array: Array,
    const Call = struct {
        method: *Expr,
        arg: *Expr,
    };
    const Literal = union(enum) {
        symbol: Symbol,
        number: []const u8,
        string: []const u8,
        builtin: []const u8,
    };
    const Builtin = struct {
        name: []const u8,
    };
    const Block = struct {
        enter: ?*Expr, // call exit with the return value of enter
        next: ?*Expr, // if there is no next, return void
        exit: ?*Expr,
    };
    const Label = struct {
        tag: Symbol, // for early exits
        expr: *Expr,
    };
    const Array = struct {
        items: []*Expr,
    };
};

fn allocDupe(value: anytype) *@TypeOf(value) {
    var res = global_allocator.?.create(@TypeOf(value)) catch @panic("oom");
    res.* = value;
    return res;
}

const Penv = struct {
    hm: std.StringHashMap(Symbol),
    start_id: usize,
    pub fn new() *Penv {
        return allocDupe(Penv{
            .hm = std.StringHashMap(Symbol).init(global_allocator.?),
            .start_id = 0,
        });
    }
    pub fn dupe(penv: Penv) *Penv {
        var dupe_hm = std.StringHashMap(Symbol).init(global_allocator.?);
        var iter = penv.hm.iterator();
        while (iter.next()) |kv| {
            dupe_hm.put(kv.key, kv.value) catch @panic("oom");
        }

        return allocDupe(Penv{
            .hm = dupe_hm,
            .start_id = penv.start_id,
        });
    }
    pub fn getsymbol(penv: Penv, name: []const u8) ?Symbol {
        return penv.hm.get(name);
    }
    pub fn defsymbol(penv: *Penv, name: []const u8) Symbol {
        const res = @intToEnum(Symbol, penv.start_id);
        if (penv.hm.fetchPut(name, res) catch @panic("oom")) |_| {
            @panic("TODO error");
        }
        penv.start_id += 1;
        return res;
    }
};

pub fn parse(penv: Penv, characters: *CharacterStream) *Expr {
    const reload = characters.save();
    //errdefer reload.call();

    const tokens = TokenStream.wrap(characters);

    std.log.info("Parsing... {s}", .{blk: {
        const rl2 = characters.save();
        defer rl2.call();
        break :blk tokens.next(.gap);
    }});
    if (tokens.eat(.gap, "block")) {
        const lcurly = tokens.eat(.gap, "{");
        // 'block' :enter ('defer' :exit)? ';' :next

        const enter_expr = parse(penv, characters);
        const defer_expr: ?*Expr = if (tokens.eat(.gap, "defer")) ( //
            parse(penv, characters) //
        ) else null;
        if (!tokens.eat(.gap, ";")) {
            std.log.err("got bad '{s}'", .{tokens.next(.gap)});
            @panic("todo error");
        }
        const next_expr = parse(penv, characters);

        return allocDupe(Expr{ .block = .{
            .enter = enter_expr,
            .next = next_expr,
            .exit = defer_expr,
        } });
    } else if (tokens.eat(.gap, "call")) {
        const method = parse(penv, characters);
        const arg = parse(penv, characters);
        return allocDupe(Expr{ .call = .{
            .method = method,
            .arg = arg,
        } });
    } else if (tokens.eat(.gap, "array")) {
        if (!tokens.eat(.gap, "[")) @panic("TODO error");
        var res_exprs = std.ArrayList(*Expr).init(global_allocator.?);
        while (!tokens.eat(.gap, "]")) {
            res_exprs.append(parse(penv, characters)) catch @panic("oom");
        }
        return allocDupe(Expr{ .array = .{
            .items = res_exprs.toOwnedSlice(),
        } });
    } else if (tokens.eat(.gap, "defsymbol")) {
        // define a symbol within the current scope
        if (!tokens.eat(.gap, "#")) @panic("TODO error");
        const symname = tokens.next(.no_gap) orelse @panic("TODO error");
        if (!tokens.eat(.gap, ";")) @panic("TODO error");

        const dupe = penv.dupe();
        _ = dupe.defsymbol(symname);
        return parse(dupe.*, characters);
    } else if (tokens.eat(.gap, "#")) {
        // this is why it would be good to have eg tokens.next(.no_gap, .identifier) because uuh
        // defsymbol #"; is completely valid code otherwise
        const symname = tokens.next(.no_gap) orelse @panic("TODO error");

        const symbol = penv.getsymbol(symname) orelse {
            std.log.err("symbol not defined #{s}", .{symname});
            @panic("symbol not defined");
        };
        return allocDupe(Expr{
            .literal = .{ .symbol = symbol },
        });
    } else if (tokens.eat(.gap, "@")) {
        const builtin_name = tokens.next(.no_gap) orelse @panic("todo Error");
        return allocDupe(Expr{
            .literal = .{ .builtin = builtin_name },
        });
    } else if (tokens.eat(.gap, "void")) {
        return allocDupe(Expr{
            .block = .{ .enter = null, .next = null, .exit = null },
        });
    } else if (tokens.eat(.gap, "\"")) {
        var res_str = std.ArrayList(u8).init(global_allocator.?);
        const sts = StringTokenStream.wrap('"', characters);
        while (true) {
            switch (sts.next()) {
                .chunk => |char| res_str.append(char) catch @panic("oom"),
                .escape_start => @panic("TODO string escapes"),
                .end => break,
            }
        }
        return allocDupe(Expr{
            .literal = .{ .string = res_str.toOwnedSlice() },
        });
    } else {
        std.log.err("unexpected {s}", .{tokens.next(.gap)});
        @panic("TODO error");
    }
}

pub fn printExpr(out: anytype, expr: Expr) void {
    switch (expr) {
        .call => |call| {
            out.writeAll("call(") catch @panic("bad");
            printExpr(out, call.method.*);
            out.writeAll(", ") catch @panic("bad");
            printExpr(out, call.arg.*);
            out.writeAll(")") catch @panic("bad");
        },
        .literal => |l| switch (l) {
            .string => |str| out.print("\"{s}\"", .{str}) catch @panic("bad"),
            .number => |num| out.print("{s}", .{num}) catch @panic("bad"),
            .symbol => |sym| out.print("{}", .{sym}) catch @panic("bad"),
            .builtin => |builtin| out.print("@{s}", .{builtin}) catch @panic("bad"),
        },
        .block => |block| {
            out.writeAll("block") catch @panic("bad");
            if (block.enter) |enter| {
                out.writeAll(" ") catch @panic("bad");
                printExpr(out, enter.*);
            }
            if (block.exit) |exit| {
                out.writeAll(" defer ") catch @panic("bad");
                printExpr(out, exit.*);
            }
            if (block.next) |next| {
                out.writeAll(",\n") catch @panic("bad");
                printExpr(out, next.*);
            } else {
                out.writeAll(";") catch @panic("bad");
            }
        },
        .label => @panic("TODO"),
        .array => |array| {
            out.writeAll("[") catch @panic("oom");
            for (array.items) |item, i| {
                if (i != 0) out.writeAll(", ") catch @panic("oom");
                printExpr(out, item.*);
            }
            out.writeAll("]") catch @panic("oom");
        },
    }
}

threadlocal var global_allocator: ?*std.mem.Allocator = null;

pub fn main() anyerror!void {
    global_allocator = std.heap.page_allocator;
    var stream = CharacterStream{
        .index = 0,
        .text = @embedFile("sample2.resyn"),
    };
    const out = std.io.getStdOut().writer();
    printExpr(out, parse(Penv.new().*, &stream).*);
}
