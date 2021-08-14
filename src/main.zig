const std = @import("std");
const streams = @import("streams.zig");
const CharacterStream = streams.CharacterStream;
const TokenStream = streams.TokenStream;
const StringTokenStream = streams.StringTokenStream;

const TyVal = struct {
    val: *Val,
    ty: *Ty,
    pub fn new(ty: Ty, val: Val) *TyVal {
        return allocDupe(TyVal{.ty = allocDupe(ty), .val = allocDupe(val)});
    }
};
pub fn printTyVal(out: anytype, ty_val: TyVal) void {
    // out.writeAll("*") catch @panic("oom");
    // printTy(out, ty_val.ty.*);
    // out.writeAll(", ") catch @panic("oom");
    printVal(out, ty_val.val.*);
}
const Ty = union(enum) {
    pub const MapEntry = struct{
        key: Symbol,
        value: *Ty,
        // is_comptime: ?*Val.Literal,
        props: struct {
            is_mutable: bool,
        },
    };
    pub const ArrayEntry = struct {
        value: *Ty,
        props: struct {
            is_mutable: bool,
        },
    };
    function: struct {
        arg: ?*Ty,
        return_type: ?*Ty,
        fn_flags: struct {
            pure: bool,
        },
    },
    map: struct {
        properties: []MapEntry,
    },
    array: struct {
        items: []ArrayEntry
    },
    symbol: Symbol,
    number: []const u8,
    string: []const u8,
    @"void": void,
    @"type": void,
};
pub fn printTy(out: anytype, ty: Ty) void {
    switch (ty) {
        .function => {
            out.writeAll("(@fn_type)TODO") catch @panic("bad");
        },
        .map => |map| {
            out.writeAll("(@map_type){") catch @panic("bad");
            for(map.properties) |prop, i| {
                if(i != 0) out.writeAll(", ") catch @panic("bad");
                if(prop.props.is_mutable) out.writeAll("*") catch @panic("bad");
                out.print("#{d}: ", .{@enumToInt(prop.key)}) catch @panic("bad");
                printTy(out, prop.value.*);
            }
            out.writeAll("}") catch @panic("bad");
        },
        .array => {
            out.writeAll("(@array_type)TODO") catch @panic("bad");
        },
        .symbol => |symbol| {
            out.print("(@typeof)#{d}", .{@enumToInt(symbol)}) catch @panic("bad");
        },
        .number => |num| {
            out.print("(@typeof){s}", .{num}) catch @panic("bad");
        },
        .string => |str| {
            out.print("(@typeof)\"{s}\"", .{str}) catch @panic("bad");
        },
        .void => {
            out.writeAll("(@typeof)@void") catch @panic("bad");
        },
        .type => {
            out.writeAll("type") catch @panic("bad");
        },
    }
}

const Val = union(enum) {
    // pure
    literal: Literal,

    // purity depends on content
    // note that an impure expression
    // can be pure if all the impure elements
    // are contained in it
    // eg block x := 1;
    //    block x  = 2;
    //    x
    // is pure even though `block x = 2; x` is impure
    call: Call,
    block: Block,
    withenv: Withenv,
    label: Label,
    array: Array,
    env: void,

    const Literal = union(enum) {
        symbol: Symbol,
        number: []const u8,
        string: []const u8,
        builtin_fn: []const u8,
        map: []struct {key: Symbol, value: *Literal},
        // note: struct{} == struct{} even though those may be different pointers.
        // - what about map types with comptime props? how do those work?
        ty: *Ty,
        @"void": void,
    };
    const Call = struct {
        method: *TyVal,
        arg: *TyVal,
    };
    const Block = struct {
        enter: *TyVal,
        next: *TyVal,
        exit: *TyVal,
    };
    const Withenv = struct {
        new_env: *TyVal,
        next: *TyVal,
    };
    const Label = struct {
        tag: Symbol,
        expr: *TyVal,
    };
    const Array = struct {
        items: []*TyVal,
    };
};
pub fn printVal(out: anytype, val: Val) void {
    switch (val) {
        .literal => |lit| switch(lit) {
            .symbol => |sym| out.print("#{d}", .{@enumToInt(sym)}) catch @panic("bad"),
            .number => |num| out.writeAll(num) catch @panic("bad"),
            .string => |str| out.print("\"{s}\"", .{str}) catch @panic("bad"),
            .builtin_fn => |bfn| out.print("@{s}", .{bfn}) catch @panic("bad"),
            .map => |_| {
                out.print("TODO_map", .{}) catch @panic("bad");
            },
            .ty => |ty| printTy(out, ty.*),
            .void => out.writeAll("@void") catch @panic("bad"),
        },
        .call => |call| {
            out.writeAll("(") catch @panic("bad");
            printTyVal(out, call.method.*);
            out.writeAll(")") catch @panic("bad");
            printTyVal(out, call.arg.*);
        },
        .block => |block| {
            out.writeAll("block ") catch @panic("bad");
            printTyVal(out, block.enter.*);
            out.writeAll(";\ndefer ") catch @panic("bad");
            printTyVal(out, block.exit.*);
            out.writeAll(";\n") catch @panic("bad");
            printTyVal(out, block.next.*);
        },
        .withenv => |withenv| {
            out.writeAll("withenv ") catch @panic("bad");
            printTyVal(out, withenv.new_env.*);
            out.writeAll(";\n") catch @panic("bad");
            printTyVal(out, withenv.next.*);
        },
        .label => {
            out.writeAll("TODO_label") catch @panic("bad");
        },
        .array => |array| {
            out.writeAll("[") catch @panic("bad");
            for(array.items) |item, i| {
                if(i != 0) out.writeAll(", ") catch @panic("bad");
                printTyVal(out, item.*);
            }
            out.writeAll("]") catch @panic("bad");
        },
        .env => {
            out.writeAll("env") catch @panic("bad");
        },
    }
}

fn analyze(expr: *Expr, env: *Ty) *TyVal {
    // TODO
    // - in ty, have a .pure which says if the content is pure relative to this node
    // - in ty, have a .all_comptime which says if this entire node can be computed at comptime
    // - after analyzing, call at comptime if pure && all_comptime
    return switch(expr.*) {
        .literal => |l| switch(l) {
            .symbol => |sym| TyVal.new(.{
                .symbol = sym,
            }, .{.literal = .{
                .symbol = sym,
            }}),
            .number => |num| TyVal.new(.{
                .number = num,
            }, .{.literal = .{
                .number = num,
            }}),
            .string => |str| TyVal.new(.{
                .string = str,
            }, .{.literal = .{
                .string = str,
            }}),
            .builtin => |b| blk: {
                if(std.mem.eql(u8, b, "empty_map")) {
                    break :blk TyVal.new(.{.map = .{
                        .properties = &.{},
                    }}, .{
                        .literal = .{.map = &.{}},
                    });
                }else if(std.mem.eql(u8, b, "void")) {
                    break :blk TyVal.new(.void, .{.literal = .void});
                }
                break :blk TyVal.new(.{.function = .{
                    .arg = null,
                    .return_type = null,
                    .fn_flags = .{.pure = true}, // depends on the fn
                }}, .{.literal = .{
                    .builtin_fn = b,
                }});
            },
            .void => TyVal.new(.void, .{.literal = .void}),
        },
        .block => |block| blk: {
            const enter = analyze(block.enter, env);
            // TODO pass enter's return type to exit somehow?
            if(enter.ty.* != .void) @panic("enter must return void");
            const next = analyze(block.next, env);
            const exit = analyze(block.exit, env);
            if(exit.ty.* != .void) @panic("exit must return void");
            break :blk TyVal.new(next.ty.*, .{.block = .{
                .enter = enter,
                .next = next,
                .exit = exit,
            }});
        },
        .withenv => |wenv| blk: {
            const new_env = analyze(wenv.new_env, env);
            const next = analyze(wenv.next, new_env.ty);
            break :blk TyVal.new(next.ty.*, .{
                .withenv = .{
                    .new_env = new_env,
                    .next = next,
                },
            });
        },
        .call => |call| blk: {
            const method = analyze(call.method, env);
            const arg = analyze(call.arg, env);
            if(method.ty.* != .function) @panic("bad method type");
            const fn_ty = method.ty.function;
            if(fn_ty.arg) |_| {
                @panic("TODO check if type matches type");
            }
            // if fn is comptime | pure and arg is a literal, call at comptime
            // if fn is comptime and arg is not a literal, error

            const default_call = Val{
                .call = .{
                    .method = method, .arg = arg,
                },
            };

            if(fn_ty.return_type) |return_type| {
                break :blk TyVal.new(return_type.*, default_call);
            }

            // call something at comptime to determine the
            // return type based on the argument type
            // I think the method body has to be analyzed based on a specified argument type or something
            // yeah and that will say the return type and return the runtime method
            if(method.val.* != .literal) @panic("method must be comptime-known to determine return type");
            if(method.val.literal != .builtin_fn) @panic("TODO");
            if(std.mem.eql(u8, method.val.literal.builtin_fn, "mapset")) {
                if(arg.ty.* != .array) @panic("@mapset bad arg");
                if(arg.ty.array.items.len != 3) @panic("@mapset req 3 arg"); // could @mapset [a, b] to delete?
                const args = arg.ty.array.items;
                const map_arg = args[0].value;
                const key_arg = args[1].value;
                const value_arg = args[2].value;
                if(map_arg.* != .map) @panic("@mapset[0] req map");
                if(key_arg.* != .symbol) @panic("@mapset[1] req symbol");
                for(map_arg.map.properties) |prop| {
                    if(prop.key == key_arg.symbol) @panic("map already contains key. use @mapmut");
                }
                var new_props = std.ArrayList(Ty.MapEntry).init(global_allocator.?);
                new_props.appendSlice(map_arg.map.properties) catch @panic("oom"); // shallow copy of comptime
                new_props.append(Ty.MapEntry{
                    .key = key_arg.symbol,
                    .value = value_arg,
                    .props = .{.is_mutable = true},
                }) catch @panic("oom");
                break :blk TyVal.new(.{
                    .map = .{.properties = new_props.toOwnedSlice()},
                }, default_call);
            }else if(std.mem.eql(u8, method.val.literal.builtin_fn, "mapget")) {
                if(arg.ty.* != .array) @panic("@mapget bad arg");
                if(arg.ty.array.items.len != 2) @panic("@mapget req 2 arg");
                const args = arg.ty.array.items;
                const map_arg = args[0].value;
                const key_arg = args[1].value;
                if(map_arg.* != .map) @panic("@mapget[0] req map");
                if(key_arg.* != .symbol) @panic("@mapget[1] req symbol");
                for(map_arg.map.properties) |prop| {
                    if(prop.key == key_arg.symbol) {
                        break :blk TyVal.new(prop.value.*, default_call);
                    }
                }
                @panic("map does not contain key.");
            }else if(std.mem.eql(u8, method.val.literal.builtin_fn, "print")) {
                break :blk TyVal.new(.void, default_call);
            }else if(std.mem.eql(u8, method.val.literal.builtin_fn, "typeOf")) {
                break :blk TyVal.new(.type, .{.literal = .{.ty = arg.ty}});
            }else if(std.mem.eql(u8, method.val.literal.builtin_fn, "compileLog")) {
                const out = std.io.getStdErr().writer();
                out.writeAll("@compileLog:lyn:col: /") catch @panic("bad");
                printTy(out, arg.ty.*);
                out.writeAll(": ") catch @panic("bad");
                printVal(out, arg.val.*);
                out.writeAll("\n\n") catch @panic("bad");
                @panic("got compileLog");
            }else std.debug.panic("TODO builtin_fn @{s}", .{method.val.literal.builtin_fn});
        },
        .label => @panic("TODO label"),
        .array => |arr| blk: {
            var res_arr = std.ArrayList(*TyVal).init(global_allocator.?);
            var res_ty = std.ArrayList(Ty.ArrayEntry).init(global_allocator.?);
            for(arr.items) |item| {
                const analyzed = analyze(item, env);
                res_arr.append(analyzed) catch @panic("oom");
                res_ty.append(.{.value = analyzed.ty, .props = .{.is_mutable = true}}) catch @panic("oom");
            }
            break :blk TyVal.new(.{.array = .{
                .items = res_ty.toOwnedSlice(),
            }}, .{.array = .{
                .items = res_arr.toOwnedSlice(),
            }});
        },
        .env => TyVal.new(env.*, .env),
    };
}

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
    /// evaluates to the local env. this is not a
    /// builtin because it has special properties
    /// (how do you say the type of something
    ///  is (void -> call @env) if there's no
    ///  magic way to get @env?)
    env,
    /// uses the return value of a to call b with
    withenv: Withenv,
    const Call = struct {
        method: *Expr,
        arg: *Expr,
    };
    const Literal = union(enum) {
        symbol: Symbol,
        number: []const u8,
        string: []const u8,
        builtin: []const u8,
        @"void": void,
    };
    const Block = struct {
        enter: *Expr, // call exit with the return value of enter
        next: *Expr,
        exit: *Expr,
    };
    const Withenv = struct {
        new_env: *Expr,
        next: *Expr,
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
            dupe_hm.put(kv.key_ptr.*, kv.value_ptr.*) catch @panic("oom");
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
    // const reload = characters.save();
    // errdefer reload.call();

    const tokens = TokenStream.wrap(characters);

    std.log.info("Parsing... {s}", .{blk: {
        const rl2 = characters.save();
        defer rl2.call();
        break :blk tokens.next(.gap);
    }});
    if (tokens.eat(.gap, "block")) {
        // 'block' :enter ('defer' :exit)? ';' :next

        const enter_expr = parse(penv, characters);
        if (!tokens.eat(.gap, ";")) {
            std.log.err("got bad '{s}'", .{tokens.next(.gap)});
            @panic("todo error");
        }
        var defer_expr: ?*Expr = if (tokens.eat(.gap, "defer")) blk: {
            const res = parse(penv, characters);
            if(!tokens.eat(.gap, ";")) {
                std.log.err("got bad '{s}'", .{tokens.next(.gap)});
                @panic("todo error");
            }
            break :blk res;
         } else null;
        const next_expr = parse(penv, characters);

        return allocDupe(Expr{ .block = .{
            .enter = enter_expr,
            .next = next_expr,
            .exit = defer_expr orelse allocDupe(.{.literal = .void}),
        } });
    } else if (tokens.eat(.gap, "withenv")) {
        const new_env = parse(penv, characters);
        if (!tokens.eat(.gap, ";")) {
            std.log.err("got bad '{s}'", .{tokens.next(.gap)});
            @panic("TODO error");
        }
        const next_expr = parse(penv, characters);
        return allocDupe(Expr{ .withenv = .{
            .new_env = new_env,
            .next = next_expr,
        } });
    } else if (tokens.eat(.gap, "(")) {
        const method = parse(penv, characters);
        if(!tokens.eat(.gap, ")")) {
            std.log.err("got bad '{s}'", .{tokens.next(.gap)});
            @panic("todo error");
        }
        const arg = parse(penv, characters);
        return allocDupe(Expr{ .call = .{
            .method = method,
            .arg = arg,
        } });
    } else if (tokens.eat(.gap, "[")) {
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
    } else if (tokens.eat(.gap, "env")) {
        return allocDupe(@as(Expr, .env));
    } else {
        std.log.err("unexpected {s}", .{tokens.next(.gap)});
        @panic("TODO error");
    }
}

pub fn printExpr(out: anytype, expr: Expr) void {
    switch (expr) {
        .call => |call| {
            out.writeAll("(") catch @panic("bad");
            printExpr(out, call.method.*);
            out.writeAll(")") catch @panic("bad");
            printExpr(out, call.arg.*);
        },
        .literal => |l| switch (l) {
            .string => |str| out.print("\"{s}\"", .{str}) catch @panic("bad"),
            .number => |num| out.print("{s}", .{num}) catch @panic("bad"),
            .symbol => |sym| out.print("#{d}", .{@enumToInt(sym)}) catch @panic("bad"),
            .builtin => |builtin| out.print("@{s}", .{builtin}) catch @panic("bad"),
            .void => out.print("@void", .{}) catch @panic("bad"),
        },
        .block => |block| {
            out.writeAll("block") catch @panic("bad");
            out.writeAll(" ") catch @panic("bad");
            printExpr(out, block.enter.*);

            out.writeAll(";\ndefer ") catch @panic("bad");
            printExpr(out, block.exit.*);

            out.writeAll(";\n") catch @panic("bad");
            printExpr(out, block.next.*);
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
        .env => {
            out.writeAll("env") catch @panic("oom");
        },
        .withenv => |withenv| {
            out.writeAll("withenv ") catch @panic("oom");
            printExpr(out, withenv.new_env.*);
            out.writeAll(";\n") catch @panic("oom");
            printExpr(out, withenv.next.*);
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

    const parsed = parse(Penv.new().*, &stream); // TODO check that the entire file was parsed
    out.writeAll("// AST:\n") catch @panic("bad");
    printExpr(out, parsed.*);
    out.writeAll("\n\n") catch @panic("bad");

    const analyzed = analyze(parsed, allocDupe(Ty{.@"void" = {}}));
    out.writeAll("// AIR:\n") catch @panic("bad");
    printTyVal(out, analyzed.*);
    out.writeAll("\n\n") catch @panic("bad");
}
