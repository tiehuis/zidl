// "CodeGen" simply emits an AST out as C/C++ code. Would like to explore a zig target so will have
// multiple codegen backends.

const std = @import("std");
const Node = @import("node.zig").Node;
const Parser = @import("Parser.zig");
const InternPool = @import("InternPool.zig");
const Compile = @import("Compile.zig");

const CodeGen = @This();

const log = std.log.scoped(.codegen);

cc: *Compile,
parser: *const Parser,
nodes: []const Node,
data: []const Node.Ref,
intern_pool: InternPool,
options: Options,

next_id: usize = 0,
indent: usize = 0,

pub const Options = struct {
    source_filepath: []const u8,
    program_identifier: []const u8 = "ZIDL 1.0",
    program_identifier_short: []const u8 = "ZIDL",
    required_rcpndr_version: u32 = 475,
};

pub const Error = error{ IoError, CodeGenError, UnexpectedNodeType };

pub fn init(cc: *Compile, parser: *const Parser, options: Options) CodeGen {
    return .{
        .cc = cc,
        .parser = parser,
        .nodes = parser.nodes.items,
        .data = parser.data.items,
        .intern_pool = parser.intern_pool,
        .options = options,
    };
}

// TODO: Use a genNode approach, similar to dump in parser but a bit more
// in-depth. Don't traverse nodes too much if possible in subroutines.

fn nodeAs(
    cg: *CodeGen,
    node_index: Node.Ref,
    comptime tag: std.meta.Tag(Node),
) !std.meta.TagPayload(Node, tag) {
    const node = cg.nodes[node_index.toInt()];
    return switch (node) {
        tag => |value| value,
        else => return error.UnexpectedNodeType,
    };
}

pub fn gen(cg: *CodeGen) !void {
    var store1: [256]u8 = undefined;
    const header_filename = try makeHeaderFilename(&store1, cg.options.source_filepath);

    var store2: [64]u8 = undefined;
    const header_guard_name = try makeHeaderGuardName(&store2, header_filename);

    try cg.genHeader(header_guard_name);

    switch (cg.nodes[0]) {
        .root => |range| {
            for (range.start..range.end) |i|
                try cg.genNode(cg.data[i], 0);
        },
        else => unreachable,
    }

    try cg.genAdditionalPrototypes();
    try cg.genFooter(header_guard_name);
}

fn genImportHeaders(cg: *CodeGen) !void {
    try cg.print(
        \\/* Headers for imported files */
        \\
        \\
    , .{});

    for (cg.parser.import_table.items) |import| {
        if (!import.is_root) continue;

        const lit = cg.intern_pool.get(import.import.subject).?;
        std.debug.assert(lit.len <= 128);

        var buf: [128]u8 = undefined;
        const header_filename = try makeHeaderFilename(&buf, lit);
        try cg.print("#include <{s}>\n", .{header_filename});
    }

    try cg.print("\n", .{});
}

fn genInterfaceRef(cg: *CodeGen, name: Node.Ref) !void {
    const interface_name = try cg.nodeAs(name, .type);
    const typename = cg.intern_pool.get(interface_name).?;
    try cg.print(
        \\#ifndef __{0s}_FWD_DEFINED__
        \\#define __{0s}_FWD_DEFINED__
        \\typedef interface {0s} {0s};
        \\#ifdef __cplusplus
        \\interface {0s};
        \\#endif /* __cplusplus */
        \\#endif
        \\
        \\
    , .{typename});
}

fn genAsyncInterfaceRef(cg: *CodeGen, name: Node.Ref) !void {
    const interface_name = try cg.nodeAs(name, .type);
    const typename = cg.intern_pool.get(interface_name).?;
    try cg.print(
        \\#ifndef __Async{0s}_FWD_DEFINED__
        \\#define __Async{0s}_FWD_DEFINED__
        \\typedef interface Async{0s} Async{0s};
        \\#ifdef __cplusplus
        \\interface Async{0s};
        \\#endif /* __cplusplus */
        \\#endif
        \\
        \\
    , .{typename});
}

fn genCoClassRef(cg: *CodeGen, ref: Node.Ref) !void {
    const interface_name = try cg.nodeAs(ref, .type);
    const typename = cg.intern_pool.get(interface_name).?;
    try cg.print(
        \\#ifndef __{0s}_FWD_DEFINED__
        \\#define __{0s}_FWD_DEFINED__
        \\#ifdef __cplusplus
        \\typedef class {0s} {0s};
        \\#else
        \\typedef struct {0s} {0s};
        \\#endif /* defined __cplusplus */
        \\#endif /* defined __{0s}_FWD_DEFINED__ */
        \\
        \\
    , .{typename});
}

fn genForwardDecls(cg: *CodeGen) !void {
    try cg.print(
        \\/* Forward declarations */
        \\
        \\
    , .{});

    for (cg.parser.forward_decl_table.items) |index| {
        const node = cg.nodes[index.toInt()];
        switch (node) {
            .interface => |interface| {
                try cg.genInterfaceRef(interface.name);
                if (try cg.getAttribute(interface.attributes, .async_uuid)) |_| {
                    try cg.genAsyncInterfaceRef(interface.name);
                }
            },
            .coclass_def => |coclass| try cg.genCoClassRef(coclass.name),
            else => {
                log.err("forward decl not implemented for node: {s}", .{@tagName(node)});
                return error.CodeGenError;
            },
        }
    }
}

fn genHeader(cg: *CodeGen, header_guard_name: []const u8) !void {
    try cg.print(
        \\/*** Autogenerated by {s} from {s} - Do not edit ***/
        \\
        \\#ifdef _WIN32
        \\#ifndef __REQUIRED_RPCNDR_H_VERSION__
        \\#define __REQUIRED_RPCNDR_H_VERSION__ {}
        \\#endif
        \\#include <rpc.h>
        \\#include <rpcndr.h>
        \\#endif
        \\
        \\#ifndef COM_NO_WINDOWS_H
        \\#include <windows.h>
        \\#include <ole2.h>
        \\#endif
        \\
        \\
    , .{
        cg.options.program_identifier,
        cg.options.source_filepath,
        cg.options.required_rcpndr_version,
    });

    try cg.print(
        \\#ifndef {0s}
        \\#define {0s}
        \\
        \\
    , .{header_guard_name});

    try cg.genForwardDecls();
    try cg.genImportHeaders();

    try cg.print(
        \\#ifdef __cplusplus
        \\extern "C" {{
        \\#endif
        \\
        \\
    , .{});
}

fn genFooter(cg: *CodeGen, header_guard_name: []const u8) !void {
    try cg.print(
        \\#ifdef __cplusplus
        \\}}
        \\#endif
        \\
        \\#endif /* {s} */
        \\
    , .{header_guard_name});
}

fn genAdditionalPrototypes(cg: *CodeGen) !void {
    try cg.print(
        \\/* Begin additional prototypes for all interfaces */
        \\
        \\
    , .{});

    for (cg.parser.ole_auto_types.items) |ole_type| {
        try cg.print(
            \\ULONG           __RPC_USER {0s}_UserSize     (ULONG *, ULONG, {0s} *);
            \\unsigned char * __RPC_USER {0s}_UserMarshal  (ULONG *, unsigned char *, {0s} *);
            \\unsigned char * __RPC_USER {0s}_UserUnmarshal(ULONG *, unsigned char *, {0s} *);
            \\void            __RPC_USER {0s}_UserFree     (ULONG *, {0s} *);
            \\
        , .{@tagName(ole_type)});
    }

    try cg.print(
        \\
        \\/* End additional prototypes */
        \\
        \\
    , .{});
}

// Dynamic node processing

fn genNode(cg: *CodeGen, node_index: Node.Ref, indent: usize) Error!void {
    _ = indent;

    const node = cg.nodes[node_index.toInt()];
    switch (node) {
        .root => unreachable,
        .cpp_quote => |ref| try cg.genCppQuote(ref),
        .interface => |interface| try cg.genInterface(interface),
        .interface_ref => |ref| try cg.genInterfaceRef(ref),
        .coclass_def => |coclass| try cg.genCoClass(coclass),
        .library_def => |library| try cg.genLibrary(library),
        .import => |ref| {
            for (ref.statements.start..ref.statements.end) |index| {
                try cg.genNode(cg.data[index], 0);
            }
        },
        .typedef => |typedef| try cg.genTypedef(typedef),
        .enum_def => |enum_def| {
            try cg.genEnumDef(enum_def);
            try cg.print(";\n", .{});
        },
        .importlib => {}, // TODO: Confirm
        .decl => |decl| {
            try cg.genType(decl.decl_spec);
            if (decl.init_decl) |d| try cg.genType(d);
            try cg.print(";\n", .{});
        },
        else => {
            std.debug.print("unsupported node type: {s}\n", .{@tagName(node)});
            return error.UnexpectedNodeType;
        },
    }
}

fn genCppQuote(cg: *CodeGen, ref: InternPool.Ref) !void {
    const lit = cg.intern_pool.get(ref).?;

    // cpp_quote stores strings escaped so we can use a reference to the source directly.
    // We need to unescape here when printing.
    var i: usize = 0;
    while (i < lit.len) {
        if (std.mem.indexOfScalarPos(u8, lit, i, '\\')) |escape_index| {
            try cg.print("{s}{c}", .{ lit[i..escape_index], lit[escape_index + 1] });
            i = escape_index + 2;
        } else {
            try cg.print("{s}", .{lit[i..]});
            break;
        }
    }

    try cg.print("\n", .{});
}

fn genName(cg: *CodeGen, node_ref: Node.Ref) !void {
    const node = cg.nodes[node_ref.toInt()];
    switch (node) {
        .type => |ref| {
            try cg.print("{s}", .{cg.intern_pool.get(ref).?});
        },
        .declarator,
        .any_declarator,
        => |decl| {
            if (decl.decl) |ok| {
                try cg.genName(ok);
            }
        },
        .func_param => |func_param| {
            if (func_param.any_decl) |a| {
                try cg.genName(a);
            }
        },
        .direct_decl => |direct_decl| {
            try cg.genName(direct_decl.base);
        },
        .decl_spec_type => |decl_spec_type| {
            try cg.genName(decl_spec_type.type);
        },
        .struct_def => |struct_def| {
            // TODO: if empty, use generated name
            try cg.print("struct {s}", .{cg.intern_pool.get(struct_def.name.?).?});
        },
        .enum_def => |enum_def| {
            try cg.print("enum {s}", .{cg.intern_pool.get(enum_def.name.?).?});
        },
        .union_def => |union_def| {
            try cg.print("union {s}", .{cg.intern_pool.get(union_def.name.?).?});
        },
        else => {
            std.debug.print("genType: unsupported node type: {s}\n", .{@tagName(node)});
            return error.UnexpectedNodeType;
        },
    }
}

fn genType(cg: *CodeGen, node_ref: Node.Ref) Error!void {
    const node = cg.nodes[node_ref.toInt()];
    switch (node) {
        .enum_def => |def| try cg.genEnumDef(def),
        .struct_def => |def| try cg.genStructDef(def),
        .array_def => |a| switch (a) {
            .empty => try cg.print("[]", .{}),
            .asterisk => try cg.print("[*]", .{}),
            .expr => |e| {
                try cg.print("[", .{});
                try cg.genExpr(e);
                try cg.print("]", .{});
            },
        },
        .safe_array => |arr| {
            try cg.print("SAFEARRAY *", .{});
            try cg.genType(arr);
        },
        .enum_ref => |ref| {
            try cg.print("enum {s}", .{cg.intern_pool.get(ref).?});
        },
        .struct_ref => |ref| {
            try cg.print("struct {s}", .{cg.intern_pool.get(ref).?});
        },
        .qualified_type => |qual_type| {
            for (qual_type.namespace.start..qual_type.namespace.end) |index| {
                try cg.genType(cg.data[index]);
                try cg.print(".", .{});
            }
            try cg.genType(qual_type.typename);
        },
        .unqualified_decl_spec_type,
        .decl_spec_type,
        => |decl_spec| {
            for (decl_spec.decl_specs.start..decl_spec.decl_specs.end) |index| {
                try cg.genType(cg.data[index]);
                try cg.print(" ", .{});
            }
            try cg.genType(decl_spec.type);
            try cg.print(" ", .{});
        },
        .decl_spec => |decl_spec| {
            try cg.print("{s}", .{@tagName(decl_spec)});
        },
        .base_type => |tag| {
            // TODO: render base type
            try cg.print("{s}", .{@tagName(tag)});
        },
        .known_type,
        .type,
        => |ref| {
            try cg.print("{s}", .{cg.intern_pool.get(ref).?});
        },
        .init_declarator => |init_decl| {
            try cg.genType(init_decl.declarator);
            if (init_decl.expr) |expr| {
                try cg.print(" = ", .{});
                try cg.genExpr(expr);
            }
        },
        .declarator,
        .any_declarator,
        => |decl| {
            if (decl.call_conv) |cc| {
                try cg.genCallConv(cc);
            }
            if (decl.is_pointer) {
                try cg.print("*", .{});
            }
            if (decl.decl) |ok| {
                try cg.genType(ok);
            }
        },
        .direct_decl => |direct_decl| {
            try cg.genType(direct_decl.base);
            for (direct_decl.suffix.start..direct_decl.suffix.end) |index| {
                try cg.genType(cg.data[index]);
            }
        },
        .arg_list => |arg_list| {
            try cg.print("(\n", .{});
            for (arg_list.args.start..arg_list.args.end) |index| {
                try cg.genType(cg.data[index]);
                try cg.print(",\n", .{});
            }
            if (arg_list.is_varargs) {
                try cg.print("...)", .{});
            }
            try cg.print(")", .{});
        },
        .func_param => |func_param| {
            try cg.genType(func_param.decl_spec);
            // TODO: Handle attributes
            if (func_param.any_decl) |a| {
                try cg.genType(a);
            }
        },
        .union_switch => |sw| {
            const name = if (sw.name) |i| cg.intern_pool.get(i).? else "__C89_NAMELESS";
            try cg.print("struct {s} {{\n", .{name});

            // Tag
            try cg.printIndent(cg.indent + 4);
            try cg.genType(sw.sfield);
            try cg.print(";\n", .{});
            // Union
            try cg.printIndent(cg.indent + 4);
            try cg.print("union ", .{});
            try cg.genNameOrFallback(null);
            try cg.print(" {{\n", .{});
            for (sw.cases.start..sw.cases.end) |index| {
                switch (cg.nodes[cg.data[index].toInt()]) {
                    .union_switch_case => |c| {
                        const sfield: Node.UnionField = try cg.nodeAs(c.field, .union_field);
                        switch (sfield) {
                            .attributes => continue,
                            .s_field => |f| {
                                try cg.printIndent(cg.indent + 8);
                                try cg.genType(f);
                            },
                            .s_field_optional => |mf| if (mf) |f| {
                                try cg.printIndent(cg.indent + 8);
                                try cg.genType(f);
                            } else continue,
                        }
                    },
                    else => unreachable,
                }
                try cg.print(";\n", .{});
            }
            try cg.printIndent(cg.indent + 4);
            try cg.print("}} u;\n", .{});
            try cg.printIndent(cg.indent);
            try cg.print("}}", .{});
        },
        .union_def => |def| {
            const name = if (def.name) |i| cg.intern_pool.get(i).? else "__C89_NAMELESS";
            try cg.print("{s} union {{\n", .{name});
            for (def.fields.start..def.fields.end) |index| {
                switch (cg.nodes[cg.data[index].toInt()]) {
                    .union_field => |field| switch (field) {
                        .attributes => continue,
                        .s_field => |f| {
                            cg.indent += 4;
                            defer cg.indent -= 4;
                            try cg.printIndent(cg.indent);
                            try cg.genType(f);
                        },
                        .s_field_optional => |mf| if (mf) |f| {
                            cg.indent += 4;
                            defer cg.indent -= 4;
                            try cg.printIndent(cg.indent);
                            try cg.genType(f);
                        } else continue,
                    },
                    else => unreachable,
                }
                try cg.print(";\n", .{});
            }
            try cg.printIndent(cg.indent);
            try cg.print("}} {s}UNIONNAME", .{name});
        },
        .union_field => |field| switch (field) {
            .attributes => unreachable,
            .s_field => |f| try cg.genType(f),
            .s_field_optional => |mf| if (mf) |f| try cg.genType(f) else {},
        },
        .union_sfield => |sfield| switch (sfield) {
            .struct_def => |sf| try cg.genType(sf.def),
            .default => |df| {
                try cg.genType(df.decl_spec);
                try cg.genType(df.declarator);
            },
        },
        else => {
            std.debug.print("genType: unsupported node type: {s}\n", .{@tagName(node)});
            return error.UnexpectedNodeType;
        },
    }
}

fn genCallConv(cg: *CodeGen, cc: Node.CallConv) !void {
    const cc_s = switch (cc) {
        .cdecl => "CDECL",
        .fastcall => "FASTCALL",
        .pascal => "PASCAL",
        .stdcall => "STDCALL",
    };
    try cg.print("{s} ", .{cc_s});
}

fn genExpr(cg: *CodeGen, node_ref: Node.Ref) !void {
    const node = cg.nodes[node_ref.toInt()];
    switch (node) {
        .cast => |c| {
            try cg.print("(", .{});
            try cg.genType(c.unqual_decl_spec);
            if (c.declarator) |d| try cg.genType(d);
            try cg.print(") (", .{});
            try cg.genExpr(c.expr);
            try cg.print(")", .{});
        },
        .expr_const, .expr_const_int => |e| try cg.genExpr(e),
        .expr => |p| {
            switch (p) {
                .false => try cg.print("FALSE", .{}),
                .true => try cg.print("TRUE", .{}),
                .null => try cg.print("NULL", .{}),
                .expr => |e| try cg.genExpr(e),

                inline .identifier,
                .number,
                .string,
                => |ref| {
                    try cg.print("{s}", .{cg.intern_pool.get(ref).?});
                },
            }
        },
        .expr_unary => |p| {
            try cg.print("{s}", .{p.op.symbol()});
            try cg.genExpr(p.arg1);
        },
        .expr_binary => |p| {
            try cg.genExpr(p.arg1);
            try cg.print(" {s} ", .{p.op.symbol()});
            try cg.genExpr(p.arg2);
        },
        .expr_ternary => |p| {
            try cg.genExpr(p.arg1);
            try cg.print(" ? ", .{});
            try cg.genExpr(p.arg2);
            try cg.print(" : ", .{});
            try cg.genExpr(p.arg3);
        },
        else => {
            log.err("genExpr: unsupported node type: {s}\n", .{@tagName(node)});
            return error.UnexpectedNodeType;
        },
    }
}

const FuncGenOptions = struct {
    prefix: []const u8,
    args: bool,
};

const func_gen_cases: [3]FuncGenOptions = .{
    // Async
    .{ .prefix = "Begin_", .args = false },
    .{ .prefix = "Finish_", .args = true },
    // Non-Async
    .{ .prefix = "", .args = true },
};

const func_gen_async = func_gen_cases[0..2];
const func_gen = func_gen_cases[2..3];

fn genVirtualFuncDecl(cg: *CodeGen, decl: Node.Decl, is_async: bool) !void {
    // call_as(func) => this should not be generated
    if (try cg.getAttribute(decl.attributes, .call_as)) |_| return;

    if (decl.init_decl) |init_decl| {
        const id = try cg.nodeAs(init_decl, .init_declarator);
        const d = try cg.nodeAs(id.declarator, .declarator);

        for (if (is_async) func_gen_async else func_gen) |vfunc| {
            try cg.printIndent(4);
            try cg.print("virtual ", .{});
            try cg.genType(decl.decl_spec); // Return Type

            if (d.call_conv) |cc| try cg.genCallConv(cc) else try cg.print("STDMETHODCALLTYPE ", .{});

            const dd: Node.DirectDeclarator = blk: {
                if (cg.nodeAs(d.decl.?, .declarator)) |de| {
                    if (de.call_conv) |cc| try cg.genCallConv(cc);
                    break :blk try cg.nodeAs(de.decl.?, .direct_decl);
                } else |_| {
                    break :blk try cg.nodeAs(d.decl.?, .direct_decl);
                }
            };

            try cg.print("{s}", .{vfunc.prefix});
            try cg.genType(dd.base); // Function Name
            try cg.print("(", .{});
            if (vfunc.args) {
                try cg.genArgList(dd.suffix, .{ .indent = 8, .split_args_by_line = true });
                if (cg.argListLen(dd.suffix) == 0) {
                    // WIDL renders empty cpp virtual decls as
                    //
                    // ```
                    //    Function(
                    //        );
                    // ```
                    try cg.print("\n", .{});
                    try cg.printIndent(8);
                }
            } else {
                try cg.print("\n", .{});
                try cg.printIndent(8);
            }
            try cg.print(") = 0;\n\n", .{});
        }
    }
}

fn genEnumDef(cg: *CodeGen, enum_def: Node.EnumDef) !void {
    try cg.print("enum ", .{});
    try cg.genNameOrFallback(enum_def.name);

    try cg.print(" {{\n", .{});
    for (enum_def.members.start..enum_def.members.end) |index| {
        try cg.printIndent(4);
        const enum_field: Node.EnumField = try cg.nodeAs(cg.data[index], .enum_field);
        try cg.print("{s}", .{cg.intern_pool.get(enum_field.name).?});
        if (enum_field.expr) |expr| {
            try cg.print(" = ", .{});
            try cg.genExpr(expr);
        }
        const last = index + 1 == enum_def.members.end;
        try cg.print("{s}\n", .{if (!last) "," else ""});
    }
    try cg.print("}}", .{});
}

fn genStructDef(cg: *CodeGen, struct_def: Node.StructDef) !void {
    try cg.print("struct ", .{});
    try cg.genNameOrFallback(struct_def.name);

    try cg.print(" {{\n", .{});
    for (struct_def.fields.start..struct_def.fields.end) |index| {
        cg.indent += 4;
        defer cg.indent -= 4;

        try cg.printIndent(cg.indent);
        const sfield: Node.StructField = try cg.nodeAs(cg.data[index], .struct_field);
        switch (sfield.field) {
            .@"union" => |ref| try cg.genType(ref),
            .default => |node| {
                try cg.genType(node.decl_spec);
                for (node.struct_decl_list.start..node.struct_decl_list.end) |j| {
                    const struct_decl: Node.StructDecl = try cg.nodeAs(cg.data[j], .struct_decl);
                    try cg.genType(struct_decl.any_decl);
                }
            },
        }
        try cg.print(";\n", .{});
    }
    try cg.printIndent(cg.indent);
    try cg.print("}}", .{});
}

fn genNameOrFallback(cg: *CodeGen, ref: ?InternPool.Ref) !void {
    if (ref) |name| {
        try cg.print("{s}", .{cg.intern_pool.get(name).?});
    } else {
        cg.next_id += 1;
        try cg.print("__{s}_{s}_generated_name_{:08}", .{
            cg.options.program_identifier_short,
            cg.getFilenameNoSuffix(cg.options.source_filepath),
            cg.next_id,
        });
    }
}

fn genTypedef(cg: *CodeGen, typedef: Node.Typedef) Error!void {
    try cg.print("typedef ", .{});
    try cg.genType(typedef.decl_spec);

    std.debug.assert(!typedef.decl_list.empty());
    try cg.genType(cg.data[typedef.decl_list.start]);
    try cg.print(";\n", .{});

    for (typedef.decl_list.start + 1..typedef.decl_list.end) |index| {
        try cg.print("typedef ", .{});
        try cg.genName(typedef.decl_spec);
        try cg.print(" ", .{});
        try cg.genType(cg.data[index]);
        try cg.print(";\n", .{});
    }
}

fn genInterfaceTopLevel(cg: *CodeGen, interface: Node.InterfaceDef) Error!void {
    for (interface.defs.start..interface.defs.end) |index| {
        switch (cg.nodes[cg.data[index].toInt()]) {
            .typedef => |typedef| try cg.genTypedef(typedef),
            .enum_def,
            .union_def,
            .struct_def,
            => {
                try cg.genType(cg.data[index]);
                try cg.print(";\n", .{});
            },
            .cpp_quote => |ref| try cg.genCppQuote(ref),
            // TODO: const decl => #define
            else => {},
        }
    }
}

fn genInterfaceClass(cg: *CodeGen, interface: Node.InterfaceDef, is_async: bool) Error!void {
    const typename = cg.intern_pool.get(cg.nodes[interface.name.toInt()].type).?;
    const prefix: []const u8 = if (is_async) "Async" else "";
    const tag: Node.Attribute.Tag = if (is_async) .async_uuid else .uuid;

    const maybe_uuid: ?u128 = if (try cg.getAttribute(interface.attributes, tag)) |attr|
        if (is_async) attr.async_uuid else attr.uuid
    else
        null;

    if (maybe_uuid) |uuid| {
        var buf: [64]u8 = undefined;
        const uuid_s = uuidToString(&buf, uuid);
        try cg.print(
            \\MIDL_INTERFACE("{s}")
            \\
        , .{uuid_s});
    }

    try cg.print("{s}{s}", .{ prefix, typename });
    if (interface.parents) |parents| {
        try cg.print(" : public ", .{});
        for (parents.start..parents.end) |index| {
            try cg.genType(cg.data[index]);
        }
    }
    try cg.print("\n{{\n", .{});

    for (interface.defs.start..interface.defs.end) |index| {
        const node = cg.nodes[cg.data[index].toInt()];
        switch (node) {
            .decl => |decl| try cg.genVirtualFuncDecl(decl, is_async),
            .typedef,
            .cpp_quote,
            .struct_def,
            => {},
            .import => unreachable,
            else => unreachable,
        }
    }

    try cg.print(
        \\}};
        \\
    , .{});

    if (maybe_uuid) |uuid| {
        const c = uuidComponents(uuid);

        try cg.print(
            \\#ifdef __CRT_UUID_DECL
            \\__CRT_UUID_DECL({s}{s}, 0x{x:08}, 0x{x:04}, 0x{x:04}, 0x{x:02},0x{x:02}, 0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02})
            \\#endif
            \\
        , .{ prefix, typename, c._96, c._80, c._64, c._56, c._48, c._40, c._32, c._24, c._16, c._8, c._0 });
    }
}

const GenArgListOptions = struct {
    indent: usize = 0,
    split_args_by_line: bool = false,
    only_name: bool = false,
};

fn isVoidType(cg: *CodeGen, index: Node.Ref) bool {
    return switch (cg.nodes[index.toInt()]) {
        .func_param => |p| cg.isVoidType(p.decl_spec),
        .decl_spec_type => |ds| cg.isVoidType(ds.type),
        .base_type => |p| switch (p) {
            .void => true,
            else => false,
        },
        else => false,
    };
}

fn argListLen(cg: *CodeGen, range: Node.Range) usize {
    // Bit messy, clean up access to the inner value. Do we even need a Range on this
    // node, since all lists are generally length 1 or 0.
    if (range.end - range.start == 1) {
        const al = cg.nodeAs(cg.data[range.start], .arg_list) catch return 0;
        const len = al.args.end - al.args.start;
        return if (len == 1 and cg.isVoidType(cg.data[al.args.start])) 0 else len;
    }
    std.debug.assert(range.end - range.start == 0);
    return 0;
}

fn genArgList(cg: *CodeGen, range: Node.Range, options: GenArgListOptions) !void {
    // `(void)` argument should not emit
    if (cg.argListLen(range) == 0) return;

    for (range.start..range.end) |i| {
        const al = try cg.nodeAs(cg.data[i], .arg_list);
        for (al.args.start..al.args.end) |j| {
            if (options.split_args_by_line) try cg.print("\n", .{});
            try cg.printIndent(options.indent);
            if (options.only_name) {
                try cg.genName(cg.data[j]);
            } else {
                try cg.genType(cg.data[j]); // .func_param
            }
            const last = j + 1 == al.args.end;
            try cg.print("{s}", .{if (!last) "," else ""});
        }
        if (al.is_varargs) {
            try cg.print(",\n", .{});
            try cg.printIndent(options.indent);
            try cg.print("...", .{});
        }
    }
}

fn genCVtableFuncDef(cg: *CodeGen, interface_name: []const u8, decl: Node.Decl, is_async: bool) !void {
    // call_as(func) => this should not be generated
    if (try cg.getAttribute(decl.attributes, .call_as)) |_| return;

    if (decl.init_decl) |init_decl| {
        const id = try cg.nodeAs(init_decl, .init_declarator);
        const d = try cg.nodeAs(id.declarator, .declarator);

        for (if (is_async) func_gen_async else func_gen) |func| {
            try cg.printIndent(4);
            try cg.genType(decl.decl_spec); // Return Type
            try cg.print("(", .{});
            if (d.call_conv) |cc| try cg.genCallConv(cc) else try cg.print("STDMETHODCALLTYPE ", .{});

            const dd: Node.DirectDeclarator = blk: {
                if (cg.nodeAs(d.decl.?, .declarator)) |de| {
                    if (de.call_conv) |cc| try cg.genCallConv(cc);
                    break :blk try cg.nodeAs(de.decl.?, .direct_decl);
                } else |_| {
                    break :blk try cg.nodeAs(d.decl.?, .direct_decl);
                }
            };

            try cg.print("*{s}", .{func.prefix});
            try cg.genType(dd.base); // Function Name
            try cg.print(")", .{});
            try cg.print("(\n", .{});
            try cg.printIndent(8);
            try cg.print("{s}{s} *This", .{ if (is_async) "Async" else "", interface_name }); // BaseType argument
            if (func.args) {
                if (cg.argListLen(dd.suffix) != 0) try cg.print(",", .{});
                try cg.genArgList(dd.suffix, .{ .indent = 8, .split_args_by_line = true });
            }
            try cg.print(");\n\n", .{});
        }
    }
}

fn findSymbol(cg: *CodeGen, name: []const u8) Error!Node {
    if (cg.parser.findSymbol(name)) |node| {
        return node;
    } else {
        log.err("failed to find symbol: {s}", .{name});
        return error.CodeGenError;
    }
}

// Render parents and recursively render parents parents' (and so forth).
fn genInterfaceCVtableParents(cg: *CodeGen, child_typename: []const u8, interface: Node.InterfaceDef) Error!void {
    if (interface.parents) |parents| {
        for (parents.start..parents.end) |index| {
            const parent_type = try cg.getInterfaceParentName(index);

            const parent = try cg.findSymbol(parent_type);
            switch (parent) {
                .interface => |iface| {
                    try cg.genInterfaceCVtableParents(child_typename, iface);

                    try cg.printIndent(4);
                    try cg.print("/*** {s} methods ***/\n", .{parent_type});
                    for (iface.defs.start..iface.defs.end) |parent_iface_index| {
                        const iface_def_node = cg.nodes[cg.data[parent_iface_index].toInt()];
                        switch (iface_def_node) {
                            // TODO: I suspect we don't handle parent async correctly
                            .decl => |decl| try cg.genCVtableFuncDef(child_typename, decl, false),
                            else => {},
                        }
                    }
                },
                else => unreachable,
            }
        }
    }
}

fn genInterfaceCVtable(cg: *CodeGen, interface: Node.InterfaceDef, is_async: bool) Error!void {
    const typename = cg.intern_pool.get(try cg.nodeAs(interface.name, .type)).?;
    const prefix = if (is_async) "Async" else "";

    try cg.print(
        \\typedef struct {s}{s}Vtbl {{
        \\    BEGIN_INTERFACE
        \\
        \\
    , .{ prefix, typename });

    try cg.genInterfaceCVtableParents(typename, interface);
    try cg.printIndent(4);
    try cg.print("/*** {s}{s} methods ***/\n", .{ prefix, typename });

    for (interface.defs.start..interface.defs.end) |index| {
        const node = cg.nodes[cg.data[index].toInt()];
        switch (node) {
            .decl => |decl| try cg.genCVtableFuncDef(typename, decl, is_async),
            .typedef,
            .cpp_quote,
            .struct_def,
            => {},
            else => unreachable,
        }
    }

    try cg.print(
        \\    END_INTERFACE
        \\}} {0s}{1s}Vtbl;
        \\
        \\interface {0s}{1s} {{
        \\    CONST_VTBL {0s}{1s}Vtbl* lpVtbl;
        \\}};
        \\
        \\
    , .{ prefix, typename });
}

fn getInterfaceParentName(cg: *CodeGen, index: usize) ![]const u8 {
    const qualified_type = try cg.nodeAs(cg.data[index], .qualified_type);
    const parent_type_ref = try cg.nodeAs(qualified_type.typename, .type);
    return cg.intern_pool.get(parent_type_ref).?;
}

fn genInterfaceCObjMacrosParents(cg: *CodeGen, child_typename: []const u8, interface: Node.InterfaceDef) Error!void {
    if (interface.parents) |parents| {
        for (parents.start..parents.end) |index| {
            const parent_type = try cg.getInterfaceParentName(index);
            const parent = try cg.findSymbol(parent_type);
            switch (parent) {
                .interface => |iface| {
                    try cg.genInterfaceCObjMacrosParents(child_typename, iface);

                    try cg.print("/*** {s} methods ***/\n", .{parent_type});
                    for (iface.defs.start..iface.defs.end) |parent_iface_index| {
                        const iface_def_node = cg.nodes[cg.data[parent_iface_index].toInt()];
                        switch (iface_def_node) {
                            .decl => |decl| try cg.genCObjectMacro(child_typename, decl, false),
                            else => {},
                        }
                    }
                },
                else => unreachable,
            }
        }
    }
}

fn genInterfaceCObjMacros(cg: *CodeGen, interface: Node.InterfaceDef, is_async: bool) Error!void {
    const prefix = if (is_async) "Async" else "";
    const typename = cg.intern_pool.get(try cg.nodeAs(interface.name, .type)).?;
    try cg.genInterfaceCObjMacrosParents(typename, interface);
    try cg.print("/*** {s}{s} methods ***/\n", .{ prefix, typename });
    for (interface.defs.start..interface.defs.end) |index| {
        const node = cg.nodes[cg.data[index].toInt()];
        switch (node) {
            .decl => |decl| try cg.genCObjectMacro(typename, decl, is_async),
            .typedef,
            .cpp_quote,
            .struct_def,
            => {},
            .import => unreachable,
            else => unreachable,
        }
    }
}

fn genInterfaceCInlineWrappersParents(cg: *CodeGen, child_typename: []const u8, interface: Node.InterfaceDef) Error!void {
    if (interface.parents) |parents| {
        for (parents.start..parents.end) |index| {
            const parent_type = try cg.getInterfaceParentName(index);
            const parent = try cg.findSymbol(parent_type);
            switch (parent) {
                .interface => |iface| {
                    try cg.genInterfaceCInlineWrappersParents(child_typename, iface);

                    try cg.print("/*** ", .{});
                    try cg.genType(cg.data[index]);
                    try cg.print(" methods ***/\n", .{});

                    for (iface.defs.start..iface.defs.end) |parent_iface_index| {
                        const iface_def_node = cg.nodes[cg.data[parent_iface_index].toInt()];
                        switch (iface_def_node) {
                            .decl => |decl| try cg.genCInlineWrapper(child_typename, decl, false),
                            else => {},
                        }
                    }
                },
                else => unreachable,
            }
        }
    }
}

fn genInterfaceCInlineWrappers(cg: *CodeGen, interface: Node.InterfaceDef, is_async: bool) Error!void {
    const prefix = if (is_async) "Async" else "";
    const typename = cg.intern_pool.get(cg.nodes[interface.name.toInt()].type).?;
    try cg.genInterfaceCInlineWrappersParents(typename, interface);
    try cg.print("/*** {s}{s} methods ***/\n", .{ prefix, typename });
    for (interface.defs.start..interface.defs.end) |index| {
        const node = cg.nodes[cg.data[index].toInt()];
        switch (node) {
            .decl => |decl| try cg.genCInlineWrapper(typename, decl, is_async),
            .typedef,
            .cpp_quote,
            .struct_def,
            => {},
            .import => unreachable,
            else => unreachable,
        }
    }
}

fn genCInlineWrapper(cg: *CodeGen, interface_name: []const u8, decl: Node.Decl, is_async: bool) !void {
    // call_as(func) => this should not be generated
    if (try cg.getAttribute(decl.attributes, .call_as)) |_| return;

    const prefix = if (is_async) "Async" else "";

    if (decl.init_decl) |init_decl| {
        const id = try cg.nodeAs(init_decl, .init_declarator);
        const d = try cg.nodeAs(id.declarator, .declarator);

        for (if (is_async) func_gen_async else func_gen) |func| {
            const dd: Node.DirectDeclarator = blk: {
                if (cg.nodeAs(d.decl.?, .declarator)) |de| {
                    if (de.call_conv) |cc| try cg.genCallConv(cc);
                    break :blk try cg.nodeAs(de.decl.?, .direct_decl);
                } else |_| {
                    break :blk try cg.nodeAs(d.decl.?, .direct_decl);
                }
            };

            try cg.print("static inline ", .{});
            try cg.genType(decl.decl_spec); // Return Type
            try cg.print("{s}{s}_{s}", .{ prefix, interface_name, func.prefix });
            try cg.genType(dd.base); // Function Name
            try cg.print("({s}{s}* This", .{ prefix, interface_name });
            if (func.args) {
                if (cg.argListLen(dd.suffix) != 0) try cg.print(",", .{});
                try cg.genArgList(dd.suffix, .{});
            }
            try cg.print(") {{\n", .{});
            try cg.printIndent(4);
            try cg.print("return This->lpVtbl->{s}", .{func.prefix});
            try cg.genType(dd.base);
            try cg.print("(This", .{});
            if (func.args) {
                if (cg.argListLen(dd.suffix) != 0) try cg.print(",", .{});
                try cg.genArgList(dd.suffix, .{ .only_name = true });
            }
            try cg.print(");\n", .{});
            try cg.print("}}\n", .{});
        }
    }
}

fn genCObjectMacro(cg: *CodeGen, interface_name: []const u8, decl: Node.Decl, is_async: bool) !void {
    // call_as(func) => this should not be generated
    if (try cg.getAttribute(decl.attributes, .call_as)) |_| return;

    const prefix = if (is_async) "Async" else "";

    if (decl.init_decl) |init_decl| {
        const id = try cg.nodeAs(init_decl, .init_declarator);
        const d = try cg.nodeAs(id.declarator, .declarator);

        for (if (is_async) func_gen_async else func_gen) |func| {
            const dd: Node.DirectDeclarator = blk: {
                if (cg.nodeAs(d.decl.?, .declarator)) |de| {
                    if (de.call_conv) |cc| try cg.genCallConv(cc);
                    break :blk try cg.nodeAs(de.decl.?, .direct_decl);
                } else |_| {
                    break :blk try cg.nodeAs(d.decl.?, .direct_decl);
                }
            };

            try cg.print("#define {s}{s}_{s}", .{ prefix, interface_name, func.prefix });
            try cg.genType(dd.base); // Function Name
            try cg.print("(This", .{});
            if (func.args) {
                if (cg.argListLen(dd.suffix) != 0) try cg.print(",", .{});
                try cg.genArgList(dd.suffix, .{ .only_name = true });
            }
            try cg.print(") ", .{});
            try cg.print("(This)->lpVtbl->{s}", .{func.prefix});
            try cg.genType(dd.base);
            try cg.print("(This", .{});
            if (func.args) {
                if (cg.argListLen(dd.suffix) != 0) try cg.print(",", .{});
                try cg.genArgList(dd.suffix, .{ .only_name = true });
            }
            try cg.print(")\n", .{});
        }
    }
}

// TODO: Confirm when we generate this as missing some conditions.
fn genInterfaceStubProxy(cg: *CodeGen, interface: Node.InterfaceDef) !void {
    _ = interface;
    //const interface_name = cg.intern_pool.get(try cg.nodeAs(interface.name, .type)).?;
    //for (interface.defs.start..interface.defs.end) |index| {
    //    const node = cg.nodes[cg.data[index].toInt()];
    //    switch (node) {
    //        .decl => |decl| try cg.genInterfaceProxy(interface_name, decl),
    //        .typedef, .cpp_quote, .struct_def, => {},
    //        .import => unreachable,
    //        else => unreachable,
    //    }
    //}
    try cg.print("\n", .{});
}

fn genInterfaceProxy(cg: *CodeGen, interface_name: []const u8, decl: Node.Decl) !void {
    if (decl.attributes) |attrs| {
        // local => not remote callable?
        if (try cg.getAttribute(attrs, .local)) |_| return;
    }

    // TODO: Not being generated sometimes (e.g. wbcodecdsp.idl).
    try cg.genType(decl.decl_spec); // Return Type
    if (decl.init_decl) |init_decl| {
        const id = try cg.nodeAs(init_decl, .init_declarator);
        const d = try cg.nodeAs(id.declarator, .declarator);

        if (d.call_conv) |cc| try cg.genCallConv(cc) else try cg.print("STDMETHODCALLTYPE ", .{});

        const dd: Node.DirectDeclarator = blk: {
            if (cg.nodeAs(d.decl.?, .declarator)) |de| {
                if (de.call_conv) |cc| try cg.genCallConv(cc);
                break :blk try cg.nodeAs(de.decl.?, .direct_decl);
            } else |_| {
                break :blk try cg.nodeAs(d.decl.?, .direct_decl);
            }
        };

        try cg.print("{s}_", .{interface_name});
        try cg.genType(dd.base); // Function Name
        try cg.print("_Proxy(\n", .{});

        try cg.printIndent(4);
        try cg.print("{s} *This", .{interface_name});
        if (cg.argListLen(dd.suffix) != 0) try cg.print(",", .{});
        try cg.genArgList(dd.suffix, .{ .indent = 4, .split_args_by_line = true });
        try cg.print(");\n", .{});

        try cg.print("void __RPC_STUB {s}_", .{interface_name});
        try cg.genType(dd.base); // Function Name
        try cg.print(
            \\_Stub(
            \\    IRpcStubBuffer* This,
            \\    IRpcChannelBuffer* pRpcChannelBuffer,
            \\    PRPC_MESSAGE pRpcMessage,
            \\    DWORD* pdwStubPhase);
            \\
        , .{});
    }
}

fn genInterfaceAttributes(cg: *CodeGen, interface: Node.InterfaceDef, is_async: bool) !void {
    const prefix = if (is_async) "Async" else "";
    const tag: Node.Attribute.Tag = if (is_async) .async_uuid else .uuid;

    const typename = cg.intern_pool.get(try cg.nodeAs(interface.name, .type)).?;
    const maybe_uuid: ?u128 = if (try cg.getAttribute(interface.attributes, tag)) |attr|
        if (is_async) attr.async_uuid else attr.uuid
    else
        null;

    if (maybe_uuid) |uuid| {
        const c = uuidComponents(uuid);
        try cg.print(
            \\DEFINE_GUID(IID_{s}{s}, 0x{x:08}, 0x{x:04}, 0x{x:04}, 0x{x:02},0x{x:02}, 0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02});
            \\
        , .{ prefix, typename, c._96, c._80, c._64, c._56, c._48, c._40, c._32, c._24, c._16, c._8, c._0 });
    }
}

fn genLibrary(cg: *CodeGen, library: Node.LibraryDef) Error!void {
    const typename = cg.intern_pool.get(library.name).?;

    try cg.print(
        \\#ifndef __{0s}_LIBRARY_DEFINED__
        \\#define __{0s}_LIBRARY_DEFINED__
        \\
        \\
    , .{typename});

    // TODO: Are UUID's required?
    const maybe_uuid: ?u128 = blk: {
        if (library.attributes) |attrs| {
            if (try cg.getAttribute(attrs, .uuid)) |attr| {
                break :blk attr.uuid;
            }
        }
        break :blk null;
    };

    if (maybe_uuid) |uuid| {
        const c = uuidComponents(uuid);
        try cg.print(
            \\DEFINE_GUID(LIBID_{s}, 0x{x:08}, 0x{x:04}, 0x{x:04}, 0x{x:02},0x{x:02}, 0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02});
            \\
            \\
        , .{ typename, c._96, c._80, c._64, c._56, c._48, c._40, c._32, c._24, c._16, c._8, c._0 });
    }

    for (library.import_statements.start..library.import_statements.end) |index| {
        try cg.genNode(cg.data[index], 0);
    }

    try cg.print(
        \\#endif /* __{0s}_LIBRARY_DEFINED__ */
        \\
    , .{typename});
}

fn genCoClass(cg: *CodeGen, coclass: Node.CoClassDef) !void {
    const typename = cg.intern_pool.get(cg.nodes[coclass.name.toInt()].type).?;

    try cg.print(
        \\/*****************************************************************************
        \\ * {0s} coclass
        \\ */
        \\
        \\
    , .{typename});

    // TODO: Are UUID's required?
    const maybe_uuid: ?u128 = blk: {
        if (coclass.attributes) |attrs| {
            if (try cg.getAttribute(attrs, .uuid)) |attr| {
                break :blk attr.uuid;
            }
        }
        break :blk null;
    };

    if (maybe_uuid) |uuid| {
        const c = uuidComponents(uuid);
        try cg.print(
            \\DEFINE_GUID(CLSID_{s}, 0x{x:08}, 0x{x:04}, 0x{x:04}, 0x{x:02},0x{x:02}, 0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02});
            \\
            \\
        , .{ typename, c._96, c._80, c._64, c._56, c._48, c._40, c._32, c._24, c._16, c._8, c._0 });
    }

    try cg.print("#ifdef __cplusplus\n", .{});
    try cg.print("class ", .{});
    if (maybe_uuid) |uuid| {
        var buf: [64]u8 = undefined;
        const uuid_s = uuidToString(&buf, uuid);
        try cg.print("DECLSPEC_UUID(\"{s}\") ", .{uuid_s});
    }
    try cg.print("{s};\n", .{typename});

    if (maybe_uuid) |uuid| {
        const c = uuidComponents(uuid);

        try cg.print(
            \\#ifdef __CRT_UUID_DECL
            \\__CRT_UUID_DECL({s}, 0x{x:08}, 0x{x:04}, 0x{x:04}, 0x{x:02},0x{x:02}, 0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02})
            \\#endif
            \\
        , .{ typename, c._96, c._80, c._64, c._56, c._48, c._40, c._32, c._24, c._16, c._8, c._0 });
    }

    try cg.print("#endif\n\n", .{});
}

fn genInterface(cg: *CodeGen, interface: Node.InterfaceDef) Error!void {
    try cg.genInterfaceInternal(interface, false);

    if (interface.attributes) |attrs| {
        if (try cg.getAttribute(attrs, .async_uuid)) |_| {
            try cg.genInterfaceInternal(interface, true);
        }
    }
}

fn genInterfaceInternal(cg: *CodeGen, interface: Node.InterfaceDef, is_async: bool) Error!void {
    const typename = cg.intern_pool.get(try cg.nodeAs(interface.name, .type)).?;
    const prefix = if (is_async) "Async" else "";

    try cg.print(
        \\/*****************************************************************************
        \\ * {0s}{1s} interface
        \\ */
        \\#ifndef __{0s}{1s}_INTERFACE_DEFINED__
        \\#define __{0s}{1s}_INTERFACE_DEFINED__
        \\
        \\
    , .{ prefix, typename });

    try cg.genInterfaceTopLevel(interface);
    try cg.genInterfaceAttributes(interface, is_async);
    try cg.print("#if defined(__cplusplus) && !defined(CINTERFACE)\n", .{});
    try cg.genInterfaceClass(interface, is_async);
    try cg.print("#else\n", .{});
    try cg.genInterfaceCVtable(interface, is_async);
    try cg.print(
        \\#ifdef COBJMACROS
        \\#ifndef WIDL_C_INLINE_WRAPPERS
        \\
    , .{});
    try cg.genInterfaceCObjMacros(interface, is_async);
    try cg.print("#else\n", .{});
    try cg.genInterfaceCInlineWrappers(interface, is_async);
    try cg.print(
        \\#endif
        \\#endif
        \\
        \\#endif
        \\
        \\
    , .{});
    try cg.genInterfaceStubProxy(interface);
    try cg.print(
        \\#endif  /* __{s}{s}_INTERFACE_DEFINED__ */
        \\
        \\
    , .{ prefix, typename });
}

fn getAttribute(cg: *CodeGen, maybe_attrs: ?Node.Range, tag: Node.Attribute.Tag) !?Node.Attribute {
    if (maybe_attrs) |attrs| {
        const attr_refs = cg.data[attrs.start..attrs.end];
        for (attr_refs) |index| {
            const attr_node = try cg.nodeAs(index, .attribute);
            if (@as(Node.Attribute.Tag, attr_node) == tag) {
                return attr_node;
            }
        }
    }
    return null;
}

fn printIndent(cg: *CodeGen, indent: usize) !void {
    for (0..indent) |_| try cg.print(" ", .{});
}

fn print(cg: *CodeGen, comptime fmt: []const u8, args: anytype) !void {
    return cg.cc.print(fmt, args) catch |err| {
        log.err("io error: {s}", .{@errorName(err)});
        return error.IoError;
    };
}

fn getFilenameNoSuffix(cg: *CodeGen, filepath: []const u8) []const u8 {
    _ = cg;

    const filename = if (std.mem.lastIndexOfScalar(u8, filepath, '/')) |pos|
        filepath[pos + 1 ..]
    else
        filepath;

    return if (std.mem.lastIndexOfScalar(u8, filename, '.')) |pos|
        filename[0..pos]
    else
        filename;
}

fn makeHeaderGuardName(buf: []u8, filename: []const u8) ![]u8 {
    if (buf.len < 4 + filename.len) return error.Overflow;
    buf[0] = '_';
    buf[1] = '_';
    for (filename, 0..) |c, i| {
        buf[2 + i] = switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9' => c,
            else => '_',
        };
    }
    buf[2 + filename.len] = '_';
    buf[2 + filename.len + 1] = '_';
    return buf[0 .. 4 + filename.len];
}

// Given an idl filepath, take the basename and convert the suffix to .h.
//
// example/file.idl -> file.h
fn makeHeaderFilename(buf: []u8, filepath: []const u8) ![]u8 {
    const suffix = if (std.mem.lastIndexOfScalar(u8, filepath, '.')) |pos|
        filepath[pos..]
    else
        "";

    if (!std.mem.eql(u8, suffix, ".idl") and !std.mem.eql(u8, suffix, ".h")) {
        log.err("expected .h or .idl file but found {s}", .{filepath});
        return error.InvalidFile;
    }

    const filename = if (std.mem.lastIndexOfScalar(u8, filepath, '/')) |i|
        filepath[i + 1 ..]
    else
        filepath;

    var i: usize = 0;
    while (i < filename.len - suffix.len) : (i += 1) {
        buf[i] = filename[i];
    }
    @memcpy(buf[i..][0..2], ".h");
    return buf[0 .. 2 + i];
}

// TODO: Put this somewhere else
pub fn uuidToString(buf: []u8, uuid: u128) []const u8 {
    std.debug.assert(buf.len >= 36);

    const uuid1: u32 = @truncate(uuid >> 96);
    const uuid2: u16 = @truncate(uuid >> 80);
    const uuid3: u16 = @truncate(uuid >> 64);
    const uuid4: u16 = @truncate(uuid >> 48);
    const uuid5: u48 = @truncate(uuid);

    return std.fmt.bufPrint(buf, "{x:08}-{x:04}-{x:04}-{x:04}-{x:012}", .{
        uuid1,
        uuid2,
        uuid3,
        uuid4,
        uuid5,
    }) catch unreachable;
}

pub const UUIDComponents = struct {
    _96: u32,
    _80: u16,
    _64: u16,
    _56: u8,
    _48: u8,
    _40: u8,
    _32: u8,
    _24: u8,
    _16: u8,
    _8: u8,
    _0: u8,
};

pub fn uuidComponents(uuid: u128) UUIDComponents {
    const h: u64 = @intCast(uuid >> 64);
    const l: u64 = @truncate(uuid);

    return .{
        ._96 = @truncate(h >> 32),
        ._80 = @truncate(h >> 16),
        ._64 = @truncate(h),
        ._56 = @truncate(l >> 56),
        ._48 = @truncate(l >> 48),
        ._40 = @truncate(l >> 40),
        ._32 = @truncate(l >> 32),
        ._24 = @truncate(l >> 24),
        ._16 = @truncate(l >> 16),
        ._8 = @truncate(l >> 8),
        ._0 = @truncate(l >> 0),
    };
}
