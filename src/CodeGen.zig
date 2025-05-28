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
        .disp_interface_def => |interface| try cg.genDispInterface(interface),
        .disp_interface_ref, .interface_ref => |ref| try cg.genInterfaceRef(ref),
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
        .struct_def => |struct_def| {
            try cg.genStructDef(struct_def, .root);
            try cg.print(";\n", .{});
        },
        .importlib => {}, // TODO: Confirm
        .decl => |decl| try cg.genDecl(decl),
        else => {
            std.debug.print("unsupported node type: {s}\n", .{@tagName(node)});
            return error.UnexpectedNodeType;
        },
    }
}

fn genDecl(cg: *CodeGen, decl: Node.Decl) !void {
    // `const` declarations are mapped to defines
    if (cg.isConstDecl(decl.decl_spec)) {
        try cg.genConstDecl(decl);
    } else {
        try cg.genType(decl.decl_spec);
        if (decl.init_decl) |d| try cg.genType(d);
        try cg.print(";\n", .{});
    }
}

fn isConstDecl(cg: *CodeGen, ref: Node.Ref) bool {
    return switch (cg.nodes[ref.toInt()]) {
        .decl => |decl| cg.isConstDecl(decl.decl_spec),
        .decl_spec_type => |ds| {
            if (ds.decl_specs.empty()) return false;
            // Must start with const
            return cg.isConstDecl(cg.data[ds.decl_specs.start]);
        },
        .decl_spec => |tag| switch (tag) {
            .@"const" => true,
            else => false,
        },
        else => false,
    };
}

fn isFuncDecl(cg: *CodeGen, ref: Node.Ref) bool {
    // ArgList => function
    return switch (cg.nodes[ref.toInt()]) {
        .decl => |decl| if (decl.init_decl) |init_decl| cg.isFuncDecl(init_decl) else false,
        .init_declarator => |init_decl| cg.isFuncDecl(init_decl.declarator),
        .any_declarator,
        .declarator,
        .abstract_declarator,
        => |any_decl| if (any_decl.decl) |decl| cg.isFuncDecl(decl) else false,
        .direct_decl => |direct_decl| {
            for (direct_decl.suffix.start..direct_decl.suffix.end) |index| {
                if (cg.isFuncDecl(cg.data[index])) return true;
            }
            return false;
        },
        .arg_list => {
            return true;
        },
        else => {
            //log.err("return false on node: ", .{});
            //@import("node_print.zig").dumpNode(cg.parser, ref);
            return false;
        },
    };
}

fn genConstDecl(cg: *CodeGen, decl: Node.Decl) !void {
    std.debug.assert(cg.isConstDecl(decl.decl_spec));

    if (decl.init_decl) |init_decl_ref| {
        const init_decl: Node.InitDeclarator = try cg.nodeAs(init_decl_ref, .init_declarator);
        try cg.print("#define ", .{});
        try cg.genName(init_decl.declarator);
        if (init_decl.expr) |expr| {
            try cg.print(" (", .{});
            try cg.genExpr(expr);
            try cg.print(")", .{});
        }
        try cg.print("\n\n", .{});
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
        .base_type => |tag| {
            try cg.print("{s}", .{tag.symbol()});
        },
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
        .qualified_type => |ty| {
            try cg.genName(ty.typename);
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
            const name = if (struct_def.name) |name| cg.intern_pool.get(name).? else "(unnamed)";
            try cg.print("struct {s}", .{name});
        },
        .enum_def => |enum_def| {
            const name = if (enum_def.name) |name| cg.intern_pool.get(name).? else "(unnamed)";
            try cg.print("enum {s}", .{name});
        },
        .union_def => |union_def| {
            const name = if (union_def.name) |name| cg.intern_pool.get(name).? else "(unnamed)";
            try cg.print("union {s}", .{name});
        },
        else => {
            std.debug.print("genName: unsupported node type: {s}\n", .{@tagName(node)});
            return error.UnexpectedNodeType;
        },
    }
}

fn genType(cg: *CodeGen, node_ref: Node.Ref) Error!void {
    return cg.genTypeContext(node_ref, .none);
}

const GenTypeContext = enum {
    // No context specified, use default behaviour.
    none,
    // The GenType call originated from within a definition (e.g. a struct def).
    def,
};

fn genTypeContext(cg: *CodeGen, node_ref: Node.Ref, context: GenTypeContext) Error!void {
    const node = cg.nodes[node_ref.toInt()];
    switch (node) {
        .enum_def => |def| try cg.genEnumDef(def),
        .struct_def => |def| try cg.genStructDef(def, .root),
        .array_def => |a| switch (a) {
            .empty => try cg.print("[]", .{}),
            .asterisk => try cg.print("[{s}]", .{if (context == .def) "1" else ""}),
            .expr => |e| {
                try cg.print("[", .{});
                try cg.genExpr(e);
                try cg.print("]", .{});
            },
        },
        .safe_array => |arr| {
            try cg.print("SAFEARRAY *", .{});
            try cg.genTypeContext(arr, context);
        },
        .enum_ref => |ref| {
            try cg.print("enum {s}", .{cg.intern_pool.get(ref).?});
        },
        .struct_ref => |ref| {
            try cg.print("struct {s}", .{cg.intern_pool.get(ref).?});
        },
        .qualified_type => |qual_type| {
            for (qual_type.namespace.start..qual_type.namespace.end) |index| {
                try cg.genTypeContext(cg.data[index], context);
                try cg.print(".", .{});
            }
            try cg.genTypeContext(qual_type.typename, context);
        },
        .unqualified_decl_spec_type,
        .decl_spec_type,
        => |decl_spec| {
            for (decl_spec.decl_specs.start..decl_spec.decl_specs.end) |index| {
                try cg.genTypeContext(cg.data[index], context);
                try cg.print(" ", .{});
            }
            try cg.genTypeContext(decl_spec.type, context);
            try cg.print(" ", .{});
        },
        .decl_spec => |decl_spec| {
            try cg.print("{s}", .{@tagName(decl_spec)});
        },
        .base_type => |tag| {
            try cg.print("{s}", .{tag.symbol()});
        },
        .known_type,
        .type,
        => |ref| {
            var name = cg.intern_pool.get(ref).?;
            if (std.mem.eql(u8, name, "BOOL")) {
                name = "WINBOOL";
            }
            try cg.print("{s}", .{name});
        },
        .init_declarator => |init_decl| {
            try cg.genTypeContext(init_decl.declarator, context);
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
                try cg.genTypeContext(ok, context);
            }
        },
        .direct_decl => |direct_decl| {
            try cg.genTypeContext(direct_decl.base, context);
            for (direct_decl.suffix.start..direct_decl.suffix.end) |index| {
                try cg.genTypeContext(cg.data[index], context);
            }
        },
        .arg_list => |arg_list| {
            try cg.print("(\n", .{});
            for (arg_list.args.start..arg_list.args.end) |index| {
                try cg.genTypeContext(cg.data[index], context);
                try cg.print(",\n", .{});
            }
            if (arg_list.is_varargs) {
                try cg.print("...)", .{});
            }
            try cg.print(")", .{});
        },
        .func_param => |func_param| {
            try cg.genTypeContext(func_param.decl_spec, context);
            // TODO: Handle attributes
            if (func_param.any_decl) |a| {
                try cg.genTypeContext(a, context);
            }
        },
        .union_switch => |sw| {
            const name = if (sw.name) |i| cg.intern_pool.get(i).? else "__C89_NAMELESS";
            try cg.print("struct {s} {{\n", .{name});

            // Tag
            try cg.printIndent(cg.indent + 4);
            try cg.genTypeContext(sw.sfield, context);
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
                                try cg.genTypeContext(f, context);
                            },
                            .s_field_optional => |mf| if (mf) |f| {
                                try cg.printIndent(cg.indent + 8);
                                try cg.genTypeContext(f, context);
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
                            try cg.genTypeContext(f, context);
                        },
                        .s_field_optional => |mf| if (mf) |f| {
                            cg.indent += 4;
                            defer cg.indent -= 4;
                            try cg.printIndent(cg.indent);
                            try cg.genTypeContext(f, context);
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
            .s_field => |f| try cg.genTypeContext(f, context),
            .s_field_optional => |mf| if (mf) |f| try cg.genTypeContext(f, context) else {},
        },
        .union_sfield => |sfield| switch (sfield) {
            .struct_def => |sf| try cg.genStructDef(cg.nodes[sf.def.toInt()].struct_def, .member),
            .default => |df| {
                try cg.genTypeContext(df.decl_spec, context);
                try cg.genTypeContext(df.declarator, context);
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

                .number => |n| switch (n) {
                    .hex_int => |v| try cg.print("0x{x}", .{v}),
                    .int => |v| try cg.print("{}", .{v}),
                    .float => |v| try cg.print("{}", .{v}),
                },

                inline .identifier,
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
            try cg.print("(", .{});
            try cg.genExpr(p.arg1);
            try cg.print(" {s} ", .{p.op.symbol()});
            try cg.genExpr(p.arg2);
            try cg.print(")", .{});
        },
        .expr_ternary => |p| {
            try cg.print("(", .{});
            try cg.genExpr(p.arg1);
            try cg.print(" ? ", .{});
            try cg.genExpr(p.arg2);
            try cg.print(" : ", .{});
            try cg.genExpr(p.arg3);
            try cg.print(")", .{});
        },
        .sizeof => |s| {
            try cg.print("sizeof(", .{});
            try cg.genType(s.unqual_decl_spec);
            try cg.print(")", .{});
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

    const func_prefix = if (try cg.getAttribute(decl.attributes, .propget)) |_|
        "get_"
    else if (try cg.getAttribute(decl.attributes, .propput)) |_|
        "put_"
    else if (try cg.getAttribute(decl.attributes, .propputref)) |_|
        "putref_"
    else
        "";

    if (decl.init_decl) |init_decl| {
        if (!cg.isFuncDecl(init_decl)) return;

        const id = try cg.nodeAs(init_decl, .init_declarator);
        const d = try cg.nodeAs(id.declarator, .declarator);

        for (if (is_async) func_gen_async else func_gen) |vfunc| {
            const dd: Node.DirectDeclarator = blk: {
                if (cg.nodeAs(d.decl.?, .declarator)) |de| {
                    if (de.call_conv) |cc| try cg.genCallConv(cc);
                    break :blk try cg.nodeAs(de.decl.?, .direct_decl);
                } else |_| {
                    break :blk try cg.nodeAs(d.decl.?, .direct_decl);
                }
            };

            // Clean this up, should share more logic. Probably should do an AST -> IR pass or at minimum clean
            // up the AST so it is less generic.
            const is_aggregate_type = !d.is_pointer and cg.isAggregateType(decl.decl_spec);
            if (is_aggregate_type) {
                const aggregate_return_type = decl.decl_spec;
                try cg.print("#ifdef WIDL_EXPLICIT_AGGREGATE_RETURNS\n", .{});
                // Render virtual func
                {
                    try cg.printIndent(4);
                    try cg.print("virtual ", .{});
                    try cg.genType(aggregate_return_type); // Return Type
                    std.debug.assert(!d.is_pointer); // Aggregate Type => not pointer
                    try cg.print("* ", .{});
                    if (d.call_conv) |cc| try cg.genCallConv(cc) else try cg.print("STDMETHODCALLTYPE ", .{});

                    try cg.print("{s}{s}", .{ vfunc.prefix, func_prefix });
                    try cg.genType(dd.base); // Function Name
                    try cg.print("(\n", .{});

                    // Will always have an initial argument
                    try cg.printIndent(8);
                    try cg.genType(aggregate_return_type);
                    try cg.print("*__ret", .{});
                    if (vfunc.args and cg.argListLen(dd.suffix) != 0) {
                        try cg.print(",", .{});
                        try cg.genArgList(dd.suffix, .{ .indent = 8, .split_args_by_line = true });
                    }
                    try cg.print(") = 0;\n", .{});
                }

                // Render non virtual proxy func
                {
                    try cg.printIndent(4);
                    try cg.genType(aggregate_return_type); // Return Type
                    if (d.call_conv) |cc| try cg.genCallConv(cc) else try cg.print("STDMETHODCALLTYPE ", .{});

                    try cg.print("{s}{s}", .{ vfunc.prefix, func_prefix });
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
                    try cg.print(")\n", .{});

                    // Body
                    try cg.printIndent(4);
                    try cg.print("{{\n", .{});
                    try cg.printIndent(8);
                    try cg.genType(aggregate_return_type);
                    try cg.print("__ret;\n", .{});
                    try cg.printIndent(8);
                    try cg.print("return *", .{});
                    try cg.genType(dd.base); // Function name
                    try cg.print("(&__ret", .{});
                    if (cg.argListLen(dd.suffix) != 0) {
                        try cg.print(",", .{});
                        try cg.genArgList(dd.suffix, .{ .split_args_by_line = false, .only_name = true });
                    }
                    try cg.print(");\n", .{});
                    try cg.printIndent(4);
                    try cg.print("}}\n", .{});
                }

                try cg.print("#else\n", .{});
            }

            try cg.printIndent(4);
            try cg.print("virtual ", .{});
            try cg.genType(decl.decl_spec); // Return Type
            if (d.is_pointer) {
                for (0..cg.pointerCount(id.declarator)) |_| try cg.print("*", .{});
                try cg.print(" ", .{});
            }
            if (d.call_conv) |cc| try cg.genCallConv(cc) else try cg.print("STDMETHODCALLTYPE ", .{});

            try cg.print("{s}{s}", .{ vfunc.prefix, func_prefix });
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
            try cg.print(") = 0;\n", .{});
            if (is_aggregate_type) {
                try cg.print("#endif\n", .{});
            }
            try cg.print("\n", .{});
        }
    }
}

const ConstVal = union(enum) {
    hex_int: isize,
    int: isize,

    pub fn inc(a: ConstVal) ConstVal {
        return switch (a) {
            .hex_int => |e| .{ .hex_int = e + 1 },
            .int => |e| .{ .int = e + 1 },
        };
    }

    fn asInt(a: ConstVal) isize {
        return switch (a) {
            inline .hex_int,
            .int,
            => |v| v,
        };
    }
};

fn resolveConstExpr(cg: *CodeGen, expr: Node.Ref) Error!ConstVal {
    const node = cg.nodes[expr.toInt()];
    return switch (node) {
        inline .expr_const,
        .expr_const_int,
        => |sub_expr| try cg.resolveConstExpr(sub_expr),
        .expr_unary => |expr_unary| {
            const a = try cg.resolveConstExpr(expr_unary.arg1);
            return .{ .int = try computeOp(expr_unary.op, a.asInt(), null, null) };
        },
        .expr_binary => |expr_binary| {
            const a = try cg.resolveConstExpr(expr_binary.arg1);
            const b = try cg.resolveConstExpr(expr_binary.arg2);
            return .{ .int = try computeOp(expr_binary.op, a.asInt(), b.asInt(), null) };
        },
        .expr_ternary => |expr_ternary| {
            const a = try cg.resolveConstExpr(expr_ternary.arg1);
            const b = try cg.resolveConstExpr(expr_ternary.arg2);
            const c = try cg.resolveConstExpr(expr_ternary.arg3);
            return .{ .int = try computeOp(expr_ternary.op, a.asInt(), b.asInt(), c.asInt()) };
        },
        .expr => |e| switch (e) {
            .false => .{ .int = 0 },
            .true => .{ .int = 1 },
            .number => |n| switch (n) {
                .hex_int => |v| .{ .hex_int = v },
                .int => |v| .{ .int = v },
                // TODO: Better handle implicit coercion in const vals. Actually handle bits etc.
                .float => |v| .{ .int = @intFromFloat(v) },
            },
            .expr => |ev| try cg.resolveConstExpr(ev),
            // TODO: May need to handle constant symbols
            .identifier,
            .string,
            => .{ .int = 0 },
            .null,
            => error.CodeGenError,
        },
        // TODO: Handle cast properly, need to be able to target a specific type
        .cast => |e| cg.resolveConstExpr(e.expr),
        else => {
            log.err("resolveConstExpr: unsupported node {s}", .{@tagName(node)});
            return error.CodeGenError;
        },
    };
}

fn computeOp(op: Node.Op, a: isize, b: ?isize, c: ?isize) !isize {
    return switch (op) {
        .ternary_conditional => if (a != 0) b.? else c.?,
        .logical_or => if (a != 0 or b.? != 0) 1 else 0,
        .logical_and => if (a != 0 and b.? != 0) 1 else 0,
        .@"and" => a & b.?,
        .@"or" => a | b.?,
        .xor => a ^ b.?,
        .equal => if (a == b.?) 1 else 0,
        .unequal => if (a != b.?) 1 else 0,
        .compare_lt => if (a < b.?) 1 else 0,
        .compare_gt => if (a > b.?) 1 else 0,
        .compare_lte => if (a <= b.?) 1 else 0,
        .compare_gte => if (a >= b.?) 1 else 0,
        .shift_left => std.math.shl(isize, a, b.?),
        .shift_right => std.math.shr(isize, a, b.?),
        .add => a + b.?,
        .sub => a - b.?,
        .mul => a * b.?,
        .div => @divTrunc(a, b.?),
        .mod => @mod(a, b.?),
        .positive => @intCast(@abs(a)),
        .negative => -a,
        .logical_not => if (a != 0) 0 else 1,
        .not => ~a,
        .inc => a + 1,
        .dec => a + 1,

        .ref,
        .deref,
        => error.CodeGenError,
    };
}

fn isAggregateType(cg: *CodeGen, ref: Node.Ref) bool {
    return switch (cg.nodes[ref.toInt()]) {
        .decl_spec_type => |dt| cg.isAggregateType(dt.type),
        .qualified_type => |qt| cg.isAggregateType(qt.typename),
        .type => |t| {
            const name = cg.intern_pool.get(t) orelse return false;
            return if (cg.parser.symbol_table.get(name)) |def| switch (cg.nodes[def.toInt()]) {
                .struct_def => true,
                else => false,
            } else false;
        },
        else => false,
    };
}

fn genEnumDef(cg: *CodeGen, enum_def: Node.EnumDef) !void {
    try cg.print("enum ", .{});
    try cg.genNameOrFallback(enum_def.name);

    // v1_enum attribute is attached to typedef, needs to be propagated through.
    var next_enum_value: ConstVal = .{ .int = 0 };

    try cg.print(" {{\n", .{});
    for (enum_def.members.start..enum_def.members.end) |index| {
        try cg.printIndent(4);
        const enum_field: Node.EnumField = try cg.nodeAs(cg.data[index], .enum_field);
        try cg.print("{s}", .{cg.intern_pool.get(enum_field.name).?});
        if (enum_field.expr) |expr| {
            try cg.print(" = ", .{});
            try cg.genExpr(expr);
            next_enum_value = try cg.resolveConstExpr(expr);
            next_enum_value = next_enum_value.inc();
        } else {
            switch (next_enum_value) {
                .hex_int => |v| try cg.print(" = 0x{x}", .{v}),
                .int => |v| try cg.print(" = {}", .{v}),
            }
            next_enum_value = next_enum_value.inc();
        }
        const last = index + 1 == enum_def.members.end;
        try cg.print("{s}\n", .{if (!last) "," else ""});
    }
    try cg.print("}}", .{});
}

// TODO: Clean this up and merge with the union definition code if possible.
fn genStructDef(cg: *CodeGen, struct_def: Node.StructDef, ty: enum { root, member }) !void {
    switch (ty) {
        .root => {
            try cg.print("struct ", .{});
            try cg.genNameOrFallback(struct_def.name);
        },
        .member => {
            try cg.print("__C89_NAMELESS struct", .{});
        },
    }

    try cg.print(" {{\n", .{});
    for (struct_def.fields.start..struct_def.fields.end) |index| {
        cg.indent += 4;
        defer cg.indent -= 4;

        const sfield: Node.StructField = try cg.nodeAs(cg.data[index], .struct_field);
        switch (sfield.field) {
            .@"union" => |ref| {
                try cg.printIndent(cg.indent);
                try cg.genTypeContext(ref, .def);
                try cg.print(";\n", .{});
            },
            .default => |node| {
                for (node.struct_decl_list.start..node.struct_decl_list.end) |j| {
                    const struct_decl: Node.StructDecl = try cg.nodeAs(cg.data[j], .struct_decl);
                    try cg.printIndent(cg.indent);
                    try cg.genTypeContext(node.decl_spec, .def);
                    try cg.genTypeContext(struct_decl.any_decl, .def);
                    try cg.print(";\n", .{});
                }
            },
        }
    }
    try cg.printIndent(cg.indent);
    try cg.print("}}", .{});
    if (ty == .member) {
        // TODO: Handle multiple anonymous structs in one union
        try cg.print(" __C89_NAMELESSSTRUCTNAME", .{});
    }
}

fn genNameOrFallback(cg: *CodeGen, ref: ?InternPool.Ref) !void {
    if (ref) |name| {
        try cg.print("{s}", .{cg.intern_pool.get(name).?});
    } else {
        try cg.print("__{s}_{s}_generated_name_{:08}", .{
            cg.options.program_identifier_short,
            cg.getFilenameNoSuffix(cg.options.source_filepath),
            cg.next_id,
        });
        cg.next_id += 1;
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
            .decl => |decl| {
                if (cg.isConstDecl(decl.decl_spec)) try cg.genConstDecl(decl);
            },
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
            .enum_def,
            .union_def,
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
        try cg.print("#ifdef __CRT_UUID_DECL\n", .{});
        try cg.print("__CRT_UUID_DECL({s}{s}, ", .{ prefix, typename });
        try cg.genUuidList(uuid);
        try cg.print(")\n", .{});
        try cg.print("#endif\n", .{});
    }
}

const GenArgListOptions = struct {
    indent: usize = 0,
    split_args_by_line: bool = false,
    only_name: bool = false,
};

fn pointerCount(cg: *CodeGen, ref: Node.Ref) usize {
    return switch (cg.nodes[ref.toInt()]) {
        .abstract_declarator,
        .any_declarator,
        .declarator,
        => |d| @intFromBool(d.is_pointer) +
            if (d.decl) |sd| cg.pointerCount(sd) else 0,
        else => 0,
    };
}

fn isVoidType(cg: *CodeGen, index: Node.Ref) bool {
    return switch (cg.nodes[index.toInt()]) {
        .func_param => |p| {
            const count = if (p.any_decl) |d| cg.pointerCount(d) else 0;
            return count == 0 and cg.isVoidType(p.decl_spec);
        },
        .any_declarator,
        .abstract_declarator,
        .declarator,
        => |d| {
            const count = cg.pointerCount(index);
            return count == 0 and if (d.decl) |decl| cg.isVoidType(decl) else false;
        },
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

fn isNamed(cg: *CodeGen, ref: Node.Ref) bool {
    return switch (cg.nodes[ref.toInt()]) {
        .func_param => |e| if (e.any_decl) |decl| cg.isNamed(decl) else false,
        .any_declarator => |d| if (d.decl) |decl| cg.isNamed(decl) else false,
        .direct_decl => true,
        else => true,
    };
}

fn genArgList(cg: *CodeGen, range: Node.Range, options: GenArgListOptions) !void {
    // `(void)` argument should not emit
    if (cg.argListLen(range) == 0) return;

    var unnamed: u8 = 0;
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
            if (!cg.isNamed(cg.data[j])) {
                // TODO: Does not handle > 26 arguments.
                // TODO: Does not handle named argument in the set.
                std.debug.assert(unnamed <= 26);
                try cg.print("{c}", .{'a' + unnamed});
                unnamed += 1;
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

    const func_prefix = if (try cg.getAttribute(decl.attributes, .propget)) |_|
        "get_"
    else if (try cg.getAttribute(decl.attributes, .propput)) |_|
        "put_"
    else if (try cg.getAttribute(decl.attributes, .propputref)) |_|
        "putref_"
    else
        "";

    if (decl.init_decl) |init_decl| {
        if (!cg.isFuncDecl(init_decl)) return;

        const id = try cg.nodeAs(init_decl, .init_declarator);
        const d = try cg.nodeAs(id.declarator, .declarator);

        for (if (is_async) func_gen_async else func_gen) |func| {
            const is_aggregate_return_type = !d.is_pointer and cg.isAggregateType(decl.decl_spec);
            try cg.printIndent(4);
            try cg.genType(decl.decl_spec); // Return Type
            if (is_aggregate_return_type) {
                std.debug.assert(!d.is_pointer);
                try cg.print("* ", .{});
            }
            if (d.is_pointer) {
                for (0..cg.pointerCount(id.declarator)) |_| try cg.print("*", .{});
                try cg.print(" ", .{});
            }
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

            try cg.print("*{s}{s}", .{ func.prefix, func_prefix });
            try cg.genType(dd.base); // Function Name
            try cg.print(")", .{});
            try cg.print("(\n", .{});
            try cg.printIndent(8);
            try cg.print("{s}{s} *This", .{ if (is_async) "Async" else "", interface_name }); // BaseType argument
            if (is_aggregate_return_type) {
                try cg.print(",\n", .{});
                try cg.printIndent(8);
                try cg.genType(decl.decl_spec);
                try cg.print("*__ret", .{});
            }
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
                    if (iface.defs.empty()) continue;

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

    if (!interface.defs.empty()) {
        try cg.printIndent(4);
        try cg.print("/*** {s}{s} methods ***/\n", .{ prefix, typename });

        for (interface.defs.start..interface.defs.end) |index| {
            const node = cg.nodes[cg.data[index].toInt()];
            switch (node) {
                .decl => |decl| try cg.genCVtableFuncDef(typename, decl, is_async),
                .typedef,
                .cpp_quote,
                .struct_def,
                .enum_def,
                .union_def,
                => {},
                else => unreachable,
            }
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
                    if (iface.defs.empty()) continue;

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
    if (interface.defs.empty()) return;
    try cg.print("/*** {s}{s} methods ***/\n", .{ prefix, typename });
    for (interface.defs.start..interface.defs.end) |index| {
        const node = cg.nodes[cg.data[index].toInt()];
        switch (node) {
            .decl => |decl| try cg.genCObjectMacro(typename, decl, is_async),
            .typedef,
            .cpp_quote,
            .struct_def,
            .enum_def,
            .union_def,
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
                    if (iface.defs.empty()) continue;

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
    if (interface.defs.empty()) return;
    try cg.print("/*** {s}{s} methods ***/\n", .{ prefix, typename });
    for (interface.defs.start..interface.defs.end) |index| {
        const node = cg.nodes[cg.data[index].toInt()];
        switch (node) {
            .decl => |decl| try cg.genCInlineWrapper(typename, decl, is_async),
            .typedef,
            .cpp_quote,
            .struct_def,
            .enum_def,
            .union_def,
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
    const func_prefix = if (try cg.getAttribute(decl.attributes, .propget)) |_|
        "get_"
    else if (try cg.getAttribute(decl.attributes, .propput)) |_|
        "put_"
    else if (try cg.getAttribute(decl.attributes, .propputref)) |_|
        "putref_"
    else
        "";

    if (decl.init_decl) |init_decl| {
        if (!cg.isFuncDecl(init_decl)) return;

        const id = try cg.nodeAs(init_decl, .init_declarator);
        const d = try cg.nodeAs(id.declarator, .declarator);

        for (if (is_async) func_gen_async else func_gen) |func| {
            const is_aggregate_return_type = !d.is_pointer and cg.isAggregateType(decl.decl_spec);
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
            if (d.is_pointer) {
                for (0..cg.pointerCount(id.declarator)) |_| try cg.print("*", .{});
                try cg.print(" ", .{});
            }
            try cg.print("{s}{s}_{s}{s}", .{ prefix, interface_name, func.prefix, func_prefix });
            try cg.genType(dd.base); // Function Name
            try cg.print("({s}{s}* This", .{ prefix, interface_name });
            if (func.args) {
                if (cg.argListLen(dd.suffix) != 0) try cg.print(",", .{});
                try cg.genArgList(dd.suffix, .{});
            }
            try cg.print(") {{\n", .{});

            if (is_aggregate_return_type) {
                try cg.printIndent(4);
                try cg.genType(decl.decl_spec);
                try cg.print("__ret;\n", .{});
            }

            try cg.printIndent(4);
            // Ideally use the ref to decl and don't need to conditions
            if (!cg.isVoidType(decl.decl_spec) or cg.pointerCount(id.declarator) != 0) {
                try cg.print("return ", .{});
            }
            try cg.print("{s}This->lpVtbl->{s}{s}", .{ if (is_aggregate_return_type) "*" else "", func.prefix, func_prefix });
            try cg.genType(dd.base);
            try cg.print("(This", .{});
            if (is_aggregate_return_type) {
                try cg.print(",&__ret", .{});
            }
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
    const func_prefix = if (try cg.getAttribute(decl.attributes, .propget)) |_|
        "get_"
    else if (try cg.getAttribute(decl.attributes, .propput)) |_|
        "put_"
    else if (try cg.getAttribute(decl.attributes, .propputref)) |_|
        "putref_"
    else
        "";

    if (decl.init_decl) |init_decl| {
        if (!cg.isFuncDecl(init_decl)) return;

        const id = try cg.nodeAs(init_decl, .init_declarator);
        const d = try cg.nodeAs(id.declarator, .declarator);

        for (if (is_async) func_gen_async else func_gen) |func| {
            const is_aggregate_return_type = !d.is_pointer and cg.isAggregateType(decl.decl_spec);
            const dd: Node.DirectDeclarator = blk: {
                if (cg.nodeAs(d.decl.?, .declarator)) |de| {
                    if (de.call_conv) |cc| try cg.genCallConv(cc);
                    break :blk try cg.nodeAs(de.decl.?, .direct_decl);
                } else |_| {
                    break :blk try cg.nodeAs(d.decl.?, .direct_decl);
                }
            };

            try cg.print("#define {s}{s}_{s}{s}", .{ prefix, interface_name, func.prefix, func_prefix });
            try cg.genType(dd.base); // Function Name
            try cg.print("(This", .{});
            if (func.args) {
                if (cg.argListLen(dd.suffix) != 0) try cg.print(",", .{});
                try cg.genArgList(dd.suffix, .{ .only_name = true });
            }
            try cg.print(") ", .{});

            if (is_aggregate_return_type) {
                try cg.print("{s}{s}_{s}{s}", .{ prefix, interface_name, func.prefix, func_prefix });
                try cg.genType(dd.base);
                try cg.print("_define_WIDL_C_INLINE_WRAPPERS_for_aggregate_return_support\n", .{});
            } else {
                try cg.print("(This)->lpVtbl->{s}{s}", .{ func.prefix, func_prefix });
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
        if (d.is_pointer) {
            for (0..cg.pointerCount(id.declarator)) |_| try cg.print("*", .{});
            try cg.print(" ", .{});
        }
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
        try cg.print("DEFINE_GUID(IID_{s}{s}, ", .{ prefix, typename });
        try cg.genUuidList(uuid);
        try cg.print(");\n", .{});
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
        try cg.print("DEFINE_GUID(LIBID_{s}, ", .{typename});
        try cg.genUuidList(uuid);
        try cg.print(");\n\n", .{});
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
        try cg.print("DEFINE_GUID(CLSID_{s}, ", .{typename});
        try cg.genUuidList(uuid);
        try cg.print(");\n\n", .{});
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
        try cg.print("#ifdef __CRT_UUID_DECL\n", .{});
        try cg.print("__CRT_UUID_DECL({s}, ", .{typename});
        try cg.genUuidList(uuid);
        try cg.print(")\n", .{});
        try cg.print("#endif\n", .{});
    }

    try cg.print("#endif\n\n", .{});
}

// A dispatch interface is similar to a standard interface, except it implies
// inheritance of the IDispatch and IUknown interfaces. There are likely some
// other details (esp around props/methods) but will get to that in future.
fn genDispInterface(cg: *CodeGen, disp_interface: Node.DispInterfaceDef) Error!void {
    //const ref_IDispatch = cg.parser.symbol_table.get("IDispatch") orelse return error.CodeGenError;
    //const ref_IUnknown = cg.parser.symbol_table.get("IUnknown") orelse return error.CodeGenError;

    const interface: Node.InterfaceDef = .{
        .name = disp_interface.name,
        .type_params = null,
        .attributes = disp_interface.attributes,
        .parents = null, //try cg.parser.addData(.{ ref_IDispatch, ref_IUnknown }),
        .requires = null,
        .defs = .{},
    };

    try cg.genInterface(interface);
}

fn genInterface(cg: *CodeGen, interface: Node.InterfaceDef) Error!void {
    try cg.genInterfaceInternal(interface, false);

    if (interface.attributes) |attrs| {
        if (try cg.getAttribute(attrs, .async_uuid)) |_| {
            try cg.genInterfaceInternal(interface, true);
        }
    }
}

fn hasFuncDefinitions(cg: *CodeGen, interface: Node.InterfaceDef) bool {
    for (interface.defs.start..interface.defs.end) |index| {
        if (cg.isFuncDecl(cg.data[index])) return true;
    }
    return false;
}

fn genInterfaceInternal(cg: *CodeGen, interface: Node.InterfaceDef, is_async: bool) Error!void {
    const typename = cg.intern_pool.get(try cg.nodeAs(interface.name, .type)).?;
    const prefix = if (is_async) "Async" else "";
    const version = if (try cg.getAttribute(interface.attributes, .version)) |v|
        cg.nodes[v.version.toInt()].version
    else
        null;

    try cg.print(
        \\/*****************************************************************************
        \\ * {0s}{1s} interface
    , .{ prefix, typename });
    if (version) |v| {
        try cg.print(" (v{})\n", .{v});
    } else {
        try cg.print("\n", .{});
    }
    try cg.print(
        \\ */
        \\#ifndef __{0s}{1s}_INTERFACE_DEFINED__
        \\#define __{0s}{1s}_INTERFACE_DEFINED__
        \\
        \\
    , .{ prefix, typename });

    if (version) |v| {
        var version_buf: [64]u8 = undefined;
        const version_str = std.fmt.bufPrint(&version_buf, "{}", .{v}) catch return error.IoError;
        for (version_str) |*c| {
            if (c.* == '.') c.* = '_';
        }
        try cg.print(
            \\extern RPC_IF_HANDLE {0s}{1s}_v{2s}_c_ifspec;
            \\extern RPC_IF_HANDLE {0s}{1s}_v{2s}_s_ifspec;
            \\
            \\
        , .{ prefix, typename, version_str });
    }

    try cg.genInterfaceTopLevel(interface);
    try cg.genInterfaceAttributes(interface, is_async);
    // TODO: This check is not sufficient. For example, interface_ref's result in this still being generated. Also
    // need to consider parents, and inheriting without defining in child functions.
    if (cg.hasFuncDefinitions(interface)) {
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
    }
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

fn genUuidList(cg: *CodeGen, uuid: u128) !void {
    const c = uuidComponents(uuid);
    try cg.print("0x{x:08}, 0x{x:04}, 0x{x:04}, 0x{x:02},0x{x:02}, 0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02},0x{x:02}", .{
        c._96,
        c._80,
        c._64,
        c._56,
        c._48,
        c._40,
        c._32,
        c._24,
        c._16,
        c._8,
        c._0,
    });
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
