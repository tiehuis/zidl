const std = @import("std");

const Parser = @This();
const InternPool = @import("InternPool.zig");
const Token = @import("tokenizer.zig").Token;
const Node = @import("node.zig").Node;
const CodeGen = @import("CodeGen.zig");
const Compile = @import("Compile.zig");
const Tokenizer = @import("tokenizer.zig").Tokenizer;

const log = std.log.scoped(.parser);

const State = struct {
    source: []const u8,
    tokens: []const Token,
    tok_i: usize,
};
state: std.ArrayList(State), // Stack

// Stored across invocations.
cc: *Compile,
intern_pool: InternPool,
nodes: std.ArrayList(Node),
data: std.ArrayList(Node.Ref),
import_table: std.ArrayList(struct { is_root: bool, import: Node.Import }),
forward_decl_table: std.ArrayList(Node.Ref),
symbol_table: std.StringHashMap(Node.Ref),
ole_auto_types: std.ArrayList(Node.OleAutoType),
options: Options,
scratch_ref: std.ArrayList(Node.Ref),
scratch_range: std.ArrayList(Node.Range),

// A lightweight stack-like view into a parent ArrayList, enabling efficient scoped
// memory usage while sharing heap allocations across all usages.
//
// When using, it is important to reset once the current scope has completed.
fn SubArrayList(comptime T: type) type {
    return struct {
        start: usize,
        base: *std.ArrayList(T),

        pub fn reset(sa: SubArrayList(T)) void {
            sa.base.items.len = sa.start;
        }

        pub fn len(sa: SubArrayList(T)) usize {
            std.debug.assert(sa.base.items.len >= sa.start);
            return sa.base.items.len - sa.start;
        }

        pub fn append(sa: SubArrayList(T), item: T) !void {
            return try sa.base.append(item);
        }

        pub fn constSlice(sa: SubArrayList(T)) []const T {
            return sa.base.items[sa.start..];
        }

        pub fn pop(sa: SubArrayList(T)) ?T {
            return if (sa.base.items.len > sa.start) sa.base.pop() else null;
        }
    };
}

fn refArray(p: *Parser) SubArrayList(Node.Ref) {
    return .{
        .base = &p.scratch_ref,
        .start = p.scratch_ref.items.len,
    };
}

fn rangeArray(p: *Parser) SubArrayList(Node.Range) {
    return .{
        .base = &p.scratch_range,
        .start = p.scratch_range.items.len,
    };
}

pub fn init(cc: *Compile, options: Options) !Parser {
    return .{
        .cc = cc,
        .state = .init(cc.allocator),
        .intern_pool = .init(cc.allocator),
        .nodes = .init(cc.allocator),
        .data = .init(cc.allocator),
        .import_table = .init(cc.allocator),
        .symbol_table = .init(cc.allocator),
        .forward_decl_table = .init(cc.allocator),
        .ole_auto_types = .init(cc.allocator),
        .options = options,
        .scratch_ref = .init(cc.allocator),
        .scratch_range = .init(cc.allocator),
    };
}

const Options = struct {
    skip_imports: bool = false,
    show_failed_optional_parses: bool = false,
};

pub const Error = error{ Overflow, OutOfMemory, ParseError, ImportError };

fn importSymbols(p: *Parser, import_filename: []const u8) Error!?Node.Ref {
    if (p.options.skip_imports) return null;

    // We may need this, but avoid some errors for now
    if (!std.mem.endsWith(u8, import_filename, ".idl")) {
        log.debug("skip import of non-idl file: {s}", .{import_filename});
        return null;
    }

    var maybe_file: ?std.fs.File = null;
    for (p.cc.options.include_paths) |include_path| {
        const full_path = try std.fs.path.join(p.cc.allocator, &.{ include_path, import_filename });
        defer p.cc.allocator.free(full_path);

        log.debug("searching include path: {s}", .{include_path});
        maybe_file = std.fs.cwd().openFile(full_path, .{}) catch continue;
        break;
    }
    const file = if (maybe_file) |f| f else {
        log.err("file not found: {s}", .{import_filename});
        p.addError(.{ .import_file_not_found = import_filename }) catch {};
        return error.ImportError;
    };

    const bytes = file.readToEndAllocOptions(p.cc.allocator, p.cc.options.max_source_size, null, .@"64", 0) catch |err| {
        log.err("failed to readFile: {s}", .{@errorName(err)});
        p.addError(.{ .import_read_file = import_filename }) catch {};
        return error.ImportError;
    };
    defer p.cc.allocator.free(bytes);

    log.debug("{}: load {s} ({})", .{ p.state.items.len, import_filename, std.fmt.fmtIntSizeDec(bytes.len) });

    const pp_bytes = p.cc.preprocess(import_filename, bytes) catch |err| {
        log.err("failed to preprocess: {s}", .{@errorName(err)});
        p.addError(.{ .import_preprocess = import_filename }) catch {};
        return error.ImportError;
    };

    var token_list = std.ArrayList(Token).init(p.cc.allocator);
    var t = Tokenizer.init(pp_bytes, .{ .version = p.cc.options.midl_version });
    var i: usize = 0;
    while (true) {
        const tok = t.next();
        try token_list.append(tok);
        i += 1;
        if (tok.tag == .eof) break;
    }

    return p.parse(pp_bytes, token_list.items) catch |err| {
        log.err("failed to parse import {s}: {s}", .{ import_filename, @errorName(err) });
        return error.ImportError;
    };
}

pub fn findSymbol(p: *const Parser, key: []const u8) ?Node {
    if (p.symbol_table.get(key)) |node_ref| {
        return p.nodes.items[node_ref.toInt()];
    }

    log.debug("failed to find symbol {s}", .{key});
    return null;
}

fn handleNode(p: *Parser, node: Node, node_index: Node.Ref) Error!void {
    try p.storeSymbol(node, node_index);
    try p.storeForwardDecl(node, node_index);
}

fn storeForwardDecl(p: *Parser, node: Node, node_index: Node.Ref) Error!void {
    if (!p.isRootState()) return;
    switch (node) {
        .interface,
        .coclass_def,
        .runtime_class_def,
        => try p.forward_decl_table.append(node_index),
        else => {},
    }
}

fn storeSymbol(p: *Parser, node: Node, node_index: Node.Ref) Error!void {
    const ref: InternPool.Ref = switch (node) {
        inline .enum_def,
        .struct_def,
        .union_def,
        => |d| if (d.name) |name| name else return,
        .interface,
        => |d| p.nodes.items[d.name.toInt()].type,
        else => return,
    };
    const key_value = p.intern_pool.get(ref).?;
    try p.symbol_table.put(key_value, node_index);
}

pub fn parse(p: *Parser, source: []const u8, tokens: []const Token) Error!Node.Ref {
    // A stack is used here to handle recursive parse calls (e.g. "import").
    try p.state.append(.{
        .source = source,
        .tokens = tokens,
        .tok_i = 0,
    });
    defer _ = p.state.pop();
    return try p.parseRoot();
}

// Root <- (GlobalStatements / AcfStatements) eof
fn parseRoot(p: *Parser) Error!Node.Ref {
    const base = p.nodes.items.len;
    const root = try p.addNode(.{ .root = undefined });
    log.debug("parseRoot base index: {}", .{base});
    const nodes = try p.parseGlobalStatements();
    // Note: If we fail here, it usually means we failed to parse a statement within
    // parseGlobalStatements or parseImportStatements. Check the include failed parse
    // show_failed_optional_parses to further identify. Should make this automatic.
    _ = try p.expectToken(.eof);
    p.nodes.items[base].root = nodes;
    return root;
}

// GlobalStatements <- GlobalStatement*
// GlobalStatement
//     <- NamespaceDef LBRACE GlobalStatements RBRACE
//      / InterfaceDef
//      / Interface SEMICOLON?
//      / DispInterface
//      / DelegateDef
//      / CoClassDef
//      / CoClass SEMICOLON
//      / ApiContractDef
//      / ApiContract SEMICOLON
//      / RuntimeClassDef
//      / RuntimeClass SEMICOLON
//      / ModuleDef
//      / LibraryDef
//      / Statement
//      / DeclBlock
fn parseGlobalStatements(p: *Parser) Error!Node.Range {
    var stmts = p.refArray();
    defer stmts.reset();

    while (p.peekToken().tag != .eof) {
        const attrs = try p.parseAttributes();
        const maybe_stmt = switch (p.peekToken().tag) {
            .keyword_interface => try p.parseInterface(attrs),
            .keyword_dispinterface => try p.parseDispInterface(attrs),
            .keyword_coclass => try p.parseCoClass(attrs),
            .keyword_apicontract => try p.parseApiContract(attrs),
            .keyword_runtimeclass => try p.parseRuntimeClass(attrs),
            .keyword_module => try p.parseModule(attrs),
            .keyword_library => try p.parseLibrary(attrs),
            .keyword_declare => blk: {
                if (attrs != null) return p.addError(.invalid_attributes_location);
                break :blk try p.parseDeclBlock();
            },
            .keyword_namespace => blk: {
                if (attrs != null) return p.addError(.invalid_attributes_location);
                const namespace_def = try p.parseNamespaceDef();
                _ = try p.expectToken(.l_brace);
                const statements = try p.parseGlobalStatements();
                _ = try p.expectToken(.r_brace);

                break :blk try p.addNode(.{ .namespace = .{
                    .qualified_name = namespace_def,
                    .statements = statements,
                } });
            },
            else => blk: {
                // TODO: Observed attributes being allowed on typedefs (e.g. [hidden]).
                //if (attrs != null) return p.addError(.invalid_attributes_location);
                break :blk try p.optional(parseStatement, Node.Ref);
            },
        };
        if (maybe_stmt) |stmt| {
            try stmts.append(stmt);
        } else {
            break;
        }
    }
    return p.addData(stmts.constSlice());
}

// ImportStatements <- ImportStatement*
// ImportStatement
//     <- NamespaceDef LBRACE ImportStatements RBRACE
//      / InterfaceDef
//      / Interface SEMICOLON
//      / DispInterface
//      / DelegateDef
//      / CoClassDef
//      / CoClass SEMICOLON
//      / ApiContractDef
//      / ApiContract SEMICOLON
//      / RuntimeClassDef
//      / RuntimeClass SEMICOLON
//      / ModuleDef
//      / LibraryDef
//      / Statement
//      / ImportLib
//      / ImportDeclBlock
fn parseImportStatements(p: *Parser) Error!Node.Range {
    var stmts = p.refArray();
    defer stmts.reset();

    while (p.peekToken().tag != .eof) {
        const attrs = try p.parseAttributes();
        const maybe_stmt = switch (p.peekToken().tag) {
            .keyword_interface => try p.parseInterface(attrs),
            .keyword_dispinterface => try p.parseDispInterface(attrs),
            .keyword_coclass => try p.parseCoClass(attrs),
            .keyword_apicontract => try p.parseApiContract(attrs),
            .keyword_runtimeclass => try p.parseRuntimeClass(attrs),
            .keyword_module => try p.parseModule(attrs),
            .keyword_library => try p.parseLibrary(attrs),
            .keyword_importlib => blk: {
                if (attrs != null) return p.addError(.invalid_attributes_location);
                break :blk try p.parseImportLib();
            },
            .keyword_declare => blk: {
                if (attrs != null) return p.addError(.invalid_attributes_location);
                break :blk try p.parseDeclBlock();
            },
            .keyword_namespace => blk: {
                if (attrs != null) return p.addError(.invalid_attributes_location);
                const namespace_def = try p.parseNamespaceDef();
                _ = try p.expectToken(.l_brace);
                const statements = try p.parseImportStatements();
                _ = try p.expectToken(.r_brace);

                break :blk try p.addNode(.{ .namespace = .{
                    .qualified_name = namespace_def,
                    .statements = statements,
                } });
            },
            else => blk: {
                // TODO: Observed attributes being allowed on typedefs (e.g. [hidden]).
                //if (attrs != null) return p.addError(.invalid_attributes_location);
                break :blk try p.optional(parseStatement, Node.Ref);
            },
        };
        if (maybe_stmt) |stmt| {
            try stmts.append(stmt);
        } else {
            break;
        }
    }
    return p.addData(stmts.constSlice());
}

// ImportDeclBlock <- KEYWORD_declare LBRACE ImportDeclStatements RBRACE
//
// DeclBlock <- KEYWORD_declare LBRACE DeclStatements RBRACE
fn parseDeclBlock(p: *Parser) Error!Node.Ref {
    _ = try p.expectToken(.keyword_declare);
    _ = try p.expectToken(.l_brace);
    const statements = try p.parseDeclStatements();
    _ = try p.expectToken(.r_brace);

    return p.addNode(.{
        .declare_block = statements,
    });
}

// ImportDeclStatements <- ImportDeclStatement*
// ImportDeclStatement <- KEYWORD_interface QualifiedType LARROW ParameterizedTypeArgs RARROW SEMICOLON
//
// DeclStatements <- DeclStatement*
// DeclStatement <- KEYWORD_interface QualifiedType LARROW ParameterizedTypeArgs RARROW SEMICOLON
fn parseDeclStatements(p: *Parser) Error!Node.Range {
    var stmts = p.refArray();
    defer stmts.reset();

    while (p.peekToken().tag == .keyword_interface) {
        _ = try p.expectToken(.keyword_interface);
        const qual_type = try p.parseQualifiedType();
        _ = try p.expectToken(.angle_bracket_left);
        const param_type_args = try p.parseListOf(parseParameterizedTypeArg);
        _ = try p.expectToken(.angle_bracket_right);
        _ = try p.expectToken(.semicolon);

        try stmts.append(try p.addNode(.{
            .declare_statement = .{
                .qual_type = qual_type,
                .param_type_args = param_type_args,
            },
        }));
    }

    return p.addData(stmts.constSlice());
}

// ImportLib <- KEYWORD_importlib LPAREN STRINGLITERAL RPAREN SEMICOLON?
fn parseImportLib(p: *Parser) Error!Node.Ref {
    _ = try p.expectToken(.keyword_importlib);
    _ = try p.expectToken(.l_paren);
    const ref = p.intern_pool.intern(try p.expectStringLiteral());
    _ = try p.expectToken(.r_paren);
    _ = p.eatToken(.semicolon);

    return try p.addNode(.{
        .importlib = ref,
    });
}

// Import <- ImportStart ImportStatements
// ImportStart <- KEYWORD_import STRINGLITERAL SEMICOLON
fn parseImport(p: *Parser) Error!Node.Ref {
    _ = try p.expectToken(.keyword_import);
    const ref = p.intern_pool.intern(try p.expectStringLiteral());
    _ = try p.expectToken(.semicolon);

    const import_name = p.intern_pool.get(ref).?;

    // Attempt to parse the subject. Use existing parse-tree if we have already
    // parsed this. Store a import table entry prior to parsing as we may need
    // to recursively parse the same instances. Backfill as required.
    var entry_index: ?usize = null;
    const root_node_index = blk: {
        for (p.import_table.items, 0..) |other, i| {
            if (ref == other.import.subject) {
                // Ensure imports from recursive imports are marked as coming from root if the
                // root import is done after.
                p.import_table.items[i].is_root = p.isRootState();

                log.debug("reusing existing import node for {s}", .{import_name});
                break :blk other.import.root;
            }
        }
        entry_index = p.import_table.items.len;
        try p.import_table.append(.{ .is_root = p.isRootState(), .import = .{
            .subject = ref,
            .statements = undefined,
            .root = undefined,
        } });
        break :blk try p.importSymbols(import_name);
    };

    const statements = try p.parseImportStatements();
    const import: Node.Import = .{
        .subject = ref,
        .statements = statements,
        .root = root_node_index,
    };

    // Update import entry with parse tree, only if we create a new entry.
    if (entry_index) |i| {
        p.import_table.items[i].import.statements = statements;
        p.import_table.items[i].import.root = root_node_index;
    }

    return try p.addNode(.{ .import = import });
}

// LibraryHeader <- KEYWORD_library IDENTIFIER
// LibraryDef <- Attributes? LibraryHeader LBRACE ImportStatements RBRACE SEMICOLON?
fn parseLibrary(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_library);
    const id_ref = try p.expectIdentifierRef();
    _ = try p.expectToken(.l_brace);
    const stmts = try p.parseImportStatements();
    _ = try p.expectToken(.r_brace);
    _ = p.eatToken(.semicolon);

    return p.addNode(.{ .library_def = .{
        .name = id_ref,
        .attributes = attributes,
        .import_statements = stmts,
    } });
}

// DispInterface <- KEYWORD_dispinterface TypeName
// DispInterfaceDef
//     <- DispInterface LBRACE DispIntProps DispIntMethods RBRACE SEMICOLON?
//      / DispInterface LBRACE Interface SEMICOLON RBRACE SEMICOLON?
// DispInterfaceRef <- KEYWORD_dispinterface TypeName
fn parseDispInterface(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_dispinterface);
    const interface_name = try p.parseTypeName();

    if (p.eatToken(.semicolon)) |_| {
        return try p.addNode(.{ .disp_interface_ref = interface_name });
    }

    _ = try p.expectToken(.l_brace);
    const disp_interface_data: Node.DispInterfaceDef.Data = blk: {
        switch (p.peekToken().tag) {
            .keyword_properties, .keyword_methods => {
                const props = try p.parseDispIntProps();
                const methods = try p.parseDispIntMethods();
                break :blk .{
                    .props_methods = .{
                        .props = props,
                        .methods = methods,
                    },
                };
            },
            else => {
                const interface = try p.parseInterface(null);
                _ = try p.expectToken(.semicolon);
                break :blk .{ .interface = interface };
            },
        }
    };
    _ = try p.expectToken(.r_brace);
    _ = p.eatToken(.semicolon);

    return try p.addNode(.{ .disp_interface_def = .{
        .name = interface_name,
        .attributes = attributes,
        .data = disp_interface_data,
    } });
}

// DispIntProps <- KEYWORD_properties COLON (SField SEMICOLON)*
fn parseDispIntProps(p: *Parser) Error!Node.Range {
    _ = try p.expectToken(.keyword_properties);
    _ = try p.expectToken(.colon);

    var props = p.refArray();
    defer props.reset();

    while (true) {
        const maybe_s_field = try p.optional(parseSField, Node.Ref);
        if (maybe_s_field) |s_field| {
            try props.append(s_field);
            _ = try p.expectToken(.semicolon);
        } else break;
    }

    return try p.addData(props.constSlice());
}

// DispIntMethods <- KEYWORD_methods COLON (FuncDef SEMICOLON)*
fn parseDispIntMethods(p: *Parser) Error!Node.Range {
    _ = try p.expectToken(.keyword_methods);
    _ = try p.expectToken(.colon);

    var props = p.refArray();
    defer props.reset();

    while (true) {
        const maybe_func_def = try p.optional(parseFuncDef, Node.Ref);
        if (maybe_func_def) |func_def| {
            try props.append(func_def);
            _ = try p.expectToken(.semicolon);
        } else break;
    }

    return try p.addData(props.constSlice());
}

// FuncDef <- Declaration
fn parseFuncDef(p: *Parser) Error!Node.Ref {
    const attributes = try p.parseAttributes();
    return try p.parseDeclaration(attributes);
}

// NamespaceDef <- KEYWORD_namespace IDENTIFIER (DOT IDENTIFIER)*
fn parseNamespaceDef(p: *Parser) Error!Node.Range {
    var stmts = p.refArray();
    defer stmts.reset();

    _ = try p.expectToken(.keyword_namespace);
    try stmts.append(try p.addNode(.{
        .type = try p.expectIdentifierRef(),
    }));
    while (p.eatToken(.period)) |_| {
        try stmts.append(try p.addNode(.{
            .type = try p.expectIdentifierRef(),
        }));
    }
    return try p.addData(stmts.constSlice());
}

// ApiContract <- KEYWORD_apicontract TypeName
// ApiContractDef <- Attributes? ApiContract LBRACE RBRACE SEMICOLON?
fn parseApiContract(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_apicontract);
    const contract_name = try p.parseTypeName();

    if (p.eatToken(.semicolon)) |_| {
        return try p.addNode(.{ .api_contract_ref = contract_name });
    }

    _ = try p.expectToken(.l_brace);
    _ = try p.expectToken(.r_brace);
    _ = p.eatToken(.semicolon);

    return try p.addNode(.{ .api_contract_def = .{
        .name = contract_name,
        .attributes = attributes,
    } });
}

// Module <- KEYWORD_module TypeName
// ModuleDef <- Attributes? Module LBRACE IntStatements RBRACE SEMICOLON?
fn parseModule(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_module);
    const class_name = try p.parseTypeName();

    if (p.eatToken(.semicolon)) |_| {
        return try p.addNode(.{ .module_ref = class_name });
    }

    _ = try p.expectToken(.l_brace);
    var ints = p.refArray();
    defer ints.reset();
    while (p.peekToken().tag != .r_brace) {
        try ints.append(try p.parseStatement());
    }
    _ = try p.expectToken(.r_brace);
    _ = p.eatToken(.semicolon);

    return try p.addNode(.{ .module_def = .{
        .name = class_name,
        .attributes = attributes,
        .statements = try p.addData(ints.constSlice()),
    } });
}

// CoClass <- KEYWORD_coclass TypeName
// CoClassDef <- Attributes? CoClass LBRACE ClassInts RBRACE SEMICOLON?
fn parseCoClass(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_coclass);
    const class_name = try p.parseTypeName();

    if (p.eatToken(.semicolon)) |_| {
        return try p.addNode(.{ .coclass_ref = class_name });
    }

    _ = try p.expectToken(.l_brace);
    var ints = p.refArray();
    defer ints.reset();
    while (p.peekToken().tag != .r_brace) {
        try ints.append(try p.parseClassInt());
    }
    _ = try p.expectToken(.r_brace);
    _ = p.eatToken(.semicolon);

    return try p.addNode(.{ .coclass_def = .{
        .name = class_name,
        .attributes = attributes,
        .ints = try p.addData(ints.constSlice()),
    } });
}

// RuntimeClass <- KEYWORD_runtimeclass TypeName
// RuntimeClassDef <- Attributes? RuntimeClass Inherit? LBRACE ClassInts RBRACE SEMICOLON?
fn parseRuntimeClass(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_runtimeclass);
    const class_name = try p.parseTypeName();

    if (p.eatToken(.semicolon)) |_| {
        return try p.addNode(.{ .runtime_class_ref = class_name });
    }

    const parents = try p.parseInherit();

    _ = try p.expectToken(.l_brace);
    var ints = p.refArray();
    defer ints.reset();
    while (p.peekToken().tag != .r_brace) {
        try ints.append(try p.parseClassInt());
    }
    _ = try p.expectToken(.r_brace);
    _ = p.eatToken(.semicolon);

    return try p.addNode(.{ .runtime_class_def = .{
        .name = class_name,
        .attributes = attributes,
        .parents = parents,
        .ints = try p.addData(ints.constSlice()),
    } });
}

// ClassInts <- ClassInt*
// ClassInt <- Attributes? (InterfaceRef / DispInterfaceRef) SEMICOLON
//
// InterfaceRef <- KEYWORD_interface (ParameterizedType / QualifiedType)
// DispInterfaceRef <- KEYWORD_dispinterface TypeName
fn parseClassInt(p: *Parser) Error!Node.Ref {
    const attrs = try p.parseAttributes();

    switch (p.peekToken().tag) {
        .keyword_dispinterface => {
            _ = p.nextToken();
            const @"type" = try p.parseTypeName();
            _ = try p.expectToken(.semicolon);
            return p.addNode(.{ .class_int = .{
                .attributes = attrs,
                .type = @"type",
            } });
        },
        .keyword_interface => {
            _ = p.nextToken();
            const @"type" = try p.parseParamOrQualType();
            _ = try p.expectToken(.semicolon);
            return p.addNode(.{ .class_int = .{
                .attributes = attrs,
                .type = @"type",
            } });
        },
        else => return p.addError(.{
            .expected = "dispinterface or interface",
        }),
    }
}

// Inherit <- COLON (ParameterizedType / QualifiedType)
fn parseInherit(p: *Parser) Error!?Node.Range {
    return if (p.eatToken(.colon)) |_|
        try p.parseListOf(parseParamOrQualType)
    else
        null;
}

// Interface <- KEYWORD_interface TypeName (LARROW TypeParameters RARROW)?
// InterfaceDef
//     <- Attributes? Interface Inherit? Requires? LBRACE IntStatements RBRACE SEMICOLON?
fn parseInterface(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_interface);
    const interface_name = try p.parseTypeName();

    if (p.eatToken(.semicolon)) |_| {
        return try p.addNode(.{ .interface_ref = interface_name });
    }

    const type_params = blk: {
        if (p.eatToken(.angle_bracket_left)) |_| {
            const param_tys = try p.parseListOf(parseTypeName);
            _ = try p.expectToken(.angle_bracket_right);
            break :blk param_tys;
        } else {
            break :blk null;
        }
    };

    const parents = try p.parseInherit();

    const requires = if (p.eatToken(.keyword_requires)) |_|
        try p.parseListOf(parseParamOrQualType)
    else
        null;

    _ = try p.expectToken(.l_brace);
    var defs = p.refArray();
    defer defs.reset();
    while (p.peekToken().tag != .r_brace) {
        try defs.append(try p.parseStatement());
    }
    _ = try p.expectToken(.r_brace);
    _ = p.eatToken(.semicolon);

    return try p.addNode(.{ .interface = .{
        .name = interface_name,
        .attributes = attributes,
        .type_params = type_params,
        .parents = parents,
        .requires = requires,
        .defs = try p.addData(defs.constSlice()),
    } });
}

// ParameterizedType / QualifiedType
fn parseParamOrQualType(p: *Parser) Error!Node.Ref {
    if (try p.optional(parseParameterizedType, Node.Ref)) |param_type| {
        return param_type;
    }
    return p.parseQualifiedType();
}

// ParameterizedType <- QualifiedType LARROW ParameterizedTypeArgs RARROW
fn parseParameterizedType(p: *Parser) Error!Node.Ref {
    const qual_type = try p.parseQualifiedType();
    _ = try p.expectToken(.angle_bracket_left);
    const type_args = try p.parseListOf(parseParameterizedTypeArg);
    _ = try p.expectToken(.angle_bracket_right);

    return try p.addNode(.{
        .parameterized_type = .{
            .qual_type = qual_type,
            .type_args = type_args,
        },
    });
}

// ParameterizedTypeArgs
//     <- ParameterizedTypeArg (COMMA ParameterizedTypeArg)*
// ParameterizedTypeArg
//     <- ParameterizedType ASTERISK?
//      / QualifiedType ASTERISK?
//      / BaseType
fn parseParameterizedTypeArg(p: *Parser) Error!Node.Ref {
    const maybe_param_type = try p.optional(parseParameterizedType, Node.Ref);
    if (maybe_param_type) |param_type| {
        const is_pointer = p.eatToken(.asterisk) != null;
        return try p.addNode(.{ .parameterized_type_arg = .{
            .is_pointer = is_pointer,
            .type = param_type,
        } });
    }

    const maybe_qual_type = try p.optional(parseQualifiedType, Node.Ref);
    if (maybe_qual_type) |qual_type| {
        const is_pointer = p.eatToken(.asterisk) != null;
        return try p.addNode(.{ .parameterized_type_arg = .{
            .is_pointer = is_pointer,
            .type = qual_type,
        } });
    }

    return try p.addNode(.{ .base_type = try p.parseBaseType() orelse
        return p.addError(.{ .expected = "base type" }) });
}

// QualifiedType <- NamespacePfx? TypeName
// NamespacePfx <- IDENTIFIER DOT (IDENTIFIER DOT)*
fn parseQualifiedType(p: *Parser) Error!Node.Ref {
    var namespace = p.refArray();
    defer namespace.reset();
    try namespace.append(try p.parseTypeName()); // TODO: Not strictly a type, just an identifier
    while (p.eatToken(.period)) |_| {
        try namespace.append(try p.parseTypeName());
    }

    const typename = namespace.pop().?;
    return try p.addNode(.{
        .qualified_type = .{
            .namespace = try p.addData(namespace.constSlice()),
            .typename = typename,
        },
    });
}

// TypeName <- IDENTIFIER / KnownType
fn parseTypeName(p: *Parser) Error!Node.Ref {
    const maybe_type_tag = try p.parseBaseType();
    if (maybe_type_tag) |tag| {
        return p.addNode(.{
            .base_type = tag,
        });
    }

    return p.addNode(.{ .type = try p.expectIdentifierRef() });
}

// Statement
//     <- CppQuote
//      / TypeDecl SEMICOLON
//      / Declaration SEMICOLON
//      / Import
//      / TypeDef SEMICOLON
//      / Pragma
//      / PragmaWarning
fn parseStatement(p: *Parser) Error!Node.Ref {
    switch (p.peekToken().tag) {
        .keyword_cpp_quote => {
            _ = try p.expectToken(.keyword_cpp_quote);
            _ = try p.expectToken(.l_paren);
            const lit = try p.expectStringLiteral();
            _ = try p.expectToken(.r_paren);
            const lit_ref = p.intern_pool.intern(lit);
            return p.addNode(.{ .cpp_quote = lit_ref });
        },
        // Import another idl file. Make the types+constants available in a symbol table which can
        // be used. No code is explicitly generated, although some things will make their way into
        // new definitions (e.g. via vtable references to a parent type), hence why we need the info.
        //
        // Defer this until after, as we can do the C++ codegen which doesn't need the type info.
        .keyword_import => return try p.parseImport(),
        .hash => {
            if (!p.eatIdentifier("pragma")) return p.addError(.{ .expected = "#pragma warning" });
            const ident = try p.expectIdentifierRef();
            return p.addNode(.{ .pragma = ident });
        },
        else => {
            // PragmaWarning
            if (p.eatIdentifier("midl_pragma")) {
                if (!p.eatIdentifier("warning")) return p.addError(.{ .expected = "midl_pragma warning" });
                _ = try p.expectToken(.l_paren);
                const name_tok = p.nextToken();
                switch (name_tok.tag) {
                    .identifier, .keyword_default => {},
                    else => return p.addError(.{ .generic = "expected identifier or default" }),
                }
                _ = try p.expectToken(.colon);
                const warn_tok = try p.expectToken(.number_literal);
                const warn_tok_string = p.getTokenString(warn_tok);
                const warn_id = std.fmt.parseInt(u32, warn_tok_string, 10) catch
                    return p.addError(.{ .invalid_pragma_warning_id = warn_tok_string });
                return p.addNode(.{ .pragma_warning = .{
                    .name = p.intern(name_tok),
                    .id = warn_id,
                } });
            }

            // TypeDecl SEMICOLON
            // Declaration SEMICOLON
            // TypeDef SEMICOLON
            const attrs = try p.parseAttributes();
            const decl = switch (p.peekToken().tag) {
                .keyword_enum => try p.parseEnum(attrs),
                .keyword_struct => try p.parseStruct(attrs),
                .keyword_union => try p.parseUnion(attrs),
                .keyword_typedef => p.parseTypedef(attrs),
                else => try p.parseDeclaration(attrs),
            };
            _ = p.eatToken(.semicolon);
            return decl;
        },
    }
}

// TypeDef <- Attributes? KEYWORD_typedef Attributes? DeclSpec DeclaratorList
fn parseTypedef(p: *Parser, prefix_attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_typedef);
    const suffix_attributes = try p.parseAttributes();

    const attr_list: ?Node.Range = if (prefix_attributes != null and suffix_attributes != null)
        try p.addDataRanges(&.{ prefix_attributes.?, suffix_attributes.? })
    else if (prefix_attributes != null)
        prefix_attributes
    else if (suffix_attributes != null)
        suffix_attributes
    else
        null;

    const decl_spec = try p.parseDeclSpec();
    const decl_list = try p.parseListOf(parseDeclarator);

    return p.addNode(.{ .typedef = .{
        .attributes = attr_list,
        .decl_spec = decl_spec,
        .decl_list = decl_list,
    } });
}

// DeclSpec <- DeclSpecNoType? Type DeclSpecNoType?
fn parseDeclSpec(p: *Parser) Error!Node.Ref {
    const prefix_decl_spec = try p.parseDeclSpecNoType();
    const @"type" = try p.parseType();
    const suffix_decl_spec = try p.parseDeclSpecNoType();

    return p.addNode(.{ .decl_spec_type = .{
        .type = @"type",
        .decl_specs = try p.addDataRanges(&.{ prefix_decl_spec, suffix_decl_spec }),
    } });
}

// Declaration <- Attributes? DeclSpec InitDeclarator
// DeclSpec <- DeclSpecNoType? Type DeclSpecNoType?
// DeclSpecNoType <- (TypeQualifier / FunctionSpecifier / StorageClassSpec)*
fn parseDeclaration(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    const prefix_decl_spec = try p.parseDeclSpecNoType();
    const @"type" = try p.parseType();
    const suffix_decl_spec = try p.parseDeclSpecNoType();
    const init_decl = try p.parseInitDeclarator();

    return p.addNode(.{
        .decl = .{
            .attributes = attributes,
            .decl_spec = try p.addNode(.{ .decl_spec_type = .{
                .type = @"type",
                .decl_specs = try p.addDataRanges(&.{ prefix_decl_spec, suffix_decl_spec }),
            } }),
            .init_decl = init_decl,
        },
    });
}

// InitDeclarator <- Declarator (EQUAL ExprConst)?
fn parseInitDeclarator(p: *Parser) Error!?Node.Ref {
    const declarator = try p.parseDeclarator();
    const expr = if (p.eatToken(.equal)) |_|
        try p.parseExprConst()
    else
        null;

    return try p.addNode(.{ .init_declarator = .{
        .declarator = declarator,
        .expr = expr,
    } });
}

// Declarator
//     <- ASTERISK TypeQualList? Declarator
//      / CallConv Declarator
//      / DirectDeclarator
fn parseDeclarator(p: *Parser) Error!Node.Ref {
    if (p.eatToken(.asterisk)) |_| {
        const type_quals = try p.parseTypeQualList();
        return p.addNode(.{ .declarator = .{
            .is_pointer = true,
            .type_quals = type_quals,
            .decl = try p.parseDeclarator(),
        } });
    }
    if (try p.parseCallConv()) |call_conv| {
        return p.addNode(.{ .declarator = .{
            .call_conv = call_conv,
            .decl = try p.optional(
                parseDeclarator,
                Node.Ref,
            ),
        } });
    }
    return p.addNode(.{ .declarator = .{
        .decl = try p.parseDirectDeclarator(),
    } });
}

// AbstractDeclaratorNoDirect
//     <- ASTERISK TypeQualList? AnyDeclarator?
//      / CallConv AnyDeclarator?
fn parseAbstractDeclaratorNoDirect(p: *Parser) Error!Node.Ref {
    if (p.eatToken(.asterisk)) |_| {
        const type_quals = try p.parseTypeQualList();
        return p.addNode(.{ .any_declarator = .{
            .is_pointer = true,
            .type_quals = type_quals,
            .decl = try p.parseAnyDeclarator(),
        } });
    }
    if (try p.parseCallConv()) |call_conv| {
        return p.addNode(.{ .any_declarator = .{
            .call_conv = call_conv,
            .decl = try p.optional(
                parseAnyDeclarator,
                Node.Ref,
            ),
        } });
    }

    return p.addError(.{ .expected = "calling convention" });
}

// AbstractDirectDeclarator <- AbstractDirectDeclaratorBase AbstractDirectDeclaratorSuffix*
// AbstractDirectDeclaratorBase <- LPAREN AbstractDeclaratorNoDirect RPAREN
// AbstractDirectDeclaratorSuffix
//     <- Array
//      / LPAREN Args? RPAREN
fn parseAbstractDirectDeclarator(p: *Parser) Error!Node.Ref {
    _ = try p.expectToken(.l_paren);
    const base = try p.parseAbstractDeclaratorNoDirect();
    _ = try p.expectToken(.r_paren);

    var suffix = p.refArray();
    defer suffix.reset();
    while (true) {
        const maybe_direct_decl = blk: {
            switch (p.peekToken().tag) {
                .l_paren => {
                    _ = try p.expectToken(.l_paren);
                    const args = try p.parseArgs();
                    _ = try p.expectToken(.r_paren);
                    break :blk args;
                },
                .l_bracket => break :blk try p.parseArray(),
                else => break :blk null,
            }
        };
        if (maybe_direct_decl) |direct_decl| {
            try suffix.append(direct_decl);
        } else {
            break;
        }
    }

    return p.addNode(.{
        .abstract_direct_decl = .{
            .base = base,
            .suffix = try p.addData(suffix.constSlice()),
        },
    });
}

// DirectDeclarator <- DirectDeclaratorBase DirectDeclaratorSuffix*
// DirectDeclaratorBase
//     <- IDENTIFIER
//      / LPAREN Declarator RPAREN
// DirectDeclaratorSuffix
//     <- Array
//      / LPAREN Args? RPAREN
fn parseDirectDeclarator(p: *Parser) Error!Node.Ref {
    return p.parseDirectDeclaratorGeneric(parseDeclarator);
}

// Implements DirectDeclarator and AnyDirectDeclarator which are identical besides the child rule used.
fn parseDirectDeclaratorGeneric(p: *Parser, comptime sub_rule: *const fn (p: *Parser) Error!Node.Ref) Error!Node.Ref {
    const base = blk: {
        if (p.eatToken(.identifier)) |tok| {
            const ident_ref = p.intern(tok);
            // TODO: Have a symbol node, do not overload type
            break :blk try p.addNode(.{ .type = ident_ref });
        }

        _ = try p.expectToken(.l_paren);
        const declarator = try sub_rule(p);
        _ = try p.expectToken(.r_paren);
        break :blk declarator;
    };

    var suffix = p.refArray();
    defer suffix.reset();
    while (true) {
        const maybe_direct_decl = blk: {
            switch (p.peekToken().tag) {
                .l_paren => {
                    _ = try p.expectToken(.l_paren);
                    const args = try p.parseArgs();
                    _ = try p.expectToken(.r_paren);
                    break :blk args;
                },
                .l_bracket => break :blk try p.parseArray(),
                else => break :blk null,
            }
        };
        if (maybe_direct_decl) |direct_decl| {
            try suffix.append(direct_decl);
        } else {
            break;
        }
    }

    return p.addNode(.{
        .direct_decl = .{
            .base = base,
            .suffix = try p.addData(suffix.constSlice()),
        },
    });
}

// Array <- LBRACKET (Expr / ASTERISK)? RBRACKET
fn parseArray(p: *Parser) Error!Node.Ref {
    _ = try p.expectToken(.l_bracket);

    const node = if (p.eatToken(.asterisk)) |_|
        try p.addNode(.{ .array_def = .asterisk })
    else if (p.peekToken().tag == .r_bracket)
        try p.addNode(.{ .array_def = .empty })
    else
        try p.addNode(.{ .array_def = .{ .expr = try p.parseExpr() } });

    _ = try p.expectToken(.r_bracket);
    return node;
}

// Args <- ArgList / ArgList COMMA ELLIPSIS
// ArgList <- Arg (COMMA Arg)*
//
// Always returns an .arg_list node. May be empty.
fn parseArgs(p: *Parser) Error!Node.Ref {
    var is_varargs = false;
    var args = p.refArray();
    defer args.reset();
    while (true) {
        const maybe_arg = try p.optional(parseArg, Node.Ref);
        if (maybe_arg) |arg| try args.append(arg) else break;
        if (p.eatToken(.comma)) |_| {} else break;
        if (p.eatToken(.period_3)) |_| {
            is_varargs = true;
            break;
        }
    }

    return try p.addNode(.{
        .arg_list = .{
            .args = try p.addData(args.constSlice()),
            .is_varargs = is_varargs,
        },
    });
}

// Need to have a better AST representation. Flatten the deeply nested
// declarations into simpler types with composed fields.
fn resolveTypeName(p: *Parser, node_ref: Node.Ref) ?[]const u8 {
    const node = p.nodes.items[node_ref.toInt()];
    return switch (node) {
        .type => |ref| p.intern_pool.get(ref).?,
        .base_type => |tag| @tagName(tag),
        .qualified_type => |qt| p.resolveTypeName(qt.typename),
        .decl_spec_type => |decl_spec| p.resolveTypeName(decl_spec.type),
        .func_param => |func_param| p.resolveTypeName(func_param.decl_spec),
        .declarator, .any_declarator => |d| if (d.decl) |dd| p.resolveTypeName(dd) else null,
        .direct_decl => |d| p.resolveTypeName(d.base),
        .enum_ref => |name| p.intern_pool.get(name),
        .safe_array => null,
        else => {
            log.err("resolveTypeName: unsupported node type: {s}", .{@tagName(node)});
            return null;
        },
    };
}

// Arg <- Attributes? DeclSpec AnyDeclarator?
fn parseArg(p: *Parser) Error!Node.Ref {
    const attrs = try p.parseAttributes();
    const decl_spec = try p.parseDeclSpec();

    const any_decl = try p.optional(
        parseAnyDeclarator,
        Node.Ref,
    );

    if (p.isRootState()) blk: {
        const typename = p.resolveTypeName(decl_spec) orelse break :blk;
        const tag: Node.OleAutoType = if (Node.OleAutoType.get(typename)) |tag|
            tag
        else
            break :blk;

        for (p.ole_auto_types.items) |e| {
            if (e == tag) break :blk;
        }
        try p.ole_auto_types.append(tag);
    }

    return p.addNode(.{ .func_param = .{
        .attributes = attrs,
        .decl_spec = decl_spec,
        .any_decl = any_decl,
    } });
}

// AnyDeclarator
//     <- ASTERISK TypeQualList? AnyDeclarator?
//      / CallConv AnyDeclarator?
//      / AnyDirectDeclarator
//
fn parseAnyDeclarator(p: *Parser) Error!Node.Ref {
    if (p.eatToken(.asterisk)) |_| {
        const type_quals = try p.parseTypeQualList();
        const decl = try p.optional(parseAnyDeclarator, Node.Ref);
        return p.addNode(.{ .any_declarator = .{
            .is_pointer = true,
            .type_quals = type_quals,
            .decl = decl,
        } });
    }
    if (try p.parseCallConv()) |call_conv| {
        const decl = try p.optional(parseAnyDeclarator, Node.Ref);
        return p.addNode(.{ .any_declarator = .{
            .call_conv = call_conv,
            .decl = decl,
        } });
    }
    return p.addNode(.{ .any_declarator = .{
        .decl = try p.parseAnyDirectDeclarator(),
    } });
}

// CallConv
//     <- KEYWORD___cdecl / KEYWORD__cdecl / KEYWORD_cdecl
//      / KEYWORD___fastcall / KEYWORD__fastcall
//      / KEYWORD___pascal / KEYWORD__pascal / KEYWORD_pascal
//      / KEYWORD___stdcall / KEYWORD__stdcall / KEYWORD_stdcall
fn parseCallConv(p: *Parser) Error!?Node.CallConv {
    const call_conv: ?Node.CallConv = switch (p.peekToken().tag) {
        .keyword_stdcall, .keyword__stdcall, .keyword___stdcall => .stdcall,
        .keyword___fastcall, .keyword__fastcall => .fastcall,
        .keyword_cdecl, .keyword__cdecl, .keyword___cdecl => .cdecl,
        .keyword___pascal, .keyword__pascal, .keyword_pascal => .pascal,
        else => return null,
    };
    _ = p.nextToken();
    return call_conv;
}

// AnyDirectDeclarator <- AnyDirectDeclaratorBase AnyDirectDeclaratorSuffix*
// AnyDirectDeclaratorBase
//     <- IDENTIFIER
//      / LPAREN AnyDeclaratorNoDirect RPAREN
// AnyDirectDeclaratorSuffix
//     <- Array
//      / LPAREN Args? RPAREN
fn parseAnyDirectDeclarator(p: *Parser) Error!Node.Ref {
    return p.parseDirectDeclaratorGeneric(parseAnyDeclaratorNoDirect);
}

// AnyDeclaratorNoDirect
//     <- ASTERISK TypeQualList? AnyDeclarator?
//      / CallConv AnyDeclarator
fn parseAnyDeclaratorNoDirect(p: *Parser) Error!Node.Ref {
    if (p.eatToken(.asterisk)) |_| {
        const type_qual = try p.optional(parseTypeQualList, Node.Range);
        const any_decl = try p.optional(parseAnyDeclarator, Node.Ref);

        return p.addNode(.{ .any_declarator = .{
            .is_pointer = true,
            .type_quals = type_qual,
            .decl = any_decl,
        } });
    }
    const call_conv = try p.parseCallConv() orelse return p.addError(.{
        .expected = "calling convention",
    });
    const any_decl = try p.parseAnyDeclarator();
    return p.addNode(.{ .any_declarator = .{
        .call_conv = call_conv,
        .decl = any_decl,
    } });
}

// Type
//     <- ParameterizedType
//      / NamespacePfx TypeName
//      / UnqualifiedType
fn parseType(p: *Parser) Error!Node.Ref {
    // TODO: This isn't strictly correct, qualified type will parse unqualified types as well.
    if (try p.optional(parseParameterizedType, Node.Ref)) |param_type| {
        return param_type;
    } else if (try p.optional(parseQualifiedType, Node.Ref)) |qual_type| {
        return qual_type;
    } else {
        return try p.parseUnqualifiedType();
    }
}

// TypeQualifier <- KEYWORD_const
// TypeQualList <- TypeQualifier*
fn parseTypeQualList(p: *Parser) Error!?Node.Range {
    var quals = p.refArray();
    defer quals.reset();
    while (true) {
        if (p.eatToken(.keyword_const)) |_| {} else break;
        try quals.append(try p.addNode(.{ .type_qual = .@"const" }));
    }
    return if (quals.len() > 0) try p.addData(quals.constSlice()) else null;
}

// UnqualifiedType
//     <- KEYWORD_void
//      / BaseType
//      / EnumDef
//      / KEYWORD_enum IDENTIFIER
//      / StructDef
//      / KEYWORD_struct IDENTIFIER
//      / UnionDef
//      / KEYWORD_union IDENTIFIER
//      / KEYWORD_SAFEARRAY LPAREN Type RPAREN
//      / KnownType
fn parseUnqualifiedType(p: *Parser) Error!Node.Ref {
    if (p.eatToken(.keyword_void)) |_| {
        return p.addNode(.{ .base_type = .void });
    }

    switch (p.peekToken().tag) {
        .keyword_enum => return try p.parseEnum(null),
        .keyword_struct => return try p.parseStruct(null),
        .keyword_union => return try p.parseUnion(null),
        .keyword_SAFEARRAY => {
            _ = p.nextToken();
            _ = try p.expectToken(.l_paren);
            const ty = try p.parseType();
            _ = try p.expectToken(.r_paren);
            return p.addNode(.{
                .safe_array = ty,
            });
        },
        else => {},
    }

    const maybe_base_type = try p.parseBaseType();
    if (maybe_base_type) |base_type| {
        return p.addNode(.{ .base_type = base_type });
    }

    if (p.peekToken().tag == .identifier) {
        return try p.parseKnownType();
    }

    return p.addError(.{ .generic = "unknown unqualified type" });
}

// BaseType
//     <- KEYWORD_byte
//      / 'wchar'
//      / KEYWORD_signed IntStd?
//      / KEYWORD_unsigned IntStd?
//      / IntStd
//      / KEYWORD_float
//      / KEYWORD_double
//      / KEYWORD_boolean
//      / 'error_status_t'
//      / KEYWORD_handle_t
fn parseBaseType(p: *Parser) Error!?Node.BaseType.Tag {
    const maybe_base_type: ?Node.BaseType.Tag = switch (p.peekToken().tag) {
        .keyword_byte => .byte,
        .keyword_boolean => .boolean,
        .keyword_float => .float,
        .keyword_double => .double,
        .identifier => if (p.eatIdentifier("wchar"))
            .wchar
        else if (p.eatIdentifier("error_status_t"))
            .error_status_t
        else
            null,
        .keyword_unsigned => {
            _ = try p.expectToken(.keyword_unsigned);
            const int_tag = try p.parseInt() orelse .u32;
            return int_tag.toSign(.unsigned);
        },
        .keyword_signed => {
            _ = try p.expectToken(.keyword_signed);
            const int_tag = try p.parseInt() orelse .i32;
            return int_tag.toSign(.signed);
        },
        else => return try p.parseInt(),
    };
    if (maybe_base_type) |_| _ = p.nextToken();
    return maybe_base_type;
}

// IntStd
//     <- KEYWORD_int
//      / KEYWORD_short KEYWORD_int?
//      / KEYWORD_small
//      / KEYWORD_long KEYWORD_int?
//      / KEYWORD_hyper KEYWORD_int?
//      / KEYWORD___int64
//      / KEYWORD_char
//      / KEYWORD___int32
//      / KEYWORD___int3264
fn parseInt(p: *Parser) Error!?Node.BaseType.Tag {
    // TODO: Validate sizes depending on target. e.g int is 16-bit on 16-bit targets.
    const token: Node.BaseType.Tag = switch (p.peekToken().tag) {
        .keyword_int => .i32,
        .keyword_short => {
            _ = p.eatToken(.keyword_short);
            return .i16;
        },
        .keyword_small => .i8,
        .keyword_long => {
            _ = p.eatToken(.keyword_long);
            return .i32;
        },
        .keyword_hyper => {
            _ = p.eatToken(.keyword_hyper);
            return .i64;
        },
        .keyword_char => .i8,
        .keyword___int32 => .i32,
        .keyword___int64 => .i64,
        .keyword___int3264 => .i3264,
        else => return null,
    };
    _ = p.nextToken();
    return token;
}

// KnownType <- IDENTIFIER
fn parseKnownType(p: *Parser) Error!Node.Ref {
    return p.addNode(.{ .known_type = try p.expectIdentifierRef() });
}

// Enums <- EnumList (COMMA EnumList)*
//
// EnumDef <- KEYWORD_enum IDENTIFIER? LBRACE Enums RBRACE
fn parseEnum(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_enum);
    const enum_ref = if (p.eatToken(.identifier)) |tok|
        p.intern_pool.intern(p.getTokenString(tok))
    else
        null;

    if (p.eatToken(.l_brace)) |_| {} else {
        if (enum_ref == null) return p.addError(.{ .generic = "enum reference must have a name" });
        return p.addNode(.{ .enum_ref = enum_ref.? });
    }

    // Enums
    var enums = p.rangeArray();
    defer enums.reset();
    while (p.peekToken().tag != .r_brace) {
        const enum_list = try p.parseEnumList();
        try enums.append(enum_list);
        if (p.eatToken(.comma)) |_| {} else {
            break;
        }
    }
    _ = try p.expectToken(.r_brace);
    return p.addNode(.{
        .enum_def = .{
            .attributes = attributes,
            .name = enum_ref,
            .members = try p.addDataRanges(enums.constSlice()),
        },
    });
}

// EnumList <- Enum (COMMA Enum)*
// TODO: This appears to allow a trailing comma. re-check other list rules.
fn parseEnumList(p: *Parser) Error!Node.Range {
    var fields = p.refArray();
    defer fields.reset();

    while (true) {
        const field = try p.parseEnumField();
        try fields.append(field);
        if (p.eatToken(.comma)) |_| {} else break;
        if (p.peekToken().tag == .r_brace) break;
    }

    return p.addData(fields.constSlice());
}

// Enum <- EnumMember (EQUAL ExprIntConst)?
// EnumMember <- Attributes? IDENTIFIER
fn parseEnumField(p: *Parser) Error!Node.Ref {
    const attr_list = try p.parseAttributes();
    const member_name = try p.expectIdentifierRef();

    const maybe_expr: ?Node.Ref = if (p.eatToken(.equal)) |_| try p.parseExpr() else null;

    return try p.addNode(.{ .enum_field = .{
        .attributes = attr_list,
        .name = member_name,
        .expr = maybe_expr,
    } });
}

// StructDef <- KEYWORD_struct IDENTIFIER? LBRACE Fields RBRACE
// Fields <- Field*
fn parseStruct(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_struct);
    const struct_ref = if (p.eatToken(.identifier)) |tok|
        p.intern_pool.intern(p.getTokenString(tok))
    else
        null;

    if (p.eatToken(.l_brace)) |_| {} else {
        if (struct_ref == null) return p.addError(.{ .generic = "struct reference must have a name" });
        return p.addNode(.{ .struct_ref = struct_ref.? });
    }

    // Fields
    var fields = p.refArray();
    defer fields.reset();
    while (p.peekToken().tag != .r_brace) {
        const field = try p.parseField();
        try fields.append(field);
    }
    _ = try p.expectToken(.r_brace);

    return p.addNode(.{
        .struct_def = .{
            .attributes = attributes,
            .name = struct_ref,
            .fields = try p.addData(fields.constSlice()),
        },
    });
}

// DeclSpec StructDeclaratorList
fn parseDeclSpecAndStructDeclList(p: *Parser) Error!Node.Ref {
    const decl_spec = try p.parseDeclSpec();
    const struct_decl_list = try p.parseListOf(parseStructDecl);
    return p.addNode(.{
        .struct_field = .{
            .attributes = null,
            .field = .{
                .default = .{
                    .decl_spec = decl_spec,
                    .struct_decl_list = struct_decl_list,
                },
            },
        },
    });
}

// Field
//     <- Attributes? DeclSpec StructDeclaratorList SEMICOLON
//      / Attributes? UnionDef SEMICOLON
fn parseField(p: *Parser) Error!Node.Ref {
    const attributes = try p.parseAttributes();

    if (try p.optional(parseDeclSpecAndStructDeclList, Node.Ref)) |node| {
        p.nodes.items[node.toInt()].struct_field.attributes = attributes;
        _ = try p.expectToken(.semicolon);
        return node;
    }

    const field = try p.parseUnion(attributes);
    _ = try p.expectToken(.semicolon);
    return p.addNode(.{
        .struct_field = .{
            .attributes = attributes,
            .field = .{ .@"union" = field },
        },
    });
}

// StructDeclaratorList <- StructDeclarator (COMMA StructDeclarator)*
// StructDeclarator <- AnyDeclarator BitField?
fn parseStructDecl(p: *Parser) Error!Node.Ref {
    const any_decl = try p.parseAnyDeclarator();
    const maybe_bit_field = if (p.eatToken(.colon)) |_|
        try p.parseExprConst()
    else
        null;

    return p.addNode(.{ .struct_decl = .{
        .any_decl = any_decl,
        .bit_field = maybe_bit_field,
    } });
}

// Exprs <- Expr (COMMA Expr)*
fn parseExprs(p: *Parser) Error!Node.Range {
    var exprs = p.refArray();
    defer exprs.reset();

    while (true) {
        const expr = try p.parseExpr();
        try exprs.append(expr);
        if (p.eatToken(.comma)) |_| {} else {
            break;
        }
    }

    return p.addData(exprs.constSlice());
}

// ExprConst <- Expr
fn parseExprConst(p: *Parser) Error!Node.Ref {
    return p.addNode(.{
        .expr_const = try p.parseExpr(),
    });
}

// ExprIntConst <- Expr
fn parseExprIntConst(p: *Parser) Error!Node.Ref {
    return p.addNode(.{
        .expr_const_int = try p.parseExpr(),
    });
}

// UnionDef
//     <- KEYWORD_union IDENTIFIER? LBRACE NeUnionFields RBRACE
//      / KEYWORD_union IDENTIFIER? KEYWORD_switch LPAREN SField RPAREN Ident? LBRACE Cases RBRACE
fn parseUnion(p: *Parser, attributes: ?Node.Range) Error!Node.Ref {
    _ = try p.expectToken(.keyword_union);

    const name_ref = if (p.eatToken(.identifier)) |tok|
        p.intern_pool.intern(p.getTokenString(tok))
    else
        null;

    if (p.eatToken(.keyword_switch)) |_| {
        _ = try p.expectToken(.l_paren);
        const sfield = try p.parseSField();
        _ = try p.expectToken(.r_paren);
        const maybe_tok = p.eatToken(.identifier);
        const ident_ref = if (maybe_tok) |tok|
            p.intern_pool.intern(p.getTokenString(tok))
        else
            null;
        _ = try p.expectToken(.l_brace);
        const cases = try p.parseCases();
        _ = try p.expectToken(.r_brace);
        return p.addNode(.{
            .union_switch = .{
                .attributes = attributes,
                .name = name_ref,
                .cases = cases,
                .sfield = sfield,
                .ident = ident_ref,
            },
        });
    } else {
        _ = try p.expectToken(.l_brace);
        const fields = try p.parseNeUnionFields();
        _ = try p.expectToken(.r_brace);
        return p.addNode(.{
            .union_def = .{
                .attributes = attributes,
                .name = name_ref,
                .fields = fields,
            },
        });
    }
}

// Cases <- Case*
fn parseCases(p: *Parser) Error!Node.Range {
    var cases = p.refArray();
    defer cases.reset();
    while (try p.parseCase()) |case| {
        try cases.append(case);
    }
    return try p.addData(cases.constSlice());
}

// Case
//     <- KEYWORD_case ExprIntConst COLON UnionField
//      / KEYWORD_default COLON UnionField
fn parseCase(p: *Parser) Error!?Node.Ref {
    switch (p.peekToken().tag) {
        .keyword_case => {
            _ = try p.expectToken(.keyword_case);
            const expr = try p.parseExprConst();
            _ = try p.expectToken(.colon);
            const field = try p.parseUnionField();
            return try p.addNode(.{
                .union_switch_case = .{
                    .case_expr = expr,
                    .field = field,
                },
            });
        },
        .keyword_default => {
            _ = try p.expectToken(.keyword_default);
            _ = try p.expectToken(.colon);
            const field = try p.parseUnionField();
            return try p.addNode(.{
                .union_switch_case = .{
                    .field = field,
                },
            });
        },
        else => return null,
    }
}

// UnionField <- SField? SEMICOLON
fn parseUnionField(p: *Parser) Error!Node.Ref {
    const s_field = try p.optional(parseSField, Node.Ref);
    const node = p.addNode(.{
        .union_field = .{
            .s_field_optional = s_field,
        },
    });
    _ = try p.expectToken(.semicolon);
    return node;
}

// NeUnionFields <- NeUnionField*
// NeUnionField <- (SField / Attributes) SEMICOLON
fn parseNeUnionFields(p: *Parser) Error!Node.Range {
    var fields = p.refArray();
    defer fields.reset();
    while (true) {
        if (try p.optional(parseSField, Node.Ref)) |s_field| {
            try fields.append(try p.addNode(.{ .union_field = .{
                .s_field = s_field,
            } }));
            _ = try p.expectToken(.semicolon);
        } else if (try p.parseAttributes()) |attrs| {
            try fields.append(try p.addNode(.{ .union_field = .{
                .attributes = attrs,
            } }));
            _ = try p.expectToken(.semicolon);
        } else {
            break;
        }
    }

    return try p.addData(fields.constSlice());
}

// DeclSpec Declarator
fn parseDeclSpecAndDeclarator(p: *Parser) Error!Node.Ref {
    const decl_spec = try p.parseDeclSpec();
    const declarator = try p.parseDeclarator();
    return p.addNode(.{
        .union_sfield = .{
            .default = .{
                .attributes = null,
                .decl_spec = decl_spec,
                .declarator = declarator,
            },
        },
    });
}

// SField
//     <- Attributes? DeclSpec Declarator
//      / Attributes? StructDef
fn parseSField(p: *Parser) Error!Node.Ref {
    const attributes = try p.parseAttributes();

    if (try p.optional(parseDeclSpecAndDeclarator, Node.Ref)) |node| {
        p.nodes.items[node.toInt()].union_sfield.default.attributes = attributes;
        return node;
    }

    const struct_def = try p.parseStruct(attributes);
    return try p.addNode(.{
        .union_sfield = .{
            .struct_def = .{
                .attributes = attributes,
                .def = struct_def,
            },
        },
    });
}

// Expr <- ExprTernary
fn parseExpr(p: *Parser) Error!Node.Ref {
    return p.parseExprTernary();
}
// ExprTernary <- ExprLogicalOr (QUESTIONMARK ExprTernary COLON ExprTernary)?
fn parseExprTernary(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprLogicalOr();
    if (p.eatToken(.question_mark)) |_| {
        const expr_true = try p.parseExprTernary();
        _ = try p.expectToken(.colon);
        const expr_false = try p.parseExprTernary();
        return p.addNode(.{ .expr_ternary = .{
            .op = .ternary_conditional,
            .arg1 = expr,
            .arg2 = expr_true,
            .arg3 = expr_false,
        } });
    }
    return expr;
}
// ExprLogicalOr <- ExprLogicalAnd (PIPE2 ExprLogicalOr)*
fn parseExprLogicalOr(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprLogicalAnd();
    if (p.eatToken(.pipe_pipe)) |_| {
        const expr2 = try p.parseExprLogicalOr();
        return p.addNode(.{ .expr_binary = .{
            .op = .logical_or,
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// ExprLogicalAnd <- ExprOr (AMPERSAND2 ExprLogicalAnd)*
fn parseExprLogicalAnd(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprOr();
    if (p.eatToken(.ampersand_ampersand)) |_| {
        const expr2 = try p.parseExprLogicalAnd();
        return p.addNode(.{ .expr_binary = .{
            .op = .logical_and,
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// ExprOr <- ExprXor (PIPE ExprOr)*
fn parseExprOr(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprXor();
    if (p.eatToken(.pipe)) |_| {
        const expr2 = try p.parseExprOr();
        return p.addNode(.{ .expr_binary = .{
            .op = .@"or",
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// ExprXor <- ExprAnd (CARET ExprXor)*
fn parseExprXor(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprAnd();
    if (p.eatToken(.caret)) |_| {
        const expr2 = try p.parseExprXor();
        return p.addNode(.{ .expr_binary = .{
            .op = .@"and",
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// ExprAnd <- ExprEquality (AMPERSAND ExprAnd)*
fn parseExprAnd(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprEquality();
    if (p.eatToken(.ampersand)) |_| {
        const expr2 = try p.parseExprAnd();
        return p.addNode(.{ .expr_binary = .{
            .op = .@"and",
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// ExprEquality <- ExprCompare ((EQUAL2 / EXCLAMATIONMARKEQUAL) ExprCompare)*
fn parseExprEquality(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprCompare();
    if (p.eatTokenOneOf(&.{
        .equal_equal,
        .bang_equal,
    })) |tok| {
        const op: Node.Op = switch (tok.tag) {
            .equal_equal => .equal,
            .bang_equal => .unequal,
            else => unreachable,
        };

        const expr2 = try p.parseExprCompare();
        return p.addNode(.{ .expr_binary = .{
            .op = op,
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// ExprCompare <- ExprShift ((LARROW / RARROW / RARROWEQUAL / LARROWEQUAL) ExprShift)*
fn parseExprCompare(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprShift();
    if (p.eatTokenOneOf(&.{
        .angle_bracket_left,
        .angle_bracket_right,
        .angle_bracket_right_equal,
        .angle_bracket_left_equal,
    })) |tok| {
        const op: Node.Op = switch (tok.tag) {
            .angle_bracket_left => .compare_lt,
            .angle_bracket_right => .compare_gt,
            .angle_bracket_right_equal => .compare_lte,
            .angle_bracket_left_equal => .compare_gte,
            else => unreachable,
        };

        const expr2 = try p.parseExprShift();
        return p.addNode(.{ .expr_binary = .{
            .op = op,
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// ExprShift <- ExprAdd ((LARROW2 / RARROW2) ExprShift)*
fn parseExprShift(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprAdd();
    if (p.eatTokenOneOf(&.{
        .angle_bracket_angle_bracket_left,
        .angle_bracket_angle_bracket_right,
    })) |tok| {
        const op: Node.Op = switch (tok.tag) {
            .angle_bracket_angle_bracket_right => .shift_right,
            .angle_bracket_angle_bracket_left => .shift_left,
            else => unreachable,
        };

        const expr2 = try p.parseExprShift();
        return p.addNode(.{ .expr_binary = .{
            .op = op,
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// ExprAdd <- ExprMult ((PLUS / MINUS) ExprAdd)*
fn parseExprAdd(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprMult();
    if (p.eatTokenOneOf(&.{
        .plus,
        .minus,
    })) |tok| {
        const op: Node.Op = switch (tok.tag) {
            .plus => .add,
            .minus => .sub,
            else => unreachable,
        };

        const expr2 = try p.parseExprAdd();
        return p.addNode(.{ .expr_binary = .{
            .op = op,
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// ExprMult <- ExprUnaryPrefix ((ASTERISK / SLASH / PERCENT) ExprUnaryPrefix)*
fn parseExprMult(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprUnaryPrefix();
    if (p.eatTokenOneOf(&.{
        .asterisk,
        .slash,
        .percent,
    })) |tok| {
        const op: Node.Op = switch (tok.tag) {
            .asterisk => .mul,
            .slash => .div,
            .percent => .mod,
            else => unreachable,
        };

        const expr2 = try p.parseExprMult();
        return p.addNode(.{ .expr_binary = .{
            .op = op,
            .arg1 = expr,
            .arg2 = expr2,
        } });
    }
    return expr;
}
// LPAREN UnqualifiedDeclSpec AbstractDeclarator? RPAREN ExprUnaryPostfix
fn parseCast(p: *Parser) Error!Node.Ref {
    _ = try p.expectToken(.l_paren);
    const unqual_decl_spec = try p.parseUnqualifiedDeclSpec();
    const declarator = try p.optional(parseAbstractDeclarator, Node.Ref);
    _ = try p.expectToken(.r_paren);
    const expr = try p.parseExprUnaryPrefix();
    return p.addNode(.{ .cast = .{
        .unqual_decl_spec = unqual_decl_spec,
        .declarator = declarator,
        .expr = expr,
    } });
}
// ExprUnaryPrefix
//     <- (PLUS / MINUS / EXCLAMATIONMARK / TILDE / PLUS2 / MINUS2 / AMPERSAND / ASTERISK) ExprUnaryPrefix
//      / KEYWORD_sizeof LPAREN UnqualifiedDeclSpec AbstractDeclarator? RPAREN
//      / LPAREN UnqualifiedDeclSpec AbstractDeclarator? RPAREN ExprUnaryPostfix
//      / ExprUnaryPostfix
fn parseExprUnaryPrefix(p: *Parser) Error!Node.Ref {
    switch (p.peekToken().tag) {
        .plus,
        .minus,
        .bang,
        .tilde,
        .plus_plus,
        .minus_minus,
        .ampersand,
        .asterisk,
        => |tag| {
            _ = p.nextToken();
            const op: Node.Op = switch (tag) {
                .plus => .positive,
                .minus => .negative,
                .bang => .logical_not,
                .tilde => .not,
                .plus_plus => .inc,
                .minus_minus => .dec,
                .ampersand => .ref,
                .asterisk => .deref,
                else => unreachable,
            };
            const expr = try p.parseExprUnaryPrefix();
            return p.addNode(.{ .expr_unary = .{
                .op = op,
                .arg1 = expr,
            } });
        },
        .keyword_sizeof => {
            _ = p.nextToken();
            _ = try p.expectToken(.l_paren);
            const unqual_decl_spec = try p.parseUnqualifiedDeclSpec();
            const abstract_decl = try p.optional(parseAbstractDeclarator, Node.Ref);
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .sizeof = .{
                .unqual_decl_spec = unqual_decl_spec,
                .abstract_decl = abstract_decl,
            } });
        },
        else => {
            if (try p.optional(parseCast, Node.Ref)) |node| return node;
            return try p.parseExprUnaryPostfix();
        },
    }
}
// ExprUnaryPostfix <- ExprPrimary ExprPostfix*
fn parseExprUnaryPostfix(p: *Parser) Error!Node.Ref {
    const expr = try p.parseExprPrimary();

    var postfix_exprs = p.refArray();
    defer postfix_exprs.reset();
    while (try p.parseExprPostfix()) |postfix_expr| {
        try postfix_exprs.append(postfix_expr);
    }

    if (postfix_exprs.len() > 0) {
        return p.addError(.{ .todo = "parseExprUnaryPostfix: implement postfix expressions" });
    } else {
        return expr;
    }
}
// ExprPostfix
//     <- (LBRACKET Expr RBRACKET)
//      / (DOT IDENTIFIER)
//      / (MEMBERPTR IDENTIFIER)
//      / (PLUS2)
//      / (MINUS2)
fn parseExprPostfix(p: *Parser) Error!?Node.Ref {
    switch (p.peekToken().tag) {
        .l_bracket => {
            _ = try p.expectToken(.l_bracket);
            const expr = try p.parseExpr();
            _ = try p.expectToken(.r_bracket);
            return expr;
        },
        .period => {
            _ = try p.expectToken(.period);
            const ident_ref = try p.expectIdentifierRef();
            _ = ident_ref;
            return p.addError(.{ .todo = "implement '.' member access" });
        },
        .arrow => {
            _ = try p.expectToken(.arrow);
            const ident_ref = try p.expectIdentifierRef();
            _ = ident_ref;
            return p.addError(.{ .todo = "implement '->' member access" });
        },
        // TODO: Would like these as standard unary operators with the expr as the arg
        .plus_plus,
        .minus_minus,
        => return p.addError(.{ .todo = "implement '--' and '++' postfix operator" }),

        else => return null,
    }
}
// ExprPrimary
//     <- INTEGER
//      / FLOAT
//      / KEYWORD_FALSE
//      / KEYWORD_TRUE
//      / KEYWORD_NULL
//      / STRINGLITERAL
//      / WSTRINGLITERAL
//      / SQSTRINGLITERAL
//      / IDENTIFIER
//      / LPAREN Expr RPAREN
fn parseExprPrimary(p: *Parser) Error!Node.Ref {
    const tok = p.nextToken();
    return switch (tok.tag) {
        .keyword_FALSE => try p.addNode(.{ .expr = .false }),
        .keyword_TRUE => try p.addNode(.{ .expr = .true }),
        .keyword_NULL => try p.addNode(.{ .expr = .null }),
        .number_literal => try p.addNode(.{
            .expr = .{
                .number = p.classifyNumberLiteral(tok) catch return error.ParseError,
            },
        }),
        .string_literal => try p.addNode(.{
            .expr = .{
                .string = p.intern(tok), // TODO cut quotes off like p.expectStringLiteral
            },
        }),
        .identifier,
        => try p.addNode(.{ .expr = .{
            .string = p.intern(tok),
        } }),
        .l_paren => {
            const expr = try p.parseExpr();
            _ = try p.expectToken(.r_paren);
            return expr;
        },
        else => return p.addError(.{ .expected = "primary expression" }),
    };
}

// UnqualifiedDeclSpec <- DeclSpecNoType? UnqualifiedType DeclSpecNoType?
fn parseUnqualifiedDeclSpec(p: *Parser) Error!Node.Ref {
    const prefix_decl_spec = try p.parseDeclSpecNoType();
    const @"type" = try p.parseUnqualifiedType();
    const suffix_decl_spec = try p.parseDeclSpecNoType();

    return p.addNode(.{ .unqualified_decl_spec_type = .{
        .type = @"type",
        .decl_specs = try p.addDataRanges(&.{ prefix_decl_spec, suffix_decl_spec }),
    } });
}

// AbstractDeclarator
//     <- ASTERISK TypeQualList? AbstractDeclarator?
//      / CallConv AbstractDeclarator?
//      / AbstractDirectDeclarator
fn parseAbstractDeclarator(p: *Parser) Error!Node.Ref {
    if (p.eatToken(.asterisk)) |_| {
        const type_quals = try p.parseTypeQualList();
        return p.addNode(.{ .declarator = .{
            .is_pointer = true,
            .type_quals = type_quals,
            .decl = try p.optional(parseDeclarator, Node.Ref),
        } });
    }
    if (try p.parseCallConv()) |call_conv| {
        return p.addNode(.{ .abstract_declarator = .{
            .call_conv = call_conv,
            .decl = try p.optional(
                parseAbstractDeclarator,
                Node.Ref,
            ),
        } });
    }
    return p.addNode(.{ .abstract_declarator = .{
        .decl = try p.parseAbstractDirectDeclarator(),
    } });
}

// DeclSpecNoType
//     <- TypeQualifier DeclSpecNoType?
//      / FunctionSpecifier DeclSpecNoType?
//      / StorageClassSpec DeclSpecNoType?
//
// StorageClassSpec
//     <- KEYWORD_extern
//      / KEYWORD_static
//      / KEYWORD_register
//
// FunctionSpecifier <- KEYWORD_inline
// TypeQualifier <- KEYWORD_const
fn parseDeclSpecNoType(p: *Parser) Error!Node.Range {
    var specs = p.refArray();
    defer specs.reset();
    while (true) {
        const tag: Node.DeclSpec = switch (p.peekToken().tag) {
            .keyword_const => .@"const",
            .keyword_inline => .@"inline",
            .keyword_extern => .@"extern",
            .keyword_static => .static,
            .keyword_register => .register,
            else => break,
        };
        _ = p.nextToken();
        try specs.append(try p.addNode(.{ .decl_spec = tag }));
    }
    return try p.addData(specs.constSlice());
}

// ContractVersion
//     <- INTEGER
//      / INTEGER DOT INTEGER
fn parseContractVersion(p: *Parser) Error!Node.Ref {
    const tok = try p.expectToken(.number_literal);
    const tok_lit = p.getTokenString(tok);

    const version = parseIntVersion(tok_lit) catch
        return p.addError(.{ .invalid_version = tok_lit });

    return p.addNode(.{
        .int_version = version,
    });
}

// ContractReq <- DeclSpec COMMA ContractVersion
fn parseContractReq(p: *Parser) Error!Node.Ref {
    const decl_spec = try p.parseDeclSpec();
    _ = try p.expectToken(.comma);
    const version = try p.parseContractVersion();

    return p.addNode(.{ .contract_req = .{
        .decl_spec = decl_spec,
        .version = version,
    } });
}

// StaticAttr <- DeclSpec COMMA ContractReq
fn parseStaticAttr(p: *Parser) Error!Node.Ref {
    const decl_spec = try p.parseDeclSpec();
    _ = try p.expectToken(.comma);
    const req = try p.parseContractReq();

    return p.addNode(.{ .static_attr = .{
        .decl_spec = decl_spec,
        .contract_req = req,
    } });
}

// Version
//     <- DECINTEGER (DOT INTEGER)?
//      / HEXINTEGER
fn parseVersion(p: *Parser) Error!Node.Ref {
    const tok = try p.expectToken(.number_literal);
    const tok_lit = p.getTokenString(tok);

    if (std.mem.startsWith(u8, tok_lit, "0x")) {
        const value = std.fmt.parseInt(u32, tok_lit, 0) catch
            return p.addError(.{ .invalid_version = tok_lit });

        return try p.addNode(.{ .version = .{
            .hex = value,
        } });
    } else if (parseIntVersion(tok_lit) catch null) |int_version| {
        return try p.addNode(.{
            .version = .{
                .int = int_version,
            },
        });
    } else {
        return p.addError(.{ .invalid_version = tok_lit });
    }
}

// INTEGER (DOT INTEGER)?
fn parseIntVersion(s: []const u8) !Node.IntVersion {
    if (std.mem.indexOfScalar(u8, s, '.')) |pos| {
        const major = try std.fmt.parseInt(u32, s[0..pos], 10);
        const minor = try std.fmt.parseInt(u32, s[pos + 1 ..], 10);
        return .{ .major = major, .minor = minor };
    }

    const value = try std.fmt.parseInt(u32, s, 10);
    return .{ .major = value };
}

// Attributes <- AttributeBlock+
// AttributeBlock <- LBRACKET AttributeList RBRACKET
// AttributeList <- Attribute (COMMA Attribute?)*
fn parseAttributes(p: *Parser) Error!?Node.Range {
    var attrs = p.refArray();
    defer attrs.reset();

    while (p.eatToken(.l_bracket)) |_| {
        while (true) {
            if (try p.optional(parseAttribute, Node.Ref)) |ref| {
                try attrs.append(ref);
            }
            const comma = p.eatToken(.comma);
            if (p.eatToken(.r_bracket)) |_| break;
            if (comma == null) return error.ParseError;
        }
    }
    return if (attrs.len() > 0) try p.addData(attrs.constSlice()) else null;
}

// Attribute
//     <- KEYWORD_ATTR_activatable LPAREN ActivatableAttr RPAREN
//      / KEYWORD_ATTR_aggretable
//      / KEYWORD_ATTR_annotation LPAREN STRINGLITERAL RPAREN
//      / KEYWORD_ATTR_appobject
//      / KEYWORD_ATTR_async
//      / KEYWORD_ATTR_autohandle
//      / KEYWORD_ATTR_bindable
//      / KEYWORD_ATTR_broadcast
//      / KEYWORD_ATTR_callas LPAREN IDENTIFIER RPAREN
//      / KEYWORD_ATTR_case LPAREN ExprListIntConst RPAREN
//      / KEYWORD_ATTR_code
//      / KEYWORD_ATTR_composable LPAREN ComposableAttr RPAREN
//      / KEYWORD_ATTR_commstatus
//      / KEYWORD_ATTR_contexthandle
//      / KEYWORD_ATTR_contexthandlenoserialize
//      / KEYWORD_ATTR_contexthandleserialize
//      / KEYWORD_ATTR_contract LPAREN ContractReq RPAREN
//      / KEYWORD_ATTR_contractversion LPAREN ContractVersion RPAREN
//      / KEYWORD_ATTR_control
//      / KEYWORD_ATTR_custom LPAREN Uuid COMMA ExprConst RPAREN
//      / KEYWORD_ATTR_decode
//      / KEYWORD_ATTR_default
//      / KEYWORD_ATTR_defaultbind
//      / KEYWORD_ATTR_defaultcollelem
//      / KEYWORD_ATTR_defaultvalue LPAREN ExprConst RPAREN
//      / KEYWORD_ATTR_defaultvtable
//      / KEYWORD_ATTR_deprecated LPAREN DeprecatedAttr RPAREN
//      / KEYWORD_ATTR_disableconsistencycheck
//      / KEYWORD_ATTR_displaybind
//      / KEYWORD_ATTR_dllname LPAREN STRINGLITERAL RPAREN
//      / KEYWORD_ATTR_dual
//      / KEYWORD_ATTR_enableallocate
//      / KEYWORD_ATTR_encode
//      / KEYWORD_ATTR_endpoint LPAREN StringList RPAREN
//      / KEYWORD_ATTR_entry LPAREN ExprConst RPAREN
//      / KEYWORD_ATTR_event_add
//      / KEYWORD_ATTR_event_remove
//      / KEYWORD_ATTR_exclusiveto LPAREN DeclSpec RPAREN
//      / KEYWORD_ATTR_explicithandle
//      / KEYWORD_ATTR_faultstatus
//      / KEYWORD_ATTR_forceallocate
//      / KEYWORD_ATTR_handle
//      / KEYWORD_ATTR_helpcontext LPAREN ExprIntConst RPAREN
//      / KEYWORD_ATTR_helpfile LPAREN STRINGLITERAL RPAREN
//      / KEYWORD_ATTR_helpstring LPAREN STRINGLITERAL RPAREN
//      / KEYWORD_ATTR_helpstringcontext LPAREN ExprIntConst RPAREN
//      / KEYWORD_ATTR_helpstringdll LPAREN STRINGLITERAL RPAREN
//      / KEYWORD_ATTR_hidden
//      / KEYWORD_ATTR_id LPAREN ExprIntConst RPAREN
//      / KEYWORD_ATTR_idempotent
//      / KEYWORD_ATTR_ignore
//      / KEYWORD_ATTR_iid_is LPAREN Expr RPAREN
//      / KEYWORD_ATTR_immediatebind
//      / KEYWORD_ATTR_implicithandle LPAREN Arg RPAREN
//      / KEYWORD_ATTR_in
//      / KEYWORD_ATTR_inputsync
//      / KEYWORD_ATTR_length_is LPAREN Exprs? RPAREN
//      / KEYWORD_ATTR_lcid (LPAREN ExprIntConst RPAREN)?
//      / KEYWORD_ATTR_licensed
//      / KEYWORD_ATTR_local
//      / KEYWORD_ATTR_marshaling_behavior LPAREN MarshalingBehavior RPAREN
//      / KEYWORD_ATTR_maybe
//      / KEYWORD_ATTR_message
//      / KEYWORD_ATTR_nocode
//      / KEYWORD_ATTR_nonbrowsable
//      / KEYWORD_ATTR_noncreatable
//      / KEYWORD_ATTR_nonextensible
//      / KEYWORD_ATTR_notify
//      / KEYWORD_ATTR_notifyflag
//      / KEYWORD_ATTR_object
//      / KEYWORD_ATTR_odl
//      / KEYWORD_ATTR_oleautomation
//      / KEYWORD_ATTR_optimize LPAREN STRINGLITERAL RPAREN
//      / KEYWORD_ATTR_optional
//      / KEYWORD_ATTR_out
//      / KEYWORD_ATTR_partialignore
//      / KEYWORD_ATTR_pointerdefault LPAREN PointerType RPAREN
//      / KEYWORD_ATTR_progid LPAREN STRINGLITERAL RPAREN
//      / KEYWORD_ATTR_propget
//      / KEYWORD_ATTR_propput
//      / KEYWORD_ATTR_propputref
//      / KEYWORD_ATTR_proxy
//      / KEYWORD_ATTR_public
//      / KEYWORD_ATTR_range LPAREN ExprIntConst COMMA ExprIntConst RPAREN
//      / KEYWORD_ATTR_readonly
//      / KEYWORD_ATTR_represent_as LPAREN Type RPAREN
//      / KEYWORD_ATTR_requestedit
//      / KEYWORD_ATTR_restricted
//      / KEYWORD_ATTR_retval
//      / KEYWORD_ATTR_size_is LPAREN Exprs? RPAREN
//      / KEYWORD_ATTR_source
//      / KEYWORD_ATTR_static LPAREN StaticAttr RPAREN
//      / KEYWORD_ATTR_strictcontexthandle
//      / KEYWORD_ATTR_string
//      / KEYWORD_ATTR_switch_is LPAREN Expr RPAREN
//      / KEYWORD_ATTR_switch_type LPAREN Type RPAREN
//      / KEYWORD_ATTR_transmit_as LPAREN Type RPAREN
//      / KEYWORD_ATTR_threading LPAREN ThreadingType RPAREN
//      / KEYWORD_ATTR_uidefault
//      / KEYWORD_ATTR_usesgetlasterror
//      / KEYWORD_ATTR_user_marshal LPAREN Type RPAREN
//      / KEYWORD_ATTR_uuid LPAREN Uuid RPAREN
//      / KEYWORD_ATTR_asyncuuid LPAREN Uuid RPAREN
//      / KEYWORD_ATTR_v1enum
//      / KEYWORD_ATTR_vararg
//      / KEYWORD_ATTR_version LPAREN Version RPAREN
//      / KEYWORD_ATTR_vi_progid LPAREN STRINGLITERAL RPAREN
//      / KEYWORD_ATTR_wire_marshal LPAREN Type RPAREN
//      / PointerType
fn parseAttribute(p: *Parser) Error!Node.Ref {
    const tok = p.peekToken();
    const tok_string = p.getTokenString(tok);

    const attr = Node.Attribute.Tag.get(tok_string) orelse
        return p.addError(.{ .invalid_attribute = tok_string });

    _ = p.nextToken();

    switch (attr) {
        inline .activatable,
        .aggregatable,
        .appobject,
        .@"async",
        .auto_handle,
        .bindable,
        .broadcast,
        .comm_status,
        .context_handle,
        .context_handle_noserialize,
        .context_handle_serialize,
        .control,
        .decode,
        .default,
        .defaultbind,
        .defaultcollelem,
        .defaultvtable,
        .disable_consistency_check,
        .displaybind,
        .dual,
        .enable_allocate,
        .encode,
        .event_add,
        .event_remove,
        .explcit_handle,
        .fault_status,
        .force_allocate,
        .handle,
        .hidden,
        .idempotent,
        .ignore,
        .immediatebind,
        .in,
        .input_sync,
        .licensed,
        .local,
        .maybe,
        .message,
        .nocode,
        .nonbrowable,
        .noncreatable,
        .nonextensible,
        .notify,
        .notify_flag,
        .object,
        .odl,
        .oleautomation,
        .optional,
        .out,
        .partial_ignore,
        .propget,
        .propput,
        .propputref,
        .proxy,
        .public,
        .readonly,
        .requestedit,
        .restricted,
        .retval,
        .source,
        .strict_context_handle,
        .string,
        .uidefault,
        .usesgetlasterror,
        .v1_enum,
        .vararg,
        // PointerType
        .ref,
        .unique,
        .ptr,
        => |tag| return p.addNode(.{ .attribute = tag }),

        inline .annotation,
        .dllname,
        .helpfile,
        .helpstring,
        .optimize,
        .progid,
        .vi_progid,
        => |tag| {
            _ = try p.expectToken(.l_paren);
            const lit_ref = p.intern_pool.intern(try p.expectStringLiteral());
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = @unionInit(
                Node.Attribute,
                @tagName(tag),
                lit_ref,
            ) });
        },

        inline .represent_as,
        .switch_type,
        .transmit_as,
        .user_marshal,
        .wire_marshal,
        => |tag| {
            _ = try p.expectToken(.l_paren);
            const ty = try p.parseType();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = @unionInit(
                Node.Attribute,
                @tagName(tag),
                ty,
            ) });
        },

        .call_as,
        => {
            _ = try p.expectToken(.l_paren);
            const ref = try p.expectIdentifierRef();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{
                .call_as = ref,
            } });
        },

        .lcid,
        => {
            const expr = if (p.eatToken(.l_paren)) |_| blk: {
                const expr = try p.parseExprIntConst();
                _ = try p.expectToken(.r_paren);
                break :blk expr;
            } else null;

            return try p.addNode(.{ .attribute = .{ .lcid = expr } });
        },

        inline .async_uuid,
        .uuid,
        => |tag| {
            _ = try p.expectToken(.l_paren);
            const uuid_val = blk: {
                if (p.peekToken().tag == .string_literal) {
                    break :blk try p.expectStringLiteral();
                } else {
                    const cs = p.currentState();
                    // At most 9 tokens. Can be less. e.g. '4000-e980' parses as a float literal.
                    // Don't parse each individual token, identify closing paren and parse as a whole.
                    const uuid_start = cs.tok_i;
                    while (cs.tokens[cs.tok_i].tag != .r_paren) : (cs.tok_i += 1) {}
                    std.debug.assert(cs.tok_i - uuid_start <= 9);
                    break :blk cs.source[cs.tokens[uuid_start].loc.start..cs.tokens[cs.tok_i - 1].loc.end];
                }
            };
            _ = try p.expectToken(.r_paren);
            const uuid_val_int = parseUUID(uuid_val) catch return p.addError(.{ .invalid_uuid = uuid_val });
            return try p.addNode(.{
                .attribute = @unionInit(
                    Node.Attribute,
                    @tagName(tag),
                    uuid_val_int,
                ),
            });
        },

        .threading => {
            _ = try p.expectToken(.l_paren);
            const threading_type_tok = try p.expectToken(.identifier);
            const threading_type_tok_string = p.getTokenString(threading_type_tok);
            const threading_type = Node.ThreadingType.get(threading_type_tok_string) orelse
                return p.addError(.{ .invalid_pointer_type = threading_type_tok_string });
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{ .threading = threading_type } });
        },

        .pointer_default => {
            _ = try p.expectToken(.l_paren);
            const ptr_type_tok = try p.expectToken(.identifier);
            const ptr_type_tok_string = p.getTokenString(ptr_type_tok);
            const ptr_type = Node.PointerType.get(ptr_type_tok_string) orelse
                return p.addError(.{ .invalid_pointer_type = ptr_type_tok_string });
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{ .pointer_default = ptr_type } });
        },

        .marshaling_behavior => {
            _ = try p.expectToken(.l_paren);
            const marshal_type_tok = try p.expectToken(.identifier);
            const marshal_type_tok_string = p.getTokenString(marshal_type_tok);
            const marshal_type = Node.MarshalingBehavior.get(marshal_type_tok_string) orelse
                return p.addError(.{ .invalid_marshal_type = marshal_type_tok_string });
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{ .marshaling_behavior = marshal_type } });
        },

        inline .length_is,
        .size_is,
        => |tag| {
            _ = try p.expectToken(.l_paren);
            _ = p.eatToken(.comma); // size_is(,expr) happens somewhat often in practice so allow
            const exprs = try p.optional(parseExprs, Node.Range);
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{
                .attribute = @unionInit(
                    Node.Attribute,
                    @tagName(tag),
                    exprs,
                ),
            });
        },

        .case,
        => {
            _ = try p.expectToken(.l_paren);
            const expr = try p.parseListOf(parseExprIntConst);
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{ .case = expr } });
        },

        .range,
        => {
            _ = try p.expectToken(.l_paren);
            const expr_lo = try p.parseExprConst();
            _ = try p.expectToken(.comma);
            const expr_hi = try p.parseExprConst();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{
                .range = .{
                    .lo = expr_lo,
                    .hi = expr_hi,
                },
            } });
        },

        inline .helpcontext,
        .helpstringcontext,
        .id,
        => |tag| {
            _ = try p.expectToken(.l_paren);
            const expr = try p.parseExprIntConst();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{
                .attribute = @unionInit(
                    Node.Attribute,
                    @tagName(tag),
                    expr,
                ),
            });
        },

        inline .defaultvalue,
        => |tag| {
            _ = try p.expectToken(.l_paren);
            const expr = try p.parseExprConst();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{
                .attribute = @unionInit(
                    Node.Attribute,
                    @tagName(tag),
                    expr,
                ),
            });
        },

        inline .iid_is,
        .switch_is,
        => |tag| {
            _ = try p.expectToken(.l_paren);
            const expr = try p.parseExpr();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{
                .attribute = @unionInit(
                    Node.Attribute,
                    @tagName(tag),
                    expr,
                ),
            });
        },

        .version,
        => {
            _ = try p.expectToken(.l_paren);
            const version = try p.parseVersion();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{ .version = version } });
        },

        .contract,
        => {
            _ = try p.expectToken(.l_paren);
            const req = try p.parseContractReq();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{ .contract = req } });
        },

        .contractversion,
        => {
            _ = try p.expectToken(.l_paren);
            const version = try p.parseContractVersion();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{ .contractversion = version } });
        },

        .static,
        => {
            _ = try p.expectToken(.l_paren);
            const static_attr = try p.parseStaticAttr();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{ .static = static_attr } });
        },

        .exclusiveto,
        => {
            _ = try p.expectToken(.l_paren);
            const decl_spec = try p.parseDeclSpec();
            _ = try p.expectToken(.r_paren);
            return try p.addNode(.{ .attribute = .{ .exclusiveto = decl_spec } });
        },

        else => return p.addError(.{ .attribute_not_implemented = tok_string }),
    }
}

// Parsing Helpers

fn expectIdentifierRef(p: *Parser) Error!InternPool.Ref {
    // Don't use expectToken, since this doesn't handle MIDL3.0 keyword exceptions
    const tok = p.eatToken(.identifier) orelse return error.ParseError;
    const lit = p.getTokenString(tok);
    return p.intern_pool.intern(lit);
}

fn parseListOf(p: *Parser, func: fn (p: *Parser) Error!Node.Ref) Error!Node.Range {
    var list = p.refArray();
    defer list.reset();
    while (true) {
        try list.append(try func(p));

        _ = p.eatToken(.comma) orelse break;
    }
    return p.addData(list.constSlice());
}

fn nextToken(p: *Parser) Token {
    const cs = p.currentState();
    const i = cs.tok_i;
    cs.tok_i += 1;
    return cs.tokens[i];
}

fn peekToken(p: *Parser) Token {
    const cs = p.currentState();
    return cs.tokens[cs.tok_i];
}

fn expectToken(p: *Parser, tag: Token.Tag) Error!Token {
    const cs = p.currentState();
    if (cs.tokens[cs.tok_i].tag != tag) {
        const token = cs.tokens[cs.tok_i];
        try p.cc.addError(.{
            .message = .{
                .unexpected_token = .{ .expected = tag, .found = token.tag },
            },
            .context = .{ .token = token },
        });
        return error.ParseError;
    }
    return p.nextToken();
}

fn addError(p: *Parser, error_msg: Compile.Error.Message) Error {
    // Only store errors for root file, other modules we currently don't preserve the source context and
    // the locations will be wrong. We report a high-level error.ImportError on the case of failed imports.
    if (p.isRootState()) {
        const cs = p.currentState();
        const token = cs.tokens[cs.tok_i - 1];
        try p.cc.addError(.{ .message = error_msg, .context = .{ .token = token } });
    }
    return error.ParseError;
}

fn trimSuffix(lit: []const u8, suffixes: []const []const u8) []const u8 {
    for (suffixes) |suffix| {
        if (lit.len > suffix.len and std.ascii.eqlIgnoreCase(lit[lit.len - suffix.len ..], suffix)) {
            return lit[0 .. lit.len - suffix.len];
        }
    }
    return lit;
}

const float_suffixes: []const []const u8 = &.{
    "dl", "dd", "df", "l", "f",
};

const int_suffixes: []const []const u8 = &.{
    "ll", "lu", "l", "u",
};

fn classifyNumberLiteral(p: *Parser, tok: Token) !Node.Number {
    std.debug.assert(tok.tag == .number_literal);
    const lit = p.getTokenString(tok);

    if (std.mem.indexOfScalar(u8, lit, '.')) |_| {
        const trimmed_lit = trimSuffix(lit, float_suffixes);
        return .{ .float = try std.fmt.parseFloat(f64, trimmed_lit) };
    } else if (std.mem.startsWith(u8, lit, "0x")) {
        const trimmed_lit = trimSuffix(lit, int_suffixes);
        return .{ .hex_int = try std.fmt.parseInt(i64, trimmed_lit[2..], 16) };
    } else {
        const trimmed_lit = trimSuffix(lit, int_suffixes);
        return .{ .int = try std.fmt.parseInt(i64, trimmed_lit, 0) };
    }
}

fn expectStringLiteral(p: *Parser) Error![]const u8 {
    const tok = try p.expectToken(.string_literal);
    const lit = p.getTokenString(tok);
    std.debug.assert(lit.len >= 2 and lit[0] == '"' and lit[lit.len - 1] == '"');
    return lit[1 .. lit.len - 1]; // omit '"'
}

fn getTokenString(p: *Parser, tok: Token) []const u8 {
    const cs = p.state.getLast();
    return cs.source[tok.loc.start..tok.loc.end];
}

fn currentState(p: *Parser) *State {
    std.debug.assert(p.state.items.len > 0);
    return &p.state.items[p.state.items.len - 1];
}

fn isRootState(p: *Parser) bool {
    return p.state.items.len == 1;
}

fn eatToken(p: *Parser, tag: Token.Tag) ?Token {
    const cs = p.currentState();
    return if (tag == cs.tokens[cs.tok_i].tag) p.nextToken() else null;
}

fn intern(p: *Parser, tok: Token) InternPool.Ref {
    const s = p.getTokenString(tok);
    return p.intern_pool.intern(s);
}

fn eatTokenOneOf(p: *Parser, tags: []const Token.Tag) ?Token {
    const cs = p.currentState();
    for (tags) |tag| {
        if (cs.tokens[cs.tok_i].tag == tag) {
            return p.nextToken();
        }
    }
    return null;
}

// Eat an identifier iff it matches an expected string.
fn eatIdentifier(p: *Parser, expected_ident: []const u8) bool {
    const ident = p.peekToken();
    if (ident.tag != .identifier) return false;
    const ident_val = p.getTokenString(ident);
    const match = std.mem.eql(u8, ident_val, expected_ident);
    if (match) _ = p.expectToken(.identifier) catch unreachable;
    return match;
}

fn addNode(p: *Parser, n: Node) Error!Node.Ref {
    const i: u32 = @intCast(p.nodes.items.len);
    try p.nodes.append(n);
    const ni: Node.Ref = @enumFromInt(i);
    try p.handleNode(n, ni);
    return ni;
}

fn addData(p: *Parser, n: []const Node.Ref) !Node.Range {
    const start: u32 = @intCast(p.data.items.len);
    try p.data.appendSlice(n);
    return .{ .start = start, .end = @intCast(start + n.len) };
}

// Given discontiguous ranges, merge them into a contiguous one. Not memory efficient, can improve later.
fn addDataRanges(p: *Parser, ranges: []const Node.Range) !Node.Range {
    const start: u32 = @intCast(p.data.items.len);

    var total_len: u32 = 0;
    for (ranges) |range| {
        total_len += range.end - range.start;
    }

    // Resize upfront so we guarantee the slice we are appending is not invalidated.
    try p.data.ensureTotalCapacity(start + total_len);

    for (ranges) |range| {
        try p.data.appendSlice(p.data.items[range.start..range.end]);
    }
    return .{ .start = start, .end = start + total_len };
}

// Loosely of the form `550e8400-e29b-41d4-a716-446655440000`.
fn parseUUID(uuid: []const u8) !u128 {
    var v: u128 = 0;
    var i: usize = 0;
    for (uuid) |c| {
        if (c == '-') continue;
        v <<= 4;
        v +%= try std.fmt.charToDigit(c, 16);
        i += 1;
    }
    if (i != 32) return error.InvalidUUID;
    return v;
}

test parseUUID {
    const expected: u128 = 0x550e8400e29b41d4a716446655440000;
    const actual = try parseUUID("550e8400-e29b-41d4-a716-446655440000");
    try std.testing.expectEqual(expected, actual);
}

// Turn a parse function into a failable, returning an optional.
fn optional(p: *Parser, func: anytype, comptime T: type) !?T {
    const cs = p.currentState();
    const error_count = p.cc.errors.items.len;
    const mark = cs.tok_i;
    const node: ?T = func(p) catch |err| switch (err) {
        error.ParseError => null,
        else => return err,
    };
    if (node == null) {
        cs.tok_i = mark;
        std.debug.assert(error_count <= p.cc.errors.items.len);
        if (p.options.show_failed_optional_parses) {
            for (error_count..p.cc.errors.items.len) |i| p.cc.errors.items[i].print("<debug>", cs.source);
        }
        p.cc.errors.items.len = error_count;
    }
    return node;
}

fn dumpToken(p: *Parser, loc: std.builtin.SourceLocation) void {
    const cs = p.currentState();
    std.debug.print("[{}:{s}] token={s}, token-index={}, state-index={}\n", .{
        loc.line, loc.fn_name, @tagName(cs.tokens[cs.tok_i].tag), cs.tok_i, p.state.items.len,
    });
}
