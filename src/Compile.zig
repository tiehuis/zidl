const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Token = tokenizer.Token;
const Tokenizer = tokenizer.Tokenizer;
const Parser = @import("Parser.zig");
const CodeGen = @import("CodeGen.zig");
const Node = @import("node.zig").Node;
const InternPool = @import("InternPool.zig");

const node_print = @import("node_print.zig");
const Compile = @This();

const aro = @import("aro");

const log = std.log.scoped(.compile);

allocator: std.mem.Allocator,
errors: std.ArrayListUnmanaged(Error),
output: Output,
options: Options,

const Output = union(enum) {
    file: std.fs.File,
    buffer: *std.ArrayList(u8),
    ignore,
};

pub fn init(allocator: std.mem.Allocator, output: Output, options: Options) !Compile {
    return .{
        .allocator = allocator,
        .errors = try .initCapacity(allocator, 0),
        .output = output,
        .options = options,
    };
}

pub fn deinit(cc: *Compile) void {
    cc.errors.deinit(cc.allocator);
}

pub fn addError(cc: *Compile, e: Error) !void {
    try cc.errors.append(cc.allocator, e);
}

pub fn print(cc: *Compile, comptime fmt: []const u8, args: anytype) !void {
    switch (cc.output) {
        .ignore => {},
        .file => |file| try file.writer().print(fmt, args),
        .buffer => |buf| try buf.writer().print(fmt, args),
    }
}

pub fn compile(cc: *Compile, filepath: []const u8, idl_content: [:0]const u8) !Node.Ref {
    var token_list = std.ArrayList(Token).init(cc.allocator);
    var t = Tokenizer.init(idl_content, .{ .version = cc.options.midl_version });
    var i: usize = 0;
    while (true) {
        const tok = t.next();
        try token_list.append(tok);
        if (cc.options.show_tokens) {
            std.debug.print("{}: ", .{i});
            t.dump(&tok);
        }
        i += 1;
        if (tok.tag == .eof) break;
    }
    log.debug("created {} tokens", .{i});

    var parser = try Parser.init(cc, .{
        .skip_imports = cc.options.skip_imports,
        .show_failed_optional_parses = cc.options.show_failed_optional_parses,
    });
    const root_node_index = try parser.parse(idl_content, token_list.items);

    log.debug("created {} nodes", .{parser.nodes.items.len});
    log.debug("created {} data nodes", .{parser.data.items.len});
    log.debug("created {} intern_pool entries", .{parser.intern_pool.count});

    if (cc.options.show_ast) {
        node_print.dumpNode(&parser, root_node_index);
    }

    if (cc.options.codegen) {
        var cg = CodeGen.init(cc, &parser, .{
            .source_filepath = filepath,
        });
        try cg.gen();
    }

    return root_node_index;
}

pub fn preprocess(
    cc: *Compile,
    filepath: []const u8,
    source_text: [:0]const u8,
) ![:0]const u8 {
    var buf = std.ArrayList(u8).init(cc.allocator);
    defer buf.deinit();

    var diagnostics: aro.Diagnostics = .{ .output = .{
        .to_file = .{
            .config = .no_color,
            .file = std.io.getStdErr(),
        },
    } };
    var comp = aro.Compilation.init(cc.allocator, &diagnostics, std.fs.cwd());
    defer comp.deinit();

    for (cc.options.include_paths) |include_path| {
        try comp.addSystemIncludeDir(include_path);
    }

    const user_macros = try comp.addSourceFromBuffer("<command line>", cc.options.user_macros);
    const builtin = try comp.generateBuiltinMacrosFromPath(.no_system_defines, filepath);

    var pp = try aro.Preprocessor.initDefault(&comp);
    defer pp.deinit();

    try pp.diagnostics.set("macro-redefined", .off);

    pp.preserve_whitespace = true;
    std.debug.assert(pp.linemarkers == .none);
    const idl_content = try comp.addSourceFromBuffer(filepath, source_text);
    try pp.preprocessSources(&.{ idl_content, builtin, user_macros });
    try pp.prettyPrintTokens(buf.writer(), .result_only);
    return cc.allocator.dupeZ(u8, buf.items);
}

pub const Options = struct {
    show_ast: bool = false,
    show_tokens: bool = false,
    codegen: bool = true,
    show_failed_optional_parses: bool = false,

    midl_version: tokenizer.MidlVersion = .midl2,
    skip_imports: bool = false,
    include_paths: []const []const u8 = &.{},
    user_macros: []const u8 = "",
};

pub const Error = struct {
    message: Message,
    context: Context,

    pub const Context = struct {
        token: Token,

        pub fn print(c: Context, source: []const u8) void {
            var s: usize = c.token.loc.start;
            while (s > 0 and source[s - 1] != '\n') s -= 1;
            var e: usize = c.token.loc.end;
            while (e < source.len and source[e] != '\n') e += 1;
            const token_line = source[s..e];
            std.debug.print("{s}\n", .{token_line});

            const span = c.token.loc.end - c.token.loc.start;
            std.debug.assert(span >= 1);
            const offset = c.token.loc.start - s;
            for (0..offset) |_| std.debug.print(" ", .{});
            std.debug.print("^", .{});
            for (0..span - 1) |_| std.debug.print("~", .{});
            std.debug.print("\n", .{});
        }

        pub fn printPrefix(c: Context, filename: []const u8, source: []const u8) void {
            const pos = c.token.loc.getLineColumn(source);
            std.debug.print("{s}:{}:{}:", .{ filename, pos.line, pos.col });
        }
    };

    pub const Message = union(enum) {
        todo: []const u8,
        expected: []const u8,
        generic: []const u8,
        unexpected_token: struct { expected: Token.Tag, found: Token.Tag },
        attribute_not_implemented: []const u8,
        invalid_attribute: []const u8,
        invalid_pointer_type: []const u8,
        invalid_marshal_type: []const u8,
        invalid_uuid: []const u8,
        invalid_attributes_location,
        invalid_pragma_warning_id: []const u8,
        invalid_version: []const u8,
        import_file_not_found: []const u8,
        import_read_file: []const u8,
        import_preprocess: []const u8,

        pub fn print(m: Message) void {
            std.debug.print(" error: ", .{});
            switch (m) {
                .unexpected_token => |d| std.debug.print(
                    "expected token {s} but found {s}\n",
                    .{ @tagName(d.expected), @tagName(d.found) },
                ),
                .attribute_not_implemented,
                .invalid_attribute,
                .invalid_pointer_type,
                .invalid_marshal_type,
                .invalid_uuid,
                .invalid_pragma_warning_id,
                .invalid_version,
                .import_file_not_found,
                .import_read_file,
                .import_preprocess,
                => |s| std.debug.print(
                    "{s}: '{s}'\n",
                    .{ m.message(), s },
                ),
                .invalid_attributes_location,
                => std.debug.print("{s}\n", .{m.message()}),
                .todo,
                .expected,
                => |s| std.debug.print("{s}: {s}\n", .{ m.message(), s }),
                .generic,
                => |s| std.debug.print("{s}\n", .{s}),
            }
        }

        pub fn message(m: Message) []const u8 {
            return switch (m) {
                .todo => "todo",
                .generic => "",
                .expected => "expected",
                .unexpected_token => "unexpected token",
                .attribute_not_implemented => "attribute not implemented",
                .invalid_attribute => "invalid attribute",
                .invalid_pointer_type => "invalid pointer type",
                .invalid_marshal_type => "invalid marsha type",
                .invalid_uuid => "invalid uuid",
                .invalid_attributes_location => "invalid attributes location",
                .invalid_pragma_warning_id => "invalid pragma warning id",
                .invalid_version => "invalid version",
                .import_file_not_found => "import file not found",
                .import_read_file => "import read file failed",
                .import_preprocess => "import preprocess failed",
            };
        }
    };

    pub fn print(e: Error, filename: []const u8, source: []const u8) void {
        e.context.printPrefix(filename, source);
        e.message.print();
        e.context.print(source);
    }
};
