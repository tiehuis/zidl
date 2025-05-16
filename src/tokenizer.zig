const std = @import("std");

const log = std.log.scoped(.tokenizer);

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,

        // NOTE: These are slightly off since we operate on preprocessed output.
        // Should handle #line directives.
        pub fn getLineColumn(loc: Loc, source: []const u8) LineColumn {
            const line = std.mem.count(u8, source[0..loc.end], "\n") + 1;
            const col = loc.end - if (line > 1)
                std.mem.lastIndexOfScalar(u8, source[0..loc.end], '\n').?
            else
                0;

            return .{ .line = line, .col = col };
        }

        pub const LineColumn = struct {
            line: usize,
            col: usize,
        };
    };

    // https://learn.microsoft.com/en-us/windows/win32/midl/reserved-keywords
    pub const midl2_keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "FALSE", .keyword_FALSE },
        .{ "ISO_LATIN_1", .keyword_ISO_LATIN_1 },
        .{ "ISO_MULTI_LINGUAL", .keyword_ISO_MULTI_LINGUAL },
        .{ "ISO_UCS", .keyword_ISO_UCS },
        .{ "NULL", .keyword_NULL },
        .{ "SAFEARRAY", .keyword_SAFEARRAY },
        .{ "TRUE", .keyword_TRUE },
        .{ "__alignof", .keyword___alignof },
        .{ "__asm", .keyword___asm },
        .{ "__cdecl", .keyword___cdecl },
        .{ "__declspec", .keyword___declspec },
        .{ "__export", .keyword___export },
        .{ "__external_symbol", .keyword___external_symbol },
        .{ "__far", .keyword___far },
        .{ "__fastcall", .keyword___fastcall },
        .{ "__float128", .keyword___float128 },
        .{ "__float80", .keyword___float80 },
        .{ "__fortran", .keyword___fortran },
        .{ "__huge", .keyword___huge },
        .{ "__inline", .keyword___inline },
        .{ "__int128", .keyword___int128 },
        .{ "__int32", .keyword___int32 },
        .{ "__int3264", .keyword___int3264 },
        .{ "__int64", .keyword___int64 },
        .{ "__interface_interception_info", .keyword___interface_interception_info },
        .{ "__loadds", .keyword___loadds },
        .{ "__method_property", .keyword___method_property },
        .{ "__near", .keyword___near },
        .{ "__pascal", .keyword___pascal },
        .{ "__ptr32", .keyword___ptr32 },
        .{ "__ptr64", .keyword___ptr64 },
        .{ "__saveregs", .keyword___saveregs },
        .{ "__segment", .keyword___segment },
        .{ "__self", .keyword___self },
        .{ "__stdcall", .keyword___stdcall },
        .{ "__unaligned", .keyword___unaligned },
        .{ "__w64", .keyword___w64 },
        .{ "__winrt_type_serialization_info", .keyword___winrt_type_serialization_info },
        .{ "_asm", .keyword__asm },
        .{ "_cdecl", .keyword__cdecl },
        .{ "_declspec", .keyword__declspec },
        .{ "_export", .keyword__export },
        .{ "_far", .keyword__far },
        .{ "_fastcall", .keyword__fastcall },
        .{ "_fortran", .keyword__fortran },
        .{ "_huge", .keyword__huge },
        .{ "_inline", .keyword__inline },
        .{ "_loadds", .keyword__loadds },
        .{ "_near", .keyword__near },
        .{ "_pascal", .keyword__pascal },
        .{ "_saveregs", .keyword__saveregs },
        .{ "_segment", .keyword__segment },
        .{ "_self", .keyword__self },
        .{ "_stdcall", .keyword__stdcall },
        .{ "auto", .keyword_auto },
        .{ "boolean", .keyword_boolean },
        .{ "byte", .keyword_byte },
        .{ "case", .keyword_case },
        .{ "cdecl", .keyword_cdecl },
        .{ "char", .keyword_char },
        .{ "coclass", .keyword_coclass },
        .{ "const", .keyword_const },
        .{ "contract", .keyword_contract },
        .{ "cpp_quote", .keyword_cpp_quote },
        .{ "declare_guid", .keyword_declare_guid },
        .{ "default", .keyword_default },
        .{ "dispinterface", .keyword_dispinterface },
        .{ "double", .keyword_double },
        .{ "enum", .keyword_enum },
        .{ "extern", .keyword_extern },
        .{ "far", .keyword_far },
        .{ "float", .keyword_float },
        .{ "handle_t", .keyword_handle_t },
        .{ "hyper", .keyword_hyper },
        .{ "import", .keyword_import },
        .{ "importlib", .keyword_importlib },
        .{ "include", .keyword_include },
        .{ "inline", .keyword_inline },
        .{ "int", .keyword_int },
        .{ "interface", .keyword_interface },
        .{ "library", .keyword_library },
        .{ "long", .keyword_long },
        .{ "methods", .keyword_methods },
        .{ "midl_pragma", .keyword_midl_pragma },
        .{ "module", .keyword_module },
        .{ "near", .keyword_near },
        .{ "pascal", .keyword_pascal },
        .{ "pipe", .keyword_pipe },
        .{ "private_char_16", .keyword_private_char_16 },
        .{ "private_char_8", .keyword_private_char_8 },
        .{ "properties", .keyword_properties },
        .{ "register", .keyword_register },
        .{ "short", .keyword_short },
        .{ "signed", .keyword_signed },
        .{ "sizeof", .keyword_sizeof },
        .{ "small", .keyword_small },
        .{ "static", .keyword_static },
        .{ "stdcall", .keyword_stdcall },
        .{ "struct", .keyword_struct },
        .{ "switch", .keyword_switch },
        .{ "typedef", .keyword_typedef },
        .{ "union", .keyword_union },
        .{ "unsigned", .keyword_unsigned },
        .{ "void", .keyword_void },
        .{ "volatile", .keyword_volatile },
    });

    // https://learn.microsoft.com/en-us/uwp/midl-3/reserved-keywords
    pub const midl3_keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "FALSE", .keyword_FALSE },
        .{ "ISO_LATIN_1", .keyword_ISO_LATIN_1 },
        .{ "ISO_MULTI_LINGUAL", .keyword_ISO_MULTI_LINGUAL },
        .{ "ISO_UCS", .keyword_ISO_UCS },
        .{ "NULL", .keyword_NULL },
        .{ "SAFEARRAY", .keyword_SAFEARRAY },
        .{ "TRUE", .keyword_TRUE },
        .{ "__alignof", .keyword___alignof },
        .{ "__asm", .keyword___asm },
        .{ "__cdecl", .keyword___cdecl },
        .{ "__declspec", .keyword___declspec },
        .{ "__export", .keyword___export },
        .{ "__external_symbol", .keyword___external_symbol },
        .{ "__far", .keyword___far },
        .{ "__fastcall", .keyword___fastcall },
        .{ "__float128", .keyword___float128 },
        .{ "__float80", .keyword___float80 },
        .{ "__fortran", .keyword___fortran },
        .{ "__huge", .keyword___huge },
        .{ "__inline", .keyword___inline },
        .{ "__int128", .keyword___int128 },
        .{ "__int32", .keyword___int32 },
        .{ "__int3264", .keyword___int3264 },
        .{ "__int64", .keyword___int64 },
        .{ "__interface_interception_info", .keyword___interface_interception_info },
        .{ "__loadds", .keyword___loadds },
        .{ "__method_property", .keyword___method_property },
        .{ "__near", .keyword___near },
        .{ "__pascal", .keyword___pascal },
        .{ "__ptr32", .keyword___ptr32 },
        .{ "__ptr64", .keyword___ptr64 },
        .{ "__saveregs", .keyword___saveregs },
        .{ "__segment", .keyword___segment },
        .{ "__self", .keyword___self },
        .{ "__stdcall", .keyword___stdcall },
        .{ "__unaligned", .keyword___unaligned },
        .{ "__w64", .keyword___w64 },
        .{ "__winrt_type_serialization_info", .keyword___winrt_type_serialization_info },
        .{ "_asm", .keyword__asm },
        .{ "_cdecl", .keyword__cdecl },
        .{ "_declspec", .keyword__declspec },
        .{ "_export", .keyword__export },
        .{ "_far", .keyword__far },
        .{ "_fastcall", .keyword__fastcall },
        .{ "_fortran", .keyword__fortran },
        .{ "_huge", .keyword__huge },
        .{ "_inline", .keyword__inline },
        .{ "_loadds", .keyword__loadds },
        .{ "_near", .keyword__near },
        .{ "_pascal", .keyword__pascal },
        .{ "_saveregs", .keyword__saveregs },
        .{ "_segment", .keyword__segment },
        .{ "_self", .keyword__self },
        .{ "_stdcall", .keyword__stdcall },
        .{ "apicontract", .keyword_apicontract },
        .{ "attribute", .keyword_attribute },
        .{ "attributename", .keyword_attributename },
        .{ "attributeusage", .keyword_attributeusage },
        .{ "auto", .keyword_auto },
        .{ "boolean", .keyword_boolean },
        .{ "byte", .keyword_byte },
        .{ "case", .keyword_case },
        .{ "cdecl", .keyword_cdecl },
        .{ "char", .keyword_char },
        .{ "coclass", .keyword_coclass },
        .{ "composable", .keyword_composable },
        .{ "const", .keyword_const },
        .{ "constructor_name", .keyword_constructor_name },
        .{ "contract", .keyword_contract },
        .{ "contractversion", .keyword_contractversion },
        .{ "cpp_quote", .keyword_cpp_quote },
        .{ "declare", .keyword_declare },
        .{ "declare_guid", .keyword_declare_guid },
        .{ "default", .keyword_default },
        .{ "delegate", .keyword_delegate },
        .{ "deprecate", .keyword_deprecate },
        .{ "deprecated", .keyword_deprecated },
        .{ "dispinterface", .keyword_dispinterface },
        .{ "double", .keyword_double },
        .{ "enum", .keyword_enum },
        .{ "event", .keyword_event },
        .{ "eventadd", .keyword_eventadd },
        .{ "eventremove", .keyword_eventremove },
        .{ "exclusiveto", .keyword_exclusiveto },
        .{ "experimental", .keyword_experimental },
        .{ "extern", .keyword_extern },
        .{ "far", .keyword_far },
        .{ "feature", .keyword_feature },
        .{ "feature_name", .keyword_feature_name },
        .{ "float", .keyword_float },
        .{ "from_contract", .keyword_from_contract },
        .{ "get", .keyword_get },
        .{ "handle_t", .keyword_handle_t },
        .{ "hyper", .keyword_hyper },
        .{ "import", .keyword_import },
        .{ "importlib", .keyword_importlib },
        .{ "include", .keyword_include },
        .{ "inline", .keyword_inline },
        .{ "int", .keyword_int },
        .{ "interface", .keyword_interface },
        .{ "interface_name", .keyword_interface_name },
        .{ "internal", .keyword_internal },
        .{ "library", .keyword_library },
        .{ "long", .keyword_long },
        .{ "methods", .keyword_methods },
        .{ "midl_pragma", .keyword_midl_pragma },
        .{ "module", .keyword_module },
        .{ "namespace", .keyword_namespace },
        .{ "near", .keyword_near },
        .{ "overridable", .keyword_overridable },
        .{ "overridable_name", .keyword_overridable_name },
        .{ "partial", .keyword_partial },
        .{ "pascal", .keyword_pascal },
        .{ "pipe", .keyword_pipe },
        .{ "private_char_16", .keyword_private_char_16 },
        .{ "private_char_8", .keyword_private_char_8 },
        .{ "properties", .keyword_properties },
        .{ "protected", .keyword_protected },
        .{ "protected_name", .keyword_protected_name },
        .{ "register", .keyword_register },
        .{ "remote_async", .keyword_remote_async },
        .{ "remote_sync", .keyword_remote_sync },
        .{ "remove", .keyword_remove },
        .{ "requires", .keyword_requires },
        .{ "return_name", .keyword_return_name },
        .{ "runtimeclass", .keyword_runtimeclass },
        .{ "set", .keyword_set },
        .{ "short", .keyword_short },
        .{ "signed", .keyword_signed },
        .{ "sizeof", .keyword_sizeof },
        .{ "small", .keyword_small },
        .{ "static", .keyword_static },
        .{ "static_name", .keyword_static_name },
        .{ "stdcall", .keyword_stdcall },
        .{ "struct", .keyword_struct },
        .{ "switch", .keyword_switch },
        .{ "typedef", .keyword_typedef },
        .{ "type", .keyword_type },
        .{ "union", .keyword_union },
        .{ "unsealed", .keyword_unsealed },
        .{ "unsigned", .keyword_unsigned },
        .{ "void", .keyword_void },
        .{ "volatile", .keyword_volatile },
    });

    pub fn getKeyword(version: MidlVersion, bytes: []const u8) ?Tag {
        return (switch (version) {
            .midl2 => midl2_keywords,
            .midl3 => midl3_keywords,
        }).get(bytes);
    }

    pub const Tag = enum {
        invalid,
        invalid_periodasterisks,
        identifier,
        string_literal,
        char_literal,
        eof,
        builtin,
        bang,
        pipe,
        pipe_pipe,
        pipe_equal,
        equal,
        equal_equal,
        equal_angle_bracket_right,
        bang_equal,
        l_paren,
        r_paren,
        semicolon,
        percent,
        percent_equal,
        l_brace,
        r_brace,
        l_bracket,
        r_bracket,
        period,
        period_3,
        caret,
        caret_equal,
        plus,
        plus_plus,
        plus_equal,
        minus,
        minus_minus,
        minus_equal,
        minus_percent,
        asterisk,
        asterisk_equal,
        arrow,
        colon,
        slash,
        slash_equal,
        comma,
        hash,
        ampersand,
        ampersand_ampersand,
        ampersand_equal,
        angle_bracket_left,
        angle_bracket_left_equal,
        angle_bracket_angle_bracket_left,
        angle_bracket_angle_bracket_left_equal,
        angle_bracket_right,
        angle_bracket_right_equal,
        angle_bracket_angle_bracket_right,
        angle_bracket_angle_bracket_right_equal,
        tilde,
        question_mark,
        number_literal,

        keyword_FALSE,
        keyword_ISO_LATIN_1,
        keyword_ISO_MULTI_LINGUAL,
        keyword_ISO_UCS,
        keyword_NULL,
        keyword_SAFEARRAY,
        keyword_TRUE,
        keyword___alignof,
        keyword___asm,
        keyword___cdecl,
        keyword___declspec,
        keyword___export,
        keyword___external_symbol,
        keyword___far,
        keyword___fastcall,
        keyword___float128,
        keyword___float80,
        keyword___fortran,
        keyword___huge,
        keyword___inline,
        keyword___int128,
        keyword___int32,
        keyword___int3264,
        keyword___int64,
        keyword___interface_interception_info,
        keyword___loadds,
        keyword___method_property,
        keyword___near,
        keyword___pascal,
        keyword___ptr32,
        keyword___ptr64,
        keyword___saveregs,
        keyword___segment,
        keyword___self,
        keyword___stdcall,
        keyword___unaligned,
        keyword___w64,
        keyword___winrt_type_serialization_info,
        keyword__asm,
        keyword__cdecl,
        keyword__declspec,
        keyword__export,
        keyword__far,
        keyword__fastcall,
        keyword__fortran,
        keyword__huge,
        keyword__inline,
        keyword__loadds,
        keyword__near,
        keyword__pascal,
        keyword__saveregs,
        keyword__segment,
        keyword__self,
        keyword__stdcall,
        keyword_apicontract,
        keyword_attribute,
        keyword_attributename,
        keyword_attributeusage,
        keyword_auto,
        keyword_boolean,
        keyword_byte,
        keyword_case,
        keyword_cdecl,
        keyword_char,
        keyword_coclass,
        keyword_composable,
        keyword_const,
        keyword_constructor_name,
        keyword_contract,
        keyword_contractversion,
        keyword_cpp_quote,
        keyword_declare,
        keyword_declare_guid,
        keyword_default,
        keyword_delegate,
        keyword_deprecate,
        keyword_deprecated,
        keyword_dispinterface,
        keyword_double,
        keyword_enum,
        keyword_event,
        keyword_eventadd,
        keyword_eventremove,
        keyword_exclusiveto,
        keyword_experimental,
        keyword_extern,
        keyword_far,
        keyword_feature,
        keyword_feature_name,
        keyword_float,
        keyword_from_contract,
        keyword_get,
        keyword_handle_t,
        keyword_hyper,
        keyword_import,
        keyword_importlib,
        keyword_include,
        keyword_inline,
        keyword_int,
        keyword_interface,
        keyword_interface_name,
        keyword_internal,
        keyword_library,
        keyword_long,
        keyword_methods,
        keyword_midl_pragma,
        keyword_module,
        keyword_namespace,
        keyword_near,
        keyword_overridable,
        keyword_overridable_name,
        keyword_partial,
        keyword_pascal,
        keyword_pipe,
        keyword_private_char_16,
        keyword_private_char_8,
        keyword_properties,
        keyword_protected,
        keyword_protected_name,
        keyword_register,
        keyword_remote_async,
        keyword_remote_sync,
        keyword_remove,
        keyword_requires,
        keyword_return_name,
        keyword_runtimeclass,
        keyword_set,
        keyword_short,
        keyword_signed,
        keyword_sizeof,
        keyword_small,
        keyword_static,
        keyword_static_name,
        keyword_stdcall,
        keyword_struct,
        keyword_switch,
        keyword_typedef,
        keyword_type,
        keyword_union,
        keyword_unsealed,
        keyword_unsigned,
        keyword_void,
        keyword_volatile,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .invalid,
                .identifier,
                .string_literal,
                .char_literal,
                .eof,
                .builtin,
                .number_literal,
                => null,

                .invalid_periodasterisks => ".**",
                .bang => "!",
                .pipe => "|",
                .pipe_equal => "|=",
                .pipe_pipe => "||",
                .equal => "=",
                .equal_equal => "==",
                .equal_angle_bracket_right => "=>",
                .bang_equal => "!=",
                .l_paren => "(",
                .r_paren => ")",
                .semicolon => ";",
                .percent => "%",
                .percent_equal => "%=",
                .l_brace => "{",
                .r_brace => "}",
                .l_bracket => "[",
                .r_bracket => "]",
                .period => ".",
                .period_3 => "...",
                .caret => "^",
                .caret_equal => "^=",
                .plus => "+",
                .plus_plus => "++",
                .plus_equal => "+=",
                .minus => "-",
                .minus_minus => "--",
                .minus_equal => "-=",
                .minus_percent => "-%",
                .asterisk => "*",
                .asterisk_equal => "*=",
                .arrow => "->",
                .colon => ":",
                .slash => "/",
                .slash_equal => "/=",
                .comma => ",",
                .hash => "#",
                .ampersand => "&",
                .ampersand_ampersand => "&&",
                .ampersand_equal => "&=",
                .angle_bracket_left => "<",
                .angle_bracket_left_equal => "<=",
                .angle_bracket_angle_bracket_left => "<<",
                .angle_bracket_angle_bracket_left_equal => "<<=",
                .angle_bracket_right => ">",
                .angle_bracket_right_equal => ">=",
                .angle_bracket_angle_bracket_right => ">>",
                .angle_bracket_angle_bracket_right_equal => ">>=",
                .tilde => "~",
                .question_mark => "?",

                .keyword_FALSE => "FALSE",
                .keyword_ISO_LATIN_1 => "ISO_LATIN_1",
                .keyword_ISO_MULTI_LINGUAL => "ISO_MULTI_LINGUAL",
                .keyword_ISO_UCS => "ISO_UCS",
                .keyword_NULL => "NULL",
                .keyword_SAFEARRAY => "SAFEARRAY",
                .keyword_TRUE => "TRUE",
                .keyword___alignof => "__alignof",
                .keyword___asm => "__asm",
                .keyword___cdecl => "__cdecl",
                .keyword___declspec => "__declspec",
                .keyword___export => "__export",
                .keyword___external_symbol => "__external_symbol",
                .keyword___far => "__far",
                .keyword___fastcall => "__fastcall",
                .keyword___float128 => "__float128",
                .keyword___float80 => "__float80",
                .keyword___fortran => "__fortran",
                .keyword___huge => "__huge",
                .keyword___inline => "__inline",
                .keyword___int128 => "__int128",
                .keyword___int32 => "__int32",
                .keyword___int3264 => "__int3264",
                .keyword___int64 => "__int64",
                .keyword___interface_interception_info => "__interface_interception_info",
                .keyword___loadds => "__loadds",
                .keyword___method_property => "__method_property",
                .keyword___near => "__near",
                .keyword___pascal => "__pascal",
                .keyword___ptr32 => "__ptr32",
                .keyword___ptr64 => "__ptr64",
                .keyword___saveregs => "__saveregs",
                .keyword___segment => "__segment",
                .keyword___self => "__self",
                .keyword___stdcall => "__stdcall",
                .keyword___unaligned => "__unaligned",
                .keyword___w64 => "__w64",
                .keyword___winrt_type_serialization_info => "__winrt_type_serialization_info",
                .keyword__asm => "_asm",
                .keyword__cdecl => "_cdecl",
                .keyword__declspec => "_declspec",
                .keyword__export => "_export",
                .keyword__far => "_far",
                .keyword__fastcall => "_fastcall",
                .keyword__fortran => "_fortran",
                .keyword__huge => "_huge",
                .keyword__inline => "_inline",
                .keyword__loadds => "_loadds",
                .keyword__near => "_near",
                .keyword__pascal => "_pascal",
                .keyword__saveregs => "_saveregs",
                .keyword__segment => "_segment",
                .keyword__self => "_self",
                .keyword__stdcall => "_stdcall",
                .keyword_apicontract => "apicontract",
                .keyword_attribute => "attribute",
                .keyword_attributename => "attributename",
                .keyword_attributeusage => "attributeusage",
                .keyword_auto => "auto",
                .keyword_boolean => "boolean",
                .keyword_byte => "byte",
                .keyword_case => "case",
                .keyword_cdecl => "cdecl",
                .keyword_char => "char",
                .keyword_coclass => "coclass",
                .keyword_composable => "composable",
                .keyword_const => "const",
                .keyword_constructor_name => "constructor_name",
                .keyword_contract => "contract",
                .keyword_contractversion => "contractversion",
                .keyword_cpp_quote => "cpp_quote",
                .keyword_declare => "declare",
                .keyword_declare_guid => "declare_guid",
                .keyword_default => "default",
                .keyword_delegate => "delegate",
                .keyword_deprecate => "deprecate",
                .keyword_deprecated => "deprecated",
                .keyword_dispinterface => "dispinterface",
                .keyword_double => "double",
                .keyword_enum => "enum",
                .keyword_event => "event",
                .keyword_eventadd => "eventadd",
                .keyword_eventremove => "eventremove",
                .keyword_exclusiveto => "exclusiveto",
                .keyword_experimental => "experimental",
                .keyword_extern => "extern",
                .keyword_far => "far",
                .keyword_feature => "feature",
                .keyword_feature_name => "feature_name",
                .keyword_float => "float",
                .keyword_from_contract => "from_contract",
                .keyword_get => "get",
                .keyword_handle_t => "handle_t",
                .keyword_hyper => "hyper",
                .keyword_import => "import",
                .keyword_importlib => "importlib",
                .keyword_include => "include",
                .keyword_inline => "inline",
                .keyword_int => "int",
                .keyword_interface => "interface",
                .keyword_interface_name => "interface_name",
                .keyword_internal => "internal",
                .keyword_library => "library",
                .keyword_long => "long",
                .keyword_methods => "methods",
                .keyword_midl_pragma => "midl_pragma",
                .keyword_module => "module",
                .keyword_namespace => "namespace",
                .keyword_near => "near",
                .keyword_overridable => "overridable",
                .keyword_overridable_name => "overridable_name",
                .keyword_partial => "partial",
                .keyword_pascal => "pascal",
                .keyword_pipe => "pipe",
                .keyword_private_char_16 => "private_char_16",
                .keyword_private_char_8 => "private_char_8",
                .keyword_properties => "properties",
                .keyword_protected => "protected",
                .keyword_protected_name => "protected_name",
                .keyword_register => "register",
                .keyword_remote_async => "remote_async",
                .keyword_remote_sync => "remote_sync",
                .keyword_remove => "remove",
                .keyword_requires => "requires",
                .keyword_return_name => "return_name",
                .keyword_runtimeclass => "runtimeclass",
                .keyword_set => "set",
                .keyword_short => "short",
                .keyword_signed => "signed",
                .keyword_sizeof => "sizeof",
                .keyword_small => "small",
                .keyword_static => "static",
                .keyword_static_name => "static_name",
                .keyword_stdcall => "stdcall",
                .keyword_struct => "struct",
                .keyword_switch => "switch",
                .keyword_typedef => "typedef",
                .keyword_type => "type",
                .keyword_union => "union",
                .keyword_unsealed => "unsealed",
                .keyword_unsigned => "unsigned",
                .keyword_void => "void",
                .keyword_volatile => "volatile",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .invalid => "invalid token",
                .identifier => "an identifier",
                .string_literal => "a string literal",
                .char_literal => "a character literal",
                .eof => "EOF",
                .builtin => "a builtin function",
                .number_literal => "a number literal",
                else => unreachable,
            };
        }
    };
};

fn print(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
}

pub const MidlVersion = enum {
    midl2,
    midl3,
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,
    options: Options,

    pub const Options = struct {
        version: MidlVersion = .midl2,
    };

    /// For debugging purposes.
    pub fn dump(self: *const Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8, options: Options) Tokenizer {
        // Skip the UTF-8 BOM if present.
        return .{
            .buffer = buffer,
            .index = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0,
            .options = options,
        };
    }

    const State = enum {
        start,
        expect_newline,
        identifier,
        builtin,
        hash_start_of_line,
        string_literal,
        string_literal_backslash,
        char_literal,
        char_literal_backslash,
        backslash,
        equal,
        bang,
        pipe,
        minus,
        minus_percent,
        asterisk,
        slash,
        line_comment_start,
        line_comment,
        comment_start,
        comment,
        comment_end,
        int,
        int_exponent,
        int_period,
        float,
        float_exponent,
        ampersand,
        caret,
        percent,
        plus,
        angle_bracket_left,
        angle_bracket_angle_bracket_left,
        angle_bracket_right,
        angle_bracket_angle_bracket_right,
        period,
        period_2,
        saw_at_sign,
        invalid,
    };

    /// After this returns invalid, it will reset on the next newline, returning tokens starting from there.
    /// An eof token will always be returned at the end.
    pub fn next(self: *Tokenizer) Token {
        var result: Token = .{
            .tag = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        state: switch (State.start) {
            .start => switch (self.buffer[self.index]) {
                0 => {
                    if (self.index == self.buffer.len) {
                        return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    } else {
                        continue :state .invalid;
                    }
                },
                ' ', '\n', '\t', '\r' => {
                    self.index += 1;
                    result.loc.start = self.index;
                    continue :state .start;
                },
                '"' => {
                    result.tag = .string_literal;
                    continue :state .string_literal;
                },
                '\'' => {
                    result.tag = .char_literal;
                    continue :state .char_literal;
                },
                'a'...'z', 'A'...'Z', '_' => {
                    result.tag = .identifier;
                    continue :state .identifier;
                },
                '(' => {
                    result.tag = .l_paren;
                    self.index += 1;
                },
                ')' => {
                    result.tag = .r_paren;
                    self.index += 1;
                },
                '{' => {
                    result.tag = .l_brace;
                    self.index += 1;
                },
                '}' => {
                    result.tag = .r_brace;
                    self.index += 1;
                },
                '[' => {
                    result.tag = .l_bracket;
                    self.index += 1;
                },
                ']' => {
                    result.tag = .r_bracket;
                    self.index += 1;
                },
                '<' => {
                    result.tag = .angle_bracket_left;
                    continue :state .angle_bracket_left;
                },
                '>' => {
                    result.tag = .angle_bracket_right;
                    continue :state .angle_bracket_right;
                },
                ';' => {
                    result.tag = .semicolon;
                    self.index += 1;
                },
                ':' => {
                    result.tag = .colon;
                    self.index += 1;
                },
                ',' => {
                    result.tag = .comma;
                    self.index += 1;
                },
                '~' => {
                    result.tag = .tilde;
                    self.index += 1;
                },
                '&' => {
                    result.tag = .ampersand;
                    self.index += 1;
                },
                '#' => {
                    result.tag = .hash;
                    if (self.index == 0 or self.buffer[self.index - 1] == '\n') {
                        result.tag = .invalid;
                        continue :state .hash_start_of_line;
                    } else {
                        self.index += 1;
                    }
                },
                '.' => {
                    result.tag = .period;
                    self.index += 1;
                },
                '-' => {
                    result.tag = .minus;
                    self.index += 1;
                },
                '|' => {
                    result.tag = .pipe;
                    self.index += 1;
                },
                '+' => {
                    result.tag = .plus;
                    self.index += 1;
                },
                '*' => {
                    result.tag = .asterisk;
                    self.index += 1;
                },
                '=' => {
                    result.tag = .equal;
                    self.index += 1;
                },
                '?' => {
                    result.tag = .question_mark;
                    self.index += 1;
                },
                '/' => continue :state .slash,
                '0'...'9' => {
                    result.tag = .number_literal;
                    self.index += 1;
                    continue :state .int;
                },
                else => continue :state .invalid,
            },

            // Hack: Ignore preprocessor '#...' lines (e.g. #pragma). Most of these are
            // handled by the preprocessing step. We do need to handle #pragma warning in
            // the parser, but want to omit other things (e.g. region).
            .hash_start_of_line => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    },
                    '\n' => {
                        log.debug("skip line: '{s}'", .{self.buffer[result.loc.start..self.index]});
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    else => continue :state .hash_start_of_line,
                }
            },

            .expect_newline => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index == self.buffer.len) {
                            result.tag = .invalid;
                        } else {
                            continue :state .invalid;
                        }
                    },
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    else => continue :state .invalid,
                }
            },
            .invalid => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else {
                        continue :state .invalid;
                    },
                    '\n' => result.tag = .invalid,
                    else => continue :state .invalid,
                }
            },
            .identifier => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .identifier,
                    else => {
                        const ident = self.buffer[result.loc.start..self.index];
                        if (Token.getKeyword(self.options.version, ident)) |tag| {
                            result.tag = tag;
                        }
                    },
                }
            },
            .string_literal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    '\\' => continue :state .string_literal_backslash,
                    '"' => self.index += 1,
                    0x01...0x08, 0x0b...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .string_literal,
                }
            },

            .string_literal_backslash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0, '\n' => result.tag = .invalid,
                    else => continue :state .string_literal,
                }
            },
            .char_literal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    '\\' => continue :state .char_literal_backslash,
                    '\'' => self.index += 1,
                    0x01...0x09, 0x0b...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .char_literal,
                }
            },

            .char_literal_backslash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    0x01...0x09, 0x0b...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .char_literal,
                }
            },
            .slash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '/' => continue :state .line_comment_start,
                    '*' => continue :state .comment_start,
                    '=' => {
                        result.tag = .slash_equal;
                        self.index += 1;
                    },
                    else => result.tag = .slash,
                }
            },
            .angle_bracket_left => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '<' => {
                        result.tag = .angle_bracket_angle_bracket_left;
                        self.index += 1;
                    },
                    else => result.tag = .angle_bracket_left,
                }
            },
            .angle_bracket_right => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '>' => {
                        result.tag = .angle_bracket_right;
                        self.index += 1;
                    },
                    else => result.tag = .angle_bracket_right,
                }
            },
            .comment_start => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    },
                    0x01...0x09, 0x0b...0x0c, 0x0e...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    '*' => continue :state .comment_end,
                    else => continue :state .comment,
                }
            },
            .comment => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    },
                    0x01...0x09, 0x0b...0x0c, 0x0e...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    '*' => continue :state .comment_end,
                    else => continue :state .comment,
                }
            },
            .comment_end => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '/' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    else => continue :state .comment,
                }
            },
            .line_comment_start => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    },
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    '\r' => continue :state .expect_newline,
                    0x01...0x09, 0x0b...0x0c, 0x0e...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .line_comment,
                }
            },
            .line_comment => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    },
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    '\r' => continue :state .expect_newline,
                    0x01...0x09, 0x0b...0x0c, 0x0e...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .line_comment,
                }
            },
            .int => switch (self.buffer[self.index]) {
                '.' => continue :state .int_period,
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                    self.index += 1;
                    continue :state .int;
                },
                'e', 'E', 'p', 'P' => {
                    continue :state .int_exponent;
                },
                else => {},
            },
            .int_exponent => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '-', '+' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    else => continue :state .int,
                }
            },
            .int_period => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    'e', 'E', 'p', 'P' => {
                        continue :state .float_exponent;
                    },
                    else => self.index -= 1,
                }
            },
            .float => switch (self.buffer[self.index]) {
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                    self.index += 1;
                    continue :state .float;
                },
                'e', 'E', 'p', 'P' => {
                    continue :state .float_exponent;
                },
                else => {},
            },
            .float_exponent => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '-', '+' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    else => continue :state .float,
                }
            },

            else => continue :state .invalid,
        }

        result.loc.end = self.index;
        return result;
    }
};
