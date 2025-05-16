const std = @import("std");
const InternPool = @import("InternPool.zig");

pub const Node = union(enum) {
    comptime {
        // TODO: Reduce the size by changing the layout.
        std.debug.assert(@sizeOf(Node) == 64);
    }

    pub const Ref = enum(u32) {
        _,

        pub fn toInt(this: Ref) u32 {
            return @intFromEnum(this);
        }
    };

    // Reference to an external Node.Ref table (Parser.data).
    pub const Range = struct {
        start: u32,
        end: u32,

        pub fn empty(r: Range) bool {
            std.debug.assert(r.end >= r.start);
            return r.end - r.start == 0;
        }
    };

    array_def: Array,
    arg_list: ArgList,
    abstract_declarator: Declarator,
    abstract_direct_decl: DirectDeclarator,
    any_declarator: Declarator,
    api_contract_def: ApiContractDef,
    api_contract_ref: Node.Ref,
    attribute: Attribute,
    attributes: Node.Range,
    base_type: BaseType.Tag,
    cast: Cast,
    call_conv: CallConv,
    class_int: ClassInt,
    coclass_def: CoClassDef,
    coclass_ref: Node.Ref,
    contract_req: ContractReq,
    cpp_quote: InternPool.Ref,
    decl_spec_type: DeclSpecType, // TODO: Make more distinct from decl_spec
    decl_spec: DeclSpec,
    decl: Decl,
    declarator: Declarator,
    declare_block: Node.Range,
    declare_statement: DeclareStatement,
    direct_decl: DirectDeclarator,
    disp_interface_def: DispInterfaceDef,
    disp_interface_methods: Node.Range,
    disp_interface_props: Node.Range,
    disp_interface_ref: Node.Ref,
    enum_def: EnumDef,
    enum_field: EnumField,
    enum_ref: InternPool.Ref,
    expr_binary: ExprBinary,
    expr_const_int: Node.Ref,
    expr_const: Node.Ref,
    expr_ternary: ExprTernary,
    expr_unary: ExprUnary,
    expr: ExprPrimary,
    func_def: FuncDef,
    func_param: FuncParam,
    import: Import,
    importlib: InternPool.Ref,
    init_declarator: InitDeclarator,
    int_version: IntVersion,
    interface_ref: Node.Ref,
    interface: InterfaceDef,
    known_type: InternPool.Ref,
    library_def: LibraryDef,
    module_def: ModuleDef,
    module_ref: Node.Ref,
    namespace: Namespace,
    parameterized_type_arg: ParameterizedTypeArg,
    parameterized_type: ParameterizedType,
    pragma_warning: PragmaWarning,
    pragma: InternPool.Ref,
    qualified_type: QualifiedType,
    root: Node.Range,
    runtime_class_def: RuntimeClassDef,
    runtime_class_ref: Node.Ref,
    safe_array: Node.Ref,
    sizeof: SizeOf,
    static_attr: StaticAttr,
    struct_decl: StructDecl,
    struct_def: StructDef,
    struct_field: StructField,
    struct_ref: InternPool.Ref,
    type_qual: TypeQual,
    type: InternPool.Ref,
    typedef: Typedef,
    union_def: UnionDef,
    union_field: UnionField,
    union_sfield: UnionSField,
    union_switch_case: UnionSwitchCase,
    union_switch: UnionSwitch,
    unqualified_decl_spec_type: DeclSpecType, // TODO: Make more distinct from decl_spec
    version: Version,

    pub const Array = union(enum) {
        empty,
        asterisk,
        expr: Node.Ref,
    };

    pub const ArgList = struct {
        args: Node.Range,
        is_varargs: bool,
    };

    pub const SizeOf = struct {
        unqual_decl_spec: Node.Ref,
        abstract_decl: ?Node.Ref,
    };

    pub const Cast = struct {
        unqual_decl_spec: Node.Ref,
        declarator: ?Node.Ref,
        expr: Node.Ref,
    };

    pub const InitDeclarator = struct {
        declarator: Node.Ref,
        expr: ?Node.Ref,
    };

    pub const Import = struct {
        subject: InternPool.Ref,
        statements: Node.Range,
        root: ?Node.Ref,
    };

    pub const LibraryDef = struct {
        name: InternPool.Ref,
        attributes: ?Node.Range,
        import_statements: Node.Range,
    };

    pub const DeclareStatement = struct {
        qual_type: Node.Ref,
        param_type_args: Node.Range,
    };

    pub const ImportLib = struct {
        subject: InternPool.Ref,
        statements: Node.Range,
    };

    pub const ParameterizedType = struct {
        qual_type: Node.Ref,
        type_args: Node.Range,
    };

    pub const ParameterizedTypeArg = struct {
        is_pointer: bool,
        type: Node.Ref,
    };

    pub const QualifiedType = struct {
        namespace: Node.Range,
        typename: Node.Ref,
    };

    pub const ClassInt = struct {
        attributes: ?Node.Range,
        type: Node.Ref,
    };

    pub const ApiContractDef = struct {
        name: Node.Ref,
        attributes: ?Node.Range,
    };

    pub const ModuleDef = struct {
        name: Node.Ref,
        attributes: ?Node.Range,
        statements: Node.Range,
    };

    pub const CoClassDef = struct {
        name: Node.Ref,
        attributes: ?Node.Range,
        ints: Node.Range,
    };

    pub const DispInterfaceDef = struct {
        name: Node.Ref,
        attributes: ?Node.Range,
        data: Data,

        pub const Data = union(enum) {
            props_methods: struct {
                props: Node.Range,
                methods: Node.Range,
            },
            interface: Node.Ref,
        };
    };

    pub const RuntimeClassDef = struct {
        name: Node.Ref,
        attributes: ?Node.Range,
        parents: ?Node.Range,
        ints: Node.Range,
    };

    pub const Namespace = struct {
        qualified_name: Node.Range,
        statements: Node.Range,
    };

    pub const ContractReq = struct {
        decl_spec: Node.Ref,
        version: Node.Ref,
    };

    pub const StaticAttr = struct {
        decl_spec: Node.Ref,
        contract_req: Node.Ref,
    };

    pub const Version = union(enum) {
        hex: u32,
        int: IntVersion,
    };

    pub const IntVersion = struct {
        major: u32,
        minor: ?u32 = null,
    };

    pub const UnionDef = struct {
        attributes: ?Node.Range,
        name: ?InternPool.Ref,
        fields: Node.Range,
    };

    pub const UnionField = union(enum) {
        attributes: Node.Range,
        s_field: Node.Ref,
        s_field_optional: ?Node.Ref,
    };

    pub const UnionSwitch = struct {
        attributes: ?Node.Range,
        sfield: Node.Ref,
        ident: ?InternPool.Ref,
        name: ?InternPool.Ref,
        cases: Node.Range,
    };

    pub const UnionSwitchCase = struct {
        case_expr: ?Node.Ref = null, // null if default
        field: Node.Ref,
    };

    pub const UnionSField = union(enum) {
        struct_def: struct {
            attributes: ?Node.Range,
            def: Node.Ref,
        },
        default: struct {
            attributes: ?Node.Range,
            decl_spec: Node.Ref,
            declarator: Node.Ref,
        },
    };

    pub const Declarator = struct {
        is_pointer: bool = false,
        call_conv: ?CallConv = null,
        type_quals: ?Node.Range = null,
        decl: ?Node.Ref,
    };

    pub const TypeQual = enum {
        @"const",
    };

    pub const ExprPrimary = union(enum) {
        false,
        true,
        null,
        number: InternPool.Ref, // TODO: Parse literal as float or integer
        string: InternPool.Ref,
        identifier: InternPool.Ref,
        expr: Node.Ref,
    };

    pub const ExprUnary = struct {
        op: Op,
        arg1: Node.Ref,
    };

    pub const ExprBinary = struct {
        op: Op,
        arg1: Node.Ref,
        arg2: Node.Ref,
    };

    pub const ExprTernary = struct {
        op: Op,
        arg1: Node.Ref,
        arg2: Node.Ref,
        arg3: Node.Ref,
    };

    pub const Op = enum {
        ternary_conditional,
        logical_or,
        logical_and,
        @"and",
        @"or",
        xor,
        equal,
        unequal,
        compare_lt,
        compare_gt,
        compare_lte,
        compare_gte,
        shift_left,
        shift_right,
        add,
        sub,
        mul,
        div,
        mod,
        positive,
        negative,
        logical_not,
        not,
        inc,
        dec,
        ref,
        deref,

        pub fn symbol(op: Op) []const u8 {
            return switch (op) {
                .ternary_conditional => unreachable,
                .logical_or => "||",
                .logical_and => "&&",
                .@"and" => "&",
                .@"or" => "|",
                .xor => "^",
                .equal => "==",
                .unequal => "!=",
                .compare_lt => "<",
                .compare_gt => ">",
                .compare_lte => "<=",
                .compare_gte => ">=",
                .shift_left => "<<",
                .shift_right => ">>",
                .add => "+",
                .sub => "-",
                .mul => "*",
                .div => "/",
                .mod => "%",
                .positive => "+",
                .negative => "-",
                .logical_not => "~",
                .not => "!",
                .inc => "++",
                .dec => "--",
                .ref => "&",
                .deref => "*",
            };
        }
    };

    pub const StructDef = struct {
        attributes: ?Node.Range,
        name: ?InternPool.Ref, // TODO: Point to symbol table for types
        fields: Node.Range,
    };

    pub const StructField = struct {
        attributes: ?Node.Range,
        field: Type,

        pub const Type = union(enum) {
            @"union": Node.Ref, // .union_def
            default: struct {
                decl_spec: Node.Ref,
                struct_decl_list: Node.Range,
            },
        };
    };

    pub const StructDecl = struct {
        any_decl: Node.Ref,
        bit_field: ?Node.Ref, // .expr
    };

    pub const EnumDef = struct {
        attributes: ?Node.Range,
        name: ?InternPool.Ref, // TODO: Point to symbol table for types
        members: Node.Range,
    };

    pub const EnumField = struct {
        attributes: ?Node.Range,
        name: InternPool.Ref,
        expr: ?Node.Ref,
    };

    pub const Typedef = struct {
        attributes: ?Node.Range,
        decl_spec: Node.Ref,
        decl_list: Node.Range,
    };

    pub const BaseType = union(BaseType.Tag) {
        comptime {
            std.debug.assert(@sizeOf(BaseType) <= 16); // TODO: should be 8
        }

        void,
        wchar,
        error_status_t,
        handle_t,
        boolean: bool,
        byte: u8,
        i8: i8,
        u8: u8,
        i16: i16,
        u16: u16,
        i32: i32,
        u32: u32,
        i64: i64,
        u64: u64,
        i3264: isize,
        u3264: usize,
        float: f32,
        double: f64,

        pub const Tag = enum {
            void,
            wchar,
            error_status_t,
            handle_t,
            boolean,
            byte,
            i8,
            u8,
            i16,
            u16,
            i32,
            u32,
            i64,
            u64,
            i3264,
            u3264,
            float,
            double,

            pub fn toSign(tag: Tag, comptime sign: enum { signed, unsigned }) Tag {
                return switch (tag) {
                    .void,
                    .wchar,
                    .error_status_t,
                    .handle_t,
                    .boolean,
                    .float,
                    .double,
                    .byte,
                    => |t| t,

                    .i8, .u8 => switch (sign) {
                        .signed => .i8,
                        .unsigned => .u8,
                    },
                    .i16, .u16 => switch (sign) {
                        .signed => .i16,
                        .unsigned => .u16,
                    },
                    .i32, .u32 => switch (sign) {
                        .signed => .i32,
                        .unsigned => .u32,
                    },
                    .i64, .u64 => switch (sign) {
                        .signed => .i64,
                        .unsigned => .u64,
                    },
                    .i3264, .u3264 => switch (sign) {
                        .signed => .i3264,
                        .unsigned => .i3264,
                    },
                };
            }
        };
    };

    pub const UnqualifiedType = struct {
        name: InternPool.Ref,
    };

    pub const DirectDeclarator = struct {
        base: Node.Ref,
        suffix: Node.Range,
    };

    pub const Decl = struct {
        attributes: ?Node.Range,
        decl_spec: Node.Ref,
        init_decl: ?Node.Ref,
    };

    // Clarify naming with other DeclSpecNoType
    pub const DeclSpecType = struct {
        type: Node.Ref,
        decl_specs: Node.Range,
    };

    pub const PragmaWarning = struct {
        name: InternPool.Ref,
        id: u32,
    };

    pub const InterfaceDef = struct {
        name: Node.Ref,
        type_params: ?Node.Range,
        attributes: ?Node.Range,
        parents: ?Node.Range,
        requires: ?Node.Range,
        defs: Node.Range, // .cpp_quote, .type_decl, .decl, .import, .type_def
    };

    pub const FuncDef = struct {
        name: InternPool.Ref,
        params: Node.Range, // .func_param
    };

    pub const FuncParam = struct {
        attributes: ?Node.Range, // .attr
        decl_spec: Node.Ref,
        any_decl: ?Node.Ref,
    };

    pub const DeclSpec = enum {
        @"const",
        @"inline",
        @"extern",
        static,
        register,
    };

    pub const Attribute = union(Tag) {
        activatable,
        aggregatable,
        annotation: InternPool.Ref,
        appobject,
        @"async",
        async_uuid: u128,
        auto_handle,
        bindable,
        broadcast,
        call_as: InternPool.Ref, // .type
        case: Node.Range, // .expr_list_const
        composable: Node.Ref, // .composable_attr
        comm_status,
        context_handle,
        context_handle_noserialize,
        context_handle_serialize,
        contract: Node.Ref, // .contract_req
        contractversion: Node.Ref, // .contract_version
        control,
        custom: Node.Ref, // .custom
        decode,
        default,
        defaultbind,
        defaultcollelem,
        defaultvalue: Node.Ref, // .expr_const
        defaultvtable,
        deprecated: Node.Ref, // deprecated_attr
        disable_consistency_check,
        displaybind,
        dllname: InternPool.Ref,
        dual,
        enable_allocate,
        encode,
        endpoint: Node.Range, // StringList
        entry: Node.Ref, // .expr_const
        event_add,
        event_remove,
        exclusiveto: Node.Ref, // .decl_spec
        explcit_handle,
        fault_status,
        force_allocate,
        handle,
        helpcontext: Node.Ref, // .expr_int_const
        helpfile: InternPool.Ref,
        helpstring: InternPool.Ref,
        helpstringcontext: Node.Ref, // .expr_int_const
        helpstringdll: InternPool.Ref,
        hidden,
        id: Node.Ref, // .expr_int_const
        idempotent,
        ignore,
        iid_is: Node.Ref, // .expr_int_const
        immediatebind,
        implicit_handle: Node.Ref, // .arg
        in,
        input_sync,
        length_is: ?Node.Ref, // .exprs
        lcid: ?Node.Ref, // .expr_int_const
        licensed,
        local,
        marshaling_behavior: MarshalingBehavior, // MarshalingBehavior
        maybe,
        message,
        nocode,
        nonbrowable,
        noncreatable,
        nonextensible,
        notify,
        notify_flag,
        object,
        odl,
        oleautomation,
        optimize: InternPool.Ref,
        optional,
        out,
        partial_ignore,
        pointer_default: PointerType,
        progid: InternPool.Ref,
        propget,
        propput,
        propputref,
        proxy,
        public,
        range: struct { lo: Node.Ref, hi: Node.Ref }, // .expr_int_const, .expr_int_const
        readonly,
        represent_as: Node.Ref, // .type
        requestedit,
        restricted,
        retval,
        size_is: ?Node.Ref, // .expr
        source,
        static: Node.Ref, // StaticAttr
        strict_context_handle,
        string,
        switch_is: Node.Ref, // .expr
        switch_type: Node.Ref, // .type
        transmit_as: Node.Ref, // .type
        threading: ThreadingType,
        uidefault,
        usesgetlasterror,
        user_marshal: Node.Ref, // .type
        uuid: u128,
        v1_enum,
        vararg,
        version: Node.Ref, // .version
        vi_progid: InternPool.Ref,
        wire_marshal: Node.Ref, // .type
        // PointerType
        ref,
        unique,
        ptr,

        pub const Tag = enum {
            activatable,
            aggregatable,
            annotation,
            appobject,
            @"async",
            async_uuid,
            auto_handle,
            bindable,
            broadcast,
            call_as,
            case,
            composable,
            comm_status,
            context_handle,
            context_handle_noserialize,
            context_handle_serialize,
            contract,
            contractversion,
            control,
            custom,
            decode,
            default,
            defaultbind,
            defaultcollelem,
            defaultvalue,
            defaultvtable,
            deprecated,
            disable_consistency_check,
            displaybind,
            dllname,
            dual,
            enable_allocate,
            encode,
            endpoint,
            entry,
            event_add,
            event_remove,
            exclusiveto,
            explcit_handle,
            fault_status,
            force_allocate,
            handle,
            helpcontext,
            helpfile,
            helpstring,
            helpstringcontext,
            helpstringdll,
            hidden,
            id,
            idempotent,
            ignore,
            iid_is,
            immediatebind,
            implicit_handle,
            in,
            input_sync,
            length_is,
            lcid,
            licensed,
            local,
            marshaling_behavior,
            maybe,
            message,
            nocode,
            nonbrowable,
            noncreatable,
            nonextensible,
            notify,
            notify_flag,
            object,
            odl,
            oleautomation,
            optimize,
            optional,
            out,
            partial_ignore,
            pointer_default,
            progid,
            propget,
            propput,
            propputref,
            proxy,
            public,
            range,
            readonly,
            represent_as,
            requestedit,
            restricted,
            retval,
            size_is,
            source,
            static,
            strict_context_handle,
            string,
            switch_is,
            switch_type,
            transmit_as,
            threading,
            uidefault,
            usesgetlasterror,
            user_marshal,
            uuid,
            v1_enum,
            vararg,
            version,
            vi_progid,
            wire_marshal,
            // PointerType
            ref,
            unique,
            ptr,

            pub fn get(str: []const u8) ?Tag {
                const attributes = std.StaticStringMap(Attribute.Tag).initComptime(.{
                    .{ "activatable", .activatable },
                    .{ "aggregatable", .aggregatable },
                    .{ "annotation", .annotation },
                    .{ "appobject", .appobject },
                    .{ "async", .@"async" },
                    .{ "async_uuid", .async_uuid },
                    .{ "auto_handle", .auto_handle },
                    .{ "bindable", .broadcast },
                    .{ "broadcast", .call_as },
                    .{ "call_as", .call_as },
                    .{ "case", .case },
                    .{ "composable", .composable },
                    .{ "comm_status", .comm_status },
                    .{ "context_handle", .context_handle },
                    .{ "context_handle_noserialize", .context_handle_noserialize },
                    .{ "context_handle_serialize", .context_handle_serialize },
                    .{ "contract", .contract },
                    .{ "contractversion", .contractversion },
                    .{ "control", .control },
                    .{ "custom", .custom },
                    .{ "decode", .decode },
                    .{ "default", .default },
                    .{ "defaultbind", .defaultbind },
                    .{ "defaultcollelem", .defaultcollelem },
                    .{ "defaultvalue", .defaultvalue },
                    .{ "defaultvtable", .defaultvtable },
                    .{ "deprecated", .deprecated },
                    .{ "disable_consistency_check", .disable_consistency_check },
                    .{ "displaybind", .displaybind },
                    .{ "dllname", .dllname },
                    .{ "dual", .dual },
                    .{ "enable_allocate", .enable_allocate },
                    .{ "encode", .encode },
                    .{ "endpoint", .endpoint },
                    .{ "entry", .entry },
                    .{ "event_add", .event_add },
                    .{ "event_remove", .event_remove },
                    .{ "exclusiveto", .exclusiveto },
                    .{ "explcit_handle", .explcit_handle },
                    .{ "fault_status", .fault_status },
                    .{ "force_allocate", .force_allocate },
                    .{ "handle", .handle },
                    .{ "helpcontext", .helpcontext },
                    .{ "helpfile", .helpfile },
                    .{ "helpstring", .helpstring },
                    .{ "helpstringcontext", .helpstringcontext },
                    .{ "helpstringdll", .helpstringdll },
                    .{ "hidden", .hidden },
                    .{ "id", .id },
                    .{ "idempotent", .idempotent },
                    .{ "ignore", .ignore },
                    .{ "iid_is", .iid_is },
                    .{ "immediatebind", .immediatebind },
                    .{ "implicit_handle", .implicit_handle },
                    .{ "in", .in },
                    .{ "input_sync", .input_sync },
                    .{ "length_is", .length_is },
                    .{ "lcid", .lcid },
                    .{ "licensed", .licensed },
                    .{ "local", .local },
                    .{ "marshaling_behavior", .marshaling_behavior },
                    .{ "maybe", .maybe },
                    .{ "message", .message },
                    .{ "nocode", .nocode },
                    .{ "nonbrowable", .nonbrowable },
                    .{ "noncreatable", .noncreatable },
                    .{ "nonextensible", .nonextensible },
                    .{ "notify", .notify },
                    .{ "notify_flag", .notify_flag },
                    .{ "object", .object },
                    .{ "odl", .odl },
                    .{ "oleautomation", .oleautomation },
                    .{ "optimize", .optimize },
                    .{ "optional", .optional },
                    .{ "out", .out },
                    .{ "partial_ignore", .partial_ignore },
                    .{ "pointer_default", .pointer_default },
                    .{ "progid", .progid },
                    .{ "propget", .propget },
                    .{ "propput", .propput },
                    .{ "propputref", .propputref },
                    .{ "proxy", .proxy },
                    .{ "public", .public },
                    .{ "range", .range },
                    .{ "readonly", .readonly },
                    .{ "represent_as", .represent_as },
                    .{ "requestedit", .requestedit },
                    .{ "restricted", .restricted },
                    .{ "retval", .retval },
                    .{ "size_is", .size_is },
                    .{ "source", .source },
                    .{ "static", .static },
                    .{ "strict_context_handle", .strict_context_handle },
                    .{ "string", .string },
                    .{ "switch_is", .switch_is },
                    .{ "switch_type", .switch_type },
                    .{ "transmit_as", .transmit_as },
                    .{ "threading", .threading },
                    .{ "uidefault", .uidefault },
                    .{ "usesgetlasterror", .usesgetlasterror },
                    .{ "user_marshal", .user_marshal },
                    .{ "uuid", .uuid },
                    .{ "v1_enum", .v1_enum },
                    .{ "vararg", .vararg },
                    .{ "version", .version },
                    .{ "vi_progid", .vi_progid },
                    .{ "wire_marshal", .wire_marshal },
                    // PointerType
                    .{ "ref", .ref },
                    .{ "unique", .unique },
                    .{ "ptr", .ptr },
                });
                return attributes.get(str);
            }
        };
    };

    pub const ThreadingType = enum {
        apartment,
        neutral,
        single,
        free,
        both,

        pub fn get(str: []const u8) ?ThreadingType {
            const threading_types = std.StaticStringMap(ThreadingType).initComptime(.{
                .{ "apartment", .apartment },
                .{ "neutral", .neutral },
                .{ "single", .single },
                .{ "free", .free },
                .{ "both", .both },
            });
            return threading_types.get(str);
        }
    };

    pub const PointerType = enum {
        ref,
        unique,
        ptr,

        pub fn get(str: []const u8) ?PointerType {
            const pointer_types = std.StaticStringMap(PointerType).initComptime(.{
                .{ "ref", .ref },
                .{ "unique", .unique },
                .{ "ptr", .ptr },
            });
            return pointer_types.get(str);
        }
    };

    pub const CallConv = enum {
        stdcall,
        fastcall,
        cdecl,
        pascal,

        pub fn get(str: []const u8) ?CallConv {
            const pointer_types = std.StaticStringMap(PointerType).initComptime(.{
                .{ "stdcall", .stdcall },
                .{ "fastcall", .fastcall },
                .{ "cdecl", .cdecl },
                .{ "pascal", .pascal },
            });
            return pointer_types.get(str);
        }
    };

    pub const MarshalingBehavior = enum {
        none,
        agile,
        standard,

        pub fn get(str: []const u8) ?MarshalingBehavior {
            const marshaling_behavior_types = std.StaticStringMap(MarshalingBehavior).initComptime(.{
                .{ "none", .none },
                .{ "agile", .agile },
                .{ "standard", .standard },
            });
            return marshaling_behavior_types.get(str);
        }
    };

    pub const OleAutoType = enum {
        BSTR,
        CURRENCY,
        DATE,
        DECIMAL,
        HWND,
        LPSTR,
        SCORE,
        VARIANT,
        VARIANT_BOOL,

        pub fn get(str: []const u8) ?OleAutoType {
            const ole_auto_types = std.StaticStringMap(OleAutoType).initComptime(.{
                .{ "BSTR", .BSTR },
                .{ "CURRENCY", .CURRENCY },
                .{ "DATE", .DATE },
                .{ "DECIMAL", .DECIMAL },
                .{ "HWND", .HWND },
                .{ "LPSTR", .LPSTR },
                .{ "SCORE", .SCORE },
                .{ "VARIANT", .VARIANT },
                .{ "VARIANT_BOOL", .VARIANT_BOOL },
            });
            return ole_auto_types.get(str);
        }
    };
};
