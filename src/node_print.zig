const std = @import("std");

const Node = @import("node.zig").Node;
const Parser = @import("Parser.zig");
const CodeGen = @import("CodeGen.zig");

pub fn dumpNode(p: *const Parser, node_index: Node.Ref) void {
    dumpNodeInner(p, node_index, 0);
}

fn dumpNodeInner(p: *const Parser, node_index: Node.Ref, indent: usize) void {
    const node = p.nodes.items[node_index.toInt()];
    switch (node) {
        .root => |range| {
            print(p, "root\n", .{});
            for (range.start..range.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },
        inline .base_type,
        .decl_spec,
        .call_conv,
        .type_qual,
        => |v| {
            printIndent(p, indent);
            print(p, "{s}: {s}\n", .{ @tagName(node), @tagName(v) });
        },

        .cpp_quote,
        .pragma,
        .type,
        .enum_ref,
        .struct_ref,
        .known_type,
        .importlib,
        => |ref| {
            printIndent(p, indent);
            print(p, "{s}: {s}\n", .{ @tagName(node), p.intern_pool.get(ref).? });
        },

        .module_ref,
        .interface_ref,
        .expr_const,
        .expr_const_int,
        .runtime_class_ref,
        .coclass_ref,
        .api_contract_ref,
        .disp_interface_ref,
        .safe_array,
        => |index| {
            printIndent(p, indent);
            print(p, "{s}\n", .{@tagName(node)});
            dumpNodeInner(p, index, indent + 1);
        },

        .attributes,
        .disp_interface_props,
        .disp_interface_methods,
        .declare_block,
        => |range| {
            for (range.start..range.end) |i|
                dumpAttribute(p, p.nodes.items[i].attribute, indent);
        },
        .attribute => |attr| {
            dumpAttribute(p, attr, indent);
        },

        .interface => |interface| {
            const typename = p.intern_pool.get(p.nodes.items[interface.name.toInt()].type).?;
            printIndent(p, indent);
            print(p, "interface: {s}\n", .{typename});
            if (interface.type_params) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }

            // TODO: consolidate storage of these between Range and the actual Attributes nodes.
            if (interface.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }

            if (interface.parents) |range| {
                for (range.start..range.end) |i| {
                    printIndent(p, indent);
                    print(p, "parents\n", .{});
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }

            if (interface.requires) |range| {
                for (range.start..range.end) |i| {
                    printIndent(p, indent);
                    print(p, "requires\n", .{});
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }

            for (interface.defs.start..interface.defs.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .unqualified_decl_spec_type,
        .decl_spec_type,
        => |decl_spec| {
            printIndent(p, indent);
            print(p, "{s}\n", .{@tagName(node)});
            dumpNodeInner(p, decl_spec.type, indent + 1);
            for (decl_spec.decl_specs.start..decl_spec.decl_specs.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .abstract_direct_decl,
        .direct_decl,
        => |direct_decl| {
            printIndent(p, indent);
            const base = switch (p.nodes.items[direct_decl.base.toInt()]) {
                .type => |name| p.intern_pool.get(name).?,
                else => "",
            };
            print(p, "direct_decl: {s}\n", .{base});
            for (direct_decl.suffix.start..direct_decl.suffix.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .decl => |decl| {
            printIndent(p, indent);
            print(p, "decl\n", .{});
            if (decl.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            dumpNodeInner(p, decl.decl_spec, indent + 1);
            if (decl.init_decl) |init_decl| {
                dumpNodeInner(p, init_decl, indent + 1);
            }
        },

        .abstract_declarator,
        .declarator,
        .any_declarator,
        => |declarator| {
            printIndent(p, indent);
            print(p, "any_decl\n", .{});
            printIndent(p, indent + 1);
            print(p, "is_pointer: {}\n", .{declarator.is_pointer});
            if (declarator.call_conv) |cc| {
                printIndent(p, indent + 1);
                print(p, "call_conv: {s}\n", .{@tagName(cc)});
            }
            if (declarator.decl) |decl| {
                dumpNodeInner(p, decl, indent + 1);
            }
        },

        .func_def => |func_def| {
            const funcname = p.intern_pool.get(func_def.name).?;
            printIndent(p, indent);
            print(p, "func_def: {s}\n", .{funcname});

            for (func_def.params.start..func_def.params.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .func_param => |param| {
            printIndent(p, indent);
            print(p, "param\n", .{});
            if (param.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            dumpNodeInner(p, param.decl_spec, indent + 1);
            if (param.any_decl) |any_decl| {
                dumpNodeInner(p, any_decl, indent + 1);
            }
        },

        .expr => |expr| {
            printIndent(p, indent);
            print(p, "expr_primary: ", .{});
            switch (expr) {
                .false => print(p, "false\n", .{}),
                .true => print(p, "true\n", .{}),
                .null => print(p, "null\n", .{}),
                .number, .string, .identifier => |ref| print(p, "{s}({s})\n", .{
                    @tagName(expr), p.intern_pool.get(ref).?,
                }),
                .expr => |index| dumpNodeInner(p, index, indent),
            }
        },

        .expr_unary => |expr| {
            printIndent(p, indent);
            print(p, "expr_unary: {s}\n", .{@tagName(expr.op)});
            dumpNodeInner(p, expr.arg1, indent + 1);
        },
        .expr_binary => |expr| {
            printIndent(p, indent);
            print(p, "expr_binary: {s}\n", .{@tagName(expr.op)});
            dumpNodeInner(p, expr.arg1, indent + 1);
            dumpNodeInner(p, expr.arg2, indent + 1);
        },
        .expr_ternary => |expr| {
            printIndent(p, indent);
            print(p, "expr_ternary: {s}\n", .{@tagName(expr.op)});
            dumpNodeInner(p, expr.arg1, indent + 1);
            dumpNodeInner(p, expr.arg2, indent + 1);
            dumpNodeInner(p, expr.arg3, indent + 1);
        },

        .typedef => |def| {
            printIndent(p, indent);
            print(p, "typedef\n", .{});
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            printIndent(p, indent + 1);
            print(p, "decl_spec\n", .{});
            dumpNodeInner(p, def.decl_spec, indent + 1);

            for (def.decl_list.start..def.decl_list.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .enum_def => |def| {
            printIndent(p, indent);
            print(p, "enum_def\n", .{});
            if (def.name) |name| {
                printIndent(p, indent + 1);
                print(p, "name: {s}\n", .{p.intern_pool.get(name).?});
            }
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            printIndent(p, indent + 1);
            print(p, "members\n", .{});
            for (def.members.start..def.members.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 2);
            }
        },

        .enum_field => |field| {
            printIndent(p, indent);
            print(p, "enum_field: {s}\n", .{p.intern_pool.get(field.name).?});
            if (field.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            if (field.expr) |expr| {
                printIndent(p, indent);
                print(p, "value\n", .{});
                dumpNodeInner(p, expr, indent + 1);
            }
        },

        .struct_def => |def| {
            printIndent(p, indent);
            print(p, "struct_def\n", .{});
            if (def.name) |name| {
                printIndent(p, indent + 1);
                print(p, "name: {s}\n", .{p.intern_pool.get(name).?});
            }
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            printIndent(p, indent + 1);
            print(p, "fields\n", .{});
            for (def.fields.start..def.fields.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 2);
            }
        },

        .struct_field => |field| {
            printIndent(p, indent);
            print(p, "struct_field\n", .{});
            if (field.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            printIndent(p, indent + 1);
            print(p, "field\n", .{});
            switch (field.field) {
                .@"union" => |index| dumpNodeInner(p, index, indent + 2),
                .default => |decl| {
                    dumpNodeInner(p, decl.decl_spec, indent + 2);
                    for (decl.struct_decl_list.start..decl.struct_decl_list.end) |i| {
                        dumpNodeInner(p, p.data.items[i], indent + 2);
                    }
                },
            }
        },

        .struct_decl => |decl| {
            printIndent(p, indent);
            print(p, "struct_decl\n", .{});

            dumpNodeInner(p, decl.any_decl, indent + 2);
            if (decl.bit_field) |bf| {
                dumpNodeInner(p, bf, indent + 1);
            }
        },

        .pragma_warning => |pw| {
            printIndent(p, indent);
            print(p, "pragma_warning: ({s}, {})", .{
                p.intern_pool.get(pw.name).?,
                pw.id,
            });
        },

        .union_def => |def| {
            printIndent(p, indent);
            print(
                p,
                "union_def: {s}\n",
                .{if (def.name) |name| p.intern_pool.get(name).? else "(anonymous)"},
            );
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            printIndent(p, indent + 1);
            print(p, "fields\n", .{});
            for (def.fields.start..def.fields.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 2);
            }
        },

        .union_field => |field| {
            printIndent(p, indent);
            print(p, "union_field\n", .{});
            switch (field) {
                .attributes => |range| {
                    for (range.start..range.end) |i| {
                        dumpNodeInner(p, p.data.items[i], indent + 1);
                    }
                },
                .s_field => |index| {
                    dumpNodeInner(p, index, indent + 1);
                },
                .s_field_optional => |maybe_index| {
                    if (maybe_index) |index| {
                        dumpNodeInner(p, index, indent + 1);
                    }
                },
            }
        },

        .union_sfield => |s_field| {
            printIndent(p, indent);
            print(p, "union_sfield\n", .{});
            switch (s_field) {
                .struct_def => |def| {
                    if (def.attributes) |range| {
                        for (range.start..range.end) |i| {
                            dumpNodeInner(p, p.data.items[i], indent + 1);
                        }
                    }
                    dumpNodeInner(p, def.def, indent + 1);
                },
                .default => |def| {
                    if (def.attributes) |range| {
                        for (range.start..range.end) |i| {
                            dumpNodeInner(p, p.data.items[i], indent + 1);
                        }
                    }
                    dumpNodeInner(p, def.decl_spec, indent + 1);
                    dumpNodeInner(p, def.declarator, indent + 1);
                },
            }
        },
        .union_switch => |def| {
            printIndent(p, indent);
            print(
                p,
                "union_sfield: {s}\n",
                .{if (def.name) |name| p.intern_pool.get(name).? else "(anonymous)"},
            );
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            if (def.ident) |ref| {
                print(p, "ident: {s}\n", .{p.intern_pool.get(ref).?});
            }
            dumpNodeInner(p, def.sfield, indent + 1);
            for (def.cases.start..def.cases.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },
        .union_switch_case => |case| {
            printIndent(p, indent);
            print(p, "union_case\n", .{});
            if (case.case_expr) |expr| {
                dumpNodeInner(p, expr, indent + 1);
            } else {
                print(p, "case: default\n", .{});
            }
            dumpNodeInner(p, case.field, indent + 1);
        },

        .version => |version| {
            printIndent(p, indent);
            switch (version) {
                .hex => |hex| {
                    print(p, "version: 0x{x}\n", .{hex});
                },
                .int => |int| {
                    if (int.minor) |minor| {
                        print(p, "version: {}.{}\n", .{ int.major, minor });
                    } else {
                        print(p, "version: {}\n", .{int.major});
                    }
                },
            }
        },

        .int_version => |int| {
            printIndent(p, indent);
            if (int.minor) |minor| {
                print(p, "version: {}.{}\n", .{ int.major, minor });
            } else {
                print(p, "version: {}\n", .{int.major});
            }
        },

        .contract_req => |req| {
            printIndent(p, indent);
            print(p, "contract_req", .{});
            dumpNodeInner(p, req.decl_spec, indent + 1);
            dumpNodeInner(p, req.version, indent + 1);
        },

        .static_attr => |req| {
            printIndent(p, indent);
            print(p, "static_attr", .{});
            dumpNodeInner(p, req.decl_spec, indent + 1);
            dumpNodeInner(p, req.contract_req, indent + 1);
        },

        .namespace => |namespace| {
            printIndent(p, indent);
            print(p, "namespace: ", .{});
            for (namespace.qualified_name.start..namespace.qualified_name.end) |i| {
                const ref = p.nodes.items[i].type;
                print(p, "{s} ", .{p.intern_pool.get(ref).?});
            }
            for (namespace.statements.start..namespace.statements.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .runtime_class_def => |def| {
            printIndent(p, indent);
            print(p, "runtime_class_def\n", .{});
            dumpNodeInner(p, def.name, indent + 1);
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            if (def.parents) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            for (def.ints.start..def.ints.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .coclass_def => |def| {
            printIndent(p, indent);
            print(p, "coclass_def\n", .{});
            dumpNodeInner(p, def.name, indent + 1);
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            for (def.ints.start..def.ints.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .module_def => |def| {
            printIndent(p, indent);
            print(p, "module_def\n", .{});
            dumpNodeInner(p, def.name, indent + 1);
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            for (def.statements.start..def.statements.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .api_contract_def => |def| {
            printIndent(p, indent);
            print(p, "api_contract_def\n", .{});
            dumpNodeInner(p, def.name, indent + 1);
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
        },

        .class_int => |int| {
            printIndent(p, indent);
            print(p, "class_int\n", .{});
            dumpNodeInner(p, int.type, indent + 1);
            if (int.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
        },

        .qualified_type => |ty| {
            printIndent(p, indent);
            print(p, "qualified_type\n", .{});
            for (ty.namespace.start..ty.namespace.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
            dumpNodeInner(p, ty.typename, indent + 1);
        },

        .parameterized_type => |ty| {
            printIndent(p, indent);
            print(p, "parameterized_type\n", .{});
            dumpNodeInner(p, ty.qual_type, indent + 1);
            for (ty.type_args.start..ty.type_args.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },
        .parameterized_type_arg => |arg| {
            printIndent(p, indent);
            print(p, "parameterized_type_arg\n", .{});
            printIndent(p, indent + 1);
            print(p, "is_pointer: {}\n", .{arg.is_pointer});
            dumpNodeInner(p, arg.type, indent + 1);
        },

        .disp_interface_def => |def| {
            printIndent(p, indent);
            print(p, "disp_interface_def\n", .{});
            dumpNodeInner(p, def.name, indent + 1);
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            switch (def.data) {
                .props_methods => |data| {
                    printIndent(p, indent + 1);
                    print(p, "props\n", .{});
                    for (data.props.start..data.props.end) |i| {
                        dumpNodeInner(p, p.data.items[i], indent + 1);
                    }
                    printIndent(p, indent + 1);
                    print(p, "methods\n", .{});
                    for (data.methods.start..data.methods.end) |i| {
                        dumpNodeInner(p, p.data.items[i], indent + 1);
                    }
                },
                .interface => |interface| {
                    dumpNodeInner(p, interface, indent + 1);
                },
            }
        },

        .declare_statement => |decl| {
            printIndent(p, indent);
            print(p, "declare_statement\n", .{});
            dumpNodeInner(p, decl.qual_type, indent + 1);
            for (decl.param_type_args.start..decl.param_type_args.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .library_def => |def| {
            printIndent(p, indent);
            print(p, "library_def: {s}\n", .{p.intern_pool.get(def.name).?});
            if (def.attributes) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
            for (def.import_statements.start..def.import_statements.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .import => |import| {
            printIndent(p, indent);
            print(p, "import: {s}\n", .{p.intern_pool.get(import.subject).?});
            for (import.statements.start..import.statements.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .init_declarator => |init_decl| {
            printIndent(p, indent);
            print(p, "init_declarator\n", .{});
            dumpNodeInner(p, init_decl.declarator, indent + 1);
            if (init_decl.expr) |expr| {
                dumpNodeInner(p, expr, indent + 1);
            }
        },

        .cast => |cast| {
            printIndent(p, indent);
            print(p, "cast\n", .{});
            dumpNodeInner(p, cast.unqual_decl_spec, indent + 1);
            if (cast.declarator) |declarator| {
                dumpNodeInner(p, declarator, indent + 1);
            }
            dumpNodeInner(p, cast.expr, indent + 1);
        },

        .sizeof => |sizeof| {
            printIndent(p, indent);
            print(p, "sizeof\n", .{});
            dumpNodeInner(p, sizeof.unqual_decl_spec, indent + 1);
            if (sizeof.abstract_decl) |abstract_decl| {
                dumpNodeInner(p, abstract_decl, indent + 1);
            }
        },

        .array_def => |array| {
            printIndent(p, indent);
            print(p, "array", .{});
            switch (array) {
                .empty => print(p, "[]\n", .{}),
                .asterisk => print(p, "[*]\n", .{}),
                .expr => |index| {
                    print(p, "[(expr)]\n", .{});
                    dumpNodeInner(p, index, indent + 1);
                },
            }
        },

        .arg_list => |arg_list| {
            printIndent(p, indent);
            print(p, "arg_list\n", .{});
            printIndent(p, indent + 1);
            print(p, "is_varargs: {}\n", .{arg_list.is_varargs});
            for (arg_list.args.start..arg_list.args.end) |index| {
                dumpNodeInner(p, p.data.items[index], indent + 1);
            }
        },
    }
}

fn dumpAttribute(p: *const Parser, attr: Node.Attribute, indent: usize) void {
    printIndent(p, indent);
    switch (attr) {
        .activatable,
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
        => print(p, "attribute: {s}\n", .{@tagName(attr)}),

        .ref,
        .unique,
        .ptr,
        => print(p, "attribute/bare-pointer-type: {s}\n", .{@tagName(attr)}),

        .async_uuid,
        .uuid,
        => |uuid| {
            var buf: [64]u8 = undefined;
            const uuid_s = CodeGen.uuidToString(&buf, uuid);
            print(p, "attribute: {s}({s})\n", .{ @tagName(attr), uuid_s });
        },

        .annotation,
        .call_as,
        .dllname,
        .helpfile,
        .helpstring,
        .helpstringdll,
        .optimize,
        .progid,
        .vi_progid,
        => |intern_ref| {
            print(p, "attribute: {s}({s})\n", .{
                @tagName(attr),
                p.intern_pool.get(intern_ref).?,
            });
        },

        .composable,
        .contract,
        .contractversion,
        .custom,
        .defaultvalue,
        .deprecated,
        .entry,
        .exclusiveto,
        .helpcontext,
        .helpstringcontext,
        .id,
        .iid_is,
        .implicit_handle,
        .represent_as,
        .static,
        .switch_is,
        .switch_type,
        .transmit_as,
        .user_marshal,
        .version,
        .wire_marshal,
        => |index| {
            print(p, "attribute: {s}\n", .{@tagName(attr)});
            dumpNodeInner(p, index, indent + 1);
        },

        .case,
        => |range| {
            print(p, "attribute: {s}\n", .{@tagName(attr)});
            for (range.start..range.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },

        .length_is,
        .size_is,
        => |maybe_range| {
            print(p, "attribute: {s}\n", .{@tagName(attr)});
            if (maybe_range) |range| {
                for (range.start..range.end) |i| {
                    dumpNodeInner(p, p.data.items[i], indent + 1);
                }
            }
        },

        .lcid,
        => |maybe_index| {
            print(p, "attribute: {s}\n", .{@tagName(attr)});
            if (maybe_index) |index| {
                dumpNodeInner(p, index, indent + 1);
            }
        },

        .threading => |threading_type| {
            print(p, "attribute: threading({s})\n", .{@tagName(threading_type)});
        },

        .pointer_default => |ptr_type| {
            print(p, "attribute: pointer_default({s})\n", .{@tagName(ptr_type)});
        },

        .marshaling_behavior => |marshal_type| {
            print(p, "attribute: marshalling_behavior({s})\n", .{@tagName(marshal_type)});
        },

        .range => |r| {
            print(p, "attribute: range\n", .{});
            dumpNodeInner(p, r.lo, indent + 1);
            dumpNodeInner(p, r.hi, indent + 1);
        },

        .endpoint,
        => |range| {
            print(p, "attribute: {s}\n", .{@tagName(attr)});
            for (range.start..range.end) |i| {
                dumpNodeInner(p, p.data.items[i], indent + 1);
            }
        },
    }
}

fn printIndent(p: *const Parser, indent: usize) void {
    for (0..indent) |_| print(p, " ", .{});
}

fn print(p: *const Parser, comptime fmt: []const u8, args: anytype) void {
    _ = p;
    std.debug.print(fmt, args);
}
