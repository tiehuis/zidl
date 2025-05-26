const std = @import("std");
const zidl = @import("zidl");

pub const std_options: std.Options = .{
    .log_level = .debug,
};

fn expect(comptime basename: []const u8) !void {
    const allocator = std.heap.page_allocator;

    const idl_filename = basename ++ ".idl";
    const h_filename = basename ++ ".h";

    var buf: std.ArrayList(u8) = .init(allocator);
    defer buf.deinit();

    const idl_bytes = @embedFile(idl_filename);
    const h_bytes = @embedFile(h_filename);

    var cc: zidl.Compile = try .init(allocator, .{ .file = std.io.getStdOut() }, .{
        .include_paths = &.{},
    });

    const idl_bytes_preprocessed = try cc.preprocess(idl_filename, idl_bytes);

    var c: zidl.Compile = try .init(allocator, .{ .buffer = &buf }, .{});
    _ = try c.compile(idl_filename, idl_bytes_preprocessed);

    try std.testing.expectEqualStrings(h_bytes, buf.items);
}

test "simple" {
    try expect("simple");
}

test "cast" {
    try expect("cast");
}

test "tlbref" {
    try expect("tlbref");
}

test "async_uuid" {
    try expect("async_uuid");
}

test "typedef_struct" {
    try expect("typedef_struct");
}

test "attributes_trailing_comma" {
    try expect("attributes_trailing_comma");
}

test "union_switch" {
    try expect("union_switch");
}

test "anonymous_struct" {
    try expect("anonymous_struct");
}

test "empty_attributes" {
    try expect("empty_attributes");
}

test "v1_enum" {
    try expect("v1_enum");
}

test "typedef_multi" {
    try expect("typedef_multi");
}

test "void" {
    try expect("void");
}

test "no_arg_name" {
    try expect("no_arg_name");
}

test "dcommon" {
    try expect("dcommon");
}

test "explicit_numbered_enum" {
    try expect("explicitly_numbered_enum");
}

test "propget_propput" {
    try expect("propget_propput");
}

test "size_is_multi_expr" {
    try expect("size_is_multi_expr");
}

test "empty_arg_list" {
    try expect("empty_arg_list");
}

test "version" {
    try expect("version");
}

test "base_type" {
    try expect("base_type");
}

test "hex_literal" {
    try expect("hex_literal");
}
