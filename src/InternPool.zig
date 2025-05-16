const std = @import("std");

const InternPool = @This();

pub const Ref = enum(u32) {
    _,
};

str_to_id: std.StringHashMap(Ref),
id_to_str: std.ArrayList([]const u8),
count: usize,

pub fn init(allocator: std.mem.Allocator) InternPool {
    return .{
        .str_to_id = .init(allocator),
        .id_to_str = .init(allocator),
        .count = 0,
    };
}

pub fn deinit(pool: *InternPool) void {
    pool.str_to_id.deinit();
    pool.id_to_str.deinit();
}

pub fn intern(pool: *InternPool, str: []const u8) Ref {
    if (pool.str_to_id.get(str)) |found| {
        return found;
    }

    std.debug.assert(pool.id_to_str.items.len == pool.count);
    const id: Ref = @enumFromInt(pool.count);
    pool.id_to_str.append(str) catch unreachable;
    pool.str_to_id.put(str, id) catch unreachable;
    pool.count += 1;
    return id;
}

pub fn get(pool: *const InternPool, ref: Ref) ?[]const u8 {
    const index = @intFromEnum(ref);

    return if (index < pool.id_to_str.items.len)
        pool.id_to_str.items[index]
    else
        null;
}

pub fn dump(pool: *const InternPool) void {
    for (pool.id_to_str.items, 0..) |s, i| {
        std.debug.print("{}: '{s}'\n", .{ i, s });
    }
}
