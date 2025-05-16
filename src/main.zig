const std = @import("std");
const zidl = @import("zidl");

const MiB = 1024 * 1024;

const log = std.log.scoped(.main);

const usage =
    \\Usage {s}: [options] file..
    \\
    \\General options:
    \\  -h, --help      Print this message.
    \\  -v, --version   Print zidl version.
    \\
    \\Compile options:
    \\  --winrt                         Use midl3.0 (default midl2.0)
    \\  --skip-imports                  Don't parse imported files
    \\  --stacktrace                    Render a stacktrace on parse error
    \\  --log-level debug|info|warn|err Print all log messages less than requested level
    \\  -D<macro>=<value>               Define a macro for the C preprocessor
    \\
    \\Debug options:
    \\  --tokens                        Print a list of tokens produced by the tokenizer
    \\  --codegen                       Perform codegen step (default: true)
    \\  --show-failed-optional-parses   Render all parsing failures for optional rules
    \\
;

fn exitUsage(program_name: []const u8) noreturn {
    std.debug.print(usage, .{program_name});
    std.process.exit(1);
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    const program_name = args[0];

    if (args.len < 2) {
        return exitUsage(program_name);
    }

    var filenames = try std.ArrayListUnmanaged([]const u8).initCapacity(allocator, 32);
    defer filenames.deinit(allocator);

    const default_include_paths: []const []const u8 = &.{
        "mingw-w64/mingw-w64-headers/include",
        "mingw-w64/mingw-w64-headers/crt",
    };

    var options: zidl.Compile.Options = .{};
    var show_stacktrace = false;
    var user_macros: std.ArrayList(u8) = .init(allocator);
    defer user_macros.deinit();

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.startsWith(u8, args[i], "-")) {
            if (std.mem.eql(u8, args[i], "--ast")) {
                options.show_ast = true;
            } else if (std.mem.eql(u8, args[i], "-h") or std.mem.eql(u8, args[i], "--help")) {
                exitUsage(program_name);
            } else if (std.mem.eql(u8, args[i], "-v") or std.mem.eql(u8, args[i], "--version")) {
                std.debug.print("zidl 1.0\n", .{});
                std.process.exit(0);
            } else if (std.mem.eql(u8, args[i], "--winrt")) {
                options.midl_version = .midl3;
            } else if (std.mem.eql(u8, args[i], "--tokens")) {
                options.show_tokens = true;
            } else if (std.mem.eql(u8, args[i], "--codegen")) {
                options.codegen = true;
            } else if (std.mem.eql(u8, args[i], "--skip-imports")) {
                options.skip_imports = true;
            } else if (std.mem.eql(u8, args[i], "--stacktrace")) {
                show_stacktrace = true;
            } else if (std.mem.eql(u8, args[i], "--show-failed-optional-parses")) {
                options.show_failed_optional_parses = true;
            } else if (std.mem.eql(u8, args[i], "--log-level")) {
                if (i + 1 >= args.len) exitUsage(program_name);
                i += 1;
                log_level = std.meta.stringToEnum(std.log.Level, args[i]) orelse {
                    log.err("unknown log level: {s}", .{args[i]});
                    exitUsage(program_name);
                };
            } else if (std.mem.startsWith(u8, args[i], "-D")) {
                const define = args[i][2..];
                try user_macros.appendSlice(define);
                try user_macros.append('\n');
            } else {
                log.err("unknown option: {s}\n", .{args[i]});
                exitUsage(program_name);
            }
        } else {
            try filenames.append(allocator, args[i]);
        }
    }

    options.user_macros = user_macros.items;

    for (filenames.items) |filename| {
        const file_dir = std.fs.path.dirname(filename) orelse ".";
        options.include_paths = .{file_dir} ++ default_include_paths;

        var file = try std.fs.cwd().openFile(filename, .{});
        const bytes = try file.readToEndAllocOptions(allocator, 128 * MiB, null, .@"64", 0);
        defer allocator.free(bytes);

        log.debug("load {s} ({})", .{ filename, std.fmt.fmtIntSizeDec(bytes.len) });

        var cc: zidl.Compile = try .init(allocator, .{ .file = std.io.getStdOut() }, options);

        const pp_bytes = cc.preprocess(filename, bytes) catch |err| {
            log.err("{s} preprocessing failure: {s}", .{ filename, @errorName(err) });
            if (show_stacktrace) std.debug.dumpStackTrace(@errorReturnTrace().?.*);
            continue;
        };
        log.debug("preprocessing done ({})", .{std.fmt.fmtIntSizeDec(pp_bytes.len)});

        if (cc.compile(filename, pp_bytes)) |_| {
            log.info("{s} ok", .{filename});
        } else |_| {
            log.info("{s} fail", .{filename});
            for (cc.errors.items) |err| {
                err.print(filename, pp_bytes);
            }
            if (show_stacktrace) std.debug.dumpStackTrace(@errorReturnTrace().?.*);
        }
    }
}

pub const std_options: std.Options = .{
    .logFn = logFn,
    .log_level = .debug,
};
var log_level: std.log.Level = std.log.default_level;

fn logFn(
    comptime message_level: std.log.Level,
    comptime scope: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    if (@intFromEnum(message_level) <= @intFromEnum(log_level)) {
        std.log.defaultLog(message_level, scope, format, args);
    }
}
