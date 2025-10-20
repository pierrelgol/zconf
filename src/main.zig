const std = @import("std");
const zconf = @import("zconf");

const Program = struct {
    name: []const u8,
    version: []const u8,
    enabled: bool,
    path: []const u8,
};

const ServerConfig = struct {
    host: []const u8,
    port: u16,
};

const AppConfig = struct {
    app_name: []const u8,
    debug: bool,
    server: ServerConfig,
    programs: []Program,
};

pub fn main() !void {
    var gpa_instance = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_instance.deinit();
    const gpa = gpa_instance.allocator();

    var arena: std.heap.ArenaAllocator = .init(gpa);
    defer arena.deinit();

    const allocator = arena.allocator();

    var manager = try zconf.ConfigManager(AppConfig).init(allocator, "app_config.json");
    defer manager.deinit();

    var initial_programs = [_]Program{
        .{ .name = "program1", .version = "1.0.0", .enabled = true, .path = "/usr/bin/program1" },
        .{ .name = "program2", .version = "2.0.0", .enabled = false, .path = "/usr/bin/program2" },
        .{ .name = "program3", .version = "1.5.0", .enabled = true, .path = "/usr/bin/program3" },
    };
    const initial_config = AppConfig{
        .app_name = "DiffTest",
        .debug = false,
        .server = .{
            .host = "localhost",
            .port = 8080,
        },
        .programs = &initial_programs,
    };

    manager.set(initial_config);
    try manager.save();
    try manager.load();
    std.debug.print("✓ Initial config saved and loaded\n", .{});

    const old_config = try manager.clone();
    std.debug.print("✓ Config cloned\n", .{});

    var modified_programs = [_]Program{
        .{ .name = "program1", .version = "1.1.0", .enabled = true, .path = "/usr/bin/program1" },

        .{ .name = "program2", .version = "2.0.0", .enabled = true, .path = "/usr/bin/program2" },

        .{ .name = "program4", .version = "1.0.0", .enabled = true, .path = "/usr/bin/program4" },
    };
    const modified_config = AppConfig{
        .app_name = "DiffTest",
        .debug = true,
        .server = .{
            .host = "localhost",
            .port = 9090,
        },
        .programs = &modified_programs,
    };

    manager.set(modified_config);
    try manager.save();
    try manager.load();
    const new_config = manager.get().?;
    std.debug.print("✓ Config modified\n", .{});

    const has_changes = manager.diffConfig(old_config, new_config);
    std.debug.print("✓ Config has changes: {}\n", .{has_changes});

    const debug_changed = try manager.diffField(bool, "debug", old_config, new_config);
    std.debug.print("✓ Debug field changed: {}\n", .{debug_changed});

    const port_changed = try manager.diffField(u16, "server.port", old_config, new_config);
    std.debug.print("✓ Server port changed: {}\n", .{port_changed});

    var diff = try manager.diffSlice(
        Program,
        "programs",
        "name",
        old_config,
        new_config,
        allocator,
    );
    defer diff.deinit(allocator);

    std.debug.print("\n=== Programs Diff ===\n", .{});
    std.debug.print("Added: {} programs\n", .{diff.added.len});
    for (diff.added) |prog| {
        std.debug.print("  + {s} (v{s})\n", .{ prog.name, prog.version });
    }

    std.debug.print("Removed: {} programs\n", .{diff.removed.len});
    for (diff.removed) |prog| {
        std.debug.print("  - {s} (v{s})\n", .{ prog.name, prog.version });
    }

    std.debug.print("Modified: {} programs\n", .{diff.modified.len});
    for (diff.modified) |prog| {
        std.debug.print("  ~ {s} (v{s})\n", .{ prog.name, prog.version });
    }

    const snapshot = try manager.takeSnapshot();

    try manager.setValue("debug", false);

    const changed_since = manager.hasChangedSince(snapshot);
    std.debug.print("\n✓ Config changed since snapshot: {}\n", .{changed_since});

    std.debug.print("\n✓ All diff examples completed!\n", .{});
}
