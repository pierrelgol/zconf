const std = @import("std");
const zconf = @import("zconf");

const AppConfig = struct {
    app_name: []const u8,
    port: u16,
    host: []const u8,
    debug: bool,
    max_connections: u32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Example 1: Create new config manager
    var manager = try zconf.ConfigManager(AppConfig).init(allocator, "config.json");
    defer manager.deinit();

    // Example 2: Set default config and save
    const default_config = AppConfig{
        .app_name = "MyApp",
        .port = 8080,
        .host = "127.0.0.1",
        .debug = false,
        .max_connections = 100,
    };

    manager.set(default_config);
    try manager.save();

    std.debug.print("Config saved to file\n", .{});

    // Example 3: Load config
    try manager.load();
    if (manager.get()) |config| {
        std.debug.print("Loaded config: {s}:{}\n", .{ config.host, config.port });
    }

    // Example 4: Modify and save
    var new_config = manager.get().?;
    new_config.port = 9090;
    manager.set(new_config);
    try manager.save();

    std.debug.print("Config updated: port changed to {}\n", .{new_config.port});

    // Example 5: Reload
    try manager.reload();
    std.debug.print("Config reloaded from file\n", .{});

    // Example 6: Get or default
    const final_config = manager.getOrDefault(default_config);
    std.debug.print("Final config: {s}:{} (debug: {})\n", .{
        final_config.host,
        final_config.port,
        final_config.debug,
    });
}
