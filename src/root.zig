const std = @import("std");
const json = std.json;

pub const ConfigError = error{
    InvalidConfigFile,
    ParseError,
    WriteError,
    FileNotFound,
    NoConfigLoaded,
};

pub fn ConfigManager(comptime T: type) type {
    return struct {
        const Self = @This();

        parent_allocator: std.mem.Allocator,
        file_path: []const u8,
        config_data: ?T,
        json_parsed: ?json.Parsed(T),

        pub fn init(parent_allocator: std.mem.Allocator, file_path: []const u8) !Self {
            const owned_path = try parent_allocator.dupe(u8, file_path);

            return Self{
                .parent_allocator = parent_allocator,
                .file_path = owned_path,
                .config_data = null,
                .json_parsed = null,
            };
        }

        pub fn deinit(self: *Self) void {
            if (self.json_parsed) |*parsed| {
                parsed.deinit();
            }
            self.parent_allocator.free(self.file_path);
        }

        pub fn load(self: *Self) !void {
            if (self.json_parsed) |*parsed| {
                parsed.deinit();
                self.json_parsed = null;
            }

            const file = std.fs.cwd().openFile(self.file_path, .{}) catch |err| {
                return switch (err) {
                    error.FileNotFound => ConfigError.FileNotFound,
                    else => err,
                };
            };
            defer file.close();

            const file_contents = try file.readToEndAlloc(self.parent_allocator, 10 * 1024 * 1024);
            defer self.parent_allocator.free(file_contents);

            const parsed = json.parseFromSlice(
                T,
                self.parent_allocator,
                file_contents,
                .{ .allocate = .alloc_always },
            ) catch |err| {
                return switch (err) {
                    else => ConfigError.ParseError,
                };
            };

            self.json_parsed = parsed;
            self.config_data = parsed.value;
        }

        pub fn save(self: *Self) !void {
            if (self.config_data == null) {
                return ConfigError.NoConfigLoaded;
            }

            const file = std.fs.cwd().createFile(self.file_path, .{}) catch |err| {
                return switch (err) {
                    else => ConfigError.WriteError,
                };
            };
            defer file.close();

            const json_string = try json.Stringify.valueAlloc(
                self.parent_allocator,
                self.config_data.?,
                .{ .whitespace = .indent_2 },
            );
            defer self.parent_allocator.free(json_string);

            try file.writeAll(json_string);
        }

        pub fn reload(self: *Self) !void {
            try self.load();
        }

        pub fn get(self: *Self) ?T {
            return self.config_data;
        }

        pub fn set(self: *Self, data: T) void {
            self.config_data = data;
        }

        pub fn getOrDefault(self: *Self, default: T) T {
            return self.config_data orelse default;
        }
    };
}
