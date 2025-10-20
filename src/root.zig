const std = @import("std");
const json = std.json;

pub const ConfigError = error{
    InvalidConfigFile,
    ParseError,
    WriteError,
    FileNotFound,
    NoConfigLoaded,
    InvalidPath,
    FieldNotFound,
    TypeMismatch,
    CloneError,
    IdentityFieldNotFound,
    InvalidIdentityField,
};

const PathSegment = struct {
    name: []const u8,
    index: ?usize = null,
};

pub fn DiffResult(comptime T: type) type {
    return struct {
        const Self = @This();

        added: []T,
        removed: []T,
        modified: []T,

        pub fn init(allocator: std.mem.Allocator) Self {
            _ = allocator;
            return .{
                .added = &[_]T{},
                .removed = &[_]T{},
                .modified = &[_]T{},
            };
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            allocator.free(self.added);
            allocator.free(self.removed);
            allocator.free(self.modified);
        }

        pub fn hasChanges(self: Self) bool {
            return self.added.len > 0 or self.removed.len > 0 or self.modified.len > 0;
        }

        pub fn totalChanges(self: Self) usize {
            return self.added.len + self.removed.len + self.modified.len;
        }
    };
}

pub const DiffOptions = struct {
    deep_compare: bool = true,
    compare_string_contents: bool = true,
    include_unchanged: bool = false,
};

pub fn ParsedResult(comptime T: type) type {
    return struct {
        const Self = @This();

        value: T,

        arena: std.heap.ArenaAllocator,

        pub fn init(parent_allocator: std.mem.Allocator) Self {
            return .{
                .value = undefined,
                .arena = std.heap.ArenaAllocator.init(parent_allocator),
            };
        }

        pub fn initWithValue(value: T, parent_allocator: std.mem.Allocator) Self {
            return .{
                .value = value,
                .arena = std.heap.ArenaAllocator.init(parent_allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        pub fn allocator(self: *Self) std.mem.Allocator {
            return self.arena.allocator();
        }
    };
}

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

        pub fn getAll(self: *Self) ?T {
            return self.config_data;
        }

        pub fn getAllOrDefault(self: *Self, default: T) T {
            return self.config_data orelse default;
        }

        pub fn getOrDefault(self: *Self, default: T) T {
            return self.config_data orelse default;
        }

        pub fn set(self: *Self, data: T) void {
            self.config_data = data;
        }

        pub fn setAll(self: *Self, data: T) void {
            self.config_data = data;
        }

        pub fn clone(self: *Self) !ParsedResult(T) {
            if (self.config_data == null) return ConfigError.NoConfigLoaded;

            const json_string = try json.Stringify.valueAlloc(
                self.parent_allocator,
                self.config_data.?,
                .{},
            );
            defer self.parent_allocator.free(json_string);

            var result = ParsedResult(T).init(self.parent_allocator);
            errdefer result.deinit();

            result.value = try json.parseFromSliceLeaky(
                T,
                result.allocator(),
                json_string,
                .{ .allocate = .alloc_always },
            );

            return result;
        }

        pub fn cloneField(self: *Self, comptime FieldType: type, comptime path: []const u8) !ParsedResult(FieldType) {
            if (self.config_data == null) return ConfigError.NoConfigLoaded;

            const field_value = try self.getField(FieldType, path);
            return try deepCloneValue(FieldType, field_value, self.parent_allocator);
        }

        fn deepCloneValue(comptime FieldType: type, value: FieldType, parent_allocator: std.mem.Allocator) !ParsedResult(FieldType) {
            const json_string = try json.Stringify.valueAlloc(
                parent_allocator,
                value,
                .{},
            );
            defer parent_allocator.free(json_string);

            var result = ParsedResult(FieldType).init(parent_allocator);
            errdefer result.deinit();

            result.value = try json.parseFromSliceLeaky(
                FieldType,
                result.allocator(),
                json_string,
                .{ .allocate = .alloc_always },
            );

            return result;
        }

        fn structsEqual(comptime StructType: type, a: StructType, b: StructType, options: DiffOptions) bool {
            const type_info = @typeInfo(StructType);

            switch (type_info) {
                .@"struct" => |struct_info| {
                    inline for (struct_info.fields) |field| {
                        const field_a = @field(a, field.name);
                        const field_b = @field(b, field.name);

                        if (!valuesEqual(@TypeOf(field_a), field_a, field_b, options)) {
                            return false;
                        }
                    }
                    return true;
                },
                else => {
                    return valuesEqual(StructType, a, b, options);
                },
            }
        }

        fn valuesEqual(comptime ValueType: type, a: ValueType, b: ValueType, options: DiffOptions) bool {
            const type_info = @typeInfo(ValueType);

            switch (type_info) {
                .int, .comptime_int, .float, .comptime_float, .bool, .@"enum" => {
                    return a == b;
                },
                .pointer => |ptr_info| {
                    switch (ptr_info.size) {
                        .slice => {
                            if (ptr_info.child == u8 and options.compare_string_contents) {
                                return std.mem.eql(u8, a, b);
                            } else {
                                if (a.len != b.len) return false;

                                for (a, b) |item_a, item_b| {
                                    if (!valuesEqual(ptr_info.child, item_a, item_b, options)) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        },
                        .one => {
                            return valuesEqual(ptr_info.child, a.*, b.*, options);
                        },
                        else => {
                            return a == b;
                        },
                    }
                },
                .optional => |opt_info| {
                    if (a == null and b == null) return true;

                    if (a == null or b == null) return false;

                    return valuesEqual(opt_info.child, a.?, b.?, options);
                },
                .@"struct" => {
                    if (options.deep_compare) {
                        return structsEqual(ValueType, a, b, options);
                    } else {
                        return structsEqual(ValueType, a, b, options);
                    }
                },
                .array => |arr_info| {
                    for (a, b) |item_a, item_b| {
                        if (!valuesEqual(arr_info.child, item_a, item_b, options)) {
                            return false;
                        }
                    }
                    return true;
                },
                else => {
                    return true;
                },
            }
        }

        pub fn diffSlice(
            self: *Self,
            comptime ItemType: type,
            comptime slice_path: []const u8,
            comptime identity_field: []const u8,
            old_config: T,
            new_config: T,
            allocator: std.mem.Allocator,
        ) !DiffResult(ItemType) {
            _ = self;

            const SliceType = []ItemType;

            const old_slice = try getSliceFromConfig(SliceType, old_config, slice_path);
            const new_slice = try getSliceFromConfig(SliceType, new_config, slice_path);

            return try diffSlicesByIdentity(
                ItemType,
                identity_field,
                old_slice,
                new_slice,
                allocator,
                .{},
            );
        }

        pub fn diffSliceWithOptions(
            self: *Self,
            comptime ItemType: type,
            comptime slice_path: []const u8,
            comptime identity_field: []const u8,
            old_config: T,
            new_config: T,
            allocator: std.mem.Allocator,
            options: DiffOptions,
        ) !DiffResult(ItemType) {
            _ = self;

            const SliceType = []ItemType;
            const old_slice = try getSliceFromConfig(SliceType, old_config, slice_path);
            const new_slice = try getSliceFromConfig(SliceType, new_config, slice_path);

            return try diffSlicesByIdentity(
                ItemType,
                identity_field,
                old_slice,
                new_slice,
                allocator,
                options,
            );
        }

        fn getSliceFromConfig(comptime SliceType: type, config: T, comptime path: []const u8) !SliceType {
            comptime var segment_count: usize = 1;
            comptime var i: usize = 0;
            comptime {
                while (i < path.len) : (i += 1) {
                    if (path[i] == '.') segment_count += 1;
                }
            }

            const segments = comptime blk: {
                var segs: [segment_count][]const u8 = undefined;
                var seg_idx: usize = 0;
                var start: usize = 0;
                i = 0;
                while (i <= path.len) : (i += 1) {
                    if (i == path.len or path[i] == '.') {
                        segs[seg_idx] = path[start..i];
                        seg_idx += 1;
                        start = i + 1;
                    }
                }
                break :blk segs;
            };

            return getSliceBySegments(SliceType, T, config, &segments);
        }

        fn getSliceBySegments(comptime SliceType: type, comptime RootType: type, root: RootType, comptime segments: []const []const u8) !SliceType {
            if (segments.len == 0) return ConfigError.InvalidPath;

            const segment = segments[0];
            if (segment.len == 0) return ConfigError.InvalidPath;

            const type_info = @typeInfo(RootType);
            switch (type_info) {
                .@"struct" => |struct_info| {
                    inline for (struct_info.fields) |field| {
                        if (std.mem.eql(u8, field.name, segment)) {
                            const field_value = @field(root, field.name);
                            const FieldType = @TypeOf(field_value);

                            if (segments.len == 1) {
                                if (FieldType != SliceType) {
                                    return ConfigError.TypeMismatch;
                                }
                                return field_value;
                            }

                            return getSliceBySegments(SliceType, FieldType, field_value, segments[1..]);
                        }
                    }
                    return ConfigError.FieldNotFound;
                },
                else => return ConfigError.FieldNotFound,
            }
        }

        fn diffSlicesByIdentity(
            comptime ItemType: type,
            comptime identity_field: []const u8,
            old_items: []const ItemType,
            new_items: []const ItemType,
            allocator: std.mem.Allocator,
            options: DiffOptions,
        ) !DiffResult(ItemType) {
            const type_info = @typeInfo(ItemType);
            if (type_info != .@"struct") return ConfigError.InvalidIdentityField;

            var has_identity_field = false;
            inline for (type_info.@"struct".fields) |field| {
                if (std.mem.eql(u8, field.name, identity_field)) {
                    has_identity_field = true;
                    break;
                }
            }
            if (!has_identity_field) return ConfigError.IdentityFieldNotFound;

            var added_list: std.ArrayListAligned(ItemType, null) = .{};
            defer added_list.deinit(allocator);
            var removed_list: std.ArrayListAligned(ItemType, null) = .{};
            defer removed_list.deinit(allocator);
            var modified_list: std.ArrayListAligned(ItemType, null) = .{};
            defer modified_list.deinit(allocator);

            for (new_items) |new_item| {
                const new_id = @field(new_item, identity_field);
                var found_in_old = false;

                for (old_items) |old_item| {
                    const old_id = @field(old_item, identity_field);

                    if (identityValuesEqual(@TypeOf(new_id), new_id, old_id)) {
                        found_in_old = true;

                        if (!structsEqual(ItemType, old_item, new_item, options)) {
                            try modified_list.append(allocator, new_item);
                        }
                        break;
                    }
                }

                if (!found_in_old) {
                    try added_list.append(allocator, new_item);
                }
            }

            for (old_items) |old_item| {
                const old_id = @field(old_item, identity_field);
                var found_in_new = false;

                for (new_items) |new_item| {
                    const new_id = @field(new_item, identity_field);

                    if (identityValuesEqual(@TypeOf(old_id), old_id, new_id)) {
                        found_in_new = true;
                        break;
                    }
                }

                if (!found_in_new) {
                    try removed_list.append(allocator, old_item);
                }
            }

            return DiffResult(ItemType){
                .added = try added_list.toOwnedSlice(allocator),
                .removed = try removed_list.toOwnedSlice(allocator),
                .modified = try modified_list.toOwnedSlice(allocator),
            };
        }

        fn identityValuesEqual(comptime IdType: type, a: IdType, b: IdType) bool {
            const type_info = @typeInfo(IdType);

            switch (type_info) {
                .pointer => |ptr_info| {
                    if (ptr_info.size == .slice and ptr_info.child == u8) {
                        return std.mem.eql(u8, a, b);
                    }
                    return a == b;
                },
                else => return a == b,
            }
        }

        pub fn diffConfig(self: *Self, old_config: T, new_config: T) bool {
            _ = self;
            return !structsEqual(T, old_config, new_config, .{});
        }

        pub fn diffField(
            self: *Self,
            comptime FieldType: type,
            comptime path: []const u8,
            old_config: T,
            new_config: T,
        ) !bool {
            const segments = try parsePath(self.parent_allocator, path);
            defer self.parent_allocator.free(segments);

            const old_value = try getFieldByPathTyped(FieldType, T, old_config, segments);
            const new_value = try getFieldByPathTyped(FieldType, T, new_config, segments);

            return !structsEqual(FieldType, old_value, new_value, .{});
        }

        pub const ConfigSnapshot = struct {
            parsed: ParsedResult(T),

            pub fn deinit(self: *ConfigSnapshot) void {
                self.parsed.deinit();
            }

            pub fn value(self: *const ConfigSnapshot) T {
                return self.parsed.value;
            }
        };

        pub fn takeSnapshot(self: *Self) !ConfigSnapshot {
            const parsed = try self.clone();
            return ConfigSnapshot{
                .parsed = parsed,
            };
        }

        pub fn hasChangedSince(self: *Self, snapshot: *const ConfigSnapshot) bool {
            if (self.config_data == null) return false;
            return !structsEqual(T, snapshot.value(), self.config_data.?, .{});
        }

        fn parsePath(allocator: std.mem.Allocator, path: []const u8) ![]PathSegment {
            var count: usize = 0;
            var iterator = std.mem.splitScalar(u8, path, '.');
            while (iterator.next()) |segment| {
                if (segment.len == 0) return ConfigError.InvalidPath;
                count += 1;
            }

            if (count == 0) return ConfigError.InvalidPath;

            const segments = try allocator.alloc(PathSegment, count);
            errdefer allocator.free(segments);

            var iter = std.mem.splitScalar(u8, path, '.');
            var i: usize = 0;
            while (iter.next()) |segment| {
                segments[i] = .{ .name = segment };
                i += 1;
            }

            return segments;
        }

        pub fn getField(self: *Self, comptime FieldType: type, comptime path: []const u8) !FieldType {
            if (self.config_data == null) return ConfigError.NoConfigLoaded;

            const segments = try parsePath(self.parent_allocator, path);
            defer self.parent_allocator.free(segments);

            return try getFieldByPathTyped(FieldType, T, self.config_data.?, segments);
        }

        fn getFieldByPathTyped(comptime FieldType: type, comptime RootType: type, root: RootType, segments: []const PathSegment) !FieldType {
            if (segments.len == 0) return ConfigError.InvalidPath;

            const segment = segments[0];
            const type_info = @typeInfo(RootType);

            switch (type_info) {
                .@"struct" => |struct_info| {
                    inline for (struct_info.fields) |field| {
                        if (std.mem.eql(u8, field.name, segment.name)) {
                            const field_value = @field(root, field.name);
                            const ActualType = @TypeOf(field_value);

                            if (segments.len == 1) {
                                if (ActualType != FieldType) return ConfigError.TypeMismatch;
                                return field_value;
                            }

                            return try getFieldByPathTyped(FieldType, ActualType, field_value, segments[1..]);
                        }
                    }

                    return ConfigError.FieldNotFound;
                },
                else => return ConfigError.FieldNotFound,
            }
        }

        pub fn getValueOrDefault(self: *Self, comptime path: []const u8, default: anytype) @TypeOf(default) {
            const DefaultType = @TypeOf(default);
            const value = self.getField(DefaultType, path) catch return default;
            return value;
        }

        pub fn setValue(self: *Self, comptime path: []const u8, value: anytype) !void {
            if (self.config_data == null) return ConfigError.NoConfigLoaded;

            const segments = try parsePath(self.parent_allocator, path);
            defer self.parent_allocator.free(segments);

            try setFieldByPath(T, &self.config_data.?, segments, value);
        }

        fn setFieldByPath(comptime RootType: type, root: *RootType, segments: []const PathSegment, value: anytype) !void {
            if (segments.len == 0) return ConfigError.InvalidPath;

            const segment = segments[0];
            const ValueType = @TypeOf(value);
            const type_info = @typeInfo(RootType);

            switch (type_info) {
                .@"struct" => |struct_info| {
                    inline for (struct_info.fields) |field| {
                        if (std.mem.eql(u8, field.name, segment.name)) {
                            const field_ptr = &@field(root.*, field.name);
                            const FieldType = @TypeOf(field_ptr.*);

                            if (segments.len == 1) {
                                if (FieldType != ValueType) return ConfigError.TypeMismatch;
                                field_ptr.* = value;
                                return;
                            }

                            try setFieldByPath(FieldType, field_ptr, segments[1..], value);
                            return;
                        }
                    }

                    return ConfigError.FieldNotFound;
                },
                else => return ConfigError.FieldNotFound,
            }
        }

        pub fn hasField(self: *Self, comptime path: []const u8) bool {
            if (self.config_data == null) return false;

            const segments = parsePath(self.parent_allocator, path) catch return false;
            defer self.parent_allocator.free(segments);

            return checkFieldExists(T, self.config_data.?, segments);
        }

        fn checkFieldExists(comptime RootType: type, root: RootType, segments: []const PathSegment) bool {
            if (segments.len == 0) return false;

            const segment = segments[0];
            const type_info = @typeInfo(RootType);

            switch (type_info) {
                .@"struct" => |struct_info| {
                    inline for (struct_info.fields) |field| {
                        if (std.mem.eql(u8, field.name, segment.name)) {
                            if (segments.len == 1) return true;

                            const field_value = @field(root, field.name);
                            const FieldType = @TypeOf(field_value);

                            return checkFieldExists(FieldType, field_value, segments[1..]);
                        }
                    }

                    return false;
                },
                else => return false,
            }
        }
    };
}

const testing = std.testing;

test "ParsedResult - init and deinit" {
    var result = ParsedResult(i32).init(testing.allocator);
    defer result.deinit();

    result.value = 42;
    try testing.expectEqual(42, result.value);
}

test "ParsedResult - initWithValue" {
    var result = ParsedResult(i32).initWithValue(100, testing.allocator);
    defer result.deinit();

    try testing.expectEqual(100, result.value);
}

test "ParsedResult - allocator returns valid allocator" {
    var result = ParsedResult(i32).init(testing.allocator);
    defer result.deinit();

    const alloc = result.allocator();
    const mem = try alloc.alloc(u8, 10);
    defer alloc.free(mem);

    try testing.expect(mem.len == 10);
}

test "ParsedResult - works with struct types" {
    const TestStruct = struct {
        name: []const u8,
        value: i32,
    };

    var result = ParsedResult(TestStruct).init(testing.allocator);
    defer result.deinit();

    result.value = .{ .name = "test", .value = 42 };
    try testing.expectEqualStrings("test", result.value.name);
    try testing.expectEqual(42, result.value.value);
}

test "DiffResult - init creates empty slices" {
    const TestType = struct { id: i32 };
    const diff = DiffResult(TestType).init(testing.allocator);

    try testing.expectEqual(0, diff.added.len);
    try testing.expectEqual(0, diff.removed.len);
    try testing.expectEqual(0, diff.modified.len);
}

test "DiffResult - hasChanges returns false when empty" {
    const TestType = struct { id: i32 };
    const diff = DiffResult(TestType).init(testing.allocator);

    try testing.expect(!diff.hasChanges());
}

test "DiffResult - hasChanges returns true with additions" {
    const TestType = struct { id: i32 };

    var items = [_]TestType{.{ .id = 1 }};
    const diff = DiffResult(TestType){
        .added = &items,
        .removed = &[_]TestType{},
        .modified = &[_]TestType{},
    };

    try testing.expect(diff.hasChanges());
}

test "DiffResult - totalChanges counts all changes" {
    const TestType = struct { id: i32 };

    var added = [_]TestType{.{ .id = 1 }};
    var removed = [_]TestType{ .{ .id = 2 }, .{ .id = 3 } };
    var modified = [_]TestType{.{ .id = 4 }};

    const diff = DiffResult(TestType){
        .added = &added,
        .removed = &removed,
        .modified = &modified,
    };

    try testing.expectEqual(4, diff.totalChanges());
}

test "ConfigManager - init stores file path" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    try testing.expectEqualStrings("test.json", manager.file_path);
    try testing.expect(manager.config_data == null);
}

test "ConfigManager - init duplicates file path" {
    const Config = struct { value: i32 };

    const path = "test.json";
    var manager = try ConfigManager(Config).init(testing.allocator, path);
    defer manager.deinit();

    try testing.expect(manager.file_path.ptr != path.ptr);
}

test "ConfigManager - deinit frees resources" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    manager.deinit();
}

test "ConfigManager - load reads valid config" {
    const Config = struct { value: i32 };

    const test_file = "test_load.json";
    const file = try std.fs.cwd().createFile(test_file, .{});
    defer std.fs.cwd().deleteFile(test_file) catch {};

    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    try testing.expect(manager.config_data != null);
    try testing.expectEqual(42, manager.config_data.?.value);
}

test "ConfigManager - load returns FileNotFound for missing file" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "nonexistent_file.json");
    defer manager.deinit();

    try testing.expectError(ConfigError.FileNotFound, manager.load());
}

test "ConfigManager - load returns ParseError for invalid JSON" {
    const Config = struct { value: i32 };

    const test_file = "test_invalid.json";
    const file = try std.fs.cwd().createFile(test_file, .{});
    defer std.fs.cwd().deleteFile(test_file) catch {};

    try file.writeAll("{invalid json");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try testing.expectError(ConfigError.ParseError, manager.load());
}

test "ConfigManager - save writes config to file" {
    const Config = struct { value: i32 };

    const test_file = "test_save.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    manager.set(.{ .value = 99 });
    try manager.save();

    const file = try std.fs.cwd().openFile(test_file, .{});
    defer file.close();

    const content = try file.readToEndAlloc(testing.allocator, 1024);
    defer testing.allocator.free(content);

    try testing.expect(std.mem.indexOf(u8, content, "99") != null);
}

test "ConfigManager - save returns NoConfigLoaded when no config set" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    try testing.expectError(ConfigError.NoConfigLoaded, manager.save());
}

test "ConfigManager - reload refreshes config from file" {
    const Config = struct { value: i32 };

    const test_file = "test_reload.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    var file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 10}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();
    try testing.expectEqual(10, manager.config_data.?.value);

    file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 20}");
    file.close();

    try manager.reload();
    try testing.expectEqual(20, manager.config_data.?.value);
}

test "ConfigManager - multiple loads clean up properly" {
    const Config = struct { value: i32 };

    const test_file = "test_multi_load.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 5}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();
    try manager.load();
    try manager.load();

    try testing.expectEqual(5, manager.config_data.?.value);
}

test "ConfigManager - get returns config data" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    manager.set(.{ .value = 42 });

    const config = manager.get();
    try testing.expect(config != null);
    try testing.expectEqual(42, config.?.value);
}

test "ConfigManager - get returns null when no config loaded" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    try testing.expect(manager.get() == null);
}

test "ConfigManager - getAll returns entire config" {
    const Config = struct { a: i32, b: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    manager.set(.{ .a = 1, .b = 2 });

    const config = manager.getAll();
    try testing.expect(config != null);
    try testing.expectEqual(1, config.?.a);
    try testing.expectEqual(2, config.?.b);
}

test "ConfigManager - getOrDefault returns config when available" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    manager.set(.{ .value = 42 });

    const config = manager.getOrDefault(.{ .value = 0 });
    try testing.expectEqual(42, config.value);
}

test "ConfigManager - getOrDefault returns default when not available" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const config = manager.getOrDefault(.{ .value = 100 });
    try testing.expectEqual(100, config.value);
}

test "ConfigManager - set and setAll update config data" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    manager.set(.{ .value = 10 });
    try testing.expectEqual(10, manager.get().?.value);

    manager.setAll(.{ .value = 20 });
    try testing.expectEqual(20, manager.get().?.value);
}

test "ConfigManager - clone creates independent copy" {
    const Config = struct { value: i32 };

    const test_file = "test_clone.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var cloned = try manager.clone();
    defer cloned.deinit();

    try testing.expectEqual(42, cloned.value.value);
}

test "ConfigManager - clone returns NoConfigLoaded when no config" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    try testing.expectError(ConfigError.NoConfigLoaded, manager.clone());
}

test "ConfigManager - clone handles nested structs" {
    const Inner = struct { x: i32 };
    const Config = struct { inner: Inner };

    const test_file = "test_clone_nested.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"inner\": {\"x\": 99}}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var cloned = try manager.clone();
    defer cloned.deinit();

    try testing.expectEqual(99, cloned.value.inner.x);
}

test "ConfigManager - clone handles slices" {
    const Config = struct { items: []i32 };

    const test_file = "test_clone_slices.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"items\": [1, 2, 3]}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var cloned = try manager.clone();
    defer cloned.deinit();

    try testing.expectEqual(3, cloned.value.items.len);
    try testing.expectEqual(1, cloned.value.items[0]);
    try testing.expectEqual(3, cloned.value.items[2]);
}

test "ConfigManager - cloneField copies specific field" {
    const Config = struct { a: i32, b: i32 };

    const test_file = "test_clone_field.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"a\": 10, \"b\": 20}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var cloned_field = try manager.cloneField(i32, "b");
    defer cloned_field.deinit();

    try testing.expectEqual(20, cloned_field.value);
}

test "ConfigManager - getField retrieves top-level field" {
    const Config = struct { value: i32 };

    const test_file = "test_getfield.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    const value = try manager.getField(i32, "value");
    try testing.expectEqual(42, value);
}

test "ConfigManager - getField retrieves nested field" {
    const Inner = struct { x: i32 };
    const Config = struct { inner: Inner };

    const test_file = "test_getfield_nested.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"inner\": {\"x\": 99}}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    const value = try manager.getField(i32, "inner.x");
    try testing.expectEqual(99, value);
}

test "ConfigManager - getField returns TypeMismatch for wrong type" {
    const Config = struct { value: i32 };

    const test_file = "test_type_mismatch.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    try testing.expectError(ConfigError.TypeMismatch, manager.getField(bool, "value"));
}

test "ConfigManager - getField returns FieldNotFound" {
    const Config = struct { value: i32 };

    const test_file = "test_field_not_found.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    try testing.expectError(ConfigError.FieldNotFound, manager.getField(i32, "nonexistent"));
}

test "ConfigManager - getField returns NoConfigLoaded" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    try testing.expectError(ConfigError.NoConfigLoaded, manager.getField(i32, "value"));
}

test "ConfigManager - setValue sets top-level field" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    manager.set(.{ .value = 10 });

    try manager.setValue("value", @as(i32, 20));

    const value = try manager.getField(i32, "value");
    try testing.expectEqual(20, value);
}

test "ConfigManager - setValue sets nested field" {
    const Inner = struct { x: i32 };
    const Config = struct { inner: Inner };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    manager.set(.{ .inner = .{ .x = 10 } });

    try manager.setValue("inner.x", @as(i32, 50));

    const value = try manager.getField(i32, "inner.x");
    try testing.expectEqual(50, value);
}

test "ConfigManager - setValue returns NoConfigLoaded" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    try testing.expectError(ConfigError.NoConfigLoaded, manager.setValue("value", @as(i32, 10)));
}

test "ConfigManager - getValueOrDefault returns field value" {
    const Config = struct { value: i32 };

    const test_file = "test_default.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    const value = manager.getValueOrDefault("value", @as(i32, 0));
    try testing.expectEqual(42, value);
}

test "ConfigManager - getValueOrDefault returns default on error" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const value = manager.getValueOrDefault("value", @as(i32, 100));
    try testing.expectEqual(100, value);
}

test "ConfigManager - hasField returns true for existing field" {
    const Config = struct { value: i32 };

    const test_file = "test_hasfield.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    try testing.expect(manager.hasField("value"));
}

test "ConfigManager - hasField returns false for nonexistent field" {
    const Config = struct { value: i32 };

    const test_file = "test_hasfield_false.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    try testing.expect(!manager.hasField("nonexistent"));
}

test "ConfigManager - hasField works with nested fields" {
    const Inner = struct { x: i32 };
    const Config = struct { inner: Inner };

    const test_file = "test_hasfield_nested.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"inner\": {\"x\": 99}}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    try testing.expect(manager.hasField("inner.x"));
    try testing.expect(!manager.hasField("inner.y"));
}

test "ConfigManager - hasField returns false when no config" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    try testing.expect(!manager.hasField("value"));
}

test "ConfigManager - diffConfig detects changes" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const old_config = Config{ .value = 10 };
    const new_config = Config{ .value = 20 };

    const has_changes = manager.diffConfig(old_config, new_config);
    try testing.expect(has_changes);
}

test "ConfigManager - diffConfig detects no changes" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const old_config = Config{ .value = 10 };
    const new_config = Config{ .value = 10 };

    const has_changes = manager.diffConfig(old_config, new_config);
    try testing.expect(!has_changes);
}

test "ConfigManager - diffConfig with nested structs" {
    const Inner = struct { x: i32 };
    const Config = struct { inner: Inner };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const old_config = Config{ .inner = .{ .x = 10 } };
    const new_config = Config{ .inner = .{ .x = 20 } };

    const has_changes = manager.diffConfig(old_config, new_config);
    try testing.expect(has_changes);
}

test "ConfigManager - diffField detects field change" {
    const Config = struct { a: i32, b: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const old_config = Config{ .a = 1, .b = 2 };
    const new_config = Config{ .a = 1, .b = 3 };

    const changed = try manager.diffField(i32, "b", old_config, new_config);
    try testing.expect(changed);
}

test "ConfigManager - diffField detects no change" {
    const Config = struct { a: i32, b: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const old_config = Config{ .a = 1, .b = 2 };
    const new_config = Config{ .a = 1, .b = 2 };

    const changed = try manager.diffField(i32, "b", old_config, new_config);
    try testing.expect(!changed);
}

test "ConfigManager - diffField with nested path" {
    const Inner = struct { x: i32, y: i32 };
    const Config = struct { inner: Inner };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const old_config = Config{ .inner = .{ .x = 10, .y = 20 } };
    const new_config = Config{ .inner = .{ .x = 10, .y = 30 } };

    const changed = try manager.diffField(i32, "inner.y", old_config, new_config);
    try testing.expect(changed);
}

test "ConfigManager - diffConfig compares string contents" {
    const Config = struct { name: []const u8 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const old_config = Config{ .name = "old" };
    const new_config = Config{ .name = "new" };

    const has_changes = manager.diffConfig(old_config, new_config);
    try testing.expect(has_changes);
}

test "ConfigManager - diffConfig with slices" {
    const Config = struct { items: []const i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    const old_items = [_]i32{ 1, 2, 3 };
    const new_items = [_]i32{ 1, 2, 4 };

    const old_config = Config{ .items = &old_items };
    const new_config = Config{ .items = &new_items };

    const has_changes = manager.diffConfig(old_config, new_config);
    try testing.expect(has_changes);
}

test "ConfigManager - diffSlice detects added items" {
    const Item = struct { id: i32, name: []const u8 };
    const Config = struct { items: []Item };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    var old_items = [_]Item{.{ .id = 1, .name = "one" }};
    var new_items = [_]Item{
        .{ .id = 1, .name = "one" },
        .{ .id = 2, .name = "two" },
    };

    const old_config = Config{ .items = &old_items };
    const new_config = Config{ .items = &new_items };

    var diff = try manager.diffSlice(Item, "items", "id", old_config, new_config, testing.allocator);
    defer diff.deinit(testing.allocator);

    try testing.expectEqual(1, diff.added.len);
    try testing.expectEqual(2, diff.added[0].id);
}

test "ConfigManager - diffSlice detects removed items" {
    const Item = struct { id: i32, name: []const u8 };
    const Config = struct { items: []Item };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    var old_items = [_]Item{
        .{ .id = 1, .name = "one" },
        .{ .id = 2, .name = "two" },
    };
    var new_items = [_]Item{.{ .id = 1, .name = "one" }};

    const old_config = Config{ .items = &old_items };
    const new_config = Config{ .items = &new_items };

    var diff = try manager.diffSlice(Item, "items", "id", old_config, new_config, testing.allocator);
    defer diff.deinit(testing.allocator);

    try testing.expectEqual(1, diff.removed.len);
    try testing.expectEqual(2, diff.removed[0].id);
}

test "ConfigManager - diffSlice detects modified items" {
    const Item = struct { id: i32, name: []const u8 };
    const Config = struct { items: []Item };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    var old_items = [_]Item{.{ .id = 1, .name = "old_name" }};
    var new_items = [_]Item{.{ .id = 1, .name = "new_name" }};

    const old_config = Config{ .items = &old_items };
    const new_config = Config{ .items = &new_items };

    var diff = try manager.diffSlice(Item, "items", "id", old_config, new_config, testing.allocator);
    defer diff.deinit(testing.allocator);

    try testing.expectEqual(1, diff.modified.len);
    try testing.expectEqual(1, diff.modified[0].id);
}

test "ConfigManager - diffSlice with no changes" {
    const Item = struct { id: i32, name: []const u8 };
    const Config = struct { items: []Item };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    var old_items = [_]Item{.{ .id = 1, .name = "one" }};
    var new_items = [_]Item{.{ .id = 1, .name = "one" }};

    const old_config = Config{ .items = &old_items };
    const new_config = Config{ .items = &new_items };

    var diff = try manager.diffSlice(Item, "items", "id", old_config, new_config, testing.allocator);
    defer diff.deinit(testing.allocator);

    try testing.expect(!diff.hasChanges());
}

test "ConfigManager - diffSlice with nested slice path" {
    const Item = struct { id: i32 };
    const Inner = struct { items: []Item };
    const Config = struct { inner: Inner };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    var old_items = [_]Item{.{ .id = 1 }};
    var new_items = [_]Item{.{ .id = 2 }};

    const old_config = Config{ .inner = .{ .items = &old_items } };
    const new_config = Config{ .inner = .{ .items = &new_items } };

    var diff = try manager.diffSlice(Item, "inner.items", "id", old_config, new_config, testing.allocator);
    defer diff.deinit(testing.allocator);

    try testing.expectEqual(1, diff.added.len);
    try testing.expectEqual(1, diff.removed.len);
}

test "ConfigManager - diffSlice with string identity field" {
    const Item = struct { name: []const u8, value: i32 };
    const Config = struct { items: []Item };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    var old_items = [_]Item{.{ .name = "item1", .value = 10 }};
    var new_items = [_]Item{.{ .name = "item1", .value = 20 }};

    const old_config = Config{ .items = &old_items };
    const new_config = Config{ .items = &new_items };

    var diff = try manager.diffSlice(Item, "items", "name", old_config, new_config, testing.allocator);
    defer diff.deinit(testing.allocator);

    try testing.expectEqual(1, diff.modified.len);
}

test "ConfigManager - diffSlice with multiple changes" {
    const Item = struct { id: i32, value: i32 };
    const Config = struct { items: []Item };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    var old_items = [_]Item{
        .{ .id = 1, .value = 10 },
        .{ .id = 2, .value = 20 },
        .{ .id = 3, .value = 30 },
    };
    var new_items = [_]Item{
        .{ .id = 1, .value = 15 },
        .{ .id = 3, .value = 30 },
        .{ .id = 4, .value = 40 },
    };

    const old_config = Config{ .items = &old_items };
    const new_config = Config{ .items = &new_items };

    var diff = try manager.diffSlice(Item, "items", "id", old_config, new_config, testing.allocator);
    defer diff.deinit(testing.allocator);

    try testing.expectEqual(1, diff.added.len);
    try testing.expectEqual(1, diff.removed.len);
    try testing.expectEqual(1, diff.modified.len);
    try testing.expectEqual(3, diff.totalChanges());
}

test "ConfigManager - diffSlice with empty slices" {
    const Item = struct { id: i32 };
    const Config = struct { items: []Item };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    var empty = [_]Item{};
    const old_config = Config{ .items = &empty };
    const new_config = Config{ .items = &empty };

    var diff = try manager.diffSlice(Item, "items", "id", old_config, new_config, testing.allocator);
    defer diff.deinit(testing.allocator);

    try testing.expect(!diff.hasChanges());
}

test "ConfigManager - diffSliceWithOptions with custom options" {
    const Item = struct { id: i32, value: i32 };
    const Config = struct { items: []Item };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    var old_items = [_]Item{.{ .id = 1, .value = 10 }};
    var new_items = [_]Item{.{ .id = 1, .value = 20 }};

    const old_config = Config{ .items = &old_items };
    const new_config = Config{ .items = &new_items };

    const options = DiffOptions{
        .deep_compare = true,
        .compare_string_contents = true,
        .include_unchanged = false,
    };

    var diff = try manager.diffSliceWithOptions(
        Item,
        "items",
        "id",
        old_config,
        new_config,
        testing.allocator,
        options,
    );
    defer diff.deinit(testing.allocator);

    try testing.expectEqual(1, diff.modified.len);
}

test "ConfigManager - takeSnapshot creates snapshot" {
    const Config = struct { value: i32 };

    const test_file = "test_snapshot.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var snapshot = try manager.takeSnapshot();
    defer snapshot.deinit();

    try testing.expectEqual(42, snapshot.value().value);
}

test "ConfigManager - hasChangedSince detects changes" {
    const Config = struct { value: i32 };

    const test_file = "test_changed.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var snapshot = try manager.takeSnapshot();
    defer snapshot.deinit();

    manager.set(.{ .value = 100 });

    try testing.expect(manager.hasChangedSince(&snapshot));
}

test "ConfigManager - hasChangedSince detects no changes" {
    const Config = struct { value: i32 };

    const test_file = "test_unchanged.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var snapshot = try manager.takeSnapshot();
    defer snapshot.deinit();

    try testing.expect(!manager.hasChangedSince(&snapshot));
}

test "ConfigManager - hasChangedSince returns false when no config" {
    const Config = struct { value: i32 };

    const test_file = "test_no_config.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var snapshot = try manager.takeSnapshot();
    defer snapshot.deinit();

    manager.config_data = null;

    try testing.expect(!manager.hasChangedSince(&snapshot));
}

test "Path parsing - single segment" {
    const Config = struct { value: i32 };

    const test_file = "test_path_single.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    const value = try manager.getField(i32, "value");
    try testing.expectEqual(42, value);
}

test "Path parsing - multiple segments" {
    const Level3 = struct { val: i32 };
    const Level2 = struct { level3: Level3 };
    const Config = struct { level2: Level2 };

    const test_file = "test_path_multi.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"level2\": {\"level3\": {\"val\": 99}}}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    const value = try manager.getField(i32, "level2.level3.val");
    try testing.expectEqual(99, value);
}

test "Path parsing - empty segment returns InvalidPath" {
    const Config = struct { value: i32 };

    const test_file = "test_empty_path.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    try testing.expectError(ConfigError.InvalidPath, manager.getField(i32, "value..nested"));
}

test "Path parsing - setValue with empty path returns InvalidPath" {
    const Config = struct { value: i32 };

    var manager = try ConfigManager(Config).init(testing.allocator, "test.json");
    defer manager.deinit();

    manager.set(.{ .value = 10 });

    try testing.expectError(ConfigError.InvalidPath, manager.setValue("", @as(i32, 20)));
}

test "Path parsing - complex nested structure" {
    const Item = struct { id: i32 };
    const Inner = struct { items: []Item, count: i32 };
    const Config = struct { data: Inner };

    const test_file = "test_complex_nested.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"data\": {\"items\": [{\"id\": 1}], \"count\": 5}}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    const count = try manager.getField(i32, "data.count");
    try testing.expectEqual(5, count);
}

test "Edge case - large config with many fields" {
    const Config = struct {
        field1: i32,
        field2: i32,
        field3: i32,
        field4: i32,
        field5: i32,
        field6: i32,
        field7: i32,
        field8: i32,
        field9: i32,
        field10: i32,
    };

    const test_file = "test_large.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll(
        \\{
        \\  "field1": 1,
        \\  "field2": 2,
        \\  "field3": 3,
        \\  "field4": 4,
        \\  "field5": 5,
        \\  "field6": 6,
        \\  "field7": 7,
        \\  "field8": 8,
        \\  "field9": 9,
        \\  "field10": 10
        \\}
    );
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    try testing.expectEqual(1, manager.get().?.field1);
    try testing.expectEqual(10, manager.get().?.field10);
}

test "Integration - load, modify, save, reload workflow" {
    const Config = struct { value: i32 };

    const test_file = "test_workflow.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 10}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();
    try testing.expectEqual(10, manager.get().?.value);

    try manager.setValue("value", @as(i32, 20));

    try manager.save();

    try manager.reload();
    try testing.expectEqual(20, manager.get().?.value);
}

test "Integration - clone and modify workflow" {
    const Config = struct { value: i32 };

    const test_file = "test_clone_workflow.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var cloned = try manager.clone();
    defer cloned.deinit();

    try testing.expectEqual(manager.get().?.value, cloned.value.value);

    manager.set(.{ .value = 100 });

    try testing.expectEqual(42, cloned.value.value);
}

test "Integration - snapshot, modify, compare workflow" {
    const Config = struct { a: i32, b: i32 };

    const test_file = "test_snapshot_workflow.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"a\": 1, \"b\": 2}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    var snapshot = try manager.takeSnapshot();
    defer snapshot.deinit();

    try testing.expect(!manager.hasChangedSince(&snapshot));

    try manager.setValue("b", @as(i32, 20));

    try testing.expect(manager.hasChangedSince(&snapshot));
}

test "Edge case - boolean field handling" {
    const Config = struct { enabled: bool };

    const test_file = "test_bool.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"enabled\": true}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    try testing.expect(manager.get().?.enabled);

    try manager.setValue("enabled", false);
    try testing.expect(!manager.get().?.enabled);
}

test "Edge case - string field handling" {
    const Config = struct { name: []const u8 };

    const test_file = "test_string.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"name\": \"test_name\"}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    const name = try manager.getField([]const u8, "name");
    try testing.expectEqualStrings("test_name", name);
}

test "Edge case - array field handling" {
    const Config = struct { values: [3]i32 };

    const test_file = "test_array.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    const file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"values\": [1, 2, 3]}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();

    const values = try manager.getField([3]i32, "values");
    try testing.expectEqual(1, values[0]);
    try testing.expectEqual(2, values[1]);
    try testing.expectEqual(3, values[2]);
}

test "Edge case - optional field handling" {
    const Config = struct { value: ?i32 };

    const test_file = "test_optional.json";
    defer std.fs.cwd().deleteFile(test_file) catch {};

    var file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": null}");
    file.close();

    var manager = try ConfigManager(Config).init(testing.allocator, test_file);
    defer manager.deinit();

    try manager.load();
    try testing.expect(manager.get().?.value == null);

    file = try std.fs.cwd().createFile(test_file, .{});
    try file.writeAll("{\"value\": 42}");
    file.close();

    try manager.reload();
    try testing.expect(manager.get().?.value != null);
    try testing.expectEqual(42, manager.get().?.value.?);
}
