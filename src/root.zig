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

        pub fn clone(self: *Self) !T {
            if (self.config_data == null) return ConfigError.NoConfigLoaded;

            const json_string = try json.Stringify.valueAlloc(
                self.parent_allocator,
                self.config_data.?,
                .{},
            );
            defer self.parent_allocator.free(json_string);

            const parsed = try json.parseFromSlice(
                T,
                self.parent_allocator,
                json_string,
                .{ .allocate = .alloc_always },
            );

            return parsed.value;
        }

        pub fn cloneField(self: *Self, comptime FieldType: type, comptime path: []const u8) !FieldType {
            if (self.config_data == null) return ConfigError.NoConfigLoaded;

            const field_value = try self.getField(FieldType, path);
            return try deepCloneValue(FieldType, field_value, self.parent_allocator);
        }

        fn deepCloneValue(comptime FieldType: type, value: FieldType, allocator: std.mem.Allocator) !FieldType {
            const json_string = try json.Stringify.valueAlloc(
                allocator,
                value,
                .{},
            );
            defer allocator.free(json_string);

            const parsed = try json.parseFromSlice(
                FieldType,
                allocator,
                json_string,
                .{ .allocate = .alloc_always },
            );

            return parsed.value;
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
            config_data: T,
            allocator: std.mem.Allocator,

            pub fn deinit(self: *ConfigSnapshot) void {
                _ = self;
            }
        };

        pub fn takeSnapshot(self: *Self) !ConfigSnapshot {
            const cloned = try self.clone();
            return ConfigSnapshot{
                .config_data = cloned,
                .allocator = self.parent_allocator,
            };
        }

        pub fn hasChangedSince(self: *Self, snapshot: ConfigSnapshot) bool {
            if (self.config_data == null) return false;
            return !structsEqual(T, snapshot.config_data, self.config_data.?, .{});
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
