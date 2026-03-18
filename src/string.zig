/// String data structures and algorithm in zig.
const std = @import("std");
const allocator = @import("allocator.zig");

/// Allocator: Heap + arena since we don't know how many things.
pub fn Trie(
    child_num: comptime_int,
    norm: comptime_int, // First char to normalize to 0
    T: anytype,
) type {
    const TrieNode = struct {
        const Self = @This();

        val: T,
        children: [child_num]?*Self,

        pub fn new() Self {
            return Self{
                .children = [_]?*Self{null} ** child_num,
                .val = T.init(),
            };
        }
    };

    return struct {
        head: *TrieNode,
        al: allocator.BumpAllo(0, TrieNode),

        const Self = @This();

        pub fn new() Self {
            var self = Self{
                .head = undefined,
                .al = .init(),
            };
            self.head = self.al.create() catch unreachable;
            self.head.* = TrieNode.new();
            return self;
        }

        pub fn add(self: *Self, data: []const u8) bool {
            var cur_node = self.head;
            for (data) |c| {
                const cc = c - norm;
                if (cur_node.children[cc] == null) {
                    const new_ptr = self.al.create() catch unreachable;
                    new_ptr.* = TrieNode.new();
                    cur_node.children[cc] = new_ptr;
                }
                // Value combine with add with is_end as a boolean
                if (!cur_node.val.add(false)) {
                    return false;
                }
                cur_node = cur_node.children[cc].?;
            }
            return cur_node.val.add(true); // Process the last missing node
        }

        /// Return the value and the size in data that we gone through,
        /// since we might not gone through the whole data.
        /// The last index is size - 1.
        pub fn get(self: Self, data: []const u8) struct { T, usize } {
            var cur_node = self.head;
            for (data, 0..) |c, i| {
                const cc = c - norm;
                if (cur_node.children[cc] == null) {
                    return .{ cur_node.val, i };
                }
                cur_node = cur_node.children[cc].?;
            }
            return .{ cur_node.val, data.len - 1 };
        }

        pub fn reset(self: *Self) void {
            self.al.reset();
            self.head = self.al.create() catch unreachable;
        }

        pub fn deinit(self: *Self) void {
            self.al.deinit();
        }
    };
}

/// Prefix trie node support count how many with this prefix and is full string
pub const PrefixTrieNodeType = struct {
    is_full_str: bool,
    prefix_count: u32,

    const Self = @This();

    pub fn init() Self {
        return Self{
            .is_full_str = false,
            .prefix_count = 0,
        };
    }

    pub fn add(self: *Self, is_end: bool) bool {
        self.prefix_count += 1;
        self.is_full_str |= is_end;
        return true;
    }
};

/// Assign each string with a unique hash (incremental counter).
/// You will have to manage the usize -> string mapping elsewhere.
pub const UniqueHashTrieNodeType = struct {
    var counter: usize = 0; // Global incremental counter

    value: usize,

    const Self = @This();

    pub fn init() Self {
        return Self{
            .value = 0,
        };
    }

    pub fn add(self: *Self, is_end: bool) bool {
        if (!is_end) {
            return true; // Doesn't do anything...
        }
        if (self.value == 0) {
            self.value = counter;
            counter += 1;
            return true;
        }
        return false;
    }
};

test "Trie test" {
    var trie = Trie(26, 'a', PrefixTrieNodeType).new();
    defer trie.deinit();
    _ = trie.add("abcd");
    var res = trie.get("ab");
    try std.testing.expect(res[0].is_full_str == false);
    try std.testing.expect(res[0].prefix_count == 1);
    try std.testing.expectEqual(1, res[1]);

    _ = trie.add("abcde");
    res = trie.get("abcd");
    try std.testing.expect(res[0].is_full_str == true);
    try std.testing.expect(res[0].prefix_count == 2);
    try std.testing.expectEqual(3, res[1]);
    res = trie.get("abcde");
    try std.testing.expect(res[0].is_full_str == true);
    try std.testing.expect(res[0].prefix_count == 1);
    try std.testing.expectEqual(4, res[1]);

    _ = trie.add("aa");
    res = trie.get("a");
    try std.testing.expect(res[0].is_full_str == false);
    try std.testing.expect(res[0].prefix_count == 3);
    try std.testing.expectEqual(0, res[1]);
}
