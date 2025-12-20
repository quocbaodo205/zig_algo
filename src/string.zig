/// String data structures and algorithm in zig.
const std = @import("std");

/// Trie, dynamic allocated with an allocator.
/// Anything pointer needs to be allocated.
pub fn Trie(
    child_num: comptime_int,
    norm: comptime_int,
    T: anytype,
    alloc: std.mem.Allocator,
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
        alloc: std.mem.Allocator,

        const Self = @This();

        pub fn new() !Self {
            const head_ptr = try alloc.create(TrieNode);
            head_ptr.* = TrieNode.new();
            return Self{
                .alloc = alloc,
                .head = head_ptr,
            };
        }

        pub fn add(self: *Self, data: []const u8) !void {
            var cur_node = self.head;
            for (data) |c| {
                const cc = c - norm;
                if (cur_node.children[cc] == null) {
                    const new_ptr = try self.alloc.create(TrieNode);
                    new_ptr.* = TrieNode.new();
                    cur_node.children[cc] = new_ptr;
                }
                // Value combine with add with is_end as a boolean
                cur_node.val.add(false); // Process the current node
                cur_node = cur_node.children[cc].?;
            }
            cur_node.val.add(true); // Process the last missing node
        }

        /// Return the value and the index in data that we gone through,
        /// since we might not gone through the whole data.
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
    };
}

/// Prefix trie node support count how many with this prefix and is full string
const PrefixTrieNodeType = struct {
    is_full_str: bool,
    prefix_count: u32,

    const Self = @This();

    pub fn init() Self {
        return Self{
            .is_full_str = false,
            .prefix_count = 0,
        };
    }

    pub fn add(self: *Self, is_end: bool) void {
        self.prefix_count += 1;
        self.is_full_str |= is_end;
    }
};

test "Trie test" {
    var trie = try Trie(26, 'a', PrefixTrieNodeType, std.heap.page_allocator).new();
    try trie.add("abcd");
    var res = trie.get("ab");
    try std.testing.expect(res[0].is_full_str == false);
    try std.testing.expect(res[0].prefix_count == 1);
    try std.testing.expectEqual(1, res[1]);

    try trie.add("abcde");
    res = trie.get("abcd");
    try std.testing.expect(res[0].is_full_str == true);
    try std.testing.expect(res[0].prefix_count == 2);
    try std.testing.expectEqual(3, res[1]);
    res = trie.get("abcde");
    try std.testing.expect(res[0].is_full_str == true);
    try std.testing.expect(res[0].prefix_count == 1);
    try std.testing.expectEqual(4, res[1]);

    try trie.add("aa");
    res = trie.get("a");
    try std.testing.expect(res[0].is_full_str == false);
    try std.testing.expect(res[0].prefix_count == 3);
    try std.testing.expectEqual(0, res[1]);
}
