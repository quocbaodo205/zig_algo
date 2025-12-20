const std = @import("std");

// =============================== Data structure in Zig ====================

/// Ring buffer (deque) with upfront max element it can hold.
/// Not growable and panic if not work
pub fn Deque(comptime T: type, max_n: comptime_int) type {
    return struct {
        arr: [max_n]?T,
        // [l..r)
        l: usize,
        r: usize,

        const Self = @This();

        pub fn new() Self {
            return Self{
                // Remember this, very useful!
                .arr = [_]?T{null} ** max_n,
                .l = 0,
                .r = 0,
            };
        }

        pub fn front(self: *Self) ?T {
            return self.arr[self.l];
        }

        pub fn back(self: Self) ?T {
            const new_r = if (self.r == 0)
                max_n - 1
            else
                self.r - 1;
            return self.arr[new_r];
        }

        /// Push an element to the back of the deque
        pub fn push_back(self: *Self, element: *const T) void {
            // Add at r
            if (self.arr[self.r] != null) {
                @panic("Array filled and cannot add more element!");
            }
            self.arr[self.r] = element.*; // Deref to copy inside
            self.r += 1;
            if (self.r >= max_n) self.r = 0;
        }

        /// Pop return and element from the back of the deque
        pub fn pop_back(self: *Self) ?T {
            // Get at r-1 and move back
            const new_r = if (self.r == 0)
                max_n - 1
            else
                self.r - 1;
            if (self.arr[new_r] == null) {
                return null;
            }
            const pop_data = self.arr[new_r].?;
            self.arr[new_r] = null;
            self.r = new_r;
            return pop_data;
        }

        /// Push an element to the front of the deque
        pub fn push_front(self: *Self, element: *const T) void {
            // Add at l-1 and move l back
            const new_l = if (self.l == 0)
                max_n - 1
            else
                self.l - 1;
            if (self.arr[new_l] != null) {
                @panic("Array filled and cannot add more element!");
            }
            self.arr[new_l] = element.*; // Deref to copy inside
            self.l = new_l;
        }

        /// Pop return and element from the front of the deque
        pub fn pop_front(self: *Self) ?T {
            // Get at l and move l forward
            if (self.arr[self.l] == null) {
                return null;
            }
            const pop_data = self.arr[self.l].?;
            self.arr[self.l] = null;
            self.l += 1;
            if (self.l >= max_n) {
                self.l = 0;
            }
            return pop_data;
        }

        /// Print the queue
        pub fn print(self: Self) void {
            std.debug.print("[", .{});
            var i = self.l;
            while (i != self.r) {
                std.debug.print("{any}, ", .{self.arr[i].?});
                i += 1;
                if (i == max_n) i = 0;
            }
            std.debug.print("]\n", .{});
        }
    };
}

test "deque test" {
    var q = Deque(usize, 10).new();
    q.push_front(&1); // 1
    try std.testing.expect(q.front() == 1);
    try std.testing.expect(q.back() == 1);
    var t: usize = 2;
    q.push_back(&t); // 1, 2
    try std.testing.expect(q.front() == 1);
    try std.testing.expect(q.back() == 2);
    q.push_front(&3); // 3, 1, 2
    try std.testing.expect(q.front() == 3);
    try std.testing.expect(q.back() == 2);
    try std.testing.expect(q.pop_back().? == 2);
    try std.testing.expect(q.pop_back().? == 1);
    try std.testing.expect(q.pop_back().? == 3);
    try std.testing.expect(q.pop_back() == null);
}

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
