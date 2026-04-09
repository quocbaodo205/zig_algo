const std = @import("std");

/// Persistent Array using a persistent segment tree (path copying).
/// Supports point get and set operations, with each set returning a new version.
/// Old versions remain valid and share unmodified nodes.
pub fn PersistentArray(comptime T: type) type {
    return struct {
        const Node = struct {
            left: ?*Node,
            right: ?*Node,
            value: T,
        };

        root: *Node,
        n: usize,
        allocator: std.mem.Allocator,

        const Self = @This();

        /// Create a new PersistentArray of size `n`, initialized to `init_value`.
        pub fn init(allocator: std.mem.Allocator, n: usize, init_value: T) !Self {
            const root = try build_rc(allocator, 0, n - 1, init_value);
            return Self{
                .root = root,
                .n = n,
                .allocator = allocator,
            };
        }

        /// Recursively build the initial segment tree.
        fn build_rc(allocator: std.mem.Allocator, l: usize, r: usize, init_value: T) !*Node {
            const node = try allocator.create(Node);
            node.* = .{
                .left = null,
                .right = null,
                .value = init_value,
            };
            if (l == r) {
                return node;
            }
            const mid = (l + r) / 2;
            node.left = try build_rc(allocator, l, mid, init_value);
            node.right = try build_rc(allocator, mid + 1, r, init_value);
            return node;
        }

        /// Get the value at index `i`.
        pub fn get(self: Self, i: usize) T {
            return get_rc(self.root, 0, self.n - 1, i);
        }

        /// Recursively get the value at index `i`.
        fn get_rc(node: *const Node, l: usize, r: usize, i: usize) T {
            if (l == r) {
                return node.value;
            }
            const mid = (l + r) / 2;
            if (i <= mid) {
                return get_rc(node.left.?, l, mid, i);
            } else {
                return get_rc(node.right.?, mid + 1, r, i);
            }
        }

        /// Set the value at index `i` to `value`, returning a new PersistentArray version.
        pub fn set(self: Self, i: usize, value: T) !Self {
            const new_root = try set_rc(self.allocator, self.root, 0, self.n - 1, i, value);
            return Self{
                .root = new_root,
                .n = self.n,
                .allocator = self.allocator,
            };
        }

        /// Recursively copy nodes and set the value at index `i`, returning the new root.
        fn set_rc(allocator: std.mem.Allocator, old_node: *const Node, l: usize, r: usize, i: usize, value: T) !*Node {
            // Create a copy of the current node
            const new_node = try allocator.create(Node);
            new_node.* = old_node.*;

            if (l == r) {
                // Update the value in the new leaf node
                new_node.value = value;
                return new_node;
            }

            const mid = (l + r) / 2;
            if (i <= mid) {
                // Recursively update left child, set new node's left to the new left child
                new_node.left = try set_rc(allocator, old_node.left.?, l, mid, i, value);
            } else {
                // Recursively update right child, set new node's right to the new right child
                new_node.right = try set_rc(allocator, old_node.right.?, mid + 1, r, i, value);
            }

            return new_node;
        }
    };
}

test "persistent array test" {
    const allocator = std.testing.allocator;

    // Initialize version 0: [10, 10, 10, 10, 10]
    var v0 = try PersistentArray(usize).init(allocator, 5, 10);
    defer {
        // Note: In a real application, we'd need a proper deallocator for all nodes,
        // but for testing with std.testing.allocator, leaks are reported at exit.
        // For simplicity, we skip deallocating here (but in real code, you should!).
    }

    // Check initial values
    try std.testing.expectEqual(@as(usize, 10), v0.get(0));
    try std.testing.expectEqual(@as(usize, 10), v0.get(1));
    try std.testing.expectEqual(@as(usize, 10), v0.get(2));
    try std.testing.expectEqual(@as(usize, 10), v0.get(3));
    try std.testing.expectEqual(@as(usize, 10), v0.get(4));

    // Create version 1: set index 1 to 20 → [10, 20, 10, 10, 10]
    const v1 = try v0.set(1, 20);
    try std.testing.expectEqual(@as(usize, 10), v0.get(1)); // v0 is unchanged!
    try std.testing.expectEqual(@as(usize, 20), v1.get(1));
    try std.testing.expectEqual(@as(usize, 10), v1.get(0));
    try std.testing.expectEqual(@as(usize, 10), v1.get(2));

    // Create version 2: set index 3 to 30 on v1 → [10, 20, 10, 30, 10]
    const v2 = try v1.set(3, 30);
    try std.testing.expectEqual(@as(usize, 10), v0.get(3)); // v0 unchanged
    try std.testing.expectEqual(@as(usize, 10), v1.get(3)); // v1 unchanged
    try std.testing.expectEqual(@as(usize, 30), v2.get(3));
    try std.testing.expectEqual(@as(usize, 20), v2.get(1));

    // Create version 3: set index 0 to 5 on v2 → [5, 20, 10, 30, 10]
    const v3 = try v2.set(0, 5);
    try std.testing.expectEqual(@as(usize, 10), v0.get(0));
    try std.testing.expectEqual(@as(usize, 10), v1.get(0));
    try std.testing.expectEqual(@as(usize, 10), v2.get(0));
    try std.testing.expectEqual(@as(usize, 5), v3.get(0));
}
