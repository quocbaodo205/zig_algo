const std = @import("std");

// =============================== Data structure in Zig ====================

/// Cartesian Tree node representation
pub const CartesianTreeNode = struct {
    parent: usize = 0,
    left: usize = 0,
    right: usize = 0,
};

/// Cartesian Tree struct, preallocated with max_n capacity
pub fn CartesianTree(comptime max_n: comptime_int) type {
    return struct {
        nodes: [max_n + 1]CartesianTreeNode = undefined,
        len: usize = 0,

        const Self = @This();

        /// Initialize empty Cartesian Tree
        pub fn init() Self {
            return Self{};
        }

        /// Build min-heap Cartesian Tree from an array of length at most max_n
        pub fn buildMin(self: *Self, comptime T: type, arr: []const T) void {
            self.len = arr.len;
            const n = self.len;

            // Initialize nodes
            for (0..n + 1) |i| {
                self.nodes[i] = .{ .parent = 0, .left = 0, .right = 0 };
            }

            for (1..n + 1) |i| {
                self.nodes[i].parent = i - 1;

                while (self.nodes[i].parent != 0 and arr[self.nodes[i].parent - 1] > arr[i - 1]) {
                    self.nodes[i].parent = self.nodes[self.nodes[i].parent].parent;
                }

                self.nodes[i].left = self.nodes[self.nodes[i].parent].right;
                self.nodes[self.nodes[i].parent].right = i;
                if (self.nodes[i].left != 0) {
                    self.nodes[self.nodes[i].left].parent = i;
                }
            }
        }

        /// Get slice of nodes 0..len+1 (1-based indexing)
        pub fn getNodes(self: *const Self) []const CartesianTreeNode {
            return self.nodes[0 .. self.len + 1];
        }
    };
}

/// Ring buffer (deque) with upfront max element it can hold.
/// Not growable and panic if not work
pub fn Deque(comptime T: type, max_n: comptime_int) type {
    return struct {
        arr: [max_n]?T,
        // [l..r)
        l: usize,
        r: usize,
        len: usize,

        const Self = @This();

        pub fn new() Self {
            return Self{
                // Remember this, very useful!
                .arr = [_]?T{null} ** max_n,
                .l = 0,
                .r = 0,
                .len = 0,
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
            self.len += 1;
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
            self.len -= 1;
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
            self.len += 1;
        }

        /// Pop return and element from the front of the deque
        pub fn pop_front(self: *Self) ?T {
            // Get at l and move l forward
            if (self.arr[self.l] == null) {
                return null;
            }
            const pop_data = self.arr[self.l].?;
            self.arr[self.l] = null;
            self.len -= 1;
            self.l += 1;
            if (self.l >= max_n) {
                self.l = 0;
            }
            return pop_data;
        }

        /// Print the queue.
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

// ====================================== UsizeSet based on std.Treap ==============================================

/// A set of usize values backed by std.Treap, using an allocator for node management.
pub const UsizeSet = struct {
    const Treap = std.Treap;
    const InnerTreap = Treap(usize, std.math.order);
    const Node = InnerTreap.Node;

    inner: InnerTreap = .{},
    gpa: std.mem.Allocator,
    len: usize = 0,

    const Self = @This();

    /// Initialize a new UsizeSet with the given allocator.
    pub fn init(gpa: std.mem.Allocator) Self {
        return Self{ .gpa = gpa };
    }

    /// Deinitialize the UsizeSet and free all allocated nodes.
    pub fn deinit(self: *Self) void {
        // Iterate through all nodes and free them
        var iter = self.inner.inorderIterator();
        while (iter.next()) |node| {
            self.gpa.destroy(node);
        }
        self.inner = .{};
        self.len = 0;
    }

    /// Add a value to the set. Does nothing if the value is already present.
    pub fn add(self: *Self, value: usize) !void {
        var entry = self.inner.getEntryFor(value);
        if (entry.node == null) {
            const new_node = try self.gpa.create(Node);
            entry.set(new_node);
            self.len += 1;
        }
    }

    /// Check if a value is present in the set.
    pub fn contains(self: *Self, value: usize) bool {
        return self.inner.getEntryFor(value).node != null;
    }

    /// Remove a value from the set. Does nothing if the value is not present.
    pub fn remove(self: *Self, value: usize) void {
        var entry = self.inner.getEntryFor(value);
        if (entry.node) |node| {
            entry.set(null);
            self.gpa.destroy(node);
            self.len -= 1;
        }
    }

    /// Get the number of elements in the set.
    pub fn getLen(self: *const Self) usize {
        return self.len;
    }

    /// Get the minimum value in the set. Returns null if the set is empty.
    pub fn getMin(self: *Self) ?usize {
        return if (self.inner.getMin()) |node| node.key else null;
    }

    /// Get the maximum value in the set. Returns null if the set is empty.
    pub fn getMax(self: *Self) ?usize {
        return if (self.inner.getMax()) |node| node.key else null;
    }

    /// Get the predecessor (previous smaller value) of the given value.
    /// Returns null if the value is not in the set or there is no predecessor.
    pub fn prev(self: *Self, value: usize) ?usize {
        const entry = self.inner.getEntryFor(value);
        if (entry.node) |node| {
            return if (node.prev()) |prev_node| prev_node.key else null;
        }
        return null;
    }

    /// Get the successor (next larger value) of the given value.
    /// Returns null if the value is not in the set or there is no successor.
    pub fn next(self: *Self, value: usize) ?usize {
        const entry = self.inner.getEntryFor(value);
        if (entry.node) |node| {
            return if (node.next()) |next_node| next_node.key else null;
        }
        return null;
    }

    /// In-order iterator over the set's values
    pub const Iterator = struct {
        inner: InnerTreap.InorderIterator,

        pub fn next(it: *Iterator) ?usize {
            return if (it.inner.next()) |node| node.key else null;
        }
    };

    /// Get an iterator over the set's values
    pub fn iterator(self: *Self) Iterator {
        return Iterator{ .inner = self.inner.inorderIterator() };
    }
};

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

test "std treap usage test" {
    const Treap = std.Treap;

    // Define treap type with i32 keys
    const MyTreap = Treap(i32, std.math.order);
    const MyNode = MyTreap.Node;

    var treap = MyTreap{};
    var nodes: [5]MyNode = undefined; // Pre-allocate nodes for testing

    // Test 1: Add elements
    const keys = [_]i32{ 5, 2, 8, 1, 3 };
    inline for (keys, 0..) |key, i| {
        var entry = treap.getEntryFor(key);
        try std.testing.expect(entry.node == null); // Should not exist yet
        entry.set(&nodes[i]);
        try std.testing.expect(entry.node != null); // Should exist now
        try std.testing.expect(entry.node.?.key == key);
    }

    // Test 2: Check if elements are in treap
    for (keys) |key| {
        const entry = treap.getEntryFor(key);
        try std.testing.expect(entry.node != null);
        try std.testing.expectEqual(key, entry.node.?.key);
    }

    // Test 3: Get min and max
    try std.testing.expect(treap.getMin() != null);
    try std.testing.expectEqual(@as(i32, 1), treap.getMin().?.key);
    try std.testing.expect(treap.getMax() != null);
    try std.testing.expectEqual(@as(i32, 8), treap.getMax().?.key);

    // Test 4: Remove an element
    {
        var entry = treap.getEntryFor(2);
        try std.testing.expect(entry.node != null);
        entry.set(null);
        // Check that it's removed
        const check_entry = treap.getEntryFor(2);
        try std.testing.expect(check_entry.node == null);
        // Check min/max are still correct
        try std.testing.expectEqual(@as(i32, 1), treap.getMin().?.key);
        try std.testing.expectEqual(@as(i32, 8), treap.getMax().?.key);
    }

    // Test 5: Remove min element
    {
        const min_node = treap.getMin().?;
        try std.testing.expectEqual(@as(i32, 1), min_node.key);
        var entry = treap.getEntryForExisting(min_node);
        entry.set(null);
        // New min should be 3
        try std.testing.expect(treap.getMin() != null);
        try std.testing.expectEqual(@as(i32, 3), treap.getMin().?.key);
    }
}

test "UsizeSet test" {
    var set = UsizeSet.init(std.testing.allocator);
    defer set.deinit();

    // Test add and contains
    try set.add(5);
    try set.add(2);
    try set.add(8);
    try set.add(1);
    try set.add(3);
    try std.testing.expect(set.contains(5));
    try std.testing.expect(set.contains(2));
    try std.testing.expect(!set.contains(10));

    // Test getMin and getMax
    try std.testing.expectEqual(@as(?usize, 1), set.getMin());
    try std.testing.expectEqual(@as(?usize, 8), set.getMax());

    // Test prev() and next()
    try std.testing.expectEqual(@as(?usize, null), set.prev(1)); // 1 is min
    try std.testing.expectEqual(@as(?usize, 1), set.prev(2));
    try std.testing.expectEqual(@as(?usize, 2), set.prev(3));
    try std.testing.expectEqual(@as(?usize, 3), set.prev(5));
    try std.testing.expectEqual(@as(?usize, 5), set.prev(8));
    try std.testing.expectEqual(@as(?usize, null), set.prev(10)); // 10 not in set

    try std.testing.expectEqual(@as(?usize, 2), set.next(1));
    try std.testing.expectEqual(@as(?usize, 3), set.next(2));
    try std.testing.expectEqual(@as(?usize, 5), set.next(3));
    try std.testing.expectEqual(@as(?usize, 8), set.next(5));
    try std.testing.expectEqual(@as(?usize, null), set.next(8)); // 8 is max
    try std.testing.expectEqual(@as(?usize, null), set.next(10)); // 10 not in set

    // Test add duplicate (should do nothing)
    try set.add(5);
    try std.testing.expect(set.contains(5));

    // Test remove
    set.remove(2);
    try std.testing.expect(!set.contains(2));
    try std.testing.expectEqual(@as(?usize, 1), set.getMin());
    try std.testing.expectEqual(@as(?usize, 8), set.getMax());

    // Test prev() and next() after removing 2
    try std.testing.expectEqual(@as(?usize, 1), set.prev(3));
    try std.testing.expectEqual(@as(?usize, 3), set.next(1));

    // Test remove min
    set.remove(1);
    try std.testing.expect(!set.contains(1));
    try std.testing.expectEqual(@as(?usize, 3), set.getMin());

    // Test prev() and next() after removing 1
    try std.testing.expectEqual(@as(?usize, null), set.prev(3));

    // Test remove max
    set.remove(8);
    try std.testing.expect(!set.contains(8));
    try std.testing.expectEqual(@as(?usize, 5), set.getMax());

    // Test prev() and next() after removing 8
    try std.testing.expectEqual(@as(?usize, null), set.next(5));
}

test "CartesianTree struct test" {
    const arr = [_]i32{ 3, 1, 4, 1, 5 };
    var ct = CartesianTree(5).init();
    ct.buildMin(i32, &arr);
    const tree = ct.getNodes();
    try std.testing.expect(tree.len == arr.len + 1);

    var root: usize = 0;
    for (1..tree.len) |i| {
        if (tree[i].parent == 0) {
            root = i;
            break;
        }
    }
    try std.testing.expect(root != 0);
    try std.testing.expectEqual(arr[root - 1], 1);
}
