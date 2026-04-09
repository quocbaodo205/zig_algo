const std = @import("std");
const persistent_array = @import("persistent_array.zig");
const PersistentArray = persistent_array.PersistentArray;

/// Disjoint Set (Union-Find) data structure with path compression and union by size.
/// Nodes are 0-indexed.
pub fn DisjointSet(comptime max_n: comptime_int) type {
    return struct {
        parent: [max_n]usize,
        size: [max_n]usize,
        count: usize,

        const Self = @This();

        /// Create a new Disjoint Set with undefined arrays.
        pub fn new() Self {
            return Self{
                .parent = undefined,
                .size = undefined,
                .count = undefined,
            };
        }

        /// Initialize Disjoint Set with `n` elements (0 to n-1).
        pub fn init(self: *Self, n: usize) void {
            self.size = @splat(1);
            self.count = n;
            for (0..n) |i| {
                self.parent[i] = i;
            }
        }

        /// Find the root of node `u` with path compression.
        pub fn find(self: *Self, u: usize) usize {
            if (self.parent[u] != u) {
                self.parent[u] = self.find(self.parent[u]);
            }
            return self.parent[u];
        }

        /// Merge the sets containing `u` and `v`.
        /// Returns true if they were in different sets (a union happened), false otherwise.
        pub fn merge(self: *Self, u: usize, v: usize) bool {
            const root_u = self.find(u);
            const root_v = self.find(v);
            if (root_u == root_v) return false;

            // Union by size: attach smaller tree to larger tree's root
            if (self.size[root_u] < self.size[root_v]) {
                self.parent[root_u] = root_v;
                self.size[root_v] += self.size[root_u];
            } else {
                self.parent[root_v] = root_u;
                self.size[root_u] += self.size[root_v];
            }
            self.count -= 1;
            return true;
        }

        /// Check if nodes `u` and `v` are in the same set.
        pub fn sameSet(self: *Self, u: usize, v: usize) bool {
            return self.find(u) == self.find(v);
        }
    };
}

/// Persistent Disjoint Set (Union-Find) data structure with union by size.
/// Nodes are 0-indexed. Each merge returns a new version; old versions remain valid.
/// No path compression (to keep persistence simple and avoid modifying multiple nodes).
pub const PersistentDisjointSet = struct {
    parent: PersistentArray(usize),
    size: PersistentArray(usize),
    count: usize,
    n: usize,

    const Self = @This();

    /// Create and initialize a new PersistentDisjointSet with `n` elements (0 to n-1).
    pub fn init(gpa: std.mem.Allocator, n: usize) !Self {
        const parent = try PersistentArray(usize).init(gpa, n, 0);
        var initial_parent = parent;
        // Initialize parent[i] = i for 0..n-1
        for (0..n) |i| {
            initial_parent = try initial_parent.set(i, i);
        }
        const size = try PersistentArray(usize).init(gpa, n, 1);
        return Self{
            .parent = initial_parent,
            .size = size,
            .count = n,
            .n = n,
        };
    }

    /// Find the root of node `u` (without path compression to preserve persistence).
    pub fn find(self: Self, u: usize) usize {
        var current = u;
        while (self.parent.get(current) != current) {
            current = self.parent.get(current);
        }
        return current;
    }

    /// Merge the sets containing `u` and `v`.
    /// Returns a new version if they were in different sets, null otherwise.
    pub fn merge(self: Self, u: usize, v: usize) !?Self {
        const root_u = self.find(u);
        const root_v = self.find(v);
        if (root_u == root_v) return null;

        var new_parent = self.parent;
        var new_size = self.size;

        // Union by size: attach smaller tree to larger tree's root
        const size_u = self.size.get(root_u);
        const size_v = self.size.get(root_v);
        if (size_u < size_v) {
            new_parent = try new_parent.set(root_u, root_v);
            new_size = try new_size.set(root_v, size_u + size_v);
        } else {
            new_parent = try new_parent.set(root_v, root_u);
            new_size = try new_size.set(root_u, size_u + size_v);
        }

        return Self{
            .parent = new_parent,
            .size = new_size,
            .count = self.count - 1,
            .n = self.n,
        };
    }

    /// Check if nodes `u` and `v` are in the same set.
    pub fn sameSet(self: Self, u: usize, v: usize) bool {
        return self.find(u) == self.find(v);
    }
};

test "disjoint set test" {
    var dsu = DisjointSet(10).new();
    dsu.init(5);
    try std.testing.expect(dsu.count == 5);

    // Merge 0 and 1
    try std.testing.expect(dsu.merge(0, 1));
    try std.testing.expect(dsu.sameSet(0, 1));
    try std.testing.expect(!dsu.sameSet(0, 2));
    try std.testing.expect(dsu.count == 4);

    // Merge 2 and 3
    try std.testing.expect(dsu.merge(2, 3));
    try std.testing.expect(dsu.sameSet(2, 3));
    try std.testing.expect(dsu.count == 3);

    // Merge 1 and 3 (so 0-1-2-3)
    try std.testing.expect(dsu.merge(1, 3));
    try std.testing.expect(dsu.sameSet(0, 3));
    try std.testing.expect(dsu.sameSet(1, 2));
    try std.testing.expect(!dsu.sameSet(0, 4));
    try std.testing.expect(dsu.count == 2);

    // Merge already connected (should return false)
    try std.testing.expect(!dsu.merge(0, 2));
    try std.testing.expect(dsu.count == 2);
}

test "persistent disjoint set test" {
    const allocator = std.testing.allocator;
    // Create initial version
    const v0 = try PersistentDisjointSet.init(allocator, 5);
    try std.testing.expect(v0.count == 5);
    try std.testing.expect(!v0.sameSet(0, 1));
    try std.testing.expect(!v0.sameSet(2, 3));

    // Merge 0 and 1, get version 1
    const v1 = (try v0.merge(0, 1)).?;
    try std.testing.expect(v1.count == 4);
    try std.testing.expect(v1.sameSet(0, 1));
    try std.testing.expect(!v0.sameSet(0, 1)); // old version still valid

    // Merge 2 and 3 on v1, get version 2
    const v2 = (try v1.merge(2, 3)).?;
    try std.testing.expect(v2.count == 3);
    try std.testing.expect(v2.sameSet(2, 3));
    try std.testing.expect(!v1.sameSet(2, 3)); // v1 still valid

    // Merge 1 and 3 on v2, get version 3
    const v3 = (try v2.merge(1, 3)).?;
    try std.testing.expect(v3.count == 2);
    try std.testing.expect(v3.sameSet(0, 3));
    try std.testing.expect(v3.sameSet(1, 2));
    try std.testing.expect(!v2.sameSet(0, 3)); // v2 still valid

    // Try merging already connected, should return null
    try std.testing.expect((try v3.merge(0, 2)) == null);
}
