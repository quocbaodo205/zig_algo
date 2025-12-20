/// Segment tree data structure + sample problems
const std = @import("std");

/// Simple SegmentTree for anytype T that support ID and OP, data range 0..n
/// Stack allocated fix size data
pub fn SegmentTree(n: comptime_int, T: anytype) type {
    return struct {
        tree: [4 * n + 5]T, // With some buffer data

        const Self = @This();

        pub fn new() Self {
            return Self{
                .tree = [_]T{T.id()} ** (4 * n + 5),
            };
        }

        fn from_array_rc(self: *Self, node: usize, l: usize, r: usize, arr: []T) void {
            if (l == r) {
                // Set this node
                self.tree[node] = arr[l];
                return;
            }
            const mid = (l + r) / 2;
            // 2 segment: l..mid and mid..=r
            if (l <= mid) {
                self.from_array_rc(node * 2, l, mid, arr);
            }
            if (mid + 1 <= r) {
                self.from_array_rc(node * 2 + 1, mid + 1, r, arr);
            }
            // Combine 2 half using op
            self.tree[node] = self.tree[node * 2].op(&self.tree[node * 2 + 1]);
        }

        pub fn from_array(arr: []T) Self {
            var tree = Self.new();
            tree.from_array_rc(1, 0, n - 1, arr);
            return tree;
        }

        /// Recursive set node -> range [l..=r], index i = val
        /// i always in the range.
        /// Node start at 1 to allow * 2 and *2+1.
        fn set_rc(self: *Self, node: usize, l: usize, r: usize, i: usize, val: *const T) void {
            if (l == i and r == i) {
                // Set this node
                self.tree[node] = val.*;
                return;
            }
            const mid = (r + l) / 2;
            // 2 segment: l..mid and mid..=r
            if (i <= mid) {
                self.set_rc(node * 2, l, mid, i, val);
            }
            if (i >= mid + 1) {
                self.set_rc(node * 2 + 1, mid + 1, r, i, val);
            }
            // Combine 2 half using op
            self.tree[node] = self.tree[node * 2].op(&self.tree[node * 2 + 1]);
        }

        /// Set i to a value. Assume in range
        pub fn set(self: *Self, i: usize, val: *const T) void {
            self.set_rc(1, 0, n - 1, i, val);
        }

        /// Recursive get node -> range [l..=r], index i
        /// i always in the range.
        /// Node start at 1 to allow * 2 and *2+1.
        fn get_rc(self: *Self, node: usize, l: usize, r: usize, i: usize) T {
            if (l == i and r == i) {
                // Set this node
                return self.tree[node];
            }
            const mid = (r + l) / 2;
            // 2 segment: l..mid and mid..=r
            if (i <= mid) {
                return self.get_rc(node * 2, l, mid, i);
            }
            return self.get_rc(node * 2 + 1, mid + 1, r, i);
        }

        /// Get the value at i. Assume in range
        pub fn get(self: *Self, i: usize) T {
            return self.get_rc(1, 0, n - 1, i);
        }

        /// Recursive get node -> range [l..=r], from [ql..=qr]
        /// If not in range, return the ID
        fn fold_rc(self: *Self, node: usize, l: usize, r: usize, ql: usize, qr: usize) T {
            if (l == ql and r == qr) {
                // Set this node
                return self.tree[node];
            }
            // Out of range
            if (r < ql or qr < l or qr < ql) {
                return T.id();
            }
            const mid = (r + l) / 2;
            const lres = self.fold_rc(node * 2, l, mid, ql, @min(mid, qr));
            const rres = self.fold_rc(node * 2 + 1, mid + 1, r, @max(mid + 1, ql), qr);
            return lres.op(&rres);
        }

        pub fn fold(self: *Self, ql: usize, qr: usize) T {
            return self.fold_rc(1, 0, n - 1, ql, qr);
        }
    };
}

const SingleSetRangeMax: type = struct {
    max: i32,

    const Self = @This();

    pub fn id() Self {
        return Self{
            .max = -1e9,
        };
    }

    // ops style accept the 2
    pub fn op(lhs: *const Self, rhs: *const Self) Self {
        return Self{
            .max = @max(lhs.max, rhs.max),
        };
    }
};

test "Segment Tree" {
    var init_arr: [10]SingleSetRangeMax = undefined;
    for (0..10) |i| {
        init_arr[i] = SingleSetRangeMax{
            .max = 0,
        };
    }
    var tree = SegmentTree(10, SingleSetRangeMax).from_array(init_arr[0..10]);

    tree.set(1, &SingleSetRangeMax{ .max = 100 });
    var res = tree.get(1);
    try std.testing.expectEqual(100, res.max);
    res = tree.get(8);
    try std.testing.expectEqual(0, res.max);
    try std.testing.expectEqual(100, tree.fold(0, 2).max);
    try std.testing.expectEqual(0, tree.fold(2, 9).max);

    tree.set(5, &SingleSetRangeMax{ .max = 7 });
    try std.testing.expectEqual(7, tree.get(5).max);
    try std.testing.expectEqual(0, tree.get(0).max);
    try std.testing.expectEqual(100, tree.get(1).max);
    try std.testing.expectEqual(100, tree.fold(0, 5).max);
    try std.testing.expectEqual(7, tree.fold(3, 5).max);
    try std.testing.expectEqual(0, tree.fold(6, 9).max);

    tree.set(1, &SingleSetRangeMax{ .max = 3 });
    try std.testing.expectEqual(7, tree.get(5).max);
    try std.testing.expectEqual(0, tree.get(0).max);
    try std.testing.expectEqual(3, tree.get(1).max);
    try std.testing.expectEqual(7, tree.fold(0, 5).max);
    try std.testing.expectEqual(7, tree.fold(3, 5).max);
    try std.testing.expectEqual(0, tree.fold(6, 9).max);
}
