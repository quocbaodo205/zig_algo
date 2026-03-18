const std = @import("std");

/// Fenwick tree support single add and range sum.
/// T is suppose to be int type, panic otherwise:
/// - i32 for small prefix sum, possibly in frequency case (+-1)
/// - i64 for the rest
pub fn FWTree(n: comptime_int, comptime T: type) type {
    return struct {
        const Self = @This();

        bits: [n + 10]T,

        pub fn init() Self {
            switch (@typeInfo(T)) {
                .int => {
                    @branchHint(.likely);
                    return Self{
                        .bits = @splat(0),
                    };
                },
                else => {
                    @panic("Type not supported");
                },
            }
        }

        /// Sum range 0..=r
        pub fn prefix_sum(self: *const Self, r: usize) T {
            switch (@typeInfo(T)) {
                .int => {
                    @branchHint(.likely);
                    var rx: i32 = @intCast(r);
                    var ret: T = 0;
                    while (rx >= 0) {
                        ret += self.bits[@intCast(rx)];
                        rx = (rx & (rx + 1)) - 1;
                    }
                    return ret;
                },
                else => {
                    @panic("Type not supported");
                },
            }
        }

        /// Sum range l..=r
        pub fn sum(self: *const Self, l: usize, r: usize) T {
            switch (@typeInfo(T)) {
                .int => {
                    @branchHint(.likely);
                    return self.prefix_sum(r) - if (l == 0) 0 else self.prefix_sum(l - 1);
                },
                else => {
                    @panic("Type not supported");
                },
            }
        }

        pub fn add(self: *Self, i: usize, delta: T) void {
            switch (@typeInfo(T)) {
                .int => {
                    @branchHint(.likely);
                    var c = i;
                    while (c <= n) {
                        self.bits[c] += delta;
                        c = c | (c + 1);
                    }
                },
                else => {
                    @panic("Type not supported");
                },
            }
        }
    };
}

/// Can be viewed as a dynamic sorted array (increasing).
/// Can only handle value in range 0..=n.
pub fn OrderedStatisticSet(n: comptime_int) type {
    return struct {
        const Self = @This();

        fw_tree: FWTree(n, i32),

        pub fn init() Self {
            return Self{
                .fw_tree = FWTree(n, i32).init(),
            };
        }

        /// Increase freq of i by f. Input negative to decrease.
        pub fn inc_freq(self: *Self, i: usize, f: i32) void {
            self.fw_tree.add(i, f);
        }

        /// The minimun position (0-based) if you were to insert this in.
        /// Also answer: how many number in the set < i?
        pub fn min_order_of(self: *const Self, i: usize) usize {
            if (i == 0) {
                return 0;
            }
            return @intCast(self.fw_tree.prefix_sum(@intCast(i - 1)));
        }

        /// Get the number at order (position, 0-based) i.
        /// TODO: Fast case for not empty set?
        pub fn get_of_order(self: *const Self, i: usize) usize {
            // Idea: Binary search r until the prefix sum at r >= i, and prefix sum at (r-1) < i.
            // That mean i exist and it's the tipping point.
            var l: usize = 0;
            var r: usize = n;
            const ii32: i32 = @intCast(i + 1);
            while (l <= r) {
                const m = (l + r) / 2;
                const ps = self.fw_tree.prefix_sum(m);
                if (ps < ii32) {
                    l = m + 1;
                    continue;
                }
                const ps_1 = if (m == 0) 0 else self.fw_tree.prefix_sum(m - 1);
                if (ps_1 < ii32) {
                    return @intCast(m);
                }
                if (m == 0) {
                    return n + 1; // ??? failed case here somehow.
                }
                r = m - 1;
            }
            return n + 1;
        }

        /// Get the lower bound of x.
        pub fn lower_bound(self: *const Self, x: usize) usize {
            const order = self.min_order_of(x);
            return self.get_of_order(order);
        }
    };
}

test "Fenwick tree" {
    var fw = FWTree(100, i32).init();
    fw.add(5, 1);
    try std.testing.expectEqual(1, fw.sum(0, 5));
    try std.testing.expectEqual(1, fw.sum(5, 10));
    fw.add(1, 2);
    fw.add(3, -1);
    try std.testing.expectEqual(2, fw.sum(0, 1));
    try std.testing.expectEqual(1, fw.sum(1, 4));
    try std.testing.expectEqual(2, fw.sum(1, 5));
    try std.testing.expectEqual(0, fw.sum(2, 5));
}

test "Order set" {
    var oset = OrderedStatisticSet(100).init();
    oset.inc_freq(1, 1);
    oset.inc_freq(3, 2);
    oset.inc_freq(2, 1);
    try std.testing.expectEqual(1, oset.get_of_order(0));
    try std.testing.expectEqual(2, oset.get_of_order(1));
    try std.testing.expectEqual(3, oset.get_of_order(2));
    try std.testing.expectEqual(3, oset.get_of_order(3));
    oset.inc_freq(2, -1);
    try std.testing.expectEqual(1, oset.get_of_order(0));
    try std.testing.expectEqual(3, oset.get_of_order(1));
    try std.testing.expectEqual(3, oset.get_of_order(2));
    oset.inc_freq(2, 2);
    oset.inc_freq(15, 2);
    oset.inc_freq(0, 1);
    // [0,1,2,2,3,3,15,15]
    try std.testing.expectEqual(0, oset.get_of_order(0));
    try std.testing.expectEqual(1, oset.get_of_order(1));
    try std.testing.expectEqual(2, oset.get_of_order(2));
    try std.testing.expectEqual(2, oset.get_of_order(3));
    try std.testing.expectEqual(3, oset.get_of_order(4));
    try std.testing.expectEqual(3, oset.get_of_order(5));
    try std.testing.expectEqual(15, oset.get_of_order(6));
    try std.testing.expectEqual(15, oset.get_of_order(7));

    // Min order of test
    try std.testing.expectEqual(0, oset.min_order_of(0));
    try std.testing.expectEqual(1, oset.min_order_of(1));
    try std.testing.expectEqual(6, oset.min_order_of(4));
    try std.testing.expectEqual(6, oset.min_order_of(5));
    try std.testing.expectEqual(6, oset.min_order_of(15));
    try std.testing.expectEqual(8, oset.min_order_of(20));
    try std.testing.expectEqual(8, oset.min_order_of(100));

    // Test lower bound
    try std.testing.expectEqual(0, oset.lower_bound(0));
    try std.testing.expectEqual(1, oset.lower_bound(1));
    try std.testing.expectEqual(2, oset.lower_bound(2));
    try std.testing.expectEqual(3, oset.lower_bound(3));
    try std.testing.expectEqual(15, oset.lower_bound(5));
    try std.testing.expectEqual(15, oset.lower_bound(15));
}
