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
