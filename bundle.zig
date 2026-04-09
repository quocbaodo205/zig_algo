const std = @import("std");
const allocator = struct {
const heap = std.heap;
const Allocator = std.mem.Allocator;
const Error = Allocator.Error;

/// Global heap + arena for quick various tasks.
/// Reset it yourself after each test cases.
pub var arena = heap.ArenaAllocator.init(heap.page_allocator);

/// Bump Allocator based on either fba + arena or page + arena depending on usage.
pub fn BumpAllo(num_elements: comptime_int, comptime T: type) type {
    const total_size = num_elements * @sizeOf(T);
    if (total_size > 0 and total_size <= 1000000) {
        // Stack allocation only with FBA
        return struct {
            const Self = @This();

            buffer: [total_size]u8,
            fba: heap.FixedBufferAllocator,
            arena: heap.ArenaAllocator,

            pub fn init() Self {
                var self = Self{
                    // Big chunk allocation on heap to avoid stack OOM.
                    .buffer = undefined,
                    .fba = undefined,
                    .arena = undefined,
                };
                self.fba = .init(&self.buffer);
                self.arena = .init(self.fba.allocator());
                return self;
            }

            pub fn reset(self: *Self) void {
                self.arena.reset(.retain_capacity);
            }

            pub fn create(self: *Self) Error!*T {
                return try self.arena.allocator().create(T);
            }

            pub fn deinit(self: *Self) void {
                self.arena.deinit();
            }

            pub fn allocator(self: *Self) Allocator {
                return self.arena.allocator();
            }
        };
    }
    return struct {
        const Self = @This();

        arena: heap.ArenaAllocator,

        pub fn init() Self {
            return Self{
                .arena = .init(heap.page_allocator),
            };
        }

        pub fn create(self: *Self) Error!*T {
            return self.arena.allocator().create(T);
        }

        pub fn reset(self: *Self) void {
            self.arena.reset(.retain_capacity);
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        pub fn allocator(self: *Self) Allocator {
            return self.arena.allocator();
        }
    };
}
};
const ds = struct {

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

};

const BUNDLE = false;

// ===================== Solving =====================

/// Disjoint Set (Union-Find) data structure with path compression and union by size.
/// Nodes are 0-indexed.
fn DisjointSet(comptime max_n: comptime_int) type {
    return struct {
        parent: [max_n]usize,
        size: [max_n]usize,
        black: [max_n]ds.UsizeSet,
        white: [max_n]ds.UsizeSet,
        count_black: usize,
        count_white: usize,
        gpa: std.mem.Allocator,

        const Self = @This();

        /// Create a new Disjoint Set with undefined arrays.
        pub fn new() Self {
            return Self{
                .parent = undefined,
                .size = undefined,
                .black = undefined,
                .white = undefined,
                .count_black = undefined,
                .count_white = undefined,
                .gpa = undefined,
            };
        }

        pub fn init(self: *Self, gpa: std.mem.Allocator, n: usize) !void {
            self.gpa = gpa;
            self.size = @splat(1);
            self.count_black = 0;
            self.count_white = 0; // Initially all are neutral
            for (0..n) |i| {
                self.parent[i] = i;
                self.black[i] = ds.UsizeSet.init(gpa);
                self.white[i] = ds.UsizeSet.init(gpa);
                // In the beginning, all nodes are neutral (not in black or white)
            }
        }

        pub fn find(self: *Self, u: usize) usize {
            if (self.parent[u] != u) {
                self.parent[u] = self.find(self.parent[u]);
            }
            return self.parent[u];
        }

        /// Merge the sets containing `u` and `v`.
        pub fn merge(self: *Self, u: usize, v: usize) !void {
            // std.debug.print("merge(u={}, v={})\n", .{ u, v });
            const root_u = self.find(u);
            const root_v = self.find(v);
            if (root_u == root_v) {
                // std.debug.print("  same root, skip\n", .{});
                return;
            }

            // Check colors of u and v
            const color_u = self.getColor(u);
            const color_v = self.getColor(v);
            // std.debug.print("  u's color: {}, v's color: {}\n", .{ color_u, color_v });

            // Handle both neutral case
            if (color_u == 3 and color_v == 3) {
                // std.debug.print("  both neutral: making u white, v black\n", .{});
                try self.white[root_u].add(u);
                try self.black[root_v].add(v);
                self.count_white += 1;
                self.count_black += 1;
            } else if (color_u == 3) {
                // std.debug.print("  u neutral: making u opposite of v\n", .{});
                if (color_v == 0) { // v is white, u becomes black
                    try self.black[root_u].add(u);
                    self.count_black += 1;
                } else { // v is black, u becomes white
                    try self.white[root_u].add(u);
                    self.count_white += 1;
                }
            } else if (color_v == 3) {
                // std.debug.print("  v neutral: making v opposite of u\n", .{});
                if (color_u == 0) { // u is white, v becomes black
                    try self.black[root_v].add(v);
                    self.count_black += 1;
                } else { // u is black, v becomes white
                    try self.white[root_v].add(v);
                    self.count_white += 1;
                }
            }

            // Now check if we need to flip colors of smaller set
            const new_color_u = self.getColor(u);
            const new_color_v = self.getColor(v);
            const need_flip = (new_color_u == new_color_v);
            // std.debug.print("  u's root: {}, color: {}\n", .{ root_u, new_color_u });
            // std.debug.print("  v's root: {}, color: {}\n", .{ root_v, new_color_v });
            // std.debug.print("  need_flip: {}\n", .{need_flip});

            // Determine which root is smaller
            const small_root: usize = if (self.size[root_u] < self.size[root_v]) root_u else root_v;
            const large_root: usize = if (self.size[root_u] < self.size[root_v]) root_v else root_u;
            // std.debug.print("  small_root: {}, large_root: {}\n", .{ small_root, large_root });

            // Flip colors of smaller root if needed
            if (need_flip) {
                // std.debug.print("  flipping colors for small_root {}\n", .{small_root});
                // Get current lengths before flipping
                const num_black = self.black[small_root].getLen();
                const num_white = self.white[small_root].getLen();
                // std.debug.print("  before flip: black={}, white={}\n", .{ num_black, num_white });

                // Update global counts
                self.count_black -= num_black;
                self.count_white -= num_white;
                self.count_black += num_white;
                self.count_white += num_black;
                // std.debug.print("  global counts after flip: black={}, white={}\n", .{ self.count_black, self.count_white });

                // Collect original black and white nodes first
                var black_list = try std.ArrayList(usize).initCapacity(self.gpa, self.black[small_root].getLen());
                defer black_list.deinit(self.gpa);
                var black_iter = self.black[small_root].iterator();
                while (black_iter.next()) |val| {
                    try black_list.append(self.gpa, val);
                }

                var white_list = try std.ArrayList(usize).initCapacity(self.gpa, self.white[small_root].getLen());
                defer white_list.deinit(self.gpa);
                var white_iter = self.white[small_root].iterator();
                while (white_iter.next()) |val| {
                    try white_list.append(self.gpa, val);
                }

                // Flip black to white
                for (black_list.items) |val| {
                    self.black[small_root].remove(val);
                    try self.white[small_root].add(val);
                }

                // Flip white to black
                for (white_list.items) |val| {
                    self.white[small_root].remove(val);
                    try self.black[small_root].add(val);
                }
            }

            // Union by size: attach smaller tree to larger tree's root
            self.parent[small_root] = large_root;
            self.size[large_root] += self.size[small_root];
            // std.debug.print("  merged small_root {} into large_root {}, new size: {}\n", .{ small_root, large_root, self.size[large_root] });

            // Merge the sets: move all elements from small_root's black/white to large_root's
            {
                var iter = self.black[small_root].iterator();
                var to_move = try std.ArrayList(usize).initCapacity(self.gpa, self.black[small_root].getLen());
                defer to_move.deinit(self.gpa);
                while (iter.next()) |val| {
                    try to_move.append(self.gpa, val);
                }
                // std.debug.print("  moving {} black nodes from {} to {}\n", .{ to_move.items.len, small_root, large_root });
                for (to_move.items) |val| {
                    self.black[small_root].remove(val);
                    try self.black[large_root].add(val);
                }
            }
            {
                var iter = self.white[small_root].iterator();
                var to_move = try std.ArrayList(usize).initCapacity(self.gpa, self.white[small_root].getLen());
                defer to_move.deinit(self.gpa);
                while (iter.next()) |val| {
                    try to_move.append(self.gpa, val);
                }
                // std.debug.print("  moving {} white nodes from {} to {}\n", .{ to_move.items.len, small_root, large_root });
                for (to_move.items) |val| {
                    self.white[small_root].remove(val);
                    try self.white[large_root].add(val);
                }
            }
        }

        pub fn sameSet(self: *Self, u: usize, v: usize) bool {
            return self.find(u) == self.find(v);
        }

        /// Get the color of index i: 3 for neutral, 0 for white, 1 for black
        pub fn getColor(self: *Self, i: usize) u2 {
            const root = self.find(i);
            if (self.white[root].contains(i)) {
                return 0;
            } else if (self.black[root].contains(i)) {
                return 1;
            } else {
                return 3; // neutral
            }
        }
    };
}

var dsu = DisjointSet(200000).new();

/// Main solving function for each test cases.
pub fn solve() !void {
    defer _ = allocator.arena.reset(.retain_capacity);
    const gpa = allocator.arena.allocator();

    const n = in.read(usize);
    const q = in.read(usize);

    try dsu.init(gpa, n);

    var is_dead = false;

    for (0..q) |_| {
        const u = in.read(usize) - 1;
        const v = in.read(usize) - 1;
        if (is_dead) {
            print("-1\n", .{});
            continue;
        }
        if (dsu.sameSet(u, v)) {
            if (dsu.getColor(u) == dsu.getColor(v)) {
                // No way to do anything.
                is_dead = true;
                print("-1\n", .{});
            } else {
                // Don't have to do anything if diff color
                print("{}\n", .{@min(dsu.count_black, dsu.count_white)});
            }
        } else {
            try dsu.merge(u, v);
            print("{}\n", .{@min(dsu.count_black, dsu.count_white)});
        }
    }
}

pub fn main() !void {
    if (BUNDLE) {
    } else {
        defer allocator.arena.deinit();
        // Support test cases reading.
        // const t = in.read(usize);
        // for (0..t) |_| {
        try solve();
        // }
        try writer.flush(); // Ending flush
    }
}

// ================================ Utils ===============================

// Sorting instruction:

/// Position of the first index i, so that arr[i] >= x
/// Sort first: std.mem.sort(u32, a.items, {}, comptime std.sort.asc(u32));
/// std.sort.lowerBound(u32, a.items, 3, comptime std.sort.asc(u32));

// ================================ IO ============================

// Definition for IO: Buffer and writer
var in = CPInput.init();
var inbuf: [100010]u8 = undefined;
var stdout_buffer: [100010]u8 = undefined;
var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
const writer = &stdout_writer.interface;

/// Output print that ignore errors to type faster
pub fn print(comptime fmt: []const u8, args: anytype) void {
    writer.print(fmt, args) catch unreachable;
}

/// Input processing for competitive programming. Read line by line and allow function to keep reading.
const CPInput = struct {
    buf: []u8,
    pos: usize,
    in: std.fs.File.Reader,

    const Self = @This();
    pub fn init() Self {
        return Self{
            .buf = undefined,
            .pos = 0,
            .in = std.fs.File.stdin().reader(&inbuf),
        };
    }

    /// Read the whole line with the '\n' at the end and return a slice.
    fn readLine(self: *Self) void {
        const data = self.in.interface.takeDelimiterInclusive('\n') catch unreachable;
        self.buf = data;
    }

    /// Advance the pos until it hit the delimiter (' ' and '\n').
    /// Automatically read a new line if no more data.
    fn take(self: *Self) []u8 {
        if (self.pos >= self.buf.len) {
            self.readLine();
            self.pos = 0;
        }
        const old_pos = self.pos;
        while (self.buf[self.pos] != ' ' and self.buf[self.pos] != '\n') {
            self.pos += 1;
        }
        // Here, at pos is a delimiter. Move pass it
        const last_pos = self.pos;
        while (self.pos < self.buf.len and ((self.buf[self.pos] == ' ') or (self.buf[self.pos] == '\n'))) {
            self.pos += 1;
        }
        return self.buf[old_pos..last_pos];
    }

    pub fn readString(self: *Self) []u8 {
        return self.take();
    }

    pub fn read(self: *Self, comptime T: type) T {
        const data = self.take();
        // Process the correct function for each type
        switch (@typeInfo(T)) {
            .int => {
                @branchHint(.likely);
                return std.fmt.parseInt(T, data, 10) catch unreachable;
            },
            .float => {
                return std.fmt.parseFloat(T, data) catch unreachable;
            },
            else => {
                @panic("Type not supported");
            },
        }
    }

    pub fn readBuffer(self: *Self, comptime T: type, arr_buffer: []T) void {
        // Process the correct function for each type
        switch (@typeInfo(T)) {
            .int => {
                @branchHint(.likely);
                for (0..arr_buffer.len) |i| {
                    const data = self.take();
                    arr_buffer[i] = std.fmt.parseInt(T, data, 10) catch unreachable;
                }
            },
            .float => {
                for (0..arr_buffer.len) |i| {
                    const data = self.take();
                    arr_buffer[i] = std.fmt.parseFloat(T, data, 10) catch unreachable;
                }
            },
            else => {
                @panic("Type not supported");
            },
        }
    }
};

