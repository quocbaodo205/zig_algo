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
const string = struct {
/// String data structures and algorithm in zig.

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

};
const graph = struct {

/// Graph with dynamic node type. Underline is an adj list of usize.
/// If node type is of Int, directly store inside the [n]ArrayList.
/// If node type is of anything else, it need to provide a bidirectional hash function <-> int.
/// Allocator: Local arena + heap based bump allocator. Free or reset after each use.
pub fn Graph(comptime node_type: type, comptime weight_type: type, max_n: comptime_int, comptime is_bidirectional: comptime_int) type {
    _ = &is_bidirectional; // Ensure the parameter is considered used
    return struct {
        // Data structure for outside edge: an edge from u -w> v
        pub const E = struct {
            u: node_type,
            v: node_type,
            w: weight_type,
        };

        // Data structure for a graph edge
        pub const GE = struct {
            v: usize,
            w: weight_type,
        };

        g: [max_n]std.ArrayList(GE),
        gpa: std.mem.Allocator,
        n: usize,

        const Self = @This();

        pub fn new() Self {
            return Self{
                .g = undefined,
                .gpa = undefined,
                .n = undefined,
            };
        }

        pub fn init(self: *Self, gpa: std.mem.Allocator, n: usize) void {
            self.gpa = gpa;
            self.n = n;
            for (0..n) |i| {
                self.g[i] = std.ArrayList(GE).initCapacity(gpa, 100) catch unreachable;
            }
        }

        pub fn fromEdgesUnweighted(self: *Self, gpa: std.mem.Allocator, n: usize, edges: []const E) Self {
            @branchHint(.likely);
            self.init(gpa, n);
            // All array list is of different memory so cannot @splat.
            switch (@typeInfo(node_type)) {
                .int => {
                    for (edges) |e| {
                        self.g[e.u].append(gpa, GE{
                            .v = e.v,
                            .w = e.w,
                        }) catch unreachable;
                        if (is_bidirectional > 0) {
                            self.g[e.v].append(gpa, GE{
                                .v = e.u,
                                .w = e.w,
                            }) catch unreachable;
                        }
                    }
                },
                else => {
                    // Need to provide a hash / unhash function
                    for (edges) |e| {
                        self.g[e.u.hash()].append(gpa, GE{
                            .v = e.v.hash(),
                            .w = e.w,
                        }) catch unreachable;
                        if (is_bidirectional > 0) {
                            self.g[e.v.hash()].append(gpa, GE{
                                .v = e.u.hash(),
                                .w = e.w,
                            }) catch unreachable;
                        }
                    }
                },
            }
            return self.*;
        }

        /// When using the graph, assume all usize. Convert outside via hash / unhash should needed.
        pub fn get(self: *const Self, u: usize) []GE {
            @branchHint(.likely);
            return self.g[u].items;
        }

        /// Reset state but keep allocated memory for multiple test cases usage.
        pub fn reset(self: *Self) void {
            for (0..self.n) |i| {
                self.g[i].shrinkRetainingCapacity(0);
            }
        }

        pub fn deinit(self: *Self) void {
            for (0..self.n) |i| {
                self.g[i].deinit(self.gpa);
            }
        }

        // ================================ Classic algo =================

        /// Minimal DFS from a vertex. Create a DFS struct that contains needed infomation.
        /// Probably never use, usually for reference only so we can expand.
        pub fn makeDFS() type {
            @branchHint(.cold);
            return struct {
                pub var used: [max_n]bool = @splat(false);

                pub fn dfs(u: usize, g: *const Self) void {
                    used[u] = true;
                    for (g.get(u)) |*ge| {
                        if (!used[ge.v]) {
                            dfs(ge.v, g);
                        }
                    }
                }
            };
        }

        /// Minimal BFS from starting vertices. Create a BFS struct that contains needed infomation.
        pub fn makeBFS() type {
            return struct {
                var q = ds.Deque(usize, max_n * 10).new();
                var in_queue: [max_n]bool = @splat(false);
                var distance: [max_n]u32 = @splat(1000000000);

                fn bfs(starts: []const usize, g: *const Self) void {
                    for (starts) |u| {
                        q.push_back(&u);
                        distance[u] = 0;
                        in_queue[u] = true;
                    }
                    while (true) {
                        if (q.pop_front()) |u| {
                            for (g.get(u)) |*ge| {
                                if (in_queue[ge.v]) {
                                    continue;
                                }
                                distance[ge.v] = distance[u] + 1;
                                q.push_back(&ge.v);
                                in_queue[ge.v] = true;
                            }
                        } else {
                            break;
                        }
                    }
                }
            };
        }

        /// Minimal topo structures. `order` should be get after calling `getTopoOrder`.
        pub fn makeTopo() type {
            return struct {
                var state: [max_n]u8 = @splat(0);
                // Reverse order is the topo order
                pub var order: [max_n]usize = undefined;
                pub var size: usize = 0;

                fn dfs(u: usize, g: *const Self) bool {
                    state[u] = 1;
                    for (g.get(u)) |*ge| {
                        if (state[ge.v] == 1) {
                            // Loop found
                            return false;
                        }
                        if (state[ge.v] == 0) {
                            // New vertext
                            const child_dfs_res = dfs(ge.v, g);
                            if (!child_dfs_res) {
                                return false;
                            }
                        }
                    }
                    state[u] = 2;
                    order[size] = u;
                    size += 1;
                    return true;
                }

                /// Recursively get the topological order.
                /// The `order` is the reverse order.
                pub fn getTopoOrder(g: *const Self) bool {
                    for (0..g.n) |u| {
                        if (state[u] == 0) {
                            const dfs_res = dfs(u, g);
                            if (!dfs_res) {
                                return false;
                            }
                        }
                    }
                    return true;
                }
            };
        }
    };
}

// ================================== Vertex types ==============================

/// GridPoint that provide hash / unhash to be able to use as a graph.
/// y = 0..m
pub fn GridPoint(m: comptime_int) type {
    return struct {
        x: usize,
        y: usize,

        const Self = @This();

        pub fn hash(self: *const Self) usize {
            return self.x * m + self.y;
        }

        pub fn unhash(u: usize) Self {
            return Self{
                .x = u / m,
                .y = u % m,
            };
        }
    };
}

/// Simple string hashing via counting with Trie.
/// To use, first you need to add all needed strings.
pub const StringVertices = struct {
    const uth = string.UniqueHashTrieNodeType;
    const trie_type = string.Trie(128, 0, uth);

    all_str: [1000][]const u8, // Mapping from pos to string
    trie: trie_type,
    al: allocator.BumpAllo(0, []const u8),
    cur_mx: usize,

    const Self = @This();

    pub const StringVertex = struct {
        str: []const u8,
        parent: *Self,

        const Inner = @This();

        pub fn hash(self: *const Inner) usize {
            const res = self.parent.trie.get(self.str);
            if (res[1] == self.str.len - 1) {
                return res[0].value;
            }
            return 1000000000; // LOL???
        }
    };

    pub fn new() Self {
        var self = Self{
            .al = .init(),
            .all_str = undefined,
            .trie = trie_type.new(),
            .cur_mx = 0,
        };
        self.all_str = undefined;

        return self;
    }

    pub fn newStrV(self: *Self, data: []const u8) StringVertex {
        return StringVertex{
            .str = data,
            .parent = self,
        };
    }

    pub fn add(self: *Self, data: []const u8) void {
        if (self.trie.add(data)) {
            const v = self.trie.get(data)[0].value;
            self.all_str[v] = data;
            self.cur_mx = v + 1;
        }
    }

    pub fn unhash(self: *const Self, u: usize) StringVertex {
        return StringVertex{
            .str = self.all_str[u],
            .parent = undefined, // No need
        };
    }

    pub fn deinit(self: *Self) void {
        self.al.deinit();
        self.trie.deinit();
    }
};

// =============================== Usage as test =======================

};
const utils = struct {

/// Compute (base^exponent) mod modu using binary exponentiation (comptime)
pub fn pow_mod_comptime(base: comptime_int, exponent: comptime_int, modu: comptime_int) comptime_int {
    var b = base % modu;
    var res: comptime_int = 1;
    var e = exponent;
    while (e > 0) {
        if (e & 1 == 1) res = res * b % modu;
        b = b * b % modu;
        e >>= 1;
    }
    return res;
}

/// Compute (base^exponent) mod modu using binary exponentiation (runtime)
pub fn pow_mod(base: usize, exponent: usize, modu: usize) usize {
    var b = base % modu;
    var res: usize = 1;
    var e = exponent;
    while (e > 0) {
        if (e & 1 == 1) res = (res * b) % modu;
        b = (b * b) % modu;
        e >>= 1;
    }
    return res;
}

/// Compute (base^exponent) mod modu using binary exponentiation (runtime, uses u128 for intermediate multiplications)
pub fn pow_mod_big(base: u64, exponent: u64, modu: u64) u64 {
    var b = base % modu;
    var res: u64 = 1;
    var e = exponent;
    while (e > 0) {
        if (e & 1 == 1) {
            res = @intCast((@as(u128, res) * @as(u128, b)) % @as(u128, modu));
        }
        b = @intCast((@as(u128, b) * @as(u128, b)) % @as(u128, modu));
        e >>= 1;
    }
    return res;
}

/// Compute modular inverse using extended Euclidean algorithm (comptime)
pub fn mod_inverse_comptime(a: comptime_int, m: comptime_int) comptime_int {
    var m_val = m;
    var a_val = a;
    var y: comptime_int = 0;
    var x: comptime_int = 1;
    while (a_val > 1) {
        const q = a_val / m_val;
        const t = m_val;
        m_val = a_val % m_val;
        a_val = t;
        const ty = y;
        y = x - q * y;
        x = ty;
    }
    if (x < 0) x += m;
    return x;
}

/// Compute modular inverse using Fermat's Little Theorem (runtime)
/// Only valid when modu is prime.
/// inv(x) = x^(modu-2) mod modu
pub fn mod_inverse(x: usize, modu: usize) usize {
    return pow_mod(x, modu - 2, modu);
}

};
const modint = struct {

/// Montgomery modular integer for a given prime modulus MOD (32-bit, uses R=2^32 with u64 intermediates)
pub fn MontgomeryModint(comptime MOD_ARG: u32) type {
    // Precompute R2_mod_M = (2^64) mod MOD_ARG (since R = 2^32, R² = 2^64)
    const R2_mod_M: u32 = @intCast(utils.pow_mod_comptime(2, 64, MOD_ARG));
    const niv: u32 = blk: {
        // Find M' such that MOD * M' ≡ -1 (mod 2^32), then niv = -M'
        // Use Newton's method: x_{k+1} = x_k * (2 - MOD * x_k) mod 2^32
        var x: u32 = 1; // Initial guess
        var i: usize = 0;
        while (i < 5) : (i += 1) { // 5 iterations are enough for 32 bits
            x = x *% (2 -% MOD_ARG *% x);
        }
        break :blk -%x;
    };

    return struct {
        pub const MOD = MOD_ARG;
        val: u32,

        const Self = @This();

        /// Convert a normal integer to Montgomery form
        pub fn fromInt(n: u32) Self {
            return Self{ .val = montgomeryMult(n, R2_mod_M) };
        }

        /// Convert back to a normal integer
        pub fn toInt(self: Self) u32 {
            return montgomeryMult(self.val, 1);
        }

        /// Montgomery multiplication: (a * b) / R mod MOD, R=2^32
        inline fn montgomeryMult(a: u32, b: u32) u32 {
            const t: u64 = @as(u64, a) * b;
            const t_low: u32 = @as(u32, @truncate(t));
            const m: u32 = t_low *% niv;
            const t_plus_m_times_MOD: u64 = t + @as(u64, m) * MOD_ARG;
            const u: u32 = @intCast(t_plus_m_times_MOD >> 32);
            return if (u >= MOD_ARG) u - MOD_ARG else u;
        }

        pub fn add(a: Self, b: Self) Self {
            const s: u64 = @as(u64, a.val) + b.val;
            return Self{ .val = @intCast(if (s >= MOD_ARG) s - MOD_ARG else s) };
        }

        pub fn sub(a: Self, b: Self) Self {
            const d: u64 = @as(u64, a.val) + MOD_ARG - b.val;
            return Self{ .val = @intCast(if (d >= MOD_ARG) d - MOD_ARG else d) };
        }

        pub fn mul(a: Self, b: Self) Self {
            return Self{ .val = montgomeryMult(a.val, b.val) };
        }

        pub fn neg(a: Self) Self {
            return Self{ .val = if (a.val == 0) 0 else MOD_ARG - a.val };
        }

        pub fn inv(a: Self) Self {
            // Fermat's little theorem: inv(x) = x^(MOD-2) mod MOD
            const exponent = MOD_ARG - 2;
            var result = Self.fromInt(1);
            var base = a;
            var e = exponent;
            while (e > 0) {
                if (e & 1 == 1) {
                    result = result.mul(base);
                }
                base = base.mul(base);
                e >>= 1;
            }
            return result;
        }
    };
}

/// Predefined MontgomeryModint for modulus 998244353 (32-bit, preferred)
pub const Modint998244353 = MontgomeryModint(998244353);
/// Predefined MontgomeryModint for modulus 1000000007 (32-bit, common in programming contests)
pub const Modint1000000007 = MontgomeryModint(1000000007);

};
const ntt = struct {

/// NTT (Number Theoretic Transform) and convolution utilities for a given Modint type
/// Requires: Modint.MOD is a prime such that Modint.MOD = c * 2^k + 1 for some c, k
pub fn NttHelpers(comptime ModintType: type, comptime ntt_root: comptime_int) type {
    const ntt_mod = ModintType.MOD;

    // Precompute the maximum log2 we might need (for MOD=998244353, it's 23)
    const max_log: usize = blk: {
        var m = ntt_mod - 1;
        var k: usize = 0;
        while (m % 2 == 0) : (m /= 2) k += 1;
        break :blk k;
    };

    // Precompute roots and inverse roots
    const roots = blk: {
        var res: [max_log + 1]ModintType = undefined;
        res[max_log] = ModintType.fromInt(@intCast(utils.pow_mod_comptime(ntt_root, (ntt_mod - 1) >> max_log, ntt_mod)));
        var i = max_log;
        while (i > 0) : (i -= 1) {
            res[i - 1] = res[i].mul(res[i]);
        }
        break :blk res;
    };
    const inv_roots = blk: {
        var res: [max_log + 1]ModintType = undefined;
        res[max_log] = ModintType.fromInt(@intCast(utils.mod_inverse_comptime(utils.pow_mod_comptime(ntt_root, (ntt_mod - 1) >> max_log, ntt_mod), ntt_mod)));
        var i = max_log;
        while (i > 0) : (i -= 1) {
            res[i - 1] = res[i].mul(res[i]);
        }
        break :blk res;
    };

    return struct {
        /// Compute bit-reversed permutation of indices (optimized)
        fn bit_reverse(i: usize, log_n: usize) usize {
            if (log_n == 0) {
                return i;
            }
            return @bitReverse(@as(u64, @intCast(i))) >> (@as(u6, @intCast(64 - log_n)));
        }

        /// Compute NTT (Number Theoretic Transform) in place
        /// a must have length a power of two
        pub fn ntt_f(a: []ModintType, invert: bool) void {
            const n = a.len;
            if (n == 1) {
                // Nothing to do for single element
                if (invert) {
                    // inv_n is 1/1 = 1, so no change
                }
                return;
            }
            const log_n = std.math.log2_int(usize, n);

            // Bit-reverse permutation
            for (0..n) |i| {
                const j = bit_reverse(i, log_n);
                if (i < j) {
                    std.mem.swap(ModintType, &a[i], &a[j]);
                }
            }

            // NTT main loop with precomputed roots
            var k: usize = 1;
            var log_k: usize = 1; // since k starts at 1=2^0, len=2k=2^1
            while (k < n) {
                const wlen = if (invert) inv_roots[log_k] else roots[log_k];
                var i: usize = 0;
                while (i < n) : (i += 2 * k) {
                    var w = ModintType.fromInt(1);
                    var j: usize = 0;
                    while (j < k) : (j += 1) {
                        const u = a[i + j];
                        const v = a[i + j + k].mul(w);
                        a[i + j] = u.add(v);
                        a[i + j + k] = u.sub(v);
                        w = w.mul(wlen);
                    }
                }
                k <<= 1;
                log_k += 1;
            }

            // Inverse NTT scaling
            if (invert) {
                const inv_n = ModintType.fromInt(@intCast(utils.mod_inverse(n, ntt_mod)));
                for (0..n) |i| {
                    a[i] = a[i].mul(inv_n);
                }
            }
        }

        /// Compute convolution of a and b modulo MOD using NTT
        /// Returns a new slice with the result, caller must free with gpa
        pub fn convolution(gpa: std.mem.Allocator, a: []const ModintType, b: []const ModintType) ![]ModintType {
            var n: usize = 1;
            const required_len = a.len + b.len - 1;
            while (n < required_len) n <<= 1;

            var fa = try gpa.alloc(ModintType, n);
            defer gpa.free(fa);
            var fb = try gpa.alloc(ModintType, n);
            defer gpa.free(fb);

            @memset(fa, ModintType.fromInt(0));
            @memset(fb, ModintType.fromInt(0));
            @memcpy(fa[0..a.len], a);
            @memcpy(fb[0..b.len], b);

            ntt_f(fa, false);
            ntt_f(fb, false);

            for (0..n) |i| {
                fa[i] = fa[i].mul(fb[i]);
            }

            ntt_f(fa, true);

            const result = try gpa.alloc(ModintType, required_len);
            @memcpy(result, fa[0..required_len]);
            return result;
        }
    };
}

/// Predefined NTT helpers for common modulus 998244353 (primitive root 3)
pub const Ntt998244353 = NttHelpers(modint.Modint998244353, 3);

};
const fps = struct {

/// Helper to create FPS struct with given options
fn FpsImpl(comptime ModintType: type, comptime use_ntt: bool, comptime fps_root: ?comptime_int) type {
    return struct {
        coeffs: []ModintType,
        gpa: std.mem.Allocator,

        const Self = @This();

        // NTT helpers (only if use_ntt is true)
        const Ntt = if (use_ntt) ntt.NttHelpers(ModintType, fps_root.?) else void;
        const MOD = ModintType.MOD;

        // Precompute inverses of 1..max_n (lazy, on first use)
        var inv_cache: []ModintType = &.{};
        var inv_cache_gpa: ?std.mem.Allocator = null;

        /// Initialize an FPS with all zeros
        pub fn init(gpa: std.mem.Allocator, max_degree: usize) !Self {
            const coeffs = try gpa.alloc(ModintType, max_degree + 1);
            @memset(coeffs, ModintType.fromInt(0));
            return Self{
                .coeffs = coeffs,
                .gpa = gpa,
            };
        }

        /// Create FPS from a slice of coefficients
        pub fn fromSlice(gpa: std.mem.Allocator, coeffs: []const ModintType, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            const copy_len = @min(coeffs.len, result.coeffs.len);
            @memcpy(result.coeffs[0..copy_len], coeffs[0..copy_len]);
            return result;
        }

        /// Create the zero polynomial (all coefficients zero)
        pub fn zero(gpa: std.mem.Allocator, max_degree: usize) !Self {
            return try Self.init(gpa, max_degree);
        }

        /// Create the polynomial 1 (constant term 1, others zero)
        pub fn one(gpa: std.mem.Allocator, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            if (result.coeffs.len > 0) {
                result.coeffs[0] = ModintType.fromInt(1);
            }
            return result;
        }

        /// Create the polynomial with all coefficients 1: 1 + x + x² + ... + x^max_degree
        pub fn allOnes(gpa: std.mem.Allocator, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            for (0..result.coeffs.len) |i| {
                result.coeffs[i] = ModintType.fromInt(1);
            }
            return result;
        }

        /// Create the polynomial with coefficients 0,1,2,...,max_degree: 0 + 1x + 2x² + ... + max_degree x^max_degree
        pub fn increasing(gpa: std.mem.Allocator, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            for (0..result.coeffs.len) |i| {
                result.coeffs[i] = ModintType.fromInt(@intCast(i));
            }
            return result;
        }

        /// Free allocated memory
        pub fn deinit(self: Self) void {
            self.gpa.free(self.coeffs);
        }

        /// Resize the FPS to a new max_degree, keeping existing coefficients
        pub fn resize(self: *Self, new_max_degree: usize) !void {
            const new_coeffs = try self.gpa.alloc(ModintType, new_max_degree + 1);
            @memset(new_coeffs, ModintType.fromInt(0));
            const copy_len = @min(self.coeffs.len, new_coeffs.len);
            @memcpy(new_coeffs[0..copy_len], self.coeffs[0..copy_len]);
            self.gpa.free(self.coeffs);
            self.coeffs = new_coeffs;
        }

        /// Add two FPS in-place: a += b, truncating to max_degree
        pub fn add(a: *Self, b: Self, max_degree: usize) !void {
            try a.resize(max_degree);
            const len = @min(a.coeffs.len, @max(a.coeffs.len, b.coeffs.len));
            for (0..len) |i| {
                const bi = if (i < b.coeffs.len) b.coeffs[i] else ModintType.fromInt(0);
                a.coeffs[i] = a.coeffs[i].add(bi);
            }
        }

        /// Subtract two FPS in-place: a -= b, truncating to max_degree
        pub fn sub(a: *Self, b: Self, max_degree: usize) !void {
            try a.resize(max_degree);
            const len = @min(a.coeffs.len, @max(a.coeffs.len, b.coeffs.len));
            for (0..len) |i| {
                const bi = if (i < b.coeffs.len) b.coeffs[i] else ModintType.fromInt(0);
                a.coeffs[i] = a.coeffs[i].sub(bi);
            }
        }

        /// Multiply two FPS in-place: a *= b, truncating to max_degree
        pub fn mul(a: *Self, b: Self, max_degree: usize) !void {
            // Need a temporary because a is both input and output
            var a_copy = try Self.fromSlice(a.gpa, a.coeffs, max_degree);
            defer a_copy.deinit();

            if (use_ntt and max_degree > 0) {
                // Use NTT convolution only for max_degree > 0
                const conv = try Ntt.convolution(a.gpa, a_copy.coeffs, b.coeffs);
                defer a.gpa.free(conv);
                try a.resize(max_degree);
                const copy_len = @min(conv.len, a.coeffs.len);
                @memcpy(a.coeffs[0..copy_len], conv[0..copy_len]);
            } else {
                // Naive O(n²) multiplication (for max_degree 0 or use_ntt false)
                var result = try Self.init(a.gpa, max_degree);
                defer result.deinit();
                for (0..a_copy.coeffs.len) |i| {
                    if (i > max_degree) break;
                    const ai = a_copy.coeffs[i];
                    if (ai.toInt() == 0) continue;
                    for (0..b.coeffs.len) |j| {
                        const k = i + j;
                        if (k > max_degree) break;
                        const bj = b.coeffs[j];
                        result.coeffs[k] = result.coeffs[k].add(ai.mul(bj));
                    }
                }
                // Copy result to a
                try a.resize(max_degree);
                @memcpy(a.coeffs, result.coeffs);
            }
        }

        /// Square the FPS in-place: a *= a, truncating to max_degree
        pub fn square(a: *Self, max_degree: usize) !void {
            var a_copy = try Self.fromSlice(a.gpa, a.coeffs, max_degree);
            defer a_copy.deinit();
            try a.mul(a_copy, max_degree);
        }

        /// Compute sum of pairwise convolutions of the given FPS: sum_{i<j} fps[i] * fps[j]
        /// Returns a new FPS, caller must free with deinit()
        pub fn sumPairwiseConvolution(gpa: std.mem.Allocator, fps_list: []const Self, max_degree: usize) !Self {
            // Compute S = sum(fps_list)
            var S = try Self.zero(gpa, max_degree);
            errdefer S.deinit();
            for (fps_list) |cfps| {
                try S.add(cfps, max_degree);
            }

            // Compute S^2
            try S.square(max_degree);

            // Compute sum_squares = sum(fps^2)
            var sum_squares = try Self.zero(gpa, max_degree);
            errdefer sum_squares.deinit();
            for (fps_list) |cfps| {
                var fps_sq = try Self.fromSlice(gpa, cfps.coeffs, max_degree);
                defer fps_sq.deinit();
                try fps_sq.square(max_degree);
                try sum_squares.add(fps_sq, max_degree);
            }

            // Compute (S^2 - sum_squares)
            try S.sub(sum_squares, max_degree);

            // Multiply by 1/2
            const inv2 = ModintType.fromInt(2).inv();
            for (S.coeffs) |*c| {
                c.* = c.*.mul(inv2);
            }

            return S;
        }

        /// Compute inverse of FPS in-place modulo x^(max_degree + 1)
        /// Requires: constant term of self is invertible modulo MOD
        pub fn inv(self: *Self, max_degree: usize) !void {
            // Initialize result: 1, accurate up to x^0 (current_degree=0)
            var result = try Self.one(self.gpa, 0);
            defer result.deinit();
            var current_degree: usize = 0;

            while (current_degree < max_degree) {
                const next_degree = @min(2 * current_degree + 1, max_degree);

                // Truncate self to next_degree
                var self_trunc = try Self.fromSlice(self.gpa, self.coeffs[0..@min(self.coeffs.len, next_degree + 1)], next_degree);
                defer self_trunc.deinit();

                // Truncate result to current_degree
                var result_trunc = try Self.fromSlice(self.gpa, result.coeffs[0..@min(result.coeffs.len, current_degree + 1)], current_degree);
                defer result_trunc.deinit();

                const a_times_b = try Self.mulHelper(self.gpa, self_trunc, result_trunc, next_degree);
                defer a_times_b.deinit();

                // two_minus_ab should be 2, not 1!
                var two_minus_ab = try Self.init(self.gpa, next_degree);
                if (two_minus_ab.coeffs.len > 0) {
                    two_minus_ab.coeffs[0] = ModintType.fromInt(2);
                }
                defer two_minus_ab.deinit();

                const tmp = try Self.subHelper(self.gpa, two_minus_ab, a_times_b, next_degree);
                defer tmp.deinit();

                // Again, truncate result_trunc and tmp to their valid degrees for the final mul
                var result_trunc2 = try Self.fromSlice(self.gpa, result_trunc.coeffs[0..@min(result_trunc.coeffs.len, current_degree + 1)], current_degree);
                defer result_trunc2.deinit();
                var tmp_trunc = try Self.fromSlice(self.gpa, tmp.coeffs[0..@min(tmp.coeffs.len, next_degree + 1)], next_degree);
                defer tmp_trunc.deinit();

                const new_result = try Self.mulHelper(self.gpa, result_trunc2, tmp_trunc, next_degree);
                result.deinit();
                result = new_result;
                current_degree = next_degree;
            }

            // Update self with result
            try self.resize(max_degree);
            @memcpy(self.coeffs, result.coeffs);
        }

        /// Compute self^exponent using binary exponentiation or ln/exp in-place, truncating to max_degree
        pub fn pow(self: *Self, exponent: usize, max_degree: usize) !void {
            // Handle trivial cases
            if (exponent == 0) {
                var one_poly = try Self.one(self.gpa, max_degree);
                defer one_poly.deinit();
                try self.resize(max_degree);
                @memcpy(self.coeffs, one_poly.coeffs);
                return;
            }
            if (exponent == 1) {
                try self.resize(max_degree);
                return;
            }

            // Find first non-zero coefficient
            var shift: usize = 0;
            while (shift < self.coeffs.len and self.coeffs[shift].toInt() == 0) : (shift += 1) {}
            if (shift >= self.coeffs.len) {
                // All zeros, result is zero
                var zero_poly = try Self.zero(self.gpa, max_degree);
                defer zero_poly.deinit();
                try self.resize(max_degree);
                @memcpy(self.coeffs, zero_poly.coeffs);
                return;
            }

            if (shift * exponent > max_degree) {
                // Result is zero
                var zero_poly = try Self.zero(self.gpa, max_degree);
                defer zero_poly.deinit();
                try self.resize(max_degree);
                @memcpy(self.coeffs, zero_poly.coeffs);
                return;
            }

            // Use ln/exp if exponent is large enough, or binary exponentiation otherwise
            const use_ln_exp = exponent > 10;

            if (use_ln_exp and shift == 0 and self.coeffs[0].toInt() == 1) {
                // Perfect case for ln/exp: starts with 1
                var ln_self = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
                defer ln_self.deinit();
                try ln_self.ln(max_degree);

                // Multiply by exponent
                for (ln_self.coeffs) |*c| {
                    c.* = c.*.mul(ModintType.fromInt(@intCast(exponent)));
                }

                // Compute exp
                try ln_self.exp(max_degree);

                // Update self
                try self.resize(max_degree);
                @memcpy(self.coeffs, ln_self.coeffs);
            } else {
                // Binary exponentiation
                var result = try Self.one(self.gpa, max_degree);
                defer result.deinit();
                var base = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
                defer base.deinit();
                var exp_val = exponent;

                while (exp_val > 0) {
                    if (exp_val % 2 == 1) {
                        const new_result = try Self.mulHelper(self.gpa, result, base, max_degree);
                        result.deinit();
                        result = new_result;
                    }
                    const new_base = try Self.mulHelper(self.gpa, base, base, max_degree);
                    base.deinit();
                    base = new_base;
                    exp_val /= 2;
                }

                // Update self with result
                try self.resize(max_degree);
                @memcpy(self.coeffs, result.coeffs);
            }
        }

        /// Get precomputed inverses of 1..n (lazy initialization)
        fn getInvCache(gpa: std.mem.Allocator, n: usize) ![]const ModintType {
            if (n == 0) return &.{};
            if (inv_cache.len <= n) {
                const new_len = @max(n + 1, inv_cache.len * 2, 32);
                if (inv_cache_gpa) |old_gpa| {
                    old_gpa.free(inv_cache);
                }
                const new_inv = try gpa.alloc(ModintType, new_len);
                @memset(new_inv, ModintType.fromInt(0));
                if (inv_cache.len > 0) {
                    @memcpy(new_inv[0..inv_cache.len], inv_cache);
                }
                for (inv_cache.len..new_len) |i| {
                    if (i == 0) {
                        new_inv[i] = ModintType.fromInt(0);
                    } else if (i == 1) {
                        new_inv[i] = ModintType.fromInt(1);
                    } else {
                        const mod_i = MOD % i;
                        const div = MOD / i;
                        const neg_div = MOD - div;
                        new_inv[i] = new_inv[mod_i].mul(ModintType.fromInt(@intCast(neg_div)));
                    }
                }
                inv_cache = new_inv;
                inv_cache_gpa = gpa;
            }
            return inv_cache[1 .. n + 1];
        }

        /// Compute derivative of polynomial in-place: P'(x)
        pub fn deriv(self: *Self, max_degree: usize) !void {
            try self.resize(max_degree);
            // For i from 1 to max_degree: new[i-1] = i * old[i]
            var i: usize = 0;
            while (i < max_degree) : (i += 1) {
                const old_i = i + 1;
                if (old_i < self.coeffs.len) {
                    self.coeffs[i] = self.coeffs[old_i].mul(ModintType.fromInt(@intCast(old_i)));
                } else {
                    self.coeffs[i] = ModintType.fromInt(0);
                }
            }
            if (max_degree < self.coeffs.len) {
                self.coeffs[max_degree] = ModintType.fromInt(0);
            }
        }

        /// Compute integral of polynomial in-place (with constant term 0): ∫P(x)dx
        pub fn integ(self: *Self, max_degree: usize) !void {
            try self.resize(max_degree);
            const invs = try getInvCache(self.gpa, max_degree);
            // For i from max_degree down to 1: new[i] = old[i-1] / i
            var i = max_degree;
            while (i >= 1) : (i -= 1) {
                const old_i = i - 1;
                if (old_i < self.coeffs.len) {
                    self.coeffs[i] = self.coeffs[old_i].mul(invs[i - 1]);
                } else {
                    self.coeffs[i] = ModintType.fromInt(0);
                }
            }
            if (self.coeffs.len > 0) {
                self.coeffs[0] = ModintType.fromInt(0);
            }
        }

        /// Compute ln of polynomial in-place: ln(P(x))
        /// Requires: constant term of self is 1
        pub fn ln(self: *Self, max_degree: usize) !void {
            // Check constant term is 1
            if (self.coeffs.len == 0 or self.coeffs[0].toInt() != 1) {
                @panic("ln requires polynomial with constant term 1");
            }

            // Compute P'
            var p_prime = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
            defer p_prime.deinit();
            try p_prime.deriv(max_degree);

            // Compute P^{-1}
            var p_inv = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
            defer p_inv.deinit();
            try p_inv.inv(max_degree);

            // Multiply P' * P^{-1}
            var result = try Self.mulHelper(self.gpa, p_prime, p_inv, max_degree);
            defer result.deinit();

            // Integrate
            try result.integ(max_degree);

            // Update self
            try self.resize(max_degree);
            @memcpy(self.coeffs, result.coeffs);
        }

        /// Compute exp of polynomial in-place: exp(P(x))
        /// Requires: constant term of self is 0
        pub fn exp(self: *Self, max_degree: usize) !void {
            // Check constant term is 0
            if (self.coeffs.len > 0 and self.coeffs[0].toInt() != 0) {
                @panic("exp requires polynomial with constant term 0");
            }

            // Initialize result: 1, accurate up to x^0 (current_degree=0)
            var result = try Self.one(self.gpa, 0);
            defer result.deinit();
            var current_degree: usize = 0;

            while (current_degree < max_degree) {
                const next_degree = @min(2 * current_degree + 1, max_degree);

                // Compute ln(result) up to next_degree
                var result_extended = try Self.fromSlice(self.gpa, result.coeffs, next_degree);
                defer result_extended.deinit();
                try result_extended.ln(next_degree);

                // Compute (1 - ln(result) + self)
                var one_poly = try Self.one(self.gpa, next_degree);
                defer one_poly.deinit();

                var self_extended = try Self.fromSlice(self.gpa, self.coeffs[0..@min(self.coeffs.len, next_degree + 1)], next_degree);
                defer self_extended.deinit();

                var tmp1 = try Self.subHelper(self.gpa, one_poly, result_extended, next_degree);
                defer tmp1.deinit();

                var tmp2 = try Self.addHelper(self.gpa, tmp1, self_extended, next_degree);
                defer tmp2.deinit();

                // Multiply with original result (truncated to current_degree)
                var result_trunc = try Self.fromSlice(self.gpa, result.coeffs, current_degree);
                defer result_trunc.deinit();
                var tmp_trunc = try Self.fromSlice(self.gpa, tmp2.coeffs, next_degree);
                defer tmp_trunc.deinit();

                const new_result = try Self.mulHelper(self.gpa, result_trunc, tmp_trunc, next_degree);
                result.deinit();
                result = new_result;
                current_degree = next_degree;
            }

            // Update self with result
            try self.resize(max_degree);
            @memcpy(self.coeffs, result.coeffs);
        }

        // Helper functions that return new Self for internal use in inv and pow
        fn addHelper(gpa: std.mem.Allocator, a: Self, b: Self, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            const len = @min(result.coeffs.len, @max(a.coeffs.len, b.coeffs.len));
            for (0..len) |i| {
                const ai = if (i < a.coeffs.len) a.coeffs[i] else ModintType.fromInt(0);
                const bi = if (i < b.coeffs.len) b.coeffs[i] else ModintType.fromInt(0);
                result.coeffs[i] = ai.add(bi);
            }
            return result;
        }

        fn subHelper(gpa: std.mem.Allocator, a: Self, b: Self, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            const len = @min(result.coeffs.len, @max(a.coeffs.len, b.coeffs.len));
            for (0..len) |i| {
                const ai = if (i < a.coeffs.len) a.coeffs[i] else ModintType.fromInt(0);
                const bi = if (i < b.coeffs.len) b.coeffs[i] else ModintType.fromInt(0);
                result.coeffs[i] = ai.sub(bi);
            }
            return result;
        }

        fn mulHelper(gpa: std.mem.Allocator, a: Self, b: Self, max_degree: usize) !Self {
            if (use_ntt) {
                // Use NTT convolution
                const conv = try Ntt.convolution(gpa, a.coeffs, b.coeffs);
                defer gpa.free(conv);
                var result = try Self.init(gpa, max_degree);
                const copy_len = @min(conv.len, result.coeffs.len);
                @memcpy(result.coeffs[0..copy_len], conv[0..copy_len]);
                return result;
            } else {
                var result = try Self.init(gpa, max_degree);
                for (0..a.coeffs.len) |i| {
                    if (i > max_degree) break;
                    const ai = a.coeffs[i];
                    if (ai.toInt() == 0) continue;
                    for (0..b.coeffs.len) |j| {
                        const k = i + j;
                        if (k > max_degree) break;
                        const bj = b.coeffs[j];
                        result.coeffs[k] = result.coeffs[k].add(ai.mul(bj));
                    }
                }
                return result;
            }
        }
    };
}

/// Formal Power Series (Polynomial) modulo `comptime MOD` using NTT (requires ROOT: primitive root of MOD)
pub fn FpsNtt(comptime ModintType: type, comptime fps_root: comptime_int) type {
    return FpsImpl(ModintType, true, fps_root);
}

/// Formal Power Series (Polynomial) modulo `comptime MOD` using naive O(n²) multiplication
pub fn FpsNaive(comptime ModintType: type) type {
    return FpsImpl(ModintType, false, null);
}

/// Predefined FPS for modulus 998244353 using NTT (primitive root 3)
pub const Modint998244353 = modint.MontgomeryModint(998244353);
pub const Fps998244353 = FpsNtt(Modint998244353, 3);

};
const Modint = fps.Modint998244353;
const FPS = fps.Fps998244353;

const BUNDLE = false;

// ===================== Solving =====================

const gtype_solve = graph.Graph(usize, void, 200001, 1);
const ESOLVE = gtype_solve.E;
var gsolve = gtype_solve.new();

/// Main solving function for each test cases.
pub fn solve() !void {
    defer _ = allocator.arena.reset(.retain_capacity);
    const al = allocator.arena.allocator();

    var edges: [200000]ESOLVE = undefined;
    const n = in.read(usize);
    for (0..n - 1) |i| {
        const u = in.read(usize) - 1;
        const v = in.read(usize) - 1;
        edges[i] = ESOLVE{
            .u = u,
            .v = v,
            .w = undefined,
        };
    }
    gsolve = gsolve.fromEdgesUnweighted(al, n, edges[0 .. n - 1]);

    const Pruner = struct {
        const ChildParent = struct { usize, usize };
        const CPResult = struct { ChildParent, ?ChildParent, usize };

        pub fn pruneToPath(gpa: std.mem.Allocator, g_ref: *const gtype_solve, num_nodes: usize) !CPResult {
            // Step 1: Build adjacency sets and degree array
            var adj_sets = try gpa.alloc(ds.UsizeSet, num_nodes);
            defer {
                for (adj_sets) |*set| {
                    set.deinit();
                }
                gpa.free(adj_sets);
            }
            for (0..num_nodes) |i| {
                adj_sets[i] = ds.UsizeSet.init(gpa);
            }

            var degree = try gpa.alloc(usize, num_nodes);
            defer gpa.free(degree);
            @memset(degree, 0);

            for (0..num_nodes) |u| {
                for (g_ref.get(u)) |ge| {
                    const v = ge.v;
                    try adj_sets[u].add(v);
                    degree[u] += 1;
                }
            }

            // Step 2: Collect initial leaves (degree 1)
            var q = ds.Deque(usize, 200001).new();

            for (0..num_nodes) |u| {
                if (degree[u] == 1) {
                    // std.debug.print("[Step 2] Adding initial leaf: {d}\n", .{u});
                    q.push_back(&u);
                }
            }
            // std.debug.print("[Step 2] Initial queue size: {d}\n", .{q.len});

            // Step 3: Prune leaves until remaining <= 2
            var step: usize = 0;
            while (q.len > 2) {
                step += 1;
                const sz = q.len;
                // std.debug.print("[Step 3] Iteration {d}, queue size: {d}\n", .{ step, sz });
                for (0..sz) |_| {
                    if (q.pop_front()) |u| {
                        // Find the neighbor of u (since degree is 1 or was 1)
                        const v = adj_sets[u].getMin() orelse adj_sets[u].getMax() orelse continue;
                        // std.debug.print("[Step 3] Pruning leaf {d}, neighbor {d}\n", .{ u, v });
                        // Remove u from v's adjacency set
                        adj_sets[v].remove(u);
                        degree[v] -= 1;
                        if (degree[v] == 1) {
                            // std.debug.print("[Step 3] Adding new leaf {d}\n", .{v});
                            q.push_back(&v);
                        }
                    } else {
                        break;
                    }
                }
            }
            // std.debug.print("[Step 3] Final queue size: {d}\n", .{q.len});

            // Step 4: Remaining nodes are whatever is left in the queue
            var path_nodes = std.ArrayList(usize).initCapacity(gpa, q.len) catch unreachable;
            defer path_nodes.deinit(gpa);
            while (q.pop_front()) |u| {
                // std.debug.print("[Step 4] Adding path node: {d}\n", .{u});
                path_nodes.append(gpa, u) catch unreachable;
            }
            // std.debug.print("[Step 4] Path nodes: {any}\n", .{path_nodes.items});

            // Step 5: Return ChildParent tuple with path length
            if (path_nodes.items.len == 1) {
                // Single node: path length is 1
                const u = path_nodes.items[0];
                // std.debug.print("[Step 5] Single node: {d}, path length: 1\n", .{u});
                return .{ .{ u, u }, null, 1 };
            } else if (path_nodes.items.len == 2) {
                // Two nodes: calculate path length by traversing from a to b
                const a = path_nodes.items[0];
                const pa = adj_sets[a].getMin().?;
                const b = path_nodes.items[1];
                const pb = adj_sets[b].getMin().?;

                // Calculate path length
                var path_length: usize = 0;
                var current: usize = a;
                var prev: usize = a; // Initialize to something, will update
                while (true) {
                    path_length += 1;
                    if (current == b) {
                        break;
                    }
                    // Find next node (not prev)
                    var iter = adj_sets[current].iterator();
                    while (iter.next()) |next_node| {
                        if (next_node != prev or path_length == 1) {
                            prev = current;
                            current = next_node;
                            break;
                        }
                    }
                }

                // std.debug.print("[Step 5] Two nodes: a={d}, pa={d}; b={d}, pb={d}; path length: {d}\n", .{ a, pa, b, pb, path_length });
                return .{ .{ a, pa }, .{ b, pb }, path_length };
            } else {
                // Should not happen if input is a tree
                @panic("Remaining nodes not 1 or 2!");
            }
        }
    };

    const prune_res = try Pruner.pruneToPath(al, &gsolve, n);

    const Tree = struct {
        count_level: []usize,
        max_level: usize,
        gpa: std.mem.Allocator,

        const Self = @This();

        pub fn new(gpa: std.mem.Allocator, max_n: usize) !Self {
            const count_level = try gpa.alloc(usize, max_n);
            return Self{
                .count_level = count_level,
                .max_level = 0,
                .gpa = gpa,
            };
        }

        pub fn deinit(self: Self) void {
            self.gpa.free(self.count_level);
        }

        pub fn init(self: *Self, nn: usize) void {
            @memset(self.count_level[0..nn], 0);
            self.max_level = 0;
        }

        pub fn dfsCountLevel(self: *Self, u: usize, p: usize, l: usize) void {
            self.count_level[l] += 1;
            for (gsolve.get(u)) |*ge| {
                if (ge.v != p) {
                    self.dfsCountLevel(ge.v, u, l + 1);
                }
            }
            self.max_level = @max(self.max_level, l);
        }
    };

    const path_length = prune_res[2];

    if (prune_res[1]) |p2| {
        // It's a path: build two FPS using one Tree instance
        var tree = try Tree.new(al, n);

        // First pass: build FPS A
        tree.init(n);
        tree.dfsCountLevel(prune_res[0][0], prune_res[0][1], 0);
        const max_degree_a = tree.max_level;

        // std.debug.print("[Debug] count_level A: ", .{});
        // for (0..max_degree_a + 1) |l| {
        //     std.debug.print("{d} ", .{tree.count_level[l]});
        // }
        // std.debug.print("\n", .{});

        var fps_a = try FPS.init(al, max_degree_a);
        for (0..max_degree_a + 1) |l| {
            fps_a.coeffs[l] = Modint.fromInt(@intCast(tree.count_level[l]));
        }

        tree.init(max_degree_a + 1); // Clear up to max_degree_a
        tree.dfsCountLevel(p2[0], p2[1], 0);
        const max_degree_b = tree.max_level;

        // std.debug.print("[Debug] count_level B: ", .{});
        // for (0..max_degree_b + 1) |l| {
        //     std.debug.print("{d} ", .{tree.count_level[l]});
        // }
        // std.debug.print("\n", .{});

        const max_degree = @max(max_degree_a, max_degree_b);
        const conv_max_degree = 2 * max_degree + 2;

        // Resize fps_a to conv_max_degree
        try fps_a.resize(conv_max_degree);

        // Build fps_b with conv_max_degree
        var fps_b = try FPS.init(al, conv_max_degree);
        for (0..max_degree_b + 1) |l| {
            fps_b.coeffs[l] = Modint.fromInt(@intCast(tree.count_level[l]));
        }

        // Build fps_c as convolution of fps_a and fps_b
        var fps_c = try FPS.fromSlice(al, fps_a.coeffs, conv_max_degree);
        try fps_c.mul(fps_b, conv_max_degree);

        // Debug print fps_c
        // std.debug.print("[Debug] fps_c coefficients: ", .{});
        // for (0..conv_max_degree + 1) |l| {
        //     std.debug.print("{d} ", .{fps_c.coeffs[l].toInt()});
        // }
        // std.debug.print("\n", .{});

        for (1..n + 1) |k| {
            if (k < path_length) {
                print("0\n", .{});
                continue;
            }
            const d = k - path_length;
            if (d <= conv_max_degree) {
                print("{}\n", .{fps_c.coeffs[d].toInt()});
            } else {
                print("0\n", .{});
            }
        }
    } else {
        // Isolated vertex case
        const u = prune_res[0][0];
        var tree = try Tree.new(al, n);

        // First, dfs the whole tree starting at u and copy the result out
        tree.init(n);
        tree.dfsCountLevel(u, u, 0);
        const whole_tree_max_degree = tree.max_level;
        const whole_tree_count_level = try al.alloc(usize, whole_tree_max_degree + 1);
        @memcpy(whole_tree_count_level, tree.count_level[0 .. whole_tree_max_degree + 1]);

        // Debug print whole tree count_level
        // std.debug.print("[Debug] whole tree count_level: ", .{});
        // for (0..whole_tree_max_degree + 1) |l| {
        //     std.debug.print("{d} ", .{whole_tree_count_level[l]});
        // }
        // std.debug.print("\n", .{});

        const neighbors_slice = gsolve.get(u);

        // Now, collect FPS for each subtree and track overall_max_degree
        var fps_list = try std.ArrayList(FPS).initCapacity(al, neighbors_slice.len);

        var overall_max_degree: usize = 0;
        var last_degree = n;

        for (neighbors_slice) |*ge| {
            const v = ge.v;
            tree.init(last_degree);
            tree.dfsCountLevel(v, u, 0);
            const subtree_max_degree = tree.max_level;
            last_degree = subtree_max_degree + 1;
            overall_max_degree = @max(overall_max_degree, subtree_max_degree);

            // Create FPS for this subtree (we'll resize later)
            var fps_subtree = try FPS.init(al, subtree_max_degree);
            for (0..subtree_max_degree + 1) |l| {
                fps_subtree.coeffs[l] = Modint.fromInt(@intCast(tree.count_level[l]));
            }
            try fps_list.append(al, fps_subtree);
        }

        overall_max_degree *= 2;
        overall_max_degree += 2;

        // Resize all FPS in fps_list to overall_max_degree
        for (fps_list.items) |*fps_item| {
            try fps_item.resize(overall_max_degree);
        }

        const final_fps = try FPS.sumPairwiseConvolution(al, fps_list.items, overall_max_degree);

        // Debug print final_fps
        // std.debug.print("[Debug] final_fps coefficients: ", .{});
        // for (0..overall_max_degree + 1) |l| {
        //     std.debug.print("{d} ", .{final_fps.coeffs[l].toInt()});
        // }
        // std.debug.print("\n", .{});

        for (1..n + 1) |k| {
            var ans = if (k <= whole_tree_max_degree + 1) whole_tree_count_level[k - 1] else 0;
            // std.debug.print("ans for k = {} before adding cross: {}\n", .{ k, ans });
            if (k >= 3 and k <= overall_max_degree + 3) {
                // Count cross-root paths
                // std.debug.print("Cross path count for k = {}: {}\n", .{ k, final_fps.coeffs[k - 3].toInt() });
                ans += final_fps.coeffs[k - 3].toInt();
            }
            print("{}\n", .{ans});
        }
    }

    defer gsolve.deinit();
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
var inbuf: [5000000]u8 = undefined;
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

