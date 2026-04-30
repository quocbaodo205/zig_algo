const std = @import("std");
const ds = @import("ds.zig");
const string = @import("string.zig"); // Use for string graph only

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

/// Tree struct for counting levels via DFS
pub fn Tree(comptime max_n: comptime_int) type {
    return struct {
        count_level: []usize,
        max_level: usize,
        gpa: std.mem.Allocator,

        const Self = @This();

        pub fn new(gpa: std.mem.Allocator) !Self {
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

        pub fn dfsCountLevel(self: *Self, u: usize, p: usize, l: usize, g: anytype) void {
            self.count_level[l] += 1;
            for (g.get(u)) |*ge| {
                if (ge.v != p) {
                    self.dfsCountLevel(ge.v, u, l + 1, g);
                }
            }
            self.max_level = @max(self.max_level, l);
        }
    };
}

/// Pruner struct for pruning a tree to its diameter path
pub fn Pruner(comptime GraphType: type, comptime max_n: comptime_int) type {
    return struct {
        const ChildParent = struct { usize, usize };
        const CPResult = struct { ChildParent, ?ChildParent, usize };

        pub fn pruneToPath(gpa: std.mem.Allocator, g_ref: *const GraphType, num_nodes: usize) !CPResult {
            // Build adjacency sets and degree array
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

            // Collect initial leaves
            var q = ds.Deque(usize, max_n).new();

            for (0..num_nodes) |u| {
                if (degree[u] == 1) {
                    q.push_back(&u);
                }
            }

            // Prune leaves until remaining <= 2
            while (q.len > 2) {
                const sz = q.len;
                for (0..sz) |_| {
                    if (q.pop_front()) |u| {
                        // Find the neighbor of u (since degree is 1 or was 1)
                        const v = adj_sets[u].getMin() orelse adj_sets[u].getMax() orelse continue;
                        // Remove u from v's adjacency set
                        adj_sets[v].remove(u);
                        degree[v] -= 1;
                        if (degree[v] == 1) {
                            q.push_back(&v);
                        }
                    } else {
                        break;
                    }
                }
            }

            // Remaining nodes are whatever is left in the queue
            var path_nodes = std.ArrayList(usize).initCapacity(gpa, q.len) catch unreachable;
            defer path_nodes.deinit(gpa);
            while (q.pop_front()) |u| {
                path_nodes.append(gpa, u) catch unreachable;
            }

            // Return ChildParent tuple with path length
            if (path_nodes.items.len == 1) {
                // Single node: path length is 1
                const u = path_nodes.items[0];
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
                var prev: usize = a;
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

                return .{ .{ a, pa }, .{ b, pb }, path_length };
            } else {
                // Should not happen if input is a tree
                @panic("Remaining nodes not 1 or 2!");
            }
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
    gpa: std.mem.Allocator,
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

    pub fn new(gpa: std.mem.Allocator) Self {
        var self = Self{
            .gpa = gpa,
            .all_str = undefined,
            .trie = trie_type.new(gpa),
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
        // No-op since Trie doesn't have deinit and we use external allocator
        _ = self;
    }
};

// =============================== Usage as test =======================

test "Test graph usize" {
    const max_n = 10;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const gtype = Graph(usize, void, max_n, 0);
    const E = gtype.E;
    var edges: [3]E = undefined;
    edges[0] = E{
        .u = 0,
        .v = 1,
        .w = undefined,
    };
    edges[1] = E{
        .u = 1,
        .v = 2,
        .w = undefined,
    };
    edges[2] = E{
        .u = 1,
        .v = 4,
        .w = undefined,
    };
    var g = gtype.new();
    g = g.fromEdgesUnweighted(alloc, max_n, &edges);

    const DFS = gtype.makeDFS();
    DFS.dfs(0, &g);
    std.debug.print("used = {any}\n", .{DFS.used});

    const start: [1]usize = [1]usize{0};
    const BFS = gtype.makeBFS();
    BFS.bfs(&start, &g);
    std.debug.print("distance = {any}\n", .{BFS.distance});

    const Topo = gtype.makeTopo();
    _ = Topo.getTopoOrder(&g);
    std.debug.print("Topo order: {any}\n", .{Topo.order[0..Topo.size]});
}

// ===================== Other node type usage ===================

test "Test graph grid" {
    const n = 10;
    const m = 10;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const gridPointType = GridPoint(m);
    const gtype = Graph(gridPointType, void, n * m, 0);
    const E = gtype.E;
    var edges: [3]E = undefined;
    edges[0] = E{
        .u = gridPointType{
            .x = 0,
            .y = 0,
        },
        .v = gridPointType{
            .x = 0,
            .y = 1,
        },
        .w = undefined,
    };
    edges[1] = E{
        .u = gridPointType{
            .x = 0,
            .y = 0,
        },
        .v = gridPointType{
            .x = 1,
            .y = 0,
        },
        .w = undefined,
    };
    edges[2] = E{
        .u = gridPointType{
            .x = 0,
            .y = 1,
        },
        .v = gridPointType{
            .x = 0,
            .y = 2,
        },
        .w = undefined,
    };
    var g = gtype.new();
    g = g.fromEdgesUnweighted(alloc, n * m, &edges);

    const DFS = gtype.makeDFS();
    DFS.dfs(0, &g);
    const s = gridPointType{ .x = 0, .y = 0 };
    const start: [1]usize = [1]usize{s.hash()};
    const BFS = gtype.makeBFS();
    BFS.bfs(&start, &g);
    std.debug.print("distance = {any}\n", .{BFS.distance});
}

test "Test graph string" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var str_v = StringVertices.new(alloc);
    defer str_v.deinit();
    str_v.add("a");
    str_v.add("b");
    str_v.add("c");
    const node_type = StringVertices.StringVertex;
    const gtype = Graph(node_type, void, 10, 0);
    const E = gtype.E;
    var edges: [3]E = undefined;
    edges[0] = E{
        .u = str_v.newStrV("a"),
        .v = str_v.newStrV("b"),
        .w = undefined,
    };
    edges[1] = E{
        .u = str_v.newStrV("b"),
        .v = str_v.newStrV("c"),
        .w = undefined,
    };
    edges[2] = E{
        .u = str_v.newStrV("c"),
        .v = str_v.newStrV("a"),
        .w = undefined,
    };
    var g = gtype.new();
    g = g.fromEdgesUnweighted(alloc, 10, &edges);
    const start: [1]usize = [1]usize{0};
    const BFS = gtype.makeBFS();
    BFS.bfs(&start, &g);
    // std.debug.print("distance = {any}\n", .{BFS.distance});
}
