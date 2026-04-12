const std = @import("std");
const allocator = @import("allocator.zig");
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
    defer g.deinit();

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
    defer g.deinit();

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

    var str_v = StringVertices.new();
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
    defer g.deinit();
    const start: [1]usize = [1]usize{0};
    const BFS = gtype.makeBFS();
    BFS.bfs(&start, &g);
    // std.debug.print("distance = {any}\n", .{BFS.distance});
}
