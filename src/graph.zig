const std = @import("std");
const allocator = @import("allocator.zig");
const dq = @import("ds.zig");

/// Graph with dynamic node type. Underline is an adj list of usize.
/// If node type is of Int, directly store inside the [n]ArrayList.
/// If node type is of anything else, it need to provide a bidirectional hash function <-> int.
/// Allocator: Local arena + heap based bump allocator. Free or reset after each use.
pub fn Graph(comptime node_type: type, comptime weight_type: type, n: comptime_int, is_bidirectional: comptime_int) type {
    return struct {
        // Data structure for outside edge: an edge from u -w> v
        const E = struct {
            u: node_type,
            v: node_type,
            w: weight_type,
        };

        // Data structure for a graph edge
        const GE = struct {
            v: usize,
            w: weight_type,
        };

        g: [n]std.ArrayList(GE),
        al: allocator.BumpAllo(0, GE),

        const Self = @This();

        pub fn new() Self {
            var self = Self{
                .g = undefined,
                .al = .init(),
            };
            for (0..n) |i| {
                self.g[i] = std.ArrayList(GE).initCapacity(self.al.allocator(), 100) catch unreachable;
            }
            return self;
        }

        pub fn fromEdgesUnweighted(edges: []const E) Self {
            @branchHint(.likely);
            var self = Self.new();
            // All array list is of different memory so cannot @splat.
            switch (@typeInfo(node_type)) {
                .int => {
                    for (edges) |e| {
                        self.g[e.u].append(self.al.allocator(), GE{
                            .v = e.v,
                            .w = e.w,
                        }) catch unreachable;
                        if (is_bidirectional > 0) {
                            self.g[e.v].append(self.al.allocator(), GE{
                                .v = e.u,
                                .w = e.w,
                            }) catch unreachable;
                        }
                    }
                },
                else => {
                    // Need to provide a hash / unhash function
                    for (edges) |e| {
                        self.g[e.u.hash()].append(self.al.allocator(), GE{
                            .v = e.v.hash(),
                            .w = e.w,
                        }) catch unreachable;
                        if (is_bidirectional > 0) {
                            self.g[e.v.hash()].append(self.al.allocator(), GE{
                                .v = e.u.hash(),
                                .w = e.w,
                            }) catch unreachable;
                        }
                    }
                },
            }
            return self;
        }

        /// When using the graph, assume all usize. Convert outside via hash / unhash should needed.
        pub fn get(self: *const Self, u: usize) []GE {
            @branchHint(.likely);
            return self.g[u].items;
        }

        /// Reset state but keep allocated memory for multiple test cases usage.
        pub fn reset(self: *Self) void {
            for (0..n) |i| {
                self.g[i].shrinkRetainingCapacity(0);
            }
            self.al.reset();
        }

        pub fn deinit(self: *Self) void {
            self.al.deinit();
        }

        // ================================ Classic algo =================

        /// Minimal DFS from a vertex. Create a DFS struct that contains needed infomation.
        /// Probably never use, usually for reference only so we can expand.
        pub fn makeDFS() type {
            @branchHint(.cold);
            return struct {
                pub var used: [n]bool = @splat(false);

                pub fn dfs(u: usize, graph: *const Self) void {
                    used[u] = true;
                    for (graph.get(u)) |*ge| {
                        if (!used[ge.v]) {
                            dfs(ge.v, graph);
                        }
                    }
                }
            };
        }

        /// Minimal BFS from starting vertices. Create a BFS struct that contains needed infomation.
        pub fn makeBFS() type {
            return struct {
                var q = dq.Deque(usize, n * 10).new();
                var in_queue: [n]bool = @splat(false);
                var distance: [n]u32 = @splat(1000000000);

                fn bfs(starts: []const usize, graph: *const Self) void {
                    for (starts) |u| {
                        q.push_back(&u);
                        distance[u] = 0;
                        in_queue[u] = true;
                    }
                    while (true) {
                        if (q.pop_front()) |u| {
                            for (graph.get(u)) |*ge| {
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
                var state: [n]u8 = @splat(0);
                // Reverse order is the topo order
                pub var order: [n]usize = undefined;
                pub var size: usize = 0;

                fn dfs(u: usize, graph: *const Self) bool {
                    state[u] = 1;
                    for (graph.get(u)) |*ge| {
                        if (state[ge.v] == 1) {
                            // Loop found
                            return false;
                        }
                        if (state[ge.v] == 0) {
                            // New vertext
                            const child_dfs_res = dfs(ge.v, graph);
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
                pub fn getTopoOrder(graph: *const Self) bool {
                    for (0..n) |u| {
                        if (state[u] == 0) {
                            const dfs_res = dfs(u, graph);
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
const StringVertices = struct {
    const string = @import("string.zig");
    const trie_type = string.Trie(26, 'a', string.UniqueHashTrieNodeType);

    all_str: std.ArrayList([]const u8), // Mapping from pos to string
    trie: trie_type,
    al: allocator.BumpAllo(0, []const u8),

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
        };
        self.all_str = std.ArrayList([]const u8).initCapacity(self.al.allocator(), 10) catch unreachable;

        return self;
    }

    pub fn newStrV(self: *Self, data: []const u8) StringVertex {
        return StringVertex{
            .str = data,
            .parent = self,
        };
    }

    pub fn add(self: *Self, data: []const u8) void {
        self.all_str.append(self.al.allocator(), data) catch unreachable;
        self.trie.add(data);
    }

    pub fn unhash(self: *const Self, u: usize) StringVertex {
        return StringVertex{
            .str = self.all_str.items[u],
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
    var g = gtype.fromEdgesUnweighted(&edges);
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
    var g = gtype.fromEdgesUnweighted(&edges);
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
    var g = gtype.fromEdgesUnweighted(&edges);
    defer g.deinit();
    const start: [1]usize = [1]usize{0};
    const BFS = gtype.makeBFS();
    BFS.bfs(&start, &g);
    // std.debug.print("distance = {any}\n", .{BFS.distance});
}
