const std = @import("std");
const allocator = @import("allocator.zig");

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
            return self.g[u].items;
        }

        pub fn deinit(self: *Self) void {
            self.al.deinit();
        }
    };
}

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
            .trie = trie_type.new() catch unreachable,
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
        self.trie.add(data) catch unreachable;
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

    // All usage can create a local struct with the run function that accept the graph

    // ================================== DFS =====================================
    // How to dfs with passed in graph: Create a running struct that contain all needed info.
    // Should be always locally based on gtype
    const DFS = struct {
        var used: [max_n]bool = @splat(false);

        fn dfs(u: usize, graph: *const gtype) void {
            std.debug.print("u = {}\n", .{u});
            used[u] = true;
            for (graph.get(u)) |ge| {
                if (!used[ge.v]) {
                    dfs(ge.v, graph);
                }
            }
        }
    };

    DFS.dfs(0, &g);
    std.debug.print("used = {any}\n", .{DFS.used});

    // ================================== BFS =====================================
    const BFS = struct {
        const dq = @import("ds.zig");
        var q = dq.Deque(usize, max_n * 10).new();
        var in_queue: [max_n]bool = @splat(false);
        var distance: [max_n]u32 = @splat(1000000000);

        fn bfs(starts: []const usize, graph: *const gtype) void {
            for (starts) |u| {
                q.push_back(&u);
                distance[u] = 0;
                in_queue[u] = true;
            }
            while (true) {
                if (q.pop_front()) |u| {
                    for (graph.get(u)) |ge| {
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

    const start: [1]usize = [1]usize{0};
    BFS.bfs(&start, &g);
    std.debug.print("distance = {any}\n", .{BFS.distance});

    // ================================== Topo =====================================
    const Topo = struct {
        var state: [max_n]u8 = @splat(0);
        // Reverse order is the topo order
        var order: [max_n]usize = @splat(0);
        var size: usize = 0;

        fn dfs(u: usize, graph: *const gtype) bool {
            state[u] = 1;
            for (graph.get(u)) |ge| {
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

        fn getTopoOrder(graph: *const gtype) bool {
            for (0..max_n) |u| {
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

    // All usage can create a local struct with the run function that accept the graph

    // ================================== DFS =====================================
    // How to dfs with passed in graph: Create a running struct that contain all needed info.
    // Should be always locally based on gtype
    const DFS = struct {
        var used: [n * m]bool = @splat(false);

        fn dfs(u: usize, graph: *const gtype) void {
            std.debug.print("u = {any}\n", .{gridPointType.unhash(u)});
            used[u] = true;
            for (graph.get(u)) |ge| {
                if (!used[ge.v]) {
                    dfs(ge.v, graph);
                }
            }
        }
    };

    DFS.dfs(0, &g);
    // std.debug.print("used = {any}\n", .{DFS.used});

    // ================================== BFS =====================================
    const BFS = struct {
        const dq = @import("ds.zig");
        var q = dq.Deque(usize, n * m * 4).new();
        var in_queue: [n * m]bool = @splat(false);
        var distance: [n * m]u32 = @splat(1000000000);

        fn bfs(starts: []const usize, graph: *const gtype) void {
            for (starts) |u| {
                q.push_back(&u);
                distance[u] = 0;
                in_queue[u] = true;
            }
            while (true) {
                if (q.pop_front()) |u| {
                    for (graph.get(u)) |ge| {
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

    const s = gridPointType{ .x = 0, .y = 0 };
    const start: [1]usize = [1]usize{s.hash()};
    BFS.bfs(&start, &g);
    // std.debug.print("distance = {any}\n", .{BFS.distance});
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
    // ================================== BFS =====================================
    const BFS = struct {
        const dq = @import("ds.zig");
        var q = dq.Deque(usize, 10).new();
        var in_queue: [10]bool = @splat(false);
        var distance: [10]u32 = @splat(1000000000);

        fn bfs(starts: []const usize, graph: *const gtype) void {
            for (starts) |u| {
                q.push_back(&u);
                distance[u] = 0;
                in_queue[u] = true;
            }
            while (true) {
                if (q.pop_front()) |u| {
                    for (graph.get(u)) |ge| {
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

    const start: [1]usize = [1]usize{0};
    BFS.bfs(&start, &g);
    std.debug.print("distance = {any}\n", .{BFS.distance});
}
