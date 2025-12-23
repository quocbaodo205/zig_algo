/// Graph definition and provide some basic functionality for iteration
const std = @import("std");

/// Graph with dynamic node type. Underline is an adj list of usize.
/// If node type is of Int, directly store inside the [n]ArrayList.
/// If node type is of anything else, it need to provide a bidirectional hash function <-> int.
/// Accept outide allocator on init.
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

        const Self = @This();

        pub fn new(allocator: std.mem.Allocator) Self {
            var g: [n]std.ArrayList(GE) = undefined;
            for (0..n) |i| {
                g[i] = std.ArrayList(GE).initCapacity(allocator, 10) catch unreachable;
            }
            return Self{
                .g = g,
            };
        }

        pub fn fromEdgesUnweighted(edges: []const E, allocator: std.mem.Allocator) Self {
            // All array list is of different memory so cannot @splat.
            var g: [n]std.ArrayList(GE) = undefined;
            for (0..n) |i| {
                g[i] = std.ArrayList(GE).initCapacity(allocator, 10) catch unreachable;
            }
            switch (@typeInfo(node_type)) {
                .int => {
                    for (edges) |e| {
                        g[e.u].append(allocator, GE{
                            .v = e.v,
                            .w = e.w,
                        }) catch unreachable;
                        if (is_bidirectional > 0) {
                            g[e.v].append(allocator, GE{
                                .v = e.u,
                                .w = e.w,
                            }) catch unreachable;
                        }
                    }
                },
                else => {
                    // Need to provide a hash / unhash function
                    for (edges) |e| {
                        g[e.u.hash()].append(allocator, GE{
                            .v = e.v.hash(),
                            .w = e.w,
                        }) catch unreachable;
                        if (is_bidirectional > 0) {
                            g[e.v.hash()].append(allocator, GE{
                                .v = e.u.hash(),
                                .w = e.w,
                            }) catch unreachable;
                        }
                    }
                },
            }
            return Self{
                .g = g,
            };
        }

        /// When using the graph, assume all usize. Convert outside via hash / unhash should needed.
        pub fn get(self: *const Self, u: usize) []GE {
            return self.g[u].items;
        }
    };
}

/// GridPoint that provide hash / unhash to be able to use as a graph.
/// y = 0..max_col
pub fn GridPoint(max_col: comptime_int) type {
    return struct {
        x: usize,
        y: usize,

        const Self = @This();

        pub fn hash(self: *const Self) usize {
            return self.x * max_col + self.y;
        }

        pub fn unhash(u: usize) Self {
            return Self{
                .x = u / max_col,
                .y = u % max_col,
            };
        }
    };
}

/// Simple string hashing via counting with Trie
const StringVertices = struct {
    const string = @import("string.zig");
    const trie_type = string.Trie(128, 0, string.UniqueHashTrieNodeType);

    all_str: std.ArrayList([]const u8),
    trie: trie_type,
    allocator: std.mem.Allocator,

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

    pub fn new(al: std.mem.Allocator) Self {
        return Self{
            .all_str = std.ArrayList([]const u8).initCapacity(al, 10) catch unreachable,
            .trie = trie_type.new(al) catch unreachable,
            .allocator = al,
        };
    }

    pub fn newStrV(self: *Self, data: []const u8) StringVertex {
        return StringVertex{
            .str = data,
            .parent = self,
        };
    }

    pub fn add(self: *Self, data: []const u8) void {
        self.all_str.append(self.allocator, data) catch unreachable;
        self.trie.add(data) catch unreachable;
    }

    pub fn unhash(self: *const Self, u: usize) StringVertex {
        return StringVertex{
            .str = self.all_str.items[u],
        };
    }
};

// =============================== Usage as test =======================

test "Test graph usize" {
    // Allocator stuff
    var buffer: [10000000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var arena = std.heap.ArenaAllocator.init(fba.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

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
    const g = gtype.fromEdgesUnweighted(&edges, allocator);

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
    // Allocator stuff
    var buffer: [10000000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var arena = std.heap.ArenaAllocator.init(fba.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const max_n = 10;
    const max_m = 10;
    const gridPointType = GridPoint(max_m);
    const gtype = Graph(gridPointType, void, max_n * max_m, 0);
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
    const g = gtype.fromEdgesUnweighted(&edges, allocator);

    // All usage can create a local struct with the run function that accept the graph

    // ================================== DFS =====================================
    // How to dfs with passed in graph: Create a running struct that contain all needed info.
    // Should be always locally based on gtype
    const DFS = struct {
        var used: [max_n * max_m]bool = @splat(false);

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
        var q = dq.Deque(usize, max_n * max_m * 4).new();
        var in_queue: [max_n * max_m]bool = @splat(false);
        var distance: [max_n * max_m]u32 = @splat(1000000000);

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
    // Allocator stuff
    var buffer: [10000000]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    var arena = std.heap.ArenaAllocator.init(fba.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    var str_v = StringVertices.new(std.heap.page_allocator);
    str_v.add("v1");
    str_v.add("v2");
    str_v.add("v3");
    const node_type = StringVertices.StringVertex;
    const gtype = Graph(node_type, void, 10, 0);
    const E = gtype.E;
    var edges: [3]E = undefined;
    edges[0] = E{
        .u = str_v.newStrV("v1"),
        .v = str_v.newStrV("v2"),
        .w = undefined,
    };
    edges[1] = E{
        .u = str_v.newStrV("v2"),
        .v = str_v.newStrV("v3"),
        .w = undefined,
    };
    edges[2] = E{
        .u = str_v.newStrV("v3"),
        .v = str_v.newStrV("v1"),
        .w = undefined,
    };
    const g = gtype.fromEdgesUnweighted(&edges, allocator);
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
