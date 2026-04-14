const std = @import("std");
const allocator = @import("allocator.zig");
const graph = @import("graph.zig");
const ds = @import("ds.zig");
const fps = @import("fps.zig");
const FPS = fps.FpsFft;

const BUNDLE = false;

// ===================== Solving =====================

const gtype_solve = graph.Graph(usize, void, 200001, 1);
const ESOLVE = gtype_solve.E;
var gsolve = gtype_solve.new();
const TreeType = graph.Tree(200001);
const PrunerType = graph.Pruner(gtype_solve, 200001);

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

    const prune_res = try PrunerType.pruneToPath(al, &gsolve, n);

    const path_length = prune_res[2];

    if (prune_res[1]) |p2| {
        // It's a path: build two FPS using one Tree instance
        var tree = try TreeType.new(al);

        // First pass: build FPS A
        tree.init(n);
        tree.dfsCountLevel(prune_res[0][0], prune_res[0][1], 0, &gsolve);
        const max_degree_a = tree.max_level;

        var fps_a = try FPS.init(al, max_degree_a);
        for (0..max_degree_a + 1) |l| {
            fps_a.coeffs[l] = @floatFromInt(tree.count_level[l]);
        }

        tree.init(max_degree_a + 1); // Clear up to max_degree_a
        tree.dfsCountLevel(p2[0], p2[1], 0, &gsolve);
        const max_degree_b = tree.max_level;

        const max_degree = @max(max_degree_a, max_degree_b);
        const conv_max_degree = 2 * max_degree + 2;

        // Resize fps_a to conv_max_degree
        try fps_a.resize(conv_max_degree);

        // Build fps_b with conv_max_degree
        var fps_b = try FPS.init(al, conv_max_degree);
        for (0..max_degree_b + 1) |l| {
            fps_b.coeffs[l] = @floatFromInt(tree.count_level[l]);
        }

        // Build fps_c as convolution of fps_a and fps_b
        var fps_c = try FPS.fromSlice(al, fps_a.coeffs, conv_max_degree);
        try fps_c.mul(fps_b, conv_max_degree);

        for (1..n + 1) |k| {
            if (k < path_length) {
                print("0\n", .{});
                continue;
            }
            const d = k - path_length;
            if (d <= conv_max_degree) {
                print("{}\n", .{@as(u64, @intFromFloat(fps_c.coeffs[d]))});
            } else {
                print("0\n", .{});
            }
        }
    } else {
        // Isolated vertex case
        const u = prune_res[0][0];
        var tree = try TreeType.new(al);

        // First, dfs the whole tree starting at u and copy the result out
        tree.init(n);
        tree.dfsCountLevel(u, u, 0, &gsolve);
        const whole_tree_max_degree = tree.max_level;
        const whole_tree_count_level = try al.alloc(usize, whole_tree_max_degree + 1);
        @memcpy(whole_tree_count_level, tree.count_level[0 .. whole_tree_max_degree + 1]);

        const neighbors_slice = gsolve.get(u);

        // Now, collect FPS for each subtree and track overall_max_degree
        var fps_list = try std.ArrayList(FPS).initCapacity(al, neighbors_slice.len);

        var overall_max_degree: usize = 0;
        var last_degree = n;

        for (neighbors_slice) |*ge| {
            const v = ge.v;
            tree.init(last_degree);
            tree.dfsCountLevel(v, u, 0, &gsolve);
            const subtree_max_degree = tree.max_level;
            last_degree = subtree_max_degree + 1;
            overall_max_degree = @max(overall_max_degree, subtree_max_degree);

            // Create FPS for this subtree (we'll resize later)
            var fps_subtree = try FPS.init(al, subtree_max_degree);
            for (0..subtree_max_degree + 1) |l| {
                fps_subtree.coeffs[l] = @floatFromInt(tree.count_level[l]);
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

        for (1..n + 1) |k| {
            var ans: u64 = if (k <= whole_tree_max_degree + 1) whole_tree_count_level[k - 1] else 0;
            if (k >= 3 and k <= overall_max_degree + 3) {
                // Count cross-root paths
                ans += @intFromFloat(final_fps.coeffs[k - 3]);
            }
            print("{}\n", .{ans});
        }
    }

    defer gsolve.deinit();
}

pub fn main() !void {
    if (BUNDLE) {
        _ = try bundle();
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

// ======================================== bundle instruction
const Regex = @import("regex").Regex;
// const graph = @import("graph.zig");
// const ds = @import("ds.zig");

/// Read all the import of the current file.
fn readAllImport(file_name: []const u8) !std.StringHashMap([]const u8) {
    var res = std.StringHashMap([]const u8).init(std.heap.page_allocator);
    // Step 1: Open the file.
    var file_read_buffer: [1000000]u8 = undefined;
    var file = std.fs.cwd().openFile(file_name, .{ .mode = .read_only }) catch unreachable;
    var reader = file.reader(&file_read_buffer);
    var re = try Regex.compile(std.heap.page_allocator, "const (.*) = @import.\"(.*[.]zig)\".");
    while (true) {
        const line = reader.interface.takeDelimiterInclusive('\n') catch |err| {
            switch (err) {
                error.EndOfStream => {
                    break;
                },
                else => {
                    @panic("wew");
                },
            }
        };
        if (std.mem.eql(u8, line, "// ======================================== bundle instruction\n")) {
            break;
        }
        if (try re.captures(line)) |cp| {
            std.debug.print("Match for file, cp0 = {s}, cp1 = {s}, cp2 = {s}\n", .{ cp.sliceAt(0).?, cp.sliceAt(1).?, cp.sliceAt(2).? });
            try res.put(cp.sliceAt(2).?, cp.sliceAt(1).?);
        }
    }
    return res;
}

/// Main bundling function: read all needed files and write to output.
pub fn bundle() ![]const u8 {
    var gpa = std.heap.page_allocator;
    // Step 1: Read files + make graph
    var all_file_mp = std.StringHashMap([]const u8).init(gpa);
    var q = ds.Deque([]const u8, 100).new();
    q.push_back(&"main.zig");
    // Graph structure
    var str_v = graph.StringVertices.new();
    defer str_v.deinit();
    const node_type = graph.StringVertices.StringVertex;
    const gtype_bundle = graph.Graph(node_type, void, 10, 0);
    const EBUNDLE = gtype_bundle.E;
    var edges = try std.ArrayList(EBUNDLE).initCapacity(gpa, 10);
    while (q.pop_front()) |f| {
        str_v.add(f);
        const true_file_name = try std.fmt.allocPrint(gpa, "src/{s}", .{f});
        std.debug.print("------- Process file {s} --------\n", .{true_file_name});
        const mp = try readAllImport(true_file_name);
        var iter = mp.iterator();
        while (iter.next()) |item| {
            // These 2 value will be gone when mp is gone...
            const tmp_k = try gpa.alloc(u8, item.key_ptr.len);
            @memcpy(tmp_k, item.key_ptr.*);
            const tmp_v = try gpa.alloc(u8, item.value_ptr.len);
            @memcpy(tmp_v, item.value_ptr.*);
            std.debug.print("k = {s}, v = {s}\n", .{ tmp_k, tmp_v });

            if (!all_file_mp.contains(item.key_ptr.*)) {
                str_v.add(tmp_k);
                try all_file_mp.put(tmp_k, tmp_v);
                q.push_back(&tmp_k);
            }
            try edges.append(gpa, EBUNDLE{
                .u = str_v.newStrV(f),
                .v = str_v.newStrV(tmp_k),
                .w = undefined,
            });
            std.debug.print("{any} -> {any}\n", .{ str_v.trie.get(f)[0].value, str_v.trie.get(tmp_k)[0].value });
        }
    }
    var gbundle = gtype_bundle.new();
    gbundle = gbundle.fromEdgesUnweighted(gpa, 10, edges.items);
    defer gbundle.deinit();

    // Step 2: Prepare a file writer
    var write_buffer: [1024]u8 = undefined;
    var wf = std.fs.cwd().createFile("bundle.zig", .{ .truncate = true }) catch unreachable;
    var file_writer = wf.writer(&write_buffer);
    var wif = &file_writer.interface;
    try wif.writeAll("const std = @import(\"std\");\n");
    var file_read_buffer: [1000000]u8 = undefined;
    var re = try Regex.compile(std.heap.page_allocator, "const (.*) = @import.\"(.*[.]zig)\".");

    // Step 3: Write file by topological order
    const topo = gtype_bundle.makeTopo();
    _ = topo.getTopoOrder(&gbundle);
    std.debug.print("Topo order: {any}\n", .{topo.order});
    for (topo.order) |u| {
        if (u >= str_v.cur_mx) {
            continue;
        }
        if (u == 0) {
            continue;
        }
        const x = str_v.unhash(u);
        std.debug.print("Read file src/{s} and write content to {s}\n", .{ x.str, all_file_mp.get(x.str).? });
        try wif.writeAll(try std.fmt.allocPrint(gpa, "const {s} = struct {s}\n", .{ all_file_mp.get(x.str).?, "{" }));
        const true_file_name = try std.fmt.allocPrint(gpa, "src/{s}", .{x.str});
        var file = std.fs.cwd().openFile(true_file_name, .{ .mode = .read_only }) catch unreachable;
        var reader = file.reader(&file_read_buffer);
        while (true) {
            const line = reader.interface.takeDelimiterInclusive('\n') catch |err| {
                switch (err) {
                    error.EndOfStream => {
                        break;
                    },
                    else => {
                        @panic("wew");
                    },
                }
            };
            if (std.mem.eql(u8, line, "const std = @import(\"std\");\n")) {
                continue;
            }
            if (line.len > 4 and std.mem.eql(u8, line[0..4], "test")) {
                break; // Ignore test.
            }
            if (std.mem.eql(u8, line, "// ======================================== bundle instruction\n")) {
                break;
            }
            if (try re.captures(line) != null) {
                continue; // Don't write this line.
            }
            try wif.writeAll(line);
        }
        try wif.writeAll("};\n");
        try wif.flush();
    }

    var file = std.fs.cwd().openFile("src/main.zig", .{ .mode = .read_only }) catch unreachable;
    var reader = file.reader(&file_read_buffer);
    while (true) {
        const line = reader.interface.takeDelimiterInclusive('\n') catch |err| {
            switch (err) {
                error.EndOfStream => {
                    break;
                },
                else => {
                    @panic("wew");
                },
            }
        };
        if (std.mem.eql(u8, line, "const BUNDLE = true;\n")) {
            try wif.writeAll("const BUNDLE = false;\n");
            continue;
        }
        if (std.mem.eql(u8, line, "const std = @import(\"std\");\n")) {
            continue;
        }
        if (std.mem.eql(u8, line, "        _ = try bundle();\n")) {
            continue; // Ignore bundle instruction.
        }
        if (line.len > 4 and std.mem.eql(u8, line[0..4], "test")) {
            break; // Ignore test.
        }
        if (std.mem.eql(u8, line, "// ======================================== bundle instruction\n")) {
            break;
        }
        if (try re.captures(line) != null) {
            continue; // Don't write this line.
        }
        try wif.writeAll(line);
    }
    try wif.flush();

    // Last step: Write the main file

    return "Done";
}
