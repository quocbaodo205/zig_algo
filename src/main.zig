const std = @import("std");
const allocator = @import("allocator.zig");
const fps = @import("fps.zig");
const modint = @import("modint.zig");

const BUNDLE = false;
const Mint = modint.Modint998244353;

// ===================== Solving =====================

/// Main solving function for each test cases.
pub fn solve() !void {
    defer _ = allocator.arena.reset(.retain_capacity);
    const al = allocator.arena.allocator();
    const n = in.read(u32);
    const m = in.read(u32);
    const initial_coef: [10]Mint = [_]Mint{ Mint.fromInt(1), Mint.fromInt(1), Mint.fromInt(1), Mint.fromInt(1), Mint.fromInt(1), Mint.fromInt(1), Mint.fromInt(1), Mint.fromInt(1), Mint.fromInt(1), Mint.fromInt(1) };
    var f = try fps.Fps998244353.fromSlice(al, &initial_coef, n);
    try f.pow(m - 1, n);
    var ans = Mint.fromInt(0);
    var psum = Mint.fromInt(0);
    const n_mod_9 = n % 9;
    for (f.coeffs, 0..) |k, i| {
        psum = psum.add(k);
        // const i_mod_9 = i % 9;
        // std.debug.print("i = {}, k = {}, psum = {}, i%9 = {}, n%9 = {}\n", .{ i, k, psum, i_mod_9, n_mod_9 });
        if (i % 9 == n_mod_9) {
            ans = ans.add(psum);
        }
    }
    const sub = n / 9;
    ans = ans.sub(Mint.fromInt(sub));
    print("{}\n", .{ans.toInt()});
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

// ======================================== bundle instruction
const Regex = @import("regex").Regex;
const graph = @import("graph.zig");
const ds = @import("ds.zig");

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
    const gtype = graph.Graph(node_type, void, 10, 0);
    const E = gtype.E;
    var edges = try std.ArrayList(E).initCapacity(gpa, 10);
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
            try edges.append(gpa, E{
                .u = str_v.newStrV(f),
                .v = str_v.newStrV(tmp_k),
                .w = undefined,
            });
            std.debug.print("{any} -> {any}\n", .{ str_v.trie.get(f)[0].value, str_v.trie.get(tmp_k)[0].value });
        }
    }
    var g = gtype.fromEdgesUnweighted(edges.items);
    defer g.deinit();

    // Step 2: Prepare a file writer
    var write_buffer: [1024]u8 = undefined;
    var wf = std.fs.cwd().createFile("bundle.zig", .{ .truncate = true }) catch unreachable;
    var file_writer = wf.writer(&write_buffer);
    var wif = &file_writer.interface;
    try wif.writeAll("const std = @import(\"std\");\n");
    var file_read_buffer: [1000000]u8 = undefined;
    var re = try Regex.compile(std.heap.page_allocator, "const (.*) = @import.\"(.*[.]zig)\".");

    // Step 3: Write file by topological order
    const topo = gtype.makeTopo();
    _ = topo.getTopoOrder(&g);
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
