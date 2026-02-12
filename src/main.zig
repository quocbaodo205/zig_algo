const std = @import("std");
const Io = std.Io;
const string = @import("string.zig");

// Some constant buffer and pre-allocation
const buffer_limit = 1000000;

var fbuffer: [buffer_limit]u8 = undefined;
var fba = std.heap.FixedBufferAllocator.init(&fbuffer);

pub fn solve() !void {
    var arena = std.heap.ArenaAllocator.init(fba.allocator());
    defer arena.deinit();
    const al = fba.allocator();

    // =================== Solve the problem =====================
    const n = in.read(usize);
    const q = in.read(usize);
    const a = in.readArrayList(n, i64, al);

    // Expression for range l..=r can be calculated as:
    // sum j in [l..=r] | -j^2*a[j] + (l+r)*j*a[j] + (1-l)*(r+1)*a[j].
    // We can store prefix sum for i^2*a[i], i*a[i], a[i] (1 based).
    var psum_i2: [300001]i64 = @splat(0);
    for (0..n) |i| {
        psum_i2[i + 1] = psum_i2[i] + @as(i64, @intCast(i + 1)) * @as(i64, @intCast(i + 1)) * a.items[i];
    }

    var psum_i: [300001]i64 = @splat(0);
    for (0..n) |i| {
        psum_i[i + 1] = psum_i[i] + @as(i64, @intCast(i + 1)) * a.items[i];
    }

    var psum: [300001]i64 = @splat(0);
    for (0..n) |i| {
        psum[i + 1] = psum[i] + a.items[i];
    }

    for (0..q) |_| {
        const l = in.read(usize);
        const r = in.read(usize);
        const li: i64 = @intCast(l);
        const ri: i64 = @intCast(r);
        const p2 = psum_i2[r] - psum_i2[l - 1]; // sum j^2*a[j]
        const p1 = psum_i[r] - psum_i[l - 1]; // sum j*a[j]
        const p = psum[r] - psum[l - 1]; // sum a[j]
        const ans: i64 = -p2 + (li + ri) * p1 + (1 - li) * (ri + 1) * p;
        print("{}\n", .{ans});
    }
}

pub fn main() !void {
    // Init the input + read all the content
    // const t = in.read(usize);
    // for (0..t) |_| {
    try solve();
    // }
    try writer.flush(); // Ending flush
}

// ================================ Utils ===============================

/// Position of the first index i, so that arr[i] >= x
/// Sort first: std.mem.sort(u32, a.items, {}, comptime std.sort.asc(u32));
pub fn lowerBoundPos(comptime T: type, arr: []const T, x: T) ?usize {
    var l: usize = 0;
    var r = arr.len - 1;
    var ans = arr.len;
    while (l <= r) {
        const mid = (l + r) / 2;
        if (arr[mid] >= x) {
            ans = mid;
            if (mid == 0) {
                break;
            }
            r = mid - 1;
        } else {
            l = mid + 1;
        }
    }
    if (ans == arr.len) {
        return null;
    }
    return ans;
}

// ================================ IO ============================

// Definition for IO: Buffer and writer
var in = CPInput.init();
var inbuf: [buffer_limit]u8 = undefined;
var stdout_buffer: [1024]u8 = undefined;
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

    /// Panic if cannot parse
    pub fn read(self: *Self, comptime T: type) T {
        const data = self.take();
        // Process the correct function for each type
        switch (@typeInfo(T)) {
            .int => {
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

    pub fn readArrayList(self: *Self, n: usize, comptime T: type, allocator: std.mem.Allocator) std.ArrayList(T) {
        var arr = std.ArrayList(T).initCapacity(allocator, n) catch unreachable;
        // Process the correct function for each type
        switch (@typeInfo(T)) {
            .int => {
                for (0..n) |_| {
                    const data = self.take();
                    arr.append(allocator, std.fmt.parseInt(T, data, 10) catch unreachable) catch unreachable;
                }
            },
            .float => {
                for (0..n) |_| {
                    const data = self.take();
                    arr.append(allocator, std.fmt.parseFloat(T, data) catch unreachable) catch unreachable;
                }
            },
            else => {
                @panic("Type not supported");
            },
        }
        return arr;
    }
};
