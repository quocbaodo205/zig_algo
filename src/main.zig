const std = @import("std");
const Io = std.Io;

// Some constant buffer and pre-allocation
const buffer_limit = 20000000;
var inbuf: [buffer_limit]u8 = undefined;
var fbuffer: [buffer_limit]u8 = undefined;
var fba = std.heap.FixedBufferAllocator.init(&fbuffer);
const fal = fba.allocator();

// Writer allocation
var stdout_buffer: [1024]u8 = undefined;
var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
const writer = &stdout_writer.interface;

/// Position of the first index i, so that arr[i] >= x
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

pub fn main() !void {
    // Init the input + read all the content
    var in = CPInput.init();
    const n = in.read(u32);
    const m = in.read(u32);
    const a = in.readArrayList(n, u32, fal);
    const b = in.readArrayList(m, u32, fal);
    std.mem.sort(u32, a.items, {}, comptime std.sort.asc(u32));
    std.mem.sort(u32, b.items, {}, comptime std.sort.asc(u32));
    // Prefix sum a
    var presum_a = try std.ArrayList(u64).initCapacity(fal, n);
    for (0..n) |i| {
        const x = a.items[i] + if (i == 0)
            0
        else
            presum_a.items[i - 1];
        try presum_a.append(fal, x);
    }

    var ans: u64 = 0;
    for (b.items) |x| {
        const lpos = lowerBoundPos(u32, a.items, x);
        if (lpos) |lpv| {
            if (lpv > 0) {
                ans += @as(u64, x) * lpv - presum_a.items[lpv - 1];
            }
            // Upper bound
            const rpos = lowerBoundPos(u32, a.items, x + 1);
            if (rpos) |rpv| {
                // Need sum from rpos..
                ans += (presum_a.items[n - 1] - if (rpv == 0) 0 else presum_a.items[rpv - 1]) - @as(u64, x) * (n - rpv);
            }
        } else {
            // All a is < x, then diff is only x * n - presum;
            ans += @as(u64, x) * n - presum_a.items[n - 1];
        }
        ans %= 998244353;
    }
    print("{}\n", .{ans});

    try writer.flush(); // Ending flush
}

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
