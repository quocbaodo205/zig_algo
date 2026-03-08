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

const BUNDLE = false;

// ===================== Solving =====================

// Static allocations for array inputs
var c: [1001]u32 = undefined;

/// Main solving function for each test cases.
pub fn solve() !void {
    defer _ = allocator.arena.reset(.retain_capacity);

    const n = in.read(usize);
    const m = in.read(usize);
    var total: u32 = 0;
    in.readBuffer(u32, c[0..m]);
    for (0..n) |_| {
        const a = in.read(usize) - 1;
        const b = in.read(u32);
        total += @min(c[a], b);
        c[a] -= @min(c[a], b);
    }
    print("{}\n", .{total});
}

pub fn main() !void {
    if (BUNDLE) {} else {
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
