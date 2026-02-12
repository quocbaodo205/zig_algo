const std = @import("std");
const heap = std.heap;

/// Bump allocator that alloc a big chunk.
pub fn BumpAllo(num_elements: comptime_int, comptime T: type) type {
    const total_size = num_elements * @sizeOf(T);
    if (total_size <= 1000000) {
        // Stack allocation only with FBA
        return struct {
            const Self = @This();

            buffer: [num_elements * @sizeOf(T)]u8,
            fba: heap.FixedBufferAllocator,
            arena: heap.ArenaAllocator,
            al: std.mem.Allocator,

            pub fn init() Self {
                var self = Self{
                    // Big chunk allocation on heap to avoid stack OOM.
                    .buffer = undefined,
                    .fba = undefined,
                    .arena = undefined,
                    .al = undefined,
                };
                self.fba = .init(&self.buffer);
                self.arena = .init(self.fba.allocator());
                self.al = self.arena.allocator();
                return self;
            }

            pub fn create(self: *Self) *T {
                return self.al.create(T) catch {
                    @panic("Cannot allocate anymore, already used up!");
                };
            }

            pub fn deinit(self: *Self) void {
                self.arena.deinit();
            }
        };
    }
    return struct {
        const Self = @This();

        buffer: []T,
        cur_pos: usize,

        pub fn init() Self {
            return Self{
                // Big chunk allocation on heap to avoid stack OOM.
                .buffer = std.heap.page_allocator.alloc(T, num_elements) catch unreachable,
                .cur_pos = 0,
            };
        }

        pub fn create(self: *Self) *T {
            if (self.cur_pos == num_elements) {
                @panic("Cannot allocate anymore, already used up!");
            }
            self.cur_pos += 1;
            return &self.buffer[self.cur_pos - 1];
        }

        pub fn deinit(self: *Self) void {
            std.heap.page_allocator.free(self.buffer);
        }
    };
}
