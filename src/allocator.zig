const std = @import("std");
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
