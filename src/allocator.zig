const std = @import("std");
const heap = std.heap;
const Allocator = std.mem.Allocator;

/// Global heap + arena for quick various tasks.
/// Reset it yourself after each test cases.
pub var arena = heap.ArenaAllocator.init(heap.page_allocator);
