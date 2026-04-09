const std = @import("std");
const ntt = @import("ntt.zig");
const modint = @import("modint.zig");

/// Helper to create FPS struct with given options
fn FpsImpl(comptime ModintType: type, comptime use_ntt: bool, comptime fps_root: ?comptime_int) type {
    return struct {
        coeffs: []ModintType,
        gpa: std.mem.Allocator,

        const Self = @This();

        // NTT helpers (only if use_ntt is true)
        const Ntt = if (use_ntt) ntt.NttHelpers(ModintType.MOD, fps_root.?) else void;

        /// Initialize an FPS with all zeros
        pub fn init(gpa: std.mem.Allocator, max_degree: usize) !Self {
            const coeffs = try gpa.alloc(ModintType, max_degree + 1);
            @memset(coeffs, ModintType.fromInt(0));
            return Self{
                .coeffs = coeffs,
                .gpa = gpa,
            };
        }

        /// Create FPS from a slice of coefficients
        pub fn fromSlice(gpa: std.mem.Allocator, coeffs: []const ModintType, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            const copy_len = @min(coeffs.len, result.coeffs.len);
            @memcpy(result.coeffs[0..copy_len], coeffs[0..copy_len]);
            return result;
        }

        /// Create the zero polynomial (all coefficients zero)
        pub fn zero(gpa: std.mem.Allocator, max_degree: usize) !Self {
            return try Self.init(gpa, max_degree);
        }

        /// Create the polynomial 1 (constant term 1, others zero)
        pub fn one(gpa: std.mem.Allocator, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            if (result.coeffs.len > 0) {
                result.coeffs[0] = ModintType.fromInt(1);
            }
            return result;
        }

        /// Free allocated memory
        pub fn deinit(self: Self) void {
            self.gpa.free(self.coeffs);
        }

        /// Resize the FPS to a new max_degree, keeping existing coefficients
        pub fn resize(self: *Self, new_max_degree: usize) !void {
            const new_coeffs = try self.gpa.alloc(ModintType, new_max_degree + 1);
            @memset(new_coeffs, ModintType.fromInt(0));
            const copy_len = @min(self.coeffs.len, new_coeffs.len);
            @memcpy(new_coeffs[0..copy_len], self.coeffs[0..copy_len]);
            self.gpa.free(self.coeffs);
            self.coeffs = new_coeffs;
        }

        /// Add two FPS in-place: a += b, truncating to max_degree
        pub fn add(a: *Self, b: Self, max_degree: usize) !void {
            try a.resize(max_degree);
            const len = @min(a.coeffs.len, @max(a.coeffs.len, b.coeffs.len));
            for (0..len) |i| {
                const bi = if (i < b.coeffs.len) b.coeffs[i] else ModintType.fromInt(0);
                a.coeffs[i] = a.coeffs[i].add(bi);
            }
        }

        /// Subtract two FPS in-place: a -= b, truncating to max_degree
        pub fn sub(a: *Self, b: Self, max_degree: usize) !void {
            try a.resize(max_degree);
            const len = @min(a.coeffs.len, @max(a.coeffs.len, b.coeffs.len));
            for (0..len) |i| {
                const bi = if (i < b.coeffs.len) b.coeffs[i] else ModintType.fromInt(0);
                a.coeffs[i] = a.coeffs[i].sub(bi);
            }
        }

        /// Multiply two FPS in-place: a *= b, truncating to max_degree
        pub fn mul(a: *Self, b: Self, max_degree: usize) !void {
            // Need a temporary because a is both input and output
            var a_copy = try Self.fromSlice(a.gpa, a.coeffs, max_degree);
            defer a_copy.deinit();

            if (use_ntt) {
                // Use NTT convolution
                const conv = try Ntt.convolution(a.gpa, a_copy.coeffs, b.coeffs);
                defer a.gpa.free(conv);
                try a.resize(max_degree);
                const copy_len = @min(conv.len, a.coeffs.len);
                @memcpy(a.coeffs[0..copy_len], conv[0..copy_len]);
            } else {
                // Naive O(n²) multiplication
                var result = try Self.init(a.gpa, max_degree);
                defer result.deinit();
                for (0..a_copy.coeffs.len) |i| {
                    if (i > max_degree) break;
                    const ai = a_copy.coeffs[i];
                    if (ai.toInt() == 0) continue;
                    for (0..b.coeffs.len) |j| {
                        const k = i + j;
                        if (k > max_degree) break;
                        const bj = b.coeffs[j];
                        result.coeffs[k] = result.coeffs[k].add(ai.mul(bj));
                    }
                }
                // Copy result to a
                try a.resize(max_degree);
                @memcpy(a.coeffs, result.coeffs);
            }
        }

        /// Compute inverse of FPS in-place modulo x^(max_degree + 1)
        /// Requires: constant term of self is invertible modulo MOD
        pub fn inv(self: *Self, max_degree: usize) !void {
            // Initialize result: 1, accurate up to x^0 (current_degree=0)
            var result = try Self.one(self.gpa, 0);
            defer result.deinit();
            var current_degree: usize = 0;

            while (current_degree < max_degree) {
                const next_degree = @min(2 * current_degree + 1, max_degree);

                // Truncate self to next_degree
                var self_trunc = try Self.fromSlice(self.gpa, self.coeffs[0..@min(self.coeffs.len, next_degree + 1)], next_degree);
                defer self_trunc.deinit();

                // Truncate result to current_degree
                var result_trunc = try Self.fromSlice(self.gpa, result.coeffs[0..@min(result.coeffs.len, current_degree + 1)], current_degree);
                defer result_trunc.deinit();

                const a_times_b = try Self.mulHelper(self.gpa, self_trunc, result_trunc, next_degree);
                defer a_times_b.deinit();

                // two_minus_ab should be 2, not 1!
                var two_minus_ab = try Self.init(self.gpa, next_degree);
                if (two_minus_ab.coeffs.len > 0) {
                    two_minus_ab.coeffs[0] = ModintType.fromInt(2);
                }
                defer two_minus_ab.deinit();

                const tmp = try Self.subHelper(self.gpa, two_minus_ab, a_times_b, next_degree);
                defer tmp.deinit();

                // Again, truncate result_trunc and tmp to their valid degrees for the final mul
                var result_trunc2 = try Self.fromSlice(self.gpa, result_trunc.coeffs[0..@min(result_trunc.coeffs.len, current_degree + 1)], current_degree);
                defer result_trunc2.deinit();
                var tmp_trunc = try Self.fromSlice(self.gpa, tmp.coeffs[0..@min(tmp.coeffs.len, next_degree + 1)], next_degree);
                defer tmp_trunc.deinit();

                const new_result = try Self.mulHelper(self.gpa, result_trunc2, tmp_trunc, next_degree);
                result.deinit();
                result = new_result;
                current_degree = next_degree;
            }

            // Update self with result
            try self.resize(max_degree);
            @memcpy(self.coeffs, result.coeffs);
        }

        /// Compute self^exponent using binary exponentiation in-place, truncating to max_degree
        pub fn pow(self: *Self, exponent: usize, max_degree: usize) !void {
            var result = try Self.one(self.gpa, max_degree);
            defer result.deinit();
            var base = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
            defer base.deinit();
            var exp = exponent;

            while (exp > 0) {
                if (exp % 2 == 1) {
                    const new_result = try Self.mulHelper(self.gpa, result, base, max_degree);
                    result.deinit();
                    result = new_result;
                }
                const new_base = try Self.mulHelper(self.gpa, base, base, max_degree);
                base.deinit();
                base = new_base;
                exp /= 2;
            }

            // Update self with result
            try self.resize(max_degree);
            @memcpy(self.coeffs, result.coeffs);
        }

        // Helper functions that return new Self for internal use in inv and pow
        fn addHelper(gpa: std.mem.Allocator, a: Self, b: Self, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            const len = @min(result.coeffs.len, @max(a.coeffs.len, b.coeffs.len));
            for (0..len) |i| {
                const ai = if (i < a.coeffs.len) a.coeffs[i] else ModintType.fromInt(0);
                const bi = if (i < b.coeffs.len) b.coeffs[i] else ModintType.fromInt(0);
                result.coeffs[i] = ai.add(bi);
            }
            return result;
        }

        fn subHelper(gpa: std.mem.Allocator, a: Self, b: Self, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            const len = @min(result.coeffs.len, @max(a.coeffs.len, b.coeffs.len));
            for (0..len) |i| {
                const ai = if (i < a.coeffs.len) a.coeffs[i] else ModintType.fromInt(0);
                const bi = if (i < b.coeffs.len) b.coeffs[i] else ModintType.fromInt(0);
                result.coeffs[i] = ai.sub(bi);
            }
            return result;
        }

        fn mulHelper(gpa: std.mem.Allocator, a: Self, b: Self, max_degree: usize) !Self {
            if (use_ntt) {
                // Use NTT convolution
                const conv = try Ntt.convolution(gpa, a.coeffs, b.coeffs);
                defer gpa.free(conv);
                var result = try Self.init(gpa, max_degree);
                const copy_len = @min(conv.len, result.coeffs.len);
                @memcpy(result.coeffs[0..copy_len], conv[0..copy_len]);
                return result;
            } else {
                var result = try Self.init(gpa, max_degree);
                for (0..a.coeffs.len) |i| {
                    if (i > max_degree) break;
                    const ai = a.coeffs[i];
                    if (ai.toInt() == 0) continue;
                    for (0..b.coeffs.len) |j| {
                        const k = i + j;
                        if (k > max_degree) break;
                        const bj = b.coeffs[j];
                        result.coeffs[k] = result.coeffs[k].add(ai.mul(bj));
                    }
                }
                return result;
            }
        }
    };
}

/// Formal Power Series (Polynomial) modulo `comptime MOD` using NTT (requires ROOT: primitive root of MOD)
pub fn FpsNtt(comptime ModintType: type, comptime fps_root: comptime_int) type {
    return FpsImpl(ModintType, true, fps_root);
}

/// Formal Power Series (Polynomial) modulo `comptime MOD` using naive O(n²) multiplication
pub fn FpsNaive(comptime ModintType: type) type {
    return FpsImpl(ModintType, false, null);
}

/// Predefined FPS for modulus 998244353 using NTT (primitive root 3)
pub const Modint998244353 = modint.MontgomeryModint(998244353);
pub const Fps998244353 = FpsNtt(Modint998244353, 3);

test "fps basic operations" {
    const gpa = std.testing.allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // Test fromSlice
    const a_coeffs = [_]Modint{ Modint.fromInt(1), Modint.fromInt(2), Modint.fromInt(3) };
    var a = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer a.deinit();

    const expected_a = [_]Modint{ Modint.fromInt(1), Modint.fromInt(2), Modint.fromInt(3), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0) };
    for (0..a.coeffs.len) |i| {
        try std.testing.expectEqual(expected_a[i].toInt(), a.coeffs[i].toInt());
    }

    // Test add
    const b_coeffs = [_]Modint{ Modint.fromInt(4), Modint.fromInt(5) };
    var b = try FPS.fromSlice(gpa, &b_coeffs, 10);
    defer b.deinit();
    var c = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer c.deinit();
    try c.add(b, 10);

    const expected_c = [_]Modint{ Modint.fromInt(5), Modint.fromInt(7), Modint.fromInt(3), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0) };
    for (0..c.coeffs.len) |i| {
        try std.testing.expectEqual(expected_c[i].toInt(), c.coeffs[i].toInt());
    }

    // Test one and zero
    var d = try FPS.one(gpa, 5);
    defer d.deinit();
    const expected_d = [_]Modint{ Modint.fromInt(1), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0) };
    for (0..6) |i| {
        try std.testing.expectEqual(expected_d[i].toInt(), d.coeffs[i].toInt());
    }

    var e = try FPS.zero(gpa, 5);
    defer e.deinit();
    const expected_e = [_]Modint{ Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0) };
    for (0..6) |i| {
        try std.testing.expectEqual(expected_e[i].toInt(), e.coeffs[i].toInt());
    }
}

test "fps multiplication" {
    const gpa = std.testing.allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // (1 + x) * (1 + x) = 1 + 2x + x²
    const a_coeffs = [_]Modint{ Modint.fromInt(1), Modint.fromInt(1) };
    var a = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer a.deinit();
    var b = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer b.deinit();
    try b.mul(a, 10);

    const expected_b = [_]Modint{ Modint.fromInt(1), Modint.fromInt(2), Modint.fromInt(1), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0) };
    for (0..b.coeffs.len) |i| {
        try std.testing.expectEqual(expected_b[i].toInt(), b.coeffs[i].toInt());
    }
}

test "fps inverse" {
    const gpa = std.testing.allocator;
    const MOD: usize = 998244353;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // Inverse of (1 - x) is 1 + x + x² + ... + x^n
    const a_coeffs = [_]Modint{ Modint.fromInt(1), Modint.fromInt(MOD - 1) }; // 1 - x
    var a = try FPS.fromSlice(gpa, &a_coeffs, 5);
    defer a.deinit();
    try a.inv(5);

    const expected_inv = [_]Modint{ Modint.fromInt(1), Modint.fromInt(1), Modint.fromInt(1), Modint.fromInt(1), Modint.fromInt(1), Modint.fromInt(1) };
    for (0..a.coeffs.len) |i| {
        try std.testing.expectEqual(expected_inv[i].toInt(), a.coeffs[i].toInt());
    }
}

test "fps pow" {
    const gpa = std.testing.allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // (1 + x)^3 = 1 + 3x + 3x² + x³
    const a_coeffs = [_]Modint{ Modint.fromInt(1), Modint.fromInt(1) };
    var a = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer a.deinit();
    try a.pow(3, 10);

    const expected_pow = [_]Modint{ Modint.fromInt(1), Modint.fromInt(3), Modint.fromInt(3), Modint.fromInt(1), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0), Modint.fromInt(0) };
    for (0..a.coeffs.len) |i| {
        try std.testing.expectEqual(expected_pow[i].toInt(), a.coeffs[i].toInt());
    }
}
