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
        const Ntt = if (use_ntt) ntt.NttHelpers(ModintType, fps_root.?) else void;
        const MOD = ModintType.MOD;

        // Precompute inverses of 1..max_n (lazy, on first use)
        var inv_cache: []ModintType = &.{};
        var inv_cache_gpa: ?std.mem.Allocator = null;

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

        /// Compute self^exponent using binary exponentiation or ln/exp in-place, truncating to max_degree
        pub fn pow(self: *Self, exponent: usize, max_degree: usize) !void {
            // Handle trivial cases
            if (exponent == 0) {
                var one_poly = try Self.one(self.gpa, max_degree);
                defer one_poly.deinit();
                try self.resize(max_degree);
                @memcpy(self.coeffs, one_poly.coeffs);
                return;
            }
            if (exponent == 1) {
                try self.resize(max_degree);
                return;
            }

            // Find first non-zero coefficient
            var shift: usize = 0;
            while (shift < self.coeffs.len and self.coeffs[shift].toInt() == 0) : (shift += 1) {}
            if (shift >= self.coeffs.len) {
                // All zeros, result is zero
                var zero_poly = try Self.zero(self.gpa, max_degree);
                defer zero_poly.deinit();
                try self.resize(max_degree);
                @memcpy(self.coeffs, zero_poly.coeffs);
                return;
            }

            if (shift * exponent > max_degree) {
                // Result is zero
                var zero_poly = try Self.zero(self.gpa, max_degree);
                defer zero_poly.deinit();
                try self.resize(max_degree);
                @memcpy(self.coeffs, zero_poly.coeffs);
                return;
            }

            // Use ln/exp if exponent is large enough, or binary exponentiation otherwise
            const use_ln_exp = exponent > 10;

            if (use_ln_exp and shift == 0 and self.coeffs[0].toInt() == 1) {
                // Perfect case for ln/exp: starts with 1
                var ln_self = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
                defer ln_self.deinit();
                try ln_self.ln(max_degree);

                // Multiply by exponent
                for (ln_self.coeffs) |*c| {
                    c.* = c.*.mul(ModintType.fromInt(@intCast(exponent)));
                }

                // Compute exp
                try ln_self.exp(max_degree);

                // Update self
                try self.resize(max_degree);
                @memcpy(self.coeffs, ln_self.coeffs);
            } else {
                // Binary exponentiation
                var result = try Self.one(self.gpa, max_degree);
                defer result.deinit();
                var base = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
                defer base.deinit();
                var exp_val = exponent;

                while (exp_val > 0) {
                    if (exp_val % 2 == 1) {
                        const new_result = try Self.mulHelper(self.gpa, result, base, max_degree);
                        result.deinit();
                        result = new_result;
                    }
                    const new_base = try Self.mulHelper(self.gpa, base, base, max_degree);
                    base.deinit();
                    base = new_base;
                    exp_val /= 2;
                }

                // Update self with result
                try self.resize(max_degree);
                @memcpy(self.coeffs, result.coeffs);
            }
        }

        /// Get precomputed inverses of 1..n (lazy initialization)
        fn getInvCache(gpa: std.mem.Allocator, n: usize) ![]const ModintType {
            if (n == 0) return &.{};
            if (inv_cache.len <= n) {
                const new_len = @max(n + 1, inv_cache.len * 2, 32);
                if (inv_cache_gpa) |old_gpa| {
                    old_gpa.free(inv_cache);
                }
                const new_inv = try gpa.alloc(ModintType, new_len);
                @memset(new_inv, ModintType.fromInt(0));
                if (inv_cache.len > 0) {
                    @memcpy(new_inv[0..inv_cache.len], inv_cache);
                }
                for (inv_cache.len..new_len) |i| {
                    if (i == 0) {
                        new_inv[i] = ModintType.fromInt(0);
                    } else if (i == 1) {
                        new_inv[i] = ModintType.fromInt(1);
                    } else {
                        const mod_i = MOD % i;
                        const div = MOD / i;
                        const neg_div = MOD - div;
                        new_inv[i] = new_inv[mod_i].mul(ModintType.fromInt(@intCast(neg_div)));
                    }
                }
                inv_cache = new_inv;
                inv_cache_gpa = gpa;
            }
            return inv_cache[1..n+1];
        }

        /// Compute derivative of polynomial in-place: P'(x)
        pub fn deriv(self: *Self, max_degree: usize) !void {
            try self.resize(max_degree);
            // For i from 1 to max_degree: new[i-1] = i * old[i]
            var i: usize = 0;
            while (i < max_degree) : (i += 1) {
                const old_i = i + 1;
                if (old_i < self.coeffs.len) {
                    self.coeffs[i] = self.coeffs[old_i].mul(ModintType.fromInt(@intCast(old_i)));
                } else {
                    self.coeffs[i] = ModintType.fromInt(0);
                }
            }
            if (max_degree < self.coeffs.len) {
                self.coeffs[max_degree] = ModintType.fromInt(0);
            }
        }

        /// Compute integral of polynomial in-place (with constant term 0): ∫P(x)dx
        pub fn integ(self: *Self, max_degree: usize) !void {
            try self.resize(max_degree);
            const invs = try getInvCache(self.gpa, max_degree);
            // For i from max_degree down to 1: new[i] = old[i-1] / i
            var i = max_degree;
            while (i >= 1) : (i -= 1) {
                const old_i = i - 1;
                if (old_i < self.coeffs.len) {
                    self.coeffs[i] = self.coeffs[old_i].mul(invs[i - 1]);
                } else {
                    self.coeffs[i] = ModintType.fromInt(0);
                }
            }
            if (self.coeffs.len > 0) {
                self.coeffs[0] = ModintType.fromInt(0);
            }
        }

        /// Compute ln of polynomial in-place: ln(P(x))
        /// Requires: constant term of self is 1
        pub fn ln(self: *Self, max_degree: usize) !void {
            // Check constant term is 1
            if (self.coeffs.len == 0 or self.coeffs[0].toInt() != 1) {
                @panic("ln requires polynomial with constant term 1");
            }

            // Compute P'
            var p_prime = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
            defer p_prime.deinit();
            try p_prime.deriv(max_degree);

            // Compute P^{-1}
            var p_inv = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
            defer p_inv.deinit();
            try p_inv.inv(max_degree);

            // Multiply P' * P^{-1}
            var result = try Self.mulHelper(self.gpa, p_prime, p_inv, max_degree);
            defer result.deinit();

            // Integrate
            try result.integ(max_degree);

            // Update self
            try self.resize(max_degree);
            @memcpy(self.coeffs, result.coeffs);
        }

        /// Compute exp of polynomial in-place: exp(P(x))
        /// Requires: constant term of self is 0
        pub fn exp(self: *Self, max_degree: usize) !void {
            // Check constant term is 0
            if (self.coeffs.len > 0 and self.coeffs[0].toInt() != 0) {
                @panic("exp requires polynomial with constant term 0");
            }

            // Initialize result: 1, accurate up to x^0 (current_degree=0)
            var result = try Self.one(self.gpa, 0);
            defer result.deinit();
            var current_degree: usize = 0;

            while (current_degree < max_degree) {
                const next_degree = @min(2 * current_degree + 1, max_degree);

                // Compute ln(result) up to next_degree
                var result_extended = try Self.fromSlice(self.gpa, result.coeffs, next_degree);
                defer result_extended.deinit();
                try result_extended.ln(next_degree);

                // Compute (1 - ln(result) + self)
                var one_poly = try Self.one(self.gpa, next_degree);
                defer one_poly.deinit();

                var self_extended = try Self.fromSlice(self.gpa, self.coeffs[0..@min(self.coeffs.len, next_degree + 1)], next_degree);
                defer self_extended.deinit();

                var tmp1 = try Self.subHelper(self.gpa, one_poly, result_extended, next_degree);
                defer tmp1.deinit();

                var tmp2 = try Self.addHelper(self.gpa, tmp1, self_extended, next_degree);
                defer tmp2.deinit();

                // Multiply with original result (truncated to current_degree)
                var result_trunc = try Self.fromSlice(self.gpa, result.coeffs, current_degree);
                defer result_trunc.deinit();
                var tmp_trunc = try Self.fromSlice(self.gpa, tmp2.coeffs, next_degree);
                defer tmp_trunc.deinit();

                const new_result = try Self.mulHelper(self.gpa, result_trunc, tmp_trunc, next_degree);
                result.deinit();
                result = new_result;
                current_degree = next_degree;
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

test "fps deriv and integ" {
    const gpa = std.testing.allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // Test derivative of 1 + x + x^2 + x^3 + x^4 + x^5
    const a_coeffs = [_]Modint{ 
        Modint.fromInt(1), 
        Modint.fromInt(1), 
        Modint.fromInt(1), 
        Modint.fromInt(1), 
        Modint.fromInt(1), 
        Modint.fromInt(1) 
    };
    var a = try FPS.fromSlice(gpa, &a_coeffs, 5);
    defer a.deinit();

    // Compute derivative
    var deriv_a = try FPS.fromSlice(gpa, a.coeffs, 4);
    defer deriv_a.deinit();
    try deriv_a.deriv(4);

    // Expected derivative up to x^3: 1 + 2x + 3x^2 +4x^3
    const expected_deriv = [_]Modint{
        Modint.fromInt(1),
        Modint.fromInt(2),
        Modint.fromInt(3),
        Modint.fromInt(4),
        Modint.fromInt(0),
    };
    for (0..deriv_a.coeffs.len) |i| {
        try std.testing.expectEqual(expected_deriv[i].toInt(), deriv_a.coeffs[i].toInt());
    }
}

test "fps ln and exp" {
    const gpa = std.testing.allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // Test that exp(ln(1 + x)) = 1 + x
    const a_coeffs = [_]Modint{ Modint.fromInt(1), Modint.fromInt(1) }; // 1 + x
    var a = try FPS.fromSlice(gpa, &a_coeffs, 5);
    defer a.deinit();

    // Compute ln(a)
    var ln_a = try FPS.fromSlice(gpa, a.coeffs, 5);
    defer ln_a.deinit();
    try ln_a.ln(5);

    // Compute exp(ln(a))
    var exp_ln_a = try FPS.fromSlice(gpa, ln_a.coeffs, 5);
    defer exp_ln_a.deinit();
    try exp_ln_a.exp(5);

    // Should equal original a
    for (0..a.coeffs.len) |i| {
        try std.testing.expectEqual(a.coeffs[i].toInt(), exp_ln_a.coeffs[i].toInt());
    }
}

test "fps pow with ln/exp" {
    const gpa = std.testing.allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // Test pow both ways
    const a_coeffs = [_]Modint{ Modint.fromInt(1), Modint.fromInt(1) }; // 1 + x
    var a1 = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer a1.deinit();
    try a1.pow(5, 10);

    // Should be (1 + x)^5
    const expected_pow = [_]Modint{
        Modint.fromInt(1),
        Modint.fromInt(5),
        Modint.fromInt(10),
        Modint.fromInt(10),
        Modint.fromInt(5),
        Modint.fromInt(1),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
    };
    for (0..a1.coeffs.len) |i| {
        try std.testing.expectEqual(expected_pow[i].toInt(), a1.coeffs[i].toInt());
    }
}
