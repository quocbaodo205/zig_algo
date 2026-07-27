const std = @import("std");
const ntt = @import("ntt.zig");
const modint = @import("modint.zig");
const fft = @import("fft.zig");

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

        /// Return the canonical (smaller) modular square root, or null if none exists.
        /// MOD must be prime. Uses Tonelli-Shanks for the general odd-prime case.
        fn scalarSqrt(value: ModintType) ?ModintType {
            const value_int = value.toInt();
            if (value_int == 0) return ModintType.fromInt(0);
            if (MOD == 2) return value;

            const one_value = ModintType.fromInt(1);
            if (value.pow((MOD - 1) / 2).toInt() != one_value.toInt()) return null;

            var root: ModintType = undefined;
            if (MOD % 4 == 3) {
                root = value.pow((MOD + 1) / 4);
            } else {
                var q: u32 = MOD - 1;
                var s: u32 = 0;
                while (q % 2 == 0) {
                    q /= 2;
                    s += 1;
                }

                var z_int: u32 = 2;
                while (ModintType.fromInt(z_int).pow((MOD - 1) / 2).toInt() != MOD - 1) {
                    z_int += 1;
                }

                var c = ModintType.fromInt(z_int).pow(q);
                var x = value.pow((q + 1) / 2);
                var t = value.pow(q);
                var m = s;

                while (t.toInt() != 1) {
                    var i: u32 = 1;
                    var t_squared = t.mul(t);
                    while (i < m and t_squared.toInt() != 1) : (i += 1) {
                        t_squared = t_squared.mul(t_squared);
                    }
                    if (i == m) return null;

                    const exponent: u32 = @as(u32, 1) << @intCast(m - i - 1);
                    const b = c.pow(exponent);
                    const b_squared = b.mul(b);
                    x = x.mul(b);
                    t = t.mul(b_squared);
                    c = b_squared;
                    m = i;
                }
                root = x;
            }

            const root_int = root.toInt();
            const other_int = MOD - root_int;
            return if (root_int <= other_int)
                root
            else
                ModintType.fromInt(other_int);
        }

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

        /// Create the polynomial with all coefficients 1: 1 + x + x² + ... + x^max_degree
        pub fn allOnes(gpa: std.mem.Allocator, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            for (0..result.coeffs.len) |i| {
                result.coeffs[i] = ModintType.fromInt(1);
            }
            return result;
        }

        /// Create the polynomial with coefficients 0,1,2,...,max_degree: 0 + 1x + 2x² + ... + max_degree x^max_degree
        pub fn increasing(gpa: std.mem.Allocator, max_degree: usize) !Self {
            var result = try Self.init(gpa, max_degree);
            for (0..result.coeffs.len) |i| {
                result.coeffs[i] = ModintType.fromInt(@intCast(i));
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

            if (use_ntt and max_degree > 0) {
                // Use NTT convolution only for max_degree > 0
                const conv = try Ntt.convolution(a.gpa, a_copy.coeffs, b.coeffs);
                defer a.gpa.free(conv);
                try a.resize(max_degree);
                const copy_len = @min(conv.len, a.coeffs.len);
                @memcpy(a.coeffs[0..copy_len], conv[0..copy_len]);
            } else {
                // Naive O(n²) multiplication (for max_degree 0 or use_ntt false)
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

        /// Square the FPS in-place: a *= a, truncating to max_degree
        pub fn square(a: *Self, max_degree: usize) !void {
            var a_copy = try Self.fromSlice(a.gpa, a.coeffs, max_degree);
            defer a_copy.deinit();
            try a.mul(a_copy, max_degree);
        }

        /// Compute sum of pairwise convolutions of the given FPS: sum_{i<j} fps[i] * fps[j]
        /// Returns a new FPS, caller must free with deinit()
        pub fn sumPairwiseConvolution(gpa: std.mem.Allocator, fps_list: []const Self, max_degree: usize) !Self {
            // Compute S = sum(fps_list)
            var S = try Self.zero(gpa, max_degree);
            errdefer S.deinit();
            for (fps_list) |cfps| {
                try S.add(cfps, max_degree);
            }

            // Compute S^2
            try S.square(max_degree);

            // Compute sum_squares = sum(fps^2)
            var sum_squares = try Self.zero(gpa, max_degree);
            errdefer sum_squares.deinit();
            for (fps_list) |cfps| {
                var fps_sq = try Self.fromSlice(gpa, cfps.coeffs, max_degree);
                defer fps_sq.deinit();
                try fps_sq.square(max_degree);
                try sum_squares.add(fps_sq, max_degree);
            }

            // Compute (S^2 - sum_squares)
            try S.sub(sum_squares, max_degree);

            // Multiply by 1/2
            const inv2 = ModintType.fromInt(2).inv();
            for (S.coeffs) |*c| {
                c.* = c.*.mul(inv2);
            }

            return S;
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

        /// Compute the canonical FPS square root in-place modulo x^(max_degree + 1).
        ///
        /// Returns error.NoSquareRoot when the first non-zero degree is odd or
        /// its coefficient is a quadratic non-residue. The all-zero series has
        /// the all-zero square root. For two scalar roots, the smaller residue
        /// is chosen; in particular, sqrt(1) has constant term +1.
        pub fn sqrt(self: *Self, max_degree: usize) !void {
            if (MOD == 2) return error.UnsupportedModulus;

            var source = try Self.fromSlice(self.gpa, self.coeffs, max_degree);
            defer source.deinit();

            var first_non_zero: usize = 0;
            while (first_non_zero <= max_degree and source.coeffs[first_non_zero].toInt() == 0) {
                first_non_zero += 1;
            }

            if (first_non_zero > max_degree) {
                try self.resize(max_degree);
                @memset(self.coeffs, ModintType.fromInt(0));
                return;
            }
            if (first_non_zero % 2 == 1) return error.NoSquareRoot;

            const leading = source.coeffs[first_non_zero];
            const leading_root = scalarSqrt(leading) orelse return error.NoSquareRoot;
            const leading_inv = leading.inv();

            // Remove x^first_non_zero and normalize the constant term to 1.
            const normalized_degree = max_degree - first_non_zero;
            var normalized = try Self.zero(self.gpa, normalized_degree);
            defer normalized.deinit();
            for (0..normalized_degree + 1) |i| {
                normalized.coeffs[i] = source.coeffs[first_non_zero + i].mul(leading_inv);
            }

            // Newton iteration: q <- (q + normalized / q) / 2.
            var result = try Self.one(self.gpa, 0);
            defer result.deinit();
            var current_degree: usize = 0;
            const inv2 = ModintType.fromInt(2).inv();

            while (current_degree < normalized_degree) {
                const next_degree = @min(2 * current_degree + 1, normalized_degree);

                var normalized_trunc = try Self.fromSlice(
                    self.gpa,
                    normalized.coeffs[0 .. next_degree + 1],
                    next_degree,
                );
                defer normalized_trunc.deinit();

                var result_extended = try Self.fromSlice(
                    self.gpa,
                    result.coeffs,
                    next_degree,
                );
                defer result_extended.deinit();

                var result_inv = try Self.fromSlice(
                    self.gpa,
                    result.coeffs,
                    next_degree,
                );
                defer result_inv.deinit();
                try result_inv.inv(next_degree);

                const quotient = try Self.mulHelper(
                    self.gpa,
                    normalized_trunc,
                    result_inv,
                    next_degree,
                );
                defer quotient.deinit();

                const new_result = try Self.addHelper(
                    self.gpa,
                    result_extended,
                    quotient,
                    next_degree,
                );
                for (new_result.coeffs) |*coefficient| {
                    coefficient.* = coefficient.*.mul(inv2);
                }

                result.deinit();
                result = new_result;
                current_degree = next_degree;
            }

            // Restore the scalar root and half of the removed x-shift.
            const result_shift = first_non_zero / 2;
            var shifted_result = try Self.zero(self.gpa, max_degree);
            defer shifted_result.deinit();
            for (result.coeffs, 0..) |coefficient, i| {
                shifted_result.coeffs[result_shift + i] = coefficient.mul(leading_root);
            }

            try self.resize(max_degree);
            @memcpy(self.coeffs, shifted_result.coeffs);
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
            return inv_cache[1 .. n + 1];
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

// Special trick for 1 / (1 - x^k) FPS: mul / div is + / - prefix sum style.
// In-place multiplication by 1 / (1 - x^k)
// mul 1 + x^k + x^2k + x^3k + ... is the same as prefix sum at step k
// fn addCoin(cur: []Modint998244353, k: usize, n: usize) void {
//     var j: usize = k;
//     while (j <= n) : (j += 1) {
//         cur[j] = cur[j].add(cur[j - k]);
//     }
// }

// In-place multiplication by (1 - x^k)
// reverse the prefix sum
// fn removeCoin(cur: []Modint998244353, k: usize, n: usize) void {
//     var j: usize = n;
//     while (j >= k) : (j -= 1) {
//         cur[j] = cur[j].sub(cur[j - k]);
//     }
// }

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

/// FPS using FFT for convolution (f64 coefficients)
pub const FpsFft = struct {
    coeffs: []f64,
    gpa: std.mem.Allocator,

    const Self = @This();

    /// Initialize an FPS with all zeros
    pub fn init(gpa: std.mem.Allocator, max_degree: usize) !Self {
        const coeffs = try gpa.alloc(f64, max_degree + 1);
        @memset(coeffs, 0.0);
        return Self{
            .coeffs = coeffs,
            .gpa = gpa,
        };
    }

    /// Create FPS from a slice of coefficients
    pub fn fromSlice(gpa: std.mem.Allocator, coeffs: []const f64, max_degree: usize) !Self {
        var result = try Self.init(gpa, max_degree);
        const copy_len = @min(coeffs.len, result.coeffs.len);
        @memcpy(result.coeffs[0..copy_len], coeffs[0..copy_len]);
        return result;
    }

    /// Create the zero polynomial (all coefficients zero)
    pub fn zero(gpa: std.mem.Allocator, max_degree: usize) !Self {
        return try Self.init(gpa, max_degree);
    }

    /// Free allocated memory
    pub fn deinit(self: Self) void {
        self.gpa.free(self.coeffs);
    }

    /// Resize the FPS to a new max_degree, keeping existing coefficients
    pub fn resize(self: *Self, new_max_degree: usize) !void {
        const new_coeffs = try self.gpa.alloc(f64, new_max_degree + 1);
        @memset(new_coeffs, 0.0);
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
            const bi = if (i < b.coeffs.len) b.coeffs[i] else 0.0;
            a.coeffs[i] += bi;
        }
    }

    /// Subtract two FPS in-place: a -= b, truncating to max_degree
    pub fn sub(a: *Self, b: Self, max_degree: usize) !void {
        try a.resize(max_degree);
        const len = @min(a.coeffs.len, @max(a.coeffs.len, b.coeffs.len));
        for (0..len) |i| {
            const bi = if (i < b.coeffs.len) b.coeffs[i] else 0.0;
            a.coeffs[i] -= bi;
        }
    }

    /// Multiply two FPS in-place: a *= b, truncating to max_degree (using FFT)
    pub fn mul(a: *Self, b: Self, max_degree: usize) !void {
        // Need a temporary because a is both input and output
        var a_copy = try Self.fromSlice(a.gpa, a.coeffs, max_degree);
        defer a_copy.deinit();

        // Use FFT convolution
        const conv = try fft.convolution(a.gpa, a_copy.coeffs, b.coeffs);
        defer a.gpa.free(conv);

        try a.resize(max_degree);
        const copy_len = @min(conv.len, a.coeffs.len);
        @memcpy(a.coeffs[0..copy_len], conv[0..copy_len]);
    }

    /// Square the FPS in-place: a *= a, truncating to max_degree
    pub fn square(a: *Self, max_degree: usize) !void {
        var a_copy = try Self.fromSlice(a.gpa, a.coeffs, max_degree);
        defer a_copy.deinit();
        try a.mul(a_copy, max_degree);
    }

    /// Compute sum of pairwise convolutions of the given FPS: sum_{i<j} fps[i] * fps[j]
    /// Returns a new FPS, caller must free with deinit()
    pub fn sumPairwiseConvolution(gpa: std.mem.Allocator, fps_list: []const Self, max_degree: usize) !Self {
        // Compute S = sum(fps_list)
        var S = try Self.zero(gpa, max_degree);
        errdefer S.deinit();
        for (fps_list) |cfps| {
            try S.add(cfps, max_degree);
        }

        // Compute S^2
        try S.square(max_degree);

        // Compute sum_squares = sum(fps^2)
        var sum_squares = try Self.zero(gpa, max_degree);
        errdefer sum_squares.deinit();
        for (fps_list) |cfps| {
            var fps_sq = try Self.fromSlice(gpa, cfps.coeffs, max_degree);
            defer fps_sq.deinit();
            try fps_sq.square(max_degree);
            try sum_squares.add(fps_sq, max_degree);
        }

        // Compute (S^2 - sum_squares)
        for (S.coeffs, 0..) |*c, i| {
            const ss = if (i < sum_squares.coeffs.len) sum_squares.coeffs[i] else 0.0;
            c.* -= ss;
        }

        // Divide by 2
        for (S.coeffs) |*c| {
            c.* /= 2.0;
        }

        return S;
    }
};

test "fps basic operations" {
    const gpa = std.heap.page_allocator;
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
    const gpa = std.heap.page_allocator;
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
    const gpa = std.heap.page_allocator;
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
    const a_coeffs = [_]Modint{ Modint.fromInt(1), Modint.fromInt(1), Modint.fromInt(1), Modint.fromInt(1), Modint.fromInt(1), Modint.fromInt(1) };
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
    const gpa = std.heap.page_allocator;
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

test "fps square" {
    const gpa = std.testing.allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // Test (1 + x)^2 = 1 + 2x + x²
    const a_coeffs = [_]Modint{ Modint.fromInt(1), Modint.fromInt(1) };
    var a = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer a.deinit();
    try a.square(10);

    const expected = [_]Modint{
        Modint.fromInt(1),
        Modint.fromInt(2),
        Modint.fromInt(1),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
    };
    for (0..a.coeffs.len) |i| {
        try std.testing.expectEqual(expected[i].toInt(), a.coeffs[i].toInt());
    }
}

test "fps sqrt" {
    const gpa = std.testing.allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Mint = Modint998244353;
    const MOD = Mint.MOD;

    // CF 438E branch: sqrt(1 - 4x) starts with +1.
    {
        const input = [_]Mint{
            Mint.fromInt(1),
            Mint.fromInt(MOD - 4),
        };
        var root = try FPS.fromSlice(gpa, &input, 5);
        defer root.deinit();
        try root.sqrt(5);

        const expected = [_]Mint{
            Mint.fromInt(1),
            Mint.fromInt(MOD - 2),
            Mint.fromInt(MOD - 2),
            Mint.fromInt(MOD - 4),
            Mint.fromInt(MOD - 10),
            Mint.fromInt(MOD - 28),
        };
        for (expected, 0..) |coefficient, i| {
            try std.testing.expectEqual(coefficient.toInt(), root.coeffs[i].toInt());
        }

        var squared = try FPS.fromSlice(gpa, root.coeffs, 5);
        defer squared.deinit();
        try squared.square(5);
        try std.testing.expectEqual(@as(u32, 1), squared.coeffs[0].toInt());
        try std.testing.expectEqual(@as(u32, MOD - 4), squared.coeffs[1].toInt());
        for (2..6) |i| {
            try std.testing.expectEqual(@as(u32, 0), squared.coeffs[i].toInt());
        }
    }

    // Shifted root: 4x² + 4x³ + x⁴ = (2x + x²)².
    {
        const input = [_]Mint{
            Mint.fromInt(0),
            Mint.fromInt(0),
            Mint.fromInt(4),
            Mint.fromInt(4),
            Mint.fromInt(1),
        };
        var root = try FPS.fromSlice(gpa, &input, 5);
        defer root.deinit();
        try root.sqrt(5);

        const expected = [_]u32{ 0, 2, 1, 0, 0, 0 };
        for (expected, 0..) |coefficient, i| {
            try std.testing.expectEqual(coefficient, root.coeffs[i].toInt());
        }
    }

    // The zero series has the zero square root.
    {
        var zero = try FPS.zero(gpa, 4);
        defer zero.deinit();
        try zero.sqrt(4);
        for (zero.coeffs) |coefficient| {
            try std.testing.expectEqual(@as(u32, 0), coefficient.toInt());
        }
    }

    // Odd valuation and a quadratic non-residue have no FPS square root.
    {
        const odd_shift = [_]Mint{ Mint.fromInt(0), Mint.fromInt(1) };
        var value = try FPS.fromSlice(gpa, &odd_shift, 3);
        defer value.deinit();
        try std.testing.expectError(error.NoSquareRoot, value.sqrt(3));
    }
    {
        const non_residue = [_]Mint{Mint.fromInt(3)};
        var value = try FPS.fromSlice(gpa, &non_residue, 3);
        defer value.deinit();
        try std.testing.expectError(error.NoSquareRoot, value.sqrt(3));
    }
}

test "fps sum pairwise convolution" {
    const gpa = std.heap.page_allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // Test with three polynomials: f1 = 1, f2 = x, f3 = x²
    // Pairwise products: f1*f2 = x, f1*f3 = x², f2*f3 = x³
    // Sum: x + x² + x³
    var f1 = try FPS.one(gpa, 10);
    defer f1.deinit();
    var f2 = try FPS.zero(gpa, 10);
    defer f2.deinit();
    f2.coeffs[1] = Modint.fromInt(1);
    var f3 = try FPS.zero(gpa, 10);
    defer f3.deinit();
    f3.coeffs[2] = Modint.fromInt(1);

    const fps_list = [_]FPS{ f1, f2, f3 };
    var result = try FPS.sumPairwiseConvolution(gpa, &fps_list, 10);
    defer result.deinit();

    const expected = [_]Modint{
        Modint.fromInt(0),
        Modint.fromInt(1),
        Modint.fromInt(1),
        Modint.fromInt(1),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
        Modint.fromInt(0),
    };
    for (0..result.coeffs.len) |i| {
        try std.testing.expectEqual(expected[i].toInt(), result.coeffs[i].toInt());
    }
}

test "fps mul max degree 0" {
    const gpa = std.testing.allocator;
    const FPS = FpsNtt(Modint998244353, 3);
    const Modint = Modint998244353;

    // Create two FPS with max_degree = 0, coeff 0 equal to 1
    var f1 = try FPS.init(gpa, 0);
    defer f1.deinit();
    f1.coeffs[0] = Modint.fromInt(1);

    var f2 = try FPS.init(gpa, 0);
    defer f2.deinit();
    f2.coeffs[0] = Modint.fromInt(1);

    // Multiply them with max_degree = 0
    try f1.mul(f2, 0);

    // Expected: 1 * 1 = 1
    const expected = [_]Modint{
        Modint.fromInt(1),
    };
    for (0..f1.coeffs.len) |i| {
        try std.testing.expectEqual(expected[i].toInt(), f1.coeffs[i].toInt());
    }
}

test "modint inv" {
    const Modint = Modint998244353;
    const a = Modint.fromInt(2);
    const inv_a = a.inv();
    const product = a.mul(inv_a);
    try std.testing.expectEqual(@as(u32, 1), product.toInt());
}

test "fps fft basic operations" {
    const gpa = std.testing.allocator;
    const FPS = FpsFft;

    // Test fromSlice
    const a_coeffs = [_]f64{ 1, 2, 3 };
    var a = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer a.deinit();

    const expected_a = [_]f64{ 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0 };
    for (0..a.coeffs.len) |i| {
        try std.testing.expectEqual(expected_a[i], a.coeffs[i]);
    }

    // Test add
    const b_coeffs = [_]f64{ 4, 5 };
    var b = try FPS.fromSlice(gpa, &b_coeffs, 10);
    defer b.deinit();
    var c = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer c.deinit();
    try c.add(b, 10);

    const expected_c = [_]f64{ 5, 7, 3, 0, 0, 0, 0, 0, 0, 0, 0 };
    for (0..c.coeffs.len) |i| {
        try std.testing.expectEqual(expected_c[i], c.coeffs[i]);
    }
}

test "fps fft multiplication" {
    const gpa = std.testing.allocator;
    const FPS = FpsFft;

    // (1 + x) * (1 + x) = 1 + 2x + x²
    const a_coeffs = [_]f64{ 1, 1 };
    var a = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer a.deinit();
    var b = try FPS.fromSlice(gpa, &a_coeffs, 10);
    defer b.deinit();
    try b.mul(a, 10);

    const expected_b = [_]f64{ 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0 };
    for (0..b.coeffs.len) |i| {
        try std.testing.expectEqual(expected_b[i], b.coeffs[i]);
    }
}

test "fps fft sum pairwise convolution" {
    const gpa = std.heap.page_allocator;
    const FPS = FpsFft;

    // Test with three polynomials: f1 = 1, f2 = x, f3 = x²
    // Pairwise products: f1*f2 = x, f1*f3 = x², f2*f3 = x³
    // Sum: x + x² + x³
    var f1 = try FPS.init(gpa, 10);
    defer f1.deinit();
    f1.coeffs[0] = 1.0;
    var f2 = try FPS.init(gpa, 10);
    defer f2.deinit();
    f2.coeffs[1] = 1.0;
    var f3 = try FPS.init(gpa, 10);
    defer f3.deinit();
    f3.coeffs[2] = 1.0;

    const fps_list = [_]FPS{ f1, f2, f3 };
    var result = try FPS.sumPairwiseConvolution(gpa, &fps_list, 10);
    defer result.deinit();

    const expected = [_]f64{ 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0 };
    for (0..result.coeffs.len) |i| {
        try std.testing.expectEqual(expected[i], result.coeffs[i]);
    }
}
