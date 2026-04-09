const std = @import("std");
const modint = struct {
const math = std.math;

/// Montgomery modular integer for a given prime modulus MOD
/// Uses R = 2^64 (since we're using u64)
pub fn MontgomeryModint(comptime MOD_ARG: u64) type {
    // Precompute R = 2^64 mod MOD and R2 = (R * R) mod MOD
    const R: u64 = (@as(u128, 1) << 64) % MOD_ARG;
    const R2: u64 = (@as(u128, R) * R) % MOD_ARG;
    const M: u64 = blk: {
        // Find M such that MOD * M ≡ -1 (mod 2^64)
        // Use Newton's method: x_{k+1} = x_k * (2 - MOD * x_k) mod 2^64
        var x: u64 = 1; // Initial guess
        var i: usize = 0;
        while (i < 6) : (i += 1) { // 6 iterations are enough for 64 bits
            x = x *% (2 -% MOD_ARG *% x);
        }
        break :blk x;
    };

    return struct {
        pub const MOD = MOD_ARG;
        val: u64,

        const Self = @This();

        /// Convert a normal integer to Montgomery form
        pub fn fromInt(n: u64) Self {
            return Self{ .val = montgomeryMult(n, R2) };
        }

        /// Convert back to a normal integer
        pub fn toInt(self: Self) u64 {
            return montgomeryMult(self.val, 1);
        }

        /// Montgomery multiplication: (a * b) / R mod MOD
        inline fn montgomeryMult(a: u64, b: u64) u64 {
            const t = @as(u128, a) * b;
            const m = @as(u64, @intCast(t)) *% M;
            const u = (t + @as(u128, m) * MOD) >> 64;
            const u64_val: u64 = @intCast(u);
            return if (u64_val >= MOD) u64_val - MOD else u64_val;
        }

        pub fn add(a: Self, b: Self) Self {
            const s = a.val + b.val;
            return Self{ .val = if (s >= MOD) s - MOD else s };
        }

        pub fn sub(a: Self, b: Self) Self {
            const d = a.val + MOD - b.val;
            return Self{ .val = if (d >= MOD) d - MOD else d };
        }

        pub fn mul(a: Self, b: Self) Self {
            return Self{ .val = montgomeryMult(a.val, b.val) };
        }

        pub fn neg(a: Self) Self {
            return Self{ .val = if (a.val == 0) 0 else MOD - a.val };
        }
    };
}

/// SIMD Montgomery modular integer for a given prime modulus MOD (8 lanes)
/// Uses R = 2^64 (since we're using u64)
pub fn MontgomeryModintx8(comptime MOD_ARG: u64) type {
    // Precompute R = 2^64 mod MOD and R2 = (R * R) mod MOD
    const R: u64 = (@as(u128, 1) << 64) % MOD_ARG;
    const R2: u64 = (@as(u128, R) * R) % MOD_ARG;
    const M: u64 = blk: {
        // Find M such that MOD * M ≡ -1 (mod 2^64)
        // Use Newton's method: x_{k+1} = x_k * (2 - MOD * x_k) mod 2^64
        var x: u64 = 1; // Initial guess
        var i: usize = 0;
        while (i < 6) : (i += 1) { // 6 iterations are enough for 64 bits
            x = x *% (2 -% MOD_ARG *% x);
        }
        break :blk x;
    };

    return struct {
        pub const MOD = MOD_ARG;
        pub const SCALAR = MontgomeryModint(MOD_ARG);
        val: @Vector(8, u64),

        const Self = @This();

        // Montgomery multiplication for scalars (used internally)
        inline fn montgomeryMultScalar(a: u64, b: u64) u64 {
            const t = @as(u128, a) * b;
            const m = @as(u64, @intCast(t)) *% M;
            const u = (t + @as(u128, m) * MOD_ARG) >> 64;
            const u64_val: u64 = @intCast(u);
            return if (u64_val >= MOD_ARG) u64_val - MOD_ARG else u64_val;
        }

        /// Convert 8 normal integers to Montgomery form
        pub fn fromInts(n: @Vector(8, u64)) Self {
            var result: @Vector(8, u64) = undefined;
            inline for (0..8) |i| {
                result[i] = montgomeryMultScalar(n[i], R2);
            }
            return Self{ .val = result };
        }

        /// Convert a single normal integer to Montgomery form (all 8 lanes)
        pub fn fromInt(n: u64) Self {
            return fromInts(@splat(n));
        }

        /// Convert from a single scalar MontgomeryModint to all 8 lanes
        pub fn fromScalar(s: SCALAR) Self {
            return Self{ .val = @splat(s.val) };
        }

        /// Convert back to 8 normal integers
        pub fn toInts(self: Self) @Vector(8, u64) {
            var result: @Vector(8, u64) = undefined;
            inline for (0..8) |i| {
                result[i] = montgomeryMultScalar(self.val[i], 1);
            }
            return result;
        }

        /// Convert back to a single normal integer (first lane, for compatibility)
        pub fn toInt(self: Self) u64 {
            return montgomeryMultScalar(self.val[0], 1);
        }

        pub fn add(a: Self, b: Self) Self {
            const s = a.val + b.val;
            const mod_vec: @Vector(8, u64) = @splat(MOD_ARG);
            const mask = s >= mod_vec;
            // Use wrapping subtraction to avoid overflow, then select
            const subtracted = s -% mod_vec;
            return Self{ .val = @select(u64, mask, subtracted, s) };
        }

        pub fn sub(a: Self, b: Self) Self {
            const mod_vec: @Vector(8, u64) = @splat(MOD_ARG);
            const d = a.val + mod_vec -% b.val; // Use wrapping subtraction for b.val
            const mask = d >= mod_vec;
            const subtracted = d -% mod_vec; // Wrapping subtraction here too
            return Self{ .val = @select(u64, mask, subtracted, d) };
        }

        pub fn mul(a: Self, b: Self) Self {
            var result: @Vector(8, u64) = undefined;
            inline for (0..8) |i| {
                result[i] = montgomeryMultScalar(a.val[i], b.val[i]);
            }
            return Self{ .val = result };
        }

        pub fn neg(a: Self) Self {
            const mod_vec: @Vector(8, u64) = @splat(MOD_ARG);
            const zero_vec: @Vector(8, u64) = @splat(0);
            const mask = a.val == zero_vec;
            const negated = mod_vec - a.val;
            return Self{ .val = @select(u64, mask, zero_vec, negated) };
        }
    };
}

/// Predefined MontgomeryModint for modulus 998244353
pub const Modint998244353 = MontgomeryModint(998244353);
/// Predefined SIMD MontgomeryModintx8 for modulus 998244353
pub const Modint998244353x8 = MontgomeryModintx8(998244353);

};
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
const combinatorics = struct {

/// Calculate combination C(n, k) using multiplicative formula to avoid overflow as much as possible.
/// Time complexity: O(k)
/// Space complexity: O(1)
pub fn comb(n: usize, k: usize) usize {
    if (k > n) return 0;
    if (k == 0 or k == n) return 1;

    // Take advantage of symmetry C(n, k) = C(n, n-k)
    const k_min = if (k > n - k) n - k else k;

    var res: usize = 1;
    var i: usize = 1;
    while (i <= k_min) : (i += 1) {
        res = res * (n - k_min + i) / i;
    }

    return res;
}

/// Calculate the number of ways to choose N items from M types with infinite quantity (stars and bars).
/// Formula: C(M + N - 1, N)
pub fn starsAndBars(m: usize, n: usize) usize {
    return comb(m + n - 1, n);
}

// ==========================================
// Modular combinatorics for large numbers
// ==========================================

/// Compute (base^exponent) mod modu using binary exponentiation.
/// Time complexity: O(log exponent)
pub fn pow_mod(base: usize, exponent: usize, modu: usize) usize {
    var result: usize = 1;
    var b = base % modu;
    var e = exponent;

    while (e > 0) {
        if (e % 2 == 1) {
            result = (result * b) % modu;
        }
        b = (b * b) % modu;
        e = e / 2;
    }

    return result;
}

/// Compute modular inverse using Fermat's Little Theorem.
/// Only valid when modu is prime.
/// inv(x) = x^(modu-2) mod modu
pub fn mod_inverse(x: usize, modu: usize) usize {
    return pow_mod(x, modu - 2, modu);
}

/// Compute C(a, b) mod p where p is prime and 0 ≤ b ≤ a < p.
/// Uses multiplicative formula and Fermat's Little Theorem for inverses.
pub fn comb_mod_small(a: usize, b: usize, p: usize) usize {
    if (b > a) return 0;
    if (b == 0 or b == a) return 1;

    // Use symmetry to minimize calculations
    const k = if (b > a - b) a - b else b;

    var numerator: usize = 1;
    var denominator: usize = 1;

    var i: usize = 0;
    while (i < k) : (i += 1) {
        numerator = (numerator * (a - i)) % p;
        denominator = (denominator * (i + 1)) % p;
    }

    return (numerator * mod_inverse(denominator, p)) % p;
}

/// Compute C(n, k) mod p where p is prime using Lucas Theorem.
/// Works for very large n and k (up to 1e18 or more).
pub fn comb_mod_lucas(n: usize, k: usize, p: usize) usize {
    if (k > n) return 0;
    if (k == 0 or k == n) return 1;

    var result: usize = 1;
    var a = n;
    var b = k;

    while (a > 0 or b > 0) {
        const ai = a % p;
        const bi = b % p;

        if (bi > ai) {
            return 0;
        }

        result = (result * comb_mod_small(ai, bi, p)) % p;

        a = a / p;
        b = b / p;
    }

    return result;
}

/// Calculate the number of ways to choose N items from M types with infinite quantity (stars and bars) modulo p.
/// Formula: C(M + N - 1, N) mod p
pub fn starsAndBarsMod(m: usize, n: usize, p: usize) usize {
    return comb_mod_lucas(m + n - 1, n, p);
}

};
const ntt = struct {

/// NTT (Number Theoretic Transform) and convolution utilities for a given prime modulus MOD
/// Requires: MOD is a prime such that MOD = c * 2^k + 1 for some c, k
pub fn NttHelpers(comptime ntt_mod: comptime_int, comptime ntt_root: comptime_int) type {
    const Modint = modint.MontgomeryModintx8(ntt_mod);

    return struct {
        /// Compute bit-reversed permutation of indices
        fn bit_reverse(i: usize, log_n: usize) usize {
            var res: usize = 0;
            var j = i;
            var k: usize = 0;
            while (k < log_n) : (k += 1) {
                res = (res << 1) | (j & 1);
                j >>= 1;
            }
            return res;
        }

        /// Compute NTT (Number Theoretic Transform) in place
        /// a must have length a power of two
        pub fn ntt_f(a: []Modint, invert: bool) void {
            const n = a.len;
            const log_n = std.math.log2_int(usize, n);

            // Bit-reverse permutation
            for (0..n) |i| {
                const j = bit_reverse(i, log_n);
                if (i < j) {
                    std.mem.swap(Modint, &a[i], &a[j]);
                }
            }

            // NTT main loop
            var len: usize = 2;
            while (len <= n) : (len <<= 1) {
                const half_len = len >> 1;
                const w_len_usize = combinatorics.pow_mod(ntt_root, (ntt_mod - 1) / len, ntt_mod);
                var w_len = Modint.fromInt(@intCast(w_len_usize));
                if (invert) {
                    w_len = Modint.fromInt(@intCast(combinatorics.mod_inverse(w_len_usize, ntt_mod)));
                }
                var i: usize = 0;
                while (i < n) : (i += len) {
                    var w = Modint.fromInt(1);
                    var j: usize = 0;
                    while (j < half_len) : (j += 1) {
                        const u = a[i + j];
                        const v = a[i + j + half_len].mul(w);
                        a[i + j] = u.add(v);
                        a[i + j + half_len] = u.sub(v);
                        w = w.mul(w_len);
                    }
                }
            }

            // Inverse NTT scaling
            if (invert) {
                const inv_n_usize = combinatorics.mod_inverse(n, ntt_mod);
                const inv_n = Modint.fromInt(@intCast(inv_n_usize));
                for (0..n) |i| {
                    a[i] = a[i].mul(inv_n);
                }
            }
        }

        /// Compute convolution of a and b modulo MOD using NTT
        /// Returns a new slice with the result, caller must free with gpa
        pub fn convolution(gpa: std.mem.Allocator, a: []const Modint, b: []const Modint) ![]Modint {
            var n: usize = 1;
            const required_len = a.len + b.len - 1;
            while (n < required_len) n <<= 1;

            var fa = try gpa.alloc(Modint, n);
            defer gpa.free(fa);
            var fb = try gpa.alloc(Modint, n);
            defer gpa.free(fb);

            @memset(fa, Modint.fromInt(0));
            @memset(fb, Modint.fromInt(0));
            for (0..a.len) |i| {
                fa[i] = a[i];
            }
            for (0..b.len) |i| {
                fb[i] = b[i];
            }

            ntt_f(fa, false);
            ntt_f(fb, false);

            for (0..n) |i| {
                fa[i] = fa[i].mul(fb[i]);
            }

            ntt_f(fa, true);

            const result = try gpa.alloc(Modint, required_len);
            @memcpy(result, fa[0..required_len]);
            return result;
        }
    };
}

/// Predefined NTT helpers for common modulus 998244353 (primitive root 3)
pub const Ntt998244353 = NttHelpers(998244353, 3);

};
const fps = struct {

/// Helper to create FPS struct with given options
fn FpsImpl(comptime ModintType: type, comptime use_ntt: bool, comptime fps_root: ?comptime_int) type {
    return struct {
        coeffs: []ModintType,
        gpa: std.mem.Allocator,

        const Self = @This();

        // NTT helpers (only if use_ntt is true)
        const Ntt = if (use_ntt) ntt.NttHelpers(ModintType.MOD, fps_root.?) else void;

        /// Initialize an FPS with all zeros, with coefficients up to x^max_degree
        pub fn init(gpa: std.mem.Allocator, max_degree: usize) !Self {
            const coeffs = try gpa.alloc(ModintType, max_degree + 1);
            @memset(coeffs, ModintType.fromInt(0));
            return Self{
                .coeffs = coeffs,
                .gpa = gpa,
            };
        }

        /// Create FPS from a slice of coefficients, up to x^max_degree
        pub fn fromSlice(gpa: std.mem.Allocator, coeffs: []const ModintType, max_degree: usize) !Self {
            const result = try Self.init(gpa, max_degree);
            const copy_len = @min(coeffs.len, result.coeffs.len);
            @memcpy(result.coeffs[0..copy_len], coeffs[0..copy_len]);
            return result;
        }

        /// Create the zero polynomial (all coefficients zero) up to x^max_degree
        pub fn zero(gpa: std.mem.Allocator, max_degree: usize) !Self {
            return try Self.init(gpa, max_degree);
        }

        /// Create the polynomial 1 (constant term 1, others zero) up to x^max_degree
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

        /// Resize (truncate or pad with zeros) to max_degree
        pub fn resize(self: *Self, max_degree: usize) !void {
            if (self.coeffs.len == max_degree + 1) return;

            const new_coeffs = try self.gpa.alloc(ModintType, max_degree + 1);
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
            var a_copy = try Self.fromSlice(a.gpa, a.coeffs, a.coeffs.len - 1);
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
pub const Modint998244353 = modint.MontgomeryModintx8(998244353);
pub const Fps998244353 = FpsNtt(Modint998244353, 3);

};

const BUNDLE = false;
const Mint = modint.Modint998244353x8;

// ===================== Solving =====================

/// Main solving function for each test cases.
pub fn solve() !void {
    defer _ = allocator.arena.reset(.retain_capacity);
    const al = allocator.arena.allocator();
    const n = in.read(usize);
    const m = in.read(usize);
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

