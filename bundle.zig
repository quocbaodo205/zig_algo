const std = @import("std");
const allocator = struct {
const heap = std.heap;
const Allocator = std.mem.Allocator;

/// Global heap + arena for quick various tasks.
/// Reset it yourself after each test cases.
pub var arena = heap.ArenaAllocator.init(heap.page_allocator);
};
const utils = struct {

/// Compute (base^exponent) mod modu using binary exponentiation (comptime)
pub fn pow_mod_comptime(base: comptime_int, exponent: comptime_int, modu: comptime_int) comptime_int {
    var b = base % modu;
    var res: comptime_int = 1;
    var e = exponent;
    while (e > 0) {
        if (e & 1 == 1) res = res * b % modu;
        b = b * b % modu;
        e >>= 1;
    }
    return res;
}

/// Compute (base^exponent) mod modu using binary exponentiation (runtime)
pub fn pow_mod(base: usize, exponent: usize, modu: usize) usize {
    var b = base % modu;
    var res: usize = 1;
    var e = exponent;
    while (e > 0) {
        if (e & 1 == 1) res = (res * b) % modu;
        b = (b * b) % modu;
        e >>= 1;
    }
    return res;
}

/// Compute (base^exponent) mod modu using binary exponentiation (runtime, uses u128 for intermediate multiplications)
pub fn pow_mod_big(base: u64, exponent: u64, modu: u64) u64 {
    var b = base % modu;
    var res: u64 = 1;
    var e = exponent;
    while (e > 0) {
        if (e & 1 == 1) {
            res = @intCast((@as(u128, res) * @as(u128, b)) % @as(u128, modu));
        }
        b = @intCast((@as(u128, b) * @as(u128, b)) % @as(u128, modu));
        e >>= 1;
    }
    return res;
}

/// Compute modular inverse using extended Euclidean algorithm (comptime)
pub fn mod_inverse_comptime(a: comptime_int, m: comptime_int) comptime_int {
    var m_val = m;
    var a_val = a;
    var y: comptime_int = 0;
    var x: comptime_int = 1;
    while (a_val > 1) {
        const q = a_val / m_val;
        const t = m_val;
        m_val = a_val % m_val;
        a_val = t;
        const ty = y;
        y = x - q * y;
        x = ty;
    }
    if (x < 0) x += m;
    return x;
}

/// Compute modular inverse using Fermat's Little Theorem (runtime)
/// Only valid when modu is prime.
/// inv(x) = x^(modu-2) mod modu
pub fn mod_inverse(x: usize, modu: usize) usize {
    return pow_mod(x, modu - 2, modu);
}

/// Compute greatest common divisor of two u64 numbers using Euclidean algorithm
pub fn gcd(a: u64, b: u64) u64 {
    var x = a;
    var y = b;
    while (y != 0) {
        const temp = y;
        y = x % y;
        x = temp;
    }
    return x;
}

/// Compute absolute value of an i64 and return as u64
pub fn abs_i64(x: i64) u64 {
    if (x == std.math.minInt(i64)) {
        return 9223372036854775808;
    }
    return if (x >= 0) @intCast(x) else @intCast(-x);
}

/// Compute absolute value of any integer type, returns unsigned type of same bit width for signed integers
pub fn abs(comptime T: type, x: T) T {
    return switch (@typeInfo(T)) {
        .int => |int_info| switch (int_info.signedness) {
            .unsigned => x,
            .signed => if (x == std.math.minInt(T))
                @as(T, 1) << (int_info.bits - 1)
            else if (x >= 0) @intCast(x) else @intCast(-x),
        },
        else => @compileError("abs only accepts integer types"),
    };
}

/// Find the largest element < x in a sorted slice using binary search
fn findClosestLower(slice: []const usize, x: usize) ?usize {
    if (slice.len == 0) return null;
    if (slice[0] >= x) return null;
    if (slice[slice.len - 1] < x) return slice[slice.len - 1];

    var low: usize = 0;
    var high: usize = slice.len - 1;
    var result: ?usize = null;
    while (low <= high) {
        const mid = low + (high - low) / 2;
        const val = slice[mid];
        if (val < x) {
            result = val;
            low = mid + 1;
        } else {
            high = mid - 1;
        }
    }
    return result;
}

/// Find the smallest element > x in a sorted slice using binary search
fn findClosestUpper(slice: []const usize, x: usize) ?usize {
    if (slice.len == 0) return null;
    if (slice[slice.len - 1] <= x) return null;
    if (slice[0] > x) return slice[0];

    var low: usize = 0;
    var high: usize = slice.len - 1;
    var result: ?usize = null;
    while (low <= high) {
        const mid = low + (high - low) / 2;
        const val = slice[mid];
        if (val > x) {
            result = val;
            high = mid - 1;
        } else {
            low = mid + 1;
        }
    }
    return result;
}

};
const modint = struct {

/// Montgomery modular integer for a given prime modulus MOD (32-bit, uses R=2^32 with u64 intermediates)
pub fn MontgomeryModint(comptime MOD_ARG: u32) type {
    // Precompute R2_mod_M = (2^64) mod MOD_ARG (since R = 2^32, R² = 2^64)
    const R2_mod_M: u32 = @intCast(utils.pow_mod_comptime(2, 64, MOD_ARG));
    const niv: u32 = blk: {
        // Find M' such that MOD * M' ≡ -1 (mod 2^32), then niv = -M'
        // Use Newton's method: x_{k+1} = x_k * (2 - MOD * x_k) mod 2^32
        var x: u32 = 1; // Initial guess
        var i: usize = 0;
        while (i < 5) : (i += 1) { // 5 iterations are enough for 32 bits
            x = x *% (2 -% MOD_ARG *% x);
        }
        break :blk -%x;
    };

    return struct {
        pub const MOD = MOD_ARG;
        val: u32,

        const Self = @This();

        /// Convert a normal integer to Montgomery form
        pub fn fromInt(n: u32) Self {
            return Self{ .val = montgomeryMult(n, R2_mod_M) };
        }

        /// Convert back to a normal integer
        pub fn toInt(self: Self) u32 {
            return montgomeryMult(self.val, 1);
        }

        /// Montgomery multiplication: (a * b) / R mod MOD, R=2^32
        inline fn montgomeryMult(a: u32, b: u32) u32 {
            const t: u64 = @as(u64, a) * b;
            const t_low: u32 = @as(u32, @truncate(t));
            const m: u32 = t_low *% niv;
            const t_plus_m_times_MOD: u64 = t + @as(u64, m) * MOD_ARG;
            const u: u32 = @intCast(t_plus_m_times_MOD >> 32);
            return if (u >= MOD_ARG) u - MOD_ARG else u;
        }

        pub fn add(a: Self, b: Self) Self {
            const s: u64 = @as(u64, a.val) + b.val;
            return Self{ .val = @intCast(if (s >= MOD_ARG) s - MOD_ARG else s) };
        }

        pub fn sub(a: Self, b: Self) Self {
            const d: u64 = @as(u64, a.val) + MOD_ARG - b.val;
            return Self{ .val = @intCast(if (d >= MOD_ARG) d - MOD_ARG else d) };
        }

        pub fn mul(a: Self, b: Self) Self {
            return Self{ .val = montgomeryMult(a.val, b.val) };
        }

        pub fn neg(a: Self) Self {
            return Self{ .val = if (a.val == 0) 0 else MOD_ARG - a.val };
        }

        pub fn inv(a: Self) Self {
            // Fermat's little theorem: inv(x) = x^(MOD-2) mod MOD
            return a.pow(MOD_ARG - 2);
        }

        pub fn pow(a: Self, exponent: u32) Self {
            var result = Self.fromInt(1);
            var base = a;
            var e = exponent;
            while (e > 0) {
                if (e & 1 == 1) {
                    result = result.mul(base);
                }
                base = base.mul(base);
                e >>= 1;
            }
            return result;
        }
    };
}

/// Predefined MontgomeryModint for modulus 998244353 (32-bit, preferred)
pub const Modint998244353 = MontgomeryModint(998244353);
/// Predefined MontgomeryModint for modulus 1000000007 (32-bit, common in programming contests)
pub const Modint1000000007 = MontgomeryModint(1000000007);

};
const fft = struct {

pub fn fft_fn(a: []std.math.Complex(f64), invert: bool) void {
    const n = a.len;

    // Bit-reverse permutation
    var j: usize = 0;
    for (1..n) |i| {
        var bit = n >> 1;
        while (j >= bit) : (bit >>= 1) {
            j -= bit;
        }
        j += bit;
        if (i < j) {
            std.mem.swap(std.math.Complex(f64), &a[i], &a[j]);
        }
    }

    // Cooley-Tukey FFT
    var len: usize = 2;
    while (len <= n) : (len <<= 1) {
        const sign: f64 = if (invert) -1 else 1;
        const ang = 2 * std.math.pi / @as(f64, @floatFromInt(len)) * sign;
        const wlen = std.math.Complex(f64).init(@cos(ang), @sin(ang));
        var i: usize = 0;
        while (i < n) : (i += len) {
            var w = std.math.Complex(f64).init(1, 0);
            for (0..len / 2) |k| {
                const u = a[i + k];
                const v = a[i + k + len / 2].mul(w);
                a[i + k] = u.add(v);
                a[i + k + len / 2] = u.sub(v);
                w = w.mul(wlen);
            }
        }
    }

    // Inverse FFT scaling
    if (invert) {
        const n_f64 = @as(f64, @floatFromInt(n));
        for (a) |*x| {
            x.*.re /= n_f64;
            x.*.im /= n_f64;
        }
    }
}

pub fn convolution(gpa: std.mem.Allocator, a: []const f64, b: []const f64) ![]f64 {
    const n = blk: {
        var n: usize = 1;
        while (n < a.len + b.len - 1) n <<= 1;
        break :blk n;
    };

    var fa = try gpa.alloc(std.math.Complex(f64), n);
    defer gpa.free(fa);
    var fb = try gpa.alloc(std.math.Complex(f64), n);
    defer gpa.free(fb);

    @memset(fa, std.math.Complex(f64).init(0, 0));
    @memset(fb, std.math.Complex(f64).init(0, 0));

    for (a, 0..) |x, i| {
        fa[i] = std.math.Complex(f64).init(x, 0);
    }
    for (b, 0..) |x, i| {
        fb[i] = std.math.Complex(f64).init(x, 0);
    }

    fft_fn(fa, false);
    fft_fn(fb, false);

    for (fa, 0..) |*x, i| {
        x.* = x.*.mul(fb[i]);
    }

    fft_fn(fa, true);

    const result = try gpa.alloc(f64, a.len + b.len - 1);
    for (result, 0..) |*r, i| {
        r.* = @round(fa[i].re);
    }

    return result;
}

};
const ntt = struct {

/// NTT (Number Theoretic Transform) and convolution utilities for a given Modint type
/// Requires: Modint.MOD is a prime such that Modint.MOD = c * 2^k + 1 for some c, k
pub fn NttHelpers(comptime ModintType: type, comptime ntt_root: comptime_int) type {
    const ntt_mod = ModintType.MOD;

    // Precompute the maximum log2 we might need (for MOD=998244353, it's 23)
    const max_log: usize = blk: {
        var m = ntt_mod - 1;
        var k: usize = 0;
        while (m % 2 == 0) : (m /= 2) k += 1;
        break :blk k;
    };

    // Precompute roots and inverse roots
    const roots = blk: {
        var res: [max_log + 1]ModintType = undefined;
        res[max_log] = ModintType.fromInt(@intCast(utils.pow_mod_comptime(ntt_root, (ntt_mod - 1) >> max_log, ntt_mod)));
        var i = max_log;
        while (i > 0) : (i -= 1) {
            res[i - 1] = res[i].mul(res[i]);
        }
        break :blk res;
    };
    const inv_roots = blk: {
        var res: [max_log + 1]ModintType = undefined;
        res[max_log] = ModintType.fromInt(@intCast(utils.mod_inverse_comptime(utils.pow_mod_comptime(ntt_root, (ntt_mod - 1) >> max_log, ntt_mod), ntt_mod)));
        var i = max_log;
        while (i > 0) : (i -= 1) {
            res[i - 1] = res[i].mul(res[i]);
        }
        break :blk res;
    };

    return struct {
        /// Compute bit-reversed permutation of indices (optimized)
        fn bit_reverse(i: usize, log_n: usize) usize {
            if (log_n == 0) {
                return i;
            }
            return @bitReverse(@as(u64, @intCast(i))) >> (@as(u6, @intCast(64 - log_n)));
        }

        /// Compute NTT (Number Theoretic Transform) in place
        /// a must have length a power of two
        pub fn ntt_f(a: []ModintType, invert: bool) void {
            const n = a.len;
            if (n == 1) {
                // Nothing to do for single element
                if (invert) {
                    // inv_n is 1/1 = 1, so no change
                }
                return;
            }
            const log_n = std.math.log2_int(usize, n);

            // Bit-reverse permutation
            for (0..n) |i| {
                const j = bit_reverse(i, log_n);
                if (i < j) {
                    std.mem.swap(ModintType, &a[i], &a[j]);
                }
            }

            // NTT main loop with precomputed roots
            var k: usize = 1;
            var log_k: usize = 1; // since k starts at 1=2^0, len=2k=2^1
            while (k < n) {
                const wlen = if (invert) inv_roots[log_k] else roots[log_k];
                var i: usize = 0;
                while (i < n) : (i += 2 * k) {
                    var w = ModintType.fromInt(1);
                    var j: usize = 0;
                    while (j < k) : (j += 1) {
                        const u = a[i + j];
                        const v = a[i + j + k].mul(w);
                        a[i + j] = u.add(v);
                        a[i + j + k] = u.sub(v);
                        w = w.mul(wlen);
                    }
                }
                k <<= 1;
                log_k += 1;
            }

            // Inverse NTT scaling
            if (invert) {
                const inv_n = ModintType.fromInt(@intCast(utils.mod_inverse(n, ntt_mod)));
                for (0..n) |i| {
                    a[i] = a[i].mul(inv_n);
                }
            }
        }

        /// Compute convolution of a and b modulo MOD using NTT
        /// Returns a new slice with the result, caller must free with gpa
        pub fn convolution(gpa: std.mem.Allocator, a: []const ModintType, b: []const ModintType) ![]ModintType {
            var n: usize = 1;
            const required_len = a.len + b.len - 1;
            while (n < required_len) n <<= 1;

            var fa = try gpa.alloc(ModintType, n);
            defer gpa.free(fa);
            var fb = try gpa.alloc(ModintType, n);
            defer gpa.free(fb);

            @memset(fa, ModintType.fromInt(0));
            @memset(fb, ModintType.fromInt(0));
            @memcpy(fa[0..a.len], a);
            @memcpy(fb[0..b.len], b);

            ntt_f(fa, false);
            ntt_f(fb, false);

            for (0..n) |i| {
                fa[i] = fa[i].mul(fb[i]);
            }

            ntt_f(fa, true);

            const result = try gpa.alloc(ModintType, required_len);
            @memcpy(result, fa[0..required_len]);
            return result;
        }
    };
}

/// Predefined NTT helpers for common modulus 998244353 (primitive root 3)
pub const Ntt998244353 = NttHelpers(modint.Modint998244353, 3);

};
const fps = struct {

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

};

const BUNDLE = false;

// ===================== Solving =====================

const Mint = fps.Modint998244353;
const FPS = fps.Fps998244353;

// In-place multiplication by 1 / (1 - x^k)
// mul 1 + x^k + x^2k + x^3k + ... is the same as prefix sum at step k
fn addCoin(cur: []Mint, k: usize, n: usize) void {
    var j: usize = k;
    while (j <= n) : (j += 1) {
        cur[j] = cur[j].add(cur[j - k]);
    }
}

// In-place multiplication by (1 - x^k)
// reverse the prefix sum
fn removeCoin(cur: []Mint, k: usize, n: usize) void {
    var j: usize = n;
    while (j >= k) : (j -= 1) {
        cur[j] = cur[j].sub(cur[j - k]);
    }
}

pub fn solve() !void {
    defer _ = allocator.arena.reset(.retain_capacity);
    const gpa = allocator.arena.allocator();

    const n = in.read(u32);
    const m = in.read(u32);
    const l = in.read(u32);

    // Single DP array of size N + 1
    var cur = try gpa.alloc(Mint, n + 1);
    @memset(cur, Mint.fromInt(0));
    cur[0] = Mint.fromInt(1);

    // Build initial window: coins [1 .. L]
    for (1..l + 1) |k| {
        addCoin(cur, k, n);
    }
    print("{}\n", .{cur[n].toInt()});

    // Slide window [i .. i + L - 1] for i = 2 .. M - L + 1
    for (2..m - l + 2) |i| {
        removeCoin(cur, i - 1, n); // Remove coin (i - 1)
        addCoin(cur, i + l - 1, n); // Add coin (i + L - 1)
        print("{}\n", .{cur[n].toInt()});
    }
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
var inbuf: [5000000]u8 = undefined;
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

