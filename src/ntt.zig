const std = @import("std");
const modint = @import("modint.zig");
const combinatorics = @import("combinatorics.zig");

// Const pow_mod and mod_inverse for precomputations
fn pow_mod_const(base: comptime_int, exp: comptime_int, m: comptime_int) comptime_int {
    var b = base % m;
    var res: comptime_int = 1;
    var e = exp;
    while (e > 0) {
        if (e & 1 == 1) res = res * b % m;
        b = b * b % m;
        e >>= 1;
    }
    return res;
}
fn mod_inverse_const(a: comptime_int, m: comptime_int) comptime_int {
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

/// NTT (Number Theoretic Transform) and convolution utilities for a given prime modulus MOD
/// Requires: MOD is a prime such that MOD = c * 2^k + 1 for some c, k
pub fn NttHelpers(comptime ntt_mod: comptime_int, comptime ntt_root: comptime_int) type {
    const Modint = modint.MontgomeryModint(ntt_mod);

    // Precompute the maximum log2 we might need (for MOD=998244353, it's 23)
    const max_log: usize = blk: {
        var m = ntt_mod - 1;
        var k: usize = 0;
        while (m % 2 == 0) : (m /= 2) k += 1;
        break :blk k;
    };

    // Precompute roots and inverse roots
    const roots = blk: {
        var res: [max_log + 1]Modint = undefined;
        res[max_log] = Modint.fromInt(pow_mod_const(ntt_root, (ntt_mod - 1) >> max_log, ntt_mod));
        var i = max_log;
        while (i > 0) : (i -= 1) {
            res[i - 1] = res[i].mul(res[i]);
        }
        break :blk res;
    };
    const inv_roots = blk: {
        var res: [max_log + 1]Modint = undefined;
        res[max_log] = Modint.fromInt(mod_inverse_const(pow_mod_const(ntt_root, (ntt_mod - 1) >> max_log, ntt_mod), ntt_mod));
        var i = max_log;
        while (i > 0) : (i -= 1) {
            res[i - 1] = res[i].mul(res[i]);
        }
        break :blk res;
    };

    return struct {
        /// Compute bit-reversed permutation of indices (optimized)
        fn bit_reverse(i: usize, log_n: usize) usize {
            return @bitReverse(@as(u64, @intCast(i))) >> (@as(u6, @intCast(64 - log_n)));
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

            // NTT main loop with precomputed roots
            var k: usize = 1;
            var log_k: usize = 1; // since k starts at 1=2^0, len=2k=2^1
            while (k < n) {
                const wlen = if (invert) inv_roots[log_k] else roots[log_k];
                var i: usize = 0;
                while (i < n) : (i += 2 * k) {
                    var w = Modint.fromInt(1);
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
                const inv_n = Modint.fromInt(@intCast(combinatorics.mod_inverse(n, ntt_mod)));
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
            @memcpy(fa[0..a.len], a);
            @memcpy(fb[0..b.len], b);

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

test "ntt convolution" {
    const gpa = std.testing.allocator;
    const Ntt = Ntt998244353;
    const Modint = modint.MontgomeryModint(998244353);

    // Test (1 + x) * (1 + x) = 1 + 2x + x²
    const a_slice = [_]Modint{ Modint.fromInt(1), Modint.fromInt(1) };
    const b_slice = [_]Modint{ Modint.fromInt(1), Modint.fromInt(1) };
    const c = try Ntt.convolution(gpa, &a_slice, &b_slice);
    defer gpa.free(c);

    const expected = [_]Modint{ Modint.fromInt(1), Modint.fromInt(2), Modint.fromInt(1) };
    for (0..c.len) |i| {
        try std.testing.expectEqual(expected[i].toInt(), c[i].toInt());
    }
}
