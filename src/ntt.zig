const std = @import("std");
const modint = @import("modint.zig");
const combinatorics = @import("combinatorics.zig");

/// NTT (Number Theoretic Transform) and convolution utilities for a given prime modulus MOD
/// Requires: MOD is a prime such that MOD = c * 2^k + 1 for some c, k
pub fn NttHelpers(comptime ntt_mod: comptime_int, comptime ntt_root: comptime_int) type {
    const Modint = modint.MontgomeryModint(ntt_mod);

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
