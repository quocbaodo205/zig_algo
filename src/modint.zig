const std = @import("std");
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

/// Predefined MontgomeryModint for modulus 998244353
pub const Modint998244353 = MontgomeryModint(998244353);

test "montgomery modint" {
    const M = Modint998244353;
    const a = M.fromInt(12345);
    const b = M.fromInt(67890);
    const c = M.fromInt((12345 * 67890) % 998244353);
    try std.testing.expectEqual(c.toInt(), a.mul(b).toInt());

    const d = M.fromInt((12345 + 67890) % 998244353);
    try std.testing.expectEqual(d.toInt(), a.add(b).toInt());

    const e = M.fromInt((12345 + 998244353 - 67890) % 998244353);
    try std.testing.expectEqual(e.toInt(), a.sub(b).toInt());
}
