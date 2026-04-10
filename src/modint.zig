const std = @import("std");
const math = std.math;

/// Helper to compute (base^exponent) mod modu using binary exponentiation (comptime)
fn pow_mod_comptime(base: comptime_int, exponent: comptime_int, modu: comptime_int) comptime_int {
    var b = base % modu;
    var res: comptime_int = 1;
    var e = exponent;
    while (e > 0) {
        if (e % 2 == 1) {
            res = (res * b) % modu;
        }
        b = (b * b) % modu;
        e = e / 2;
    }
    return res;
}

/// Montgomery modular integer for a given prime modulus MOD (32-bit, uses R=2^32 with u64 intermediates)
pub fn MontgomeryModint(comptime MOD_ARG: u32) type {
    // Precompute R2_mod_M = (2^64) mod MOD_ARG (since R = 2^32, R² = 2^64)
    const R2_mod_M: u32 = @intCast(pow_mod_comptime(2, 64, MOD_ARG));
    const niv: u32 = blk: {
        // Find M' such that MOD * M' ≡ -1 (mod 2^32), then niv = -M'
        // Use Newton's method: x_{k+1} = x_k * (2 - MOD * x_k) mod 2^32
        var x: u32 = 1; // Initial guess
        var i: usize = 0;
        while (i < 5) : (i += 1) { // 5 iterations are enough for 32 bits
            x = x *% (2 -% MOD_ARG *% x);
        }
        break :blk -% x;
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
    };
}

/// Predefined MontgomeryModint for modulus 998244353 (32-bit, preferred)
pub const Modint998244353 = MontgomeryModint(998244353);

test "montgomery modint" {
    const M = Modint998244353;
    const a = M.fromInt(12345);
    const b = M.fromInt(67890);
    const c = M.fromInt(@intCast((12345 * 67890) % 998244353));
    try std.testing.expectEqual(c.toInt(), a.mul(b).toInt());

    const d = M.fromInt(@intCast((12345 + 67890) % 998244353));
    try std.testing.expectEqual(d.toInt(), a.add(b).toInt());

    const e = M.fromInt(@intCast((12345 + 998244353 - 67890) % 998244353));
    try std.testing.expectEqual(e.toInt(), a.sub(b).toInt());
}

test "montgomery modint (small modulus test)" {
    const M = MontgomeryModint(7);
    const a = M.fromInt(3);
    const b = M.fromInt(5);
    const c = M.fromInt((3 * 5) % 7); // 15 mod 7 = 1
    try std.testing.expectEqual(c.toInt(), a.mul(b).toInt());

    const d = M.fromInt((3 + 5) % 7); // 8 mod7=1
    try std.testing.expectEqual(d.toInt(), a.add(b).toInt());
}
