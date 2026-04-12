const std = @import("std");
const utils = @import("utils.zig");

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
pub const pow_mod = utils.pow_mod;
pub const mod_inverse = utils.mod_inverse;

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

/// Precomputed factorials and inverse factorials for a given MontgomeryModint type
pub fn CombinatoricModint(comptime MintType: type) type {
    return struct {
        const Self = @This();

        fact: []MintType,
        inv_fact: []MintType,
        gpa: std.mem.Allocator,

        pub fn init(gpa: std.mem.Allocator, max_n: u64) !Self {
            var fact = try gpa.alloc(MintType, max_n + 1);
            errdefer gpa.free(fact);

            var inv_fact = try gpa.alloc(MintType, max_n + 1);
            errdefer gpa.free(inv_fact);

            fact[0] = MintType.fromInt(1);
            for (1..max_n + 1) |i| {
                fact[i] = fact[i - 1].mul(MintType.fromInt(@intCast(i)));
            }

            inv_fact[max_n] = blk: {
                const inv = utils.pow_mod_big(fact[max_n].toInt(), MintType.MOD - 2, MintType.MOD);
                break :blk MintType.fromInt(@intCast(inv));
            };
            var i: u64 = max_n;
            while (i >= 1) : (i -= 1) {
                inv_fact[i - 1] = inv_fact[i].mul(MintType.fromInt(@intCast(i)));
            }

            return Self{
                .fact = fact,
                .inv_fact = inv_fact,
                .gpa = gpa,
            };
        }

        pub fn deinit(self: *Self) void {
            self.gpa.free(self.fact);
            self.gpa.free(self.inv_fact);
        }

        pub fn comb(self: *const Self, n: u64, k: u64) MintType {
            if (k > n) return MintType.fromInt(0);
            if (k == 0 or k == n) return MintType.fromInt(1);
            return self.fact[n].mul(self.inv_fact[k]).mul(self.inv_fact[n - k]);
        }

        /// Calculate the number of ways to choose N items from M types with infinite quantity (stars and bars).
        pub fn starsAndBars(self: *const Self, m: u64, n: u64) MintType {
            return self.comb(m + n - 1, n);
        }
    };
}

test "combinatorics" {
    // Test comb function
    try std.testing.expect(comb(5, 2) == 10);
    try std.testing.expect(comb(10, 3) == 120);
    try std.testing.expect(comb(0, 0) == 1);
    try std.testing.expect(comb(5, 5) == 1);
    try std.testing.expect(comb(5, 0) == 1);
    try std.testing.expect(comb(5, 6) == 0);

    // Test stars and bars
    try std.testing.expect(starsAndBars(2, 3) == 4); // Types: A,B; Items: AAA, AAB, ABB, BBB
    try std.testing.expect(starsAndBars(3, 2) == 6); // AA, AB, AC, BB, BC, CC
}

test "modular combinatorics" {
    const p = 998244353; // Common prime modulus

    // Test pow_mod
    try std.testing.expect(pow_mod(2, 10, p) == 1024);
    try std.testing.expect(pow_mod(3, 5, 7) == 5); // 243 mod 7 = 5

    // Test mod_inverse
    try std.testing.expect((2 * mod_inverse(2, p)) % p == 1);
    try std.testing.expect((5 * mod_inverse(5, p)) % p == 1);

    // Test comb_mod_small
    try std.testing.expect(comb_mod_small(5, 2, p) == 10);
    try std.testing.expect(comb_mod_small(10, 3, p) == 120);

    // Test comb_mod_lucas with small numbers
    try std.testing.expect(comb_mod_lucas(5, 2, p) == 10);
    try std.testing.expect(comb_mod_lucas(10, 3, p) == 120);

    // Test Lucas with numbers larger than p/2 using symmetry
    try std.testing.expect(comb_mod_lucas(10, 7, p) == 120); // C(10,7)=C(10,3)=120
}
