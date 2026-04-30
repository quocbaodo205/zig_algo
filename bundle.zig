const std = @import("std");
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

/// Number of weak compositions of `total` into `parts` non-negative parts.
/// Twelvefold way: indistinguishable balls (`total`) into distinguishable bins (`parts`),
/// bins allowed to be empty. Also known as "stars and bars".
/// Formula: C(total + parts - 1, parts - 1) = C(total + parts - 1, total)
pub fn weakComposition(parts: usize, total: usize) usize {
    if (parts == 0) return if (total == 0) 1 else 0;
    return comb(total + parts - 1, total);
}

/// Number of (positive) compositions of `total` into `parts` positive parts.
/// Twelvefold way: indistinguishable balls (`total`) into distinguishable bins (`parts`),
/// no bin empty. Each part must be >= 1.
/// Formula: C(total - 1, parts - 1)
pub fn composition(parts: usize, total: usize) usize {
    if (parts == 0) return if (total == 0) 1 else 0;
    if (total < parts) return 0;
    return comb(total - 1, parts - 1);
}

/// Backward-compatible alias for `weakComposition` (stars and bars).
pub fn starsAndBars(bins: usize, balls: usize) usize {
    return weakComposition(bins, balls);
}

/// General function: calculate number of ways to choose k slots from n,
/// where picking a slot blocks the next m slots from being picked.
/// Formula: C(n - m*(k - 1), k)
pub fn nonConsecutiveGeneral(n: usize, k: usize, m: usize) usize {
    if (k == 0) return 1;
    if (k > n) return 0;
    const required = m * (k - 1);
    if (n < required + k) return 0; // Need at least k + m*(k-1) slots
    return comb(n - required, k);
}

/// Calculate the number of ways to choose k non-consecutive slots from n slots.
/// (Special case: m=1, blocks 1 slot after each pick)
/// Formula: C(n - k + 1, k)
pub fn nonConsecutive(n: usize, k: usize) usize {
    return nonConsecutiveGeneral(n, k, 1);
}

/// Calculate the number of ways to choose k slots from n, where picking a slot
/// blocks the next 2 slots (i+1 and i+2) from being picked.
/// (Special case: m=2)
/// Formula: C(n - 2*(k - 1), k)
pub fn nonConsecutiveBlock2(n: usize, k: usize) usize {
    return nonConsecutiveGeneral(n, k, 2);
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

/// Number of weak compositions of `total` into `parts` non-negative parts, modulo p.
/// Twelvefold way: indistinguishable balls into distinguishable bins, bins may be empty.
/// Formula: C(total + parts - 1, total) mod p
pub fn weakCompositionMod(parts: usize, total: usize, p: usize) usize {
    if (parts == 0) return if (total == 0) 1 % p else 0;
    return comb_mod_lucas(total + parts - 1, total, p);
}

/// Number of (positive) compositions of `total` into `parts` positive parts, modulo p.
/// Twelvefold way: indistinguishable balls into distinguishable bins, no bin empty.
/// Formula: C(total - 1, parts - 1) mod p
pub fn compositionMod(parts: usize, total: usize, p: usize) usize {
    if (parts == 0) return if (total == 0) 1 % p else 0;
    if (total < parts) return 0;
    return comb_mod_lucas(total - 1, parts - 1, p);
}

/// Backward-compatible alias for `weakCompositionMod` (stars and bars mod p).
pub fn starsAndBarsMod(bins: usize, balls: usize, p: usize) usize {
    return weakCompositionMod(bins, balls, p);
}

/// General function (modular): calculate number of ways to choose k slots from n,
/// where picking a slot blocks the next m slots from being picked, modulo p.
/// Formula: C(n - m*(k - 1), k) mod p
pub fn nonConsecutiveGeneralMod(n: usize, k: usize, m: usize, p: usize) usize {
    if (k == 0) return 1;
    if (k > n) return 0;
    const required = m * (k - 1);
    if (n < required + k) return 0;
    return comb_mod_lucas(n - required, k, p);
}

/// Calculate the number of ways to choose k non-consecutive slots from n slots modulo p.
/// (Special case: m=1, blocks 1 slot after each pick)
/// Formula: C(n - k + 1, k) mod p
pub fn nonConsecutiveMod(n: usize, k: usize, p: usize) usize {
    return nonConsecutiveGeneralMod(n, k, 1, p);
}

/// Calculate the number of ways to choose k slots from n (blocking next 2 slots) modulo p.
/// (Special case: m=2)
/// Formula: C(n - 2*(k - 1), k) mod p
pub fn nonConsecutiveBlock2Mod(n: usize, k: usize, p: usize) usize {
    return nonConsecutiveGeneralMod(n, k, 2, p);
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

        /// Number of weak compositions of `total` into `parts` non-negative parts.
        /// Twelvefold way: indistinguishable balls into distinguishable bins, bins may be empty.
        pub fn weakComposition(self: *const Self, parts: u64, total: u64) MintType {
            if (parts == 0) return if (total == 0) MintType.fromInt(1) else MintType.fromInt(0);
            return self.comb(total + parts - 1, total);
        }

        /// Number of (positive) compositions of `total` into `parts` positive parts.
        /// Twelvefold way: indistinguishable balls into distinguishable bins, no bin empty.
        pub fn composition(self: *const Self, parts: u64, total: u64) MintType {
            if (parts == 0) return if (total == 0) MintType.fromInt(1) else MintType.fromInt(0);
            if (total < parts) return MintType.fromInt(0);
            return self.comb(total - 1, parts - 1);
        }

        /// Backward-compatible alias for `weakComposition` (stars and bars).
        pub fn starsAndBars(self: *const Self, bins: u64, balls: u64) MintType {
            return self.weakComposition(bins, balls);
        }

        /// General function: calculate number of ways to choose k slots from n,
        /// where picking a slot blocks the next m slots from being picked.
        pub fn nonConsecutiveGeneral(self: *const Self, n: u64, k: u64, m: u64) MintType {
            if (k == 0) return MintType.fromInt(1);
            if (k > n) return MintType.fromInt(0);
            const required = m * (k - 1);
            if (n < required + k) return MintType.fromInt(0);
            return self.comb(n - required, k);
        }

        /// Calculate the number of ways to choose k non-consecutive slots from n slots.
        /// (Special case: m=1, blocks 1 slot after each pick)
        pub fn nonConsecutive(self: *const Self, n: u64, k: u64) MintType {
            return self.nonConsecutiveGeneral(n, k, 1);
        }

        /// Calculate the number of ways to choose k slots from n, where picking a slot
        /// blocks the next 2 slots (i+1 and i+2) from being picked.
        /// (Special case: m=2)
        pub fn nonConsecutiveBlock2(self: *const Self, n: u64, k: u64) MintType {
            return self.nonConsecutiveGeneral(n, k, 2);
        }
    };
}

// ==========================================
// Permutation cycle splitting

/// Result of splitting a permutation into cycles.
pub const CycleSplit = struct {
    /// `root[i]` = the representative (smallest index) of the cycle containing position `i`.
    root: []usize,
    /// `length[i]` = size of the cycle containing `i` if `i` is the cycle's root, otherwise 0.
    length: []usize,
    /// `roots[c]` = the representative of the `c`-th cycle, for `c` in `0..cycle_count`.
    /// Listed in increasing order (the order in which cycles are discovered).
    roots: []usize,
    /// Total number of cycles in the permutation.
    cycle_count: usize,
    gpa: std.mem.Allocator,

    pub fn deinit(self: *CycleSplit) void {
        self.gpa.free(self.root);
        self.gpa.free(self.length);
        self.gpa.free(self.roots);
    }
};

/// Split a permutation into cycles.
pub fn cycleSplit(gpa: std.mem.Allocator, perm: []const usize) !CycleSplit {
    const n = perm.len;
    var root = try gpa.alloc(usize, n);
    errdefer gpa.free(root);
    var length = try gpa.alloc(usize, n);
    errdefer gpa.free(length);
    var visited = try gpa.alloc(bool, n);
    defer gpa.free(visited);
    @memset(visited, false);
    @memset(length, 0);

    var roots_list: std.ArrayList(usize) = .empty;
    errdefer roots_list.deinit(gpa);

    for (0..n) |start| {
        if (visited[start]) continue;
        var j: usize = start;
        var len: usize = 0;
        while (!visited[j]) {
            visited[j] = true;
            root[j] = start;
            len += 1;
            j = perm[j];
        }
        length[start] = len;
        try roots_list.append(gpa, start);
    }

    const roots = try roots_list.toOwnedSlice(gpa);

    return CycleSplit{
        .root = root,
        .length = length,
        .roots = roots,
        .cycle_count = roots.len,
        .gpa = gpa,
    };
}

pub fn permPow(gpa: std.mem.Allocator, perm: []const usize, k: usize) ![]usize {
    const n = perm.len;
    var result = try gpa.alloc(usize, n);
    errdefer gpa.free(result);
    if (n == 0) return result;

    var cs = try cycleSplit(gpa, perm);
    defer cs.deinit();

    for (cs.roots) |r| {
        const L = cs.length[r];
        const kk = k % L; // rotation amount on this cycle
        // Step 1: walk kk steps from r to get the anchor y = sigma^kk(r).
        var y: usize = r;
        var s: usize = 0;
        while (s < kk) : (s += 1) y = perm[y];
        result[r] = y;
        // Step 2: propagate around the cycle: result[sigma(a)] = sigma(result[a]).
        var a: usize = r;
        var b: usize = perm[r];
        while (b != r) {
            result[b] = perm[result[a]];
            a = b;
            b = perm[b];
        }
    }

    return result;
}

// ------------------------------------------
// Cycle-type enumeration (integer partitions of n as cycle spectra)
//
// A cycle type is a vector `a[1..n]` where `a[i]` is the number of i-cycles;
// it must satisfy sum(i * a[i]) = n. Every such type is realised by exactly
//   n! / product(i^a[i] * a[i]!)
// labelled permutations, and the order of any permutation of that type is
//   lcm(i : a[i] > 0).
// Many problems (e.g. ABC226 F) sum a function of the order over all types,
// weighted by the above multiplicity, so this routine threads the LCM and the
// modular denominator through the recursion and hands a finished type to a
// caller-supplied visitor.

/// Exact gcd on u128. Required because the LCM of cycle lengths for n up to
/// ~50 (Landau's function g(n)) can exceed u64, so the u64 gcd in `utils` is
/// not enough here.
fn gcdU128(a_initial: u128, b_initial: u128) u128 {
    var a = a_initial;
    var b = b_initial;
    while (b != 0) {
        const r = a % b;
        a = b;
        b = r;
    }
    return a;
}

/// Fold `cycle_len` into the running LCM. `current_lcm` is kept exact (u128).
fn lcmWithCycle(current_lcm: u128, cycle_len: usize) u128 {
    const length: u128 = @intCast(cycle_len);
    return current_lcm / gcdU128(current_lcm, length) * length;
}

/// Enumerate every cycle type of `n` in decreasing cycle length, calling
/// `visit` once per complete type with `(lcm, denominator, n_factorial)`:
///   * `lcm`          — exact lcm of all used cycle lengths (u128);
///   * `denominator`  — product(i^a[i] * a[i]!) modulo `MintType.MOD`;
///   * `n_factorial`  — n! modulo `MintType.MOD` (read off `comb.fact[n]`).
///
/// `visit` is a comptime function `fn (*Context, u128, MintType, MintType) void`
/// so the leaf action — turning a cycle type into a problem-specific score
/// contribution — stays with the caller and this routine stays generic.
pub fn enumerateCycleTypes(
    comptime MintType: type,
    comptime Context: type,
    comptime visit: fn (*Context, u128, MintType, MintType) void,
    comb_table: *const CombinatoricModint(MintType),
    context: *Context,
    max_cycle_len: usize,
    remaining: usize,
    current_lcm: u128,
    denominator: MintType,
    n_factorial: MintType,
) void {
    if (remaining == 0) {
        // TODO: score counting is problem-specific. This leaf only forwards
        // (lcm, denominator, n_factorial) to `visit`; a typical caller adds
        //   lcm^K * n! / denominator
        // to its answer (as in ABC226 F, where the score is the permutation
        // order raised to the K-th power), but any function of the cycle type
        // fits in the callback — e.g. just counting types, or summing n!/
        // denominator to verify the partition-of-identity invariant.
        visit(context, current_lcm, denominator, n_factorial);
        return;
    }
    if (max_cycle_len == 0) return;

    // Every length greater than `remaining` has forced multiplicity zero.
    const cycle_len = @min(max_cycle_len, remaining);

    // At the last possible length, all remaining elements must be 1-cycles.
    // This avoids visiting invalid leaves with remaining > 0 and no lengths.
    if (cycle_len == 1) {
        const final_denominator = denominator.mul(comb_table.fact[remaining]);
        visit(context, current_lcm, final_denominator, n_factorial);
        return;
    }

    const cycle_len_mint = MintType.fromInt(@intCast(cycle_len));
    var count = remaining / cycle_len;
    while (true) {
        const used = cycle_len * count;
        const next_lcm = if (count == 0)
            current_lcm
        else
            lcmWithCycle(current_lcm, cycle_len);
        // i^a[i] * a[i]!  — the a[i]! part reuses the shared factorial table.
        const next_denominator = denominator
            .mul(cycle_len_mint.pow(@intCast(count)))
            .mul(comb_table.fact[count]);

        enumerateCycleTypes(
            MintType,
            Context,
            visit,
            comb_table,
            context,
            cycle_len - 1,
            remaining - used,
            next_lcm,
            next_denominator,
            n_factorial,
        );

        // Pick a[cycle_len] from large to small without unsigned underflow.
        if (count == 0) break;
        count -= 1;
    }
}

};
const allocator = struct {
const heap = std.heap;
const Allocator = std.mem.Allocator;

/// Global heap + arena for quick various tasks.
/// Reset it yourself after each test cases.
pub var arena = heap.ArenaAllocator.init(heap.page_allocator);
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
const prime = struct {

/// Compute natural log of x (comptime)
fn comptime_ln(x: comptime_float) comptime_float {
    return @log(x);
}

/// PrimeDS is a generic struct that holds primes up to max_n (comptime)
pub fn PrimeDS(comptime max_n: comptime_int) type {
    // Approximate number of primes up to max_n: ~max_n / ln(max_n), multiply by 2 to be safe
    const approx_primes = if (max_n < 2) 0 else blk: {
        const x = @as(comptime_float, @floatFromInt(max_n));
        const ln_x = comptime_ln(x);
        const est = x / ln_x;
        break :blk @as(comptime_int, @intFromFloat(@ceil(est * 2)));
    };

    return struct {
        const Self = @This();

        pub const Factor = struct {
            prime: usize,
            exponent: usize,
        };

        primes: [approx_primes]usize,
        prime_count: usize,
        is_prime: [max_n + 1]bool,
        factors: [MAX_FACTORS]Factor,
        factor_count: usize,

        // Maximum number of distinct prime factors (for numbers up to 2^64, it's at most 15)
        const MAX_FACTORS = 20;

        /// Initialize PrimeDS using linear sieve (Euler's sieve)
        pub fn init() Self {
            var self = Self{
                .primes = if (approx_primes == 0) [_]usize{} else undefined,
                .prime_count = 0,
                .is_prime = [_]bool{true} ** (max_n + 1),
                .factors = undefined,
                .factor_count = 0,
            };

            if (max_n >= 0) self.is_prime[0] = false;
            if (max_n >= 1) self.is_prime[1] = false;

            if (max_n >= 2) {
                var i: usize = 2;
                while (i <= max_n) : (i += 1) {
                    if (self.is_prime[i]) {
                        self.primes[self.prime_count] = i;
                        self.prime_count += 1;
                    }
                    var j: usize = 0;
                    while (j < self.prime_count) : (j += 1) {
                        const p = self.primes[j];
                        const product = i * p;
                        if (product > max_n) break;
                        self.is_prime[product] = false;
                        if (i % p == 0) break;
                    }
                }
            }

            return self;
        }

        /// Get the primes slice
        pub fn primesSlice(self: *const Self) []const usize {
            return self.primes[0..self.prime_count];
        }

        /// Return the closest prime <= x, or null if none exists
        pub fn closestPrimeLower(self: *const Self, x: usize) ?usize {
            if (x < 2) return null;
            const primes_slice = self.primesSlice();
            if (primes_slice.len == 0) return null;
            if (primes_slice[0] > x) return null;
            if (primes_slice[primes_slice.len - 1] <= x) return primes_slice[primes_slice.len - 1];
            
            var low: usize = 0;
            var high: usize = primes_slice.len - 1;
            var result: ?usize = null;
            while (low <= high) {
                const mid = low + (high - low) / 2;
                const p = primes_slice[mid];
                if (p == x) {
                    return p;
                } else if (p < x) {
                    result = p;
                    low = mid + 1;
                } else {
                    high = mid - 1;
                }
            }
            return result;
        }

        /// Return the closest prime >= x, or null if none exists
        pub fn closestPrimeUpper(self: *const Self, x: usize) ?usize {
            if (x > max_n) return null;
            const primes_slice = self.primesSlice();
            if (primes_slice.len == 0) return null;
            if (primes_slice[primes_slice.len - 1] < x) return null;
            if (primes_slice[0] >= x) return primes_slice[0];
            
            var low: usize = 0;
            var high: usize = primes_slice.len - 1;
            var result: ?usize = null;
            while (low <= high) {
                const mid = low + (high - low) / 2;
                const p = primes_slice[mid];
                if (p == x) {
                    return p;
                } else if (p > x) {
                    result = p;
                    high = mid - 1;
                } else {
                    low = mid + 1;
                }
            }
            return result;
        }

        /// Factorize a number x into its prime factors, returns a slice of Factor
        /// x must be <= max_n
        pub fn factorize(self: *Self, x: usize) []const Factor {
            self.factor_count = 0;
            var n = x;

            for (self.primesSlice()) |p| {
                if (p * p > n) break;
                if (n % p == 0) {
                    var exp: usize = 0;
                    while (n % p == 0) {
                        exp += 1;
                        n /= p;
                    }
                    self.factors[self.factor_count] = Factor{ .prime = p, .exponent = exp };
                    self.factor_count += 1;
                }
            }

            if (n > 1) {
                self.factors[self.factor_count] = Factor{ .prime = n, .exponent = 1 };
                self.factor_count += 1;
            }

            return self.factors[0..self.factor_count];
        }
    };
}

};

const BUNDLE = false;

// ===================== Solving =====================

const Modint = fps.Modint998244353;
const FPS = fps.Fps998244353;
const Combinatorics = combinatorics.CombinatoricModint(Modint);

pub fn solve() !void {
    defer _ = allocator.arena.reset(.retain_capacity);
    const gpa = allocator.arena.allocator();

    const n = in.read(u32);

    const pr = prime.PrimeDS(250_000).init();

    const comb = try Combinatorics.init(gpa, n + 1);

    // construct egf of prime.
    var g = try FPS.init(gpa, n);
    g.coeffs[0] = Modint.fromInt(1);
    var i: usize = 0;
    while (i < pr.prime_count and pr.primes[i] <= n) : (i += 1) {
        const p = pr.primes[i];
        g.coeffs[p] = comb.inv_fact[p]; // x^p / p!
    }

    // Since R = x * phi(R), apply Lagrange inversion
    try g.pow(n, n);
    var ans = g.coeffs[n - 1];
    ans = ans.mul(comb.fact[n - 1]);
    ans = ans.mul(Modint.fromInt(n).inv()); // (n - 1)! / n: fix the root label to 1

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

