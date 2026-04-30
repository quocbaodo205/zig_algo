const std = @import("std");
const utils = @import("utils.zig");
const modint = @import("modint.zig");

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

test "combinatorics" {
    // Test comb function
    try std.testing.expect(comb(5, 2) == 10);
    try std.testing.expect(comb(10, 3) == 120);
    try std.testing.expect(comb(0, 0) == 1);
    try std.testing.expect(comb(5, 5) == 1);
    try std.testing.expect(comb(5, 0) == 1);
    try std.testing.expect(comb(5, 6) == 0);

    // Test stars and bars (alias for weakComposition)
    try std.testing.expect(starsAndBars(2, 3) == 4); // Types: A,B; Items: AAA, AAB, ABB, BBB
    try std.testing.expect(starsAndBars(3, 2) == 6); // AA, AB, AC, BB, BC, CC

    // Test weak composition (indistinguishable balls, distinguishable bins, bins may be empty)
    try std.testing.expect(weakComposition(2, 3) == 4); // C(3+2-1, 3) = C(4,3) = 4
    try std.testing.expect(weakComposition(3, 2) == 6); // C(2+3-1, 2) = C(4,2) = 6
    try std.testing.expect(weakComposition(3, 0) == 1); // all bins empty
    try std.testing.expect(weakComposition(0, 0) == 1); // empty composition
    try std.testing.expect(weakComposition(0, 5) == 0); // no bins but balls remain

    // Test (positive) composition (indistinguishable balls, distinguishable bins, no bin empty)
    try std.testing.expect(composition(3, 6) == 10); // C(6-1, 3-1) = C(5,2) = 10
    try std.testing.expect(composition(2, 5) == 4); // C(5-1, 2-1) = C(4,1) = 4: (1,4),(2,3),(3,2),(4,1)
    try std.testing.expect(composition(1, 5) == 1); // only (5)
    try std.testing.expect(composition(5, 3) == 0); // total < parts, impossible
    try std.testing.expect(composition(0, 0) == 1); // empty composition
    try std.testing.expect(composition(0, 5) == 0); // no parts but balls remain

    // Test non-consecutive selection (m=1)
    try std.testing.expect(nonConsecutive(5, 2) == 6);
    try std.testing.expect(nonConsecutive(5, 3) == 1);
    try std.testing.expect(nonConsecutive(5, 4) == 0);
    try std.testing.expect(nonConsecutive(10, 2) == 36); // C(10-2+1, 2) = C(9,2)=36
    try std.testing.expect(nonConsecutive(0, 0) == 1);
    try std.testing.expect(nonConsecutive(7, 3) == 10); // C(7-3+1, 3) = C(5,3)=10

    // Test non-consecutive with blocking 2 slots after each pick (m=2)
    try std.testing.expect(nonConsecutiveBlock2(5, 2) == 3); // C(5-2*(2-1), 2) = C(3,2)=3 → (1,3)? Wait no: n=5, k=2, m=2: allowed pairs are (1,4), (1,5), (2,5) → exactly 3! Correct!
    try std.testing.expect(nonConsecutiveBlock2(5, 3) == 0);
    try std.testing.expect(nonConsecutiveBlock2(7, 2) == 10); // C(7-2*(2-1), 2) = C(5,2)=10
    try std.testing.expect(nonConsecutiveBlock2(10, 3) == 20); // C(10-2*(3-1), 3) = C(6,3)=20

    // Test general function
    try std.testing.expect(nonConsecutiveGeneral(5, 2, 1) == 6); // m=1 same as nonConsecutive
    try std.testing.expect(nonConsecutiveGeneral(5, 2, 2) == 3); // m=2 same as nonConsecutiveBlock2
    try std.testing.expect(nonConsecutiveGeneral(10, 2, 3) == 21); // m=3: C(10-3*(2-1), 2)=C(7,2)=21
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

    // Test weak composition mod p (stars and bars mod p)
    try std.testing.expect(weakCompositionMod(2, 3, p) == 4); // C(4,3) = 4
    try std.testing.expect(weakCompositionMod(3, 2, p) == 6); // C(4,2) = 6
    try std.testing.expect(weakCompositionMod(0, 0, p) == 1 % p);

    // Test positive composition mod p
    try std.testing.expect(compositionMod(3, 6, p) == 10); // C(5,2) = 10
    try std.testing.expect(compositionMod(2, 5, p) == 4); // C(4,1) = 4
    try std.testing.expect(compositionMod(5, 3, p) == 0); // total < parts
}

test "permutation cycle split" {
    const gpa = std.testing.allocator;

    // Identity permutation: n cycles, each of length 1, every element is its own root.
    {
        const perm = [_]usize{ 0, 1, 2, 3 };
        var cs = try cycleSplit(gpa, &perm);
        defer cs.deinit();
        try std.testing.expect(cs.cycle_count == 4);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 0, 1, 2, 3 }, cs.root);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 1, 1, 1, 1 }, cs.length);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 0, 1, 2, 3 }, cs.roots);
    }

    // Single n-cycle: (0 1 2 3 4), root = 0, length only at index 0.
    {
        const perm = [_]usize{ 1, 2, 3, 4, 0 }; // sigma(0)=1, sigma(1)=2, ..., sigma(4)=0
        var cs = try cycleSplit(gpa, &perm);
        defer cs.deinit();
        try std.testing.expect(cs.cycle_count == 1);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 0, 0, 0, 0, 0 }, cs.root);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 5, 0, 0, 0, 0 }, cs.length);
        try std.testing.expectEqualSlices(usize, &[_]usize{0}, cs.roots);
    }

    // Mixed: cycles (0 2)(1 3 4). Roots are the smallest index of each cycle: 0 and 1.
    {
        // 0 -> 2 -> 0 ; 1 -> 3 -> 4 -> 1
        const perm = [_]usize{ 2, 3, 0, 4, 1 };
        var cs = try cycleSplit(gpa, &perm);
        defer cs.deinit();
        try std.testing.expect(cs.cycle_count == 2);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 0, 1, 0, 1, 1 }, cs.root);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 2, 3, 0, 0, 0 }, cs.length);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 0, 1 }, cs.roots);
    }

    // Empty permutation: zero cycles.
    {
        const perm = [_]usize{};
        var cs = try cycleSplit(gpa, &perm);
        defer cs.deinit();
        try std.testing.expect(cs.cycle_count == 0);
        try std.testing.expect(cs.root.len == 0);
        try std.testing.expect(cs.length.len == 0);
        try std.testing.expect(cs.roots.len == 0);
    }
}

test "permutation power" {
    const gpa = std.testing.allocator;

    // Single 5-cycle (0 1 2 3 4): sigma^k is a rotation by k mod 5.
    {
        const perm = [_]usize{ 1, 2, 3, 4, 0 };
        // k = 1: identity-of-sigma, i.e. sigma itself
        {
            const r = try permPow(gpa, &perm, 1);
            defer gpa.free(r);
            try std.testing.expectEqualSlices(usize, &[_]usize{ 1, 2, 3, 4, 0 }, r);
        }
        // k = 2: sigma^2(i) = sigma(sigma(i)) -> {2,3,4,0,1}
        {
            const r = try permPow(gpa, &perm, 2);
            defer gpa.free(r);
            try std.testing.expectEqualSlices(usize, &[_]usize{ 2, 3, 4, 0, 1 }, r);
        }
        // k = 5: full rotation -> identity
        {
            const r = try permPow(gpa, &perm, 5);
            defer gpa.free(r);
            try std.testing.expectEqualSlices(usize, &[_]usize{ 0, 1, 2, 3, 4 }, r);
        }
        // k = 7 == 2 mod 5: same as sigma^2
        {
            const r = try permPow(gpa, &perm, 7);
            defer gpa.free(r);
            try std.testing.expectEqualSlices(usize, &[_]usize{ 2, 3, 4, 0, 1 }, r);
        }
    }

    // Mixed (0 2)(1 3 4): each cycle rotates by k mod its own length.
    //   cycle (0 2) has length 2; cycle (1 3 4) has length 3.
    //   k = 5: 5 mod 2 = 1 -> (0 2) rotated by 1 = (0 2); 5 mod 3 = 2 -> (1 3 4) rotated by 2 = (1 4 3).
    //   So sigma^5: 0->2, 2->0, 1->4, 3->1, 4->3  => result = [2, 4, 0, 1, 3].
    {
        const perm = [_]usize{ 2, 3, 0, 4, 1 };
        const r = try permPow(gpa, &perm, 5);
        defer gpa.free(r);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 2, 4, 0, 1, 3 }, r);
    }

    // k = 0 always yields the identity, regardless of the permutation.
    {
        const perm = [_]usize{ 2, 3, 0, 4, 1 };
        const r = try permPow(gpa, &perm, 0);
        defer gpa.free(r);
        try std.testing.expectEqualSlices(usize, &[_]usize{ 0, 1, 2, 3, 4 }, r);
    }

    // Empty permutation: empty result.
    {
        const perm = [_]usize{};
        const r = try permPow(gpa, &perm, 0);
        defer gpa.free(r);
        try std.testing.expect(r.len == 0);
    }
}

test "enumerateCycleTypes covers all permutations" {
    const M = modint.Modint998244353;
    const Comb = CombinatoricModint(M);

    // Sum over all cycle types of n! / product(i^a[i] * a[i]!) must equal n!,
    // since every permutation has exactly one cycle type.
    const n: usize = 9;
    var comb_table = try Comb.init(std.testing.allocator, n);
    defer comb_table.deinit();
    const n_factorial = comb_table.fact[n];

    const Visitor = struct {
        sum: M,
        pub fn visit(self: *@This(), lcm: u128, denominator: M, nf: M) void {
            _ = lcm; // unused: this visitor only checks the multiplicity sum.
            self.sum = self.sum.add(nf.mul(denominator.inv()));
        }
    };
    var v = Visitor{ .sum = M.fromInt(0) };

    enumerateCycleTypes(M, Visitor, Visitor.visit, &comb_table, &v, n, n, 1, M.fromInt(1), n_factorial);

    try std.testing.expectEqual(n_factorial.toInt(), v.sum.toInt());
}
