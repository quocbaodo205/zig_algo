const std = @import("std");

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

test "pow mod tests" {
    // Test comptime pow_mod
    try std.testing.expect(pow_mod_comptime(2, 10, 998244353) == 1024);
    try std.testing.expect(pow_mod_comptime(3, 5, 7) == 5); // 243 mod 7 = 5

    // Test runtime pow_mod
    try std.testing.expect(pow_mod(2, 10, 998244353) == 1024);
    try std.testing.expect(pow_mod(3, 5, 7) == 5); // 243 mod 7 = 5

    // Test pow_mod_big
    try std.testing.expect(pow_mod_big(2, 10, 998244353) == 1024);
    try std.testing.expect(pow_mod_big(3, 5, 7) == 5); // 243 mod 7 = 5
    const big_prime: u64 = 36028797018963977;
    try std.testing.expect(pow_mod_big(2, 100, big_prime) == pow_mod_comptime(2, 100, big_prime));

    // Test mod_inverse
    const p = 998244353;
    try std.testing.expect((2 * mod_inverse(2, p)) % p == 1);
    try std.testing.expect((5 * mod_inverse(5, p)) % p == 1);

    // Test mod_inverse_comptime
    try std.testing.expect(mod_inverse_comptime(2, p) == mod_inverse(2, p));
    try std.testing.expect(mod_inverse_comptime(5, p) == mod_inverse(5, p));
}
