const std = @import("std");

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

test "fft convolution" {
    const gpa = std.testing.allocator;

    // Test (1 + x) * (1 + x) = 1 + 2x + x²
    const a = [_]f64{ 1, 1 };
    const b = [_]f64{ 1, 1 };
    const conv = try convolution(gpa, &a, &b);
    defer gpa.free(conv);

    const expected = [_]f64{ 1, 2, 1 };
    for (conv, 0..) |c, i| {
        try std.testing.expectEqual(expected[i], c);
    }
}
