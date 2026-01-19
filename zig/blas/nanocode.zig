const std = @import("std");
const posix = std.posix;

// BLAS via Accelerate.framework (macOS). Build with:
//   zig build-exe nanocode.zig -OReleaseFast -fstrip -framework Accelerate
//
// This is a "BLAS flavor": it uses SGEMM for a tiny dot-product "score"
// on every prompt, plus a /blas micro-benchmark.

extern fn cblas_sgemm(
    order: c_int,
    trans_a: c_int,
    trans_b: c_int,
    m: c_int,
    n: c_int,
    k: c_int,
    alpha: f32,
    a: [*]const f32,
    lda: c_int,
    b: [*]const f32,
    ldb: c_int,
    beta: f32,
    c: [*]f32,
    ldc: c_int,
) void;

const CblasRowMajor: c_int = 101;
const CblasNoTrans: c_int = 111;

const B = "\x1b[1m";
const D = "\x1b[2m";
const R = "\x1b[0m";
const BL = "\x1b[34m";
const G = "\x1b[32m";
const C = "\x1b[36m";

const Dim: usize = 256;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    try print("{s}nanocode{s} | {s}Zig + BLAS (Accelerate){s}\n\n", .{ B, R, D, R });

    const fixed = try alloc.alloc(f32, Dim);
    defer alloc.free(fixed);
    initFixed(fixed);

    const vec = try alloc.alloc(f32, Dim);
    defer alloc.free(vec);

    var line_buf: [4096]u8 = undefined;
    while (true) {
        try print("{s}{s}❯{s} ", .{ B, BL, R });
        const line = (try readLine(&line_buf)) orelse break;
        const input = std.mem.trim(u8, line, " \r\n");
        if (input.len == 0) continue;

        if (std.mem.eql(u8, input, "/q")) break;
        if (std.mem.eql(u8, input, "/c")) {
            try print("{s}⏺ Cleared{s}\n", .{ G, R });
            continue;
        }
        if (std.mem.eql(u8, input, "/blas")) {
            try blasBench(alloc);
            continue;
        }

        fillFromText(vec, input);
        const score = blasDot(fixed, vec);
        try print("{s}⏺{s} score={d:.3} | {s}\n", .{ C, R, score, input });
    }
}

fn readLine(buf: *[4096]u8) !?[]u8 {
    var total: usize = 0;
    while (total < buf.len - 1) {
        const n = posix.read(0, buf[total .. total + 1]) catch return null;
        if (n == 0) return null;
        if (buf[total] == '\n') break;
        total += 1;
    }
    return buf[0..total];
}

fn print(comptime fmt: []const u8, args: anytype) !void {
    var tmp: [4096]u8 = undefined;
    const out = std.fmt.bufPrint(&tmp, fmt, args) catch tmp[0..0];
    _ = posix.write(1, out) catch {};
}

fn initFixed(dst: []f32) void {
    for (dst, 0..) |*v, i| {
        const x: u32 = @intCast((i * 73 + 17) % 997);
        v.* = @as(f32, @floatFromInt(x)) / 997.0;
    }
}

fn fnv1a64(bytes: []const u8) u64 {
    var h: u64 = 14695981039346656037;
    for (bytes) |b| {
        h ^= b;
        h *%= 1099511628211;
    }
    return h;
}

fn xorshift64(state: *u64) u64 {
    var x = state.*;
    x ^= x << 13;
    x ^= x >> 7;
    x ^= x << 17;
    state.* = x;
    return x;
}

fn fillFromText(dst: []f32, input: []const u8) void {
    var s: u64 = fnv1a64(input) ^ 0x9E3779B97F4A7C15;
    for (dst, 0..) |*v, i| {
        s ^= @as(u64, i) *% 0xD6E8FEB86659FD93;
        const r = xorshift64(&s);
        const mant: u32 = @intCast((r >> 40) & 0xFFFF);
        v.* = @as(f32, @floatFromInt(mant)) / 65535.0;
    }
}

fn blasDot(a: []const f32, b: []const f32) f32 {
    var out: [1]f32 = .{0};
    const k: c_int = @intCast(a.len);
    cblas_sgemm(
        CblasRowMajor,
        CblasNoTrans,
        CblasNoTrans,
        1,
        1,
        k,
        1.0,
        a.ptr,
        k,
        b.ptr,
        1,
        0.0,
        out[0..].ptr,
        1,
    );
    return out[0];
}

fn blasBench(alloc: std.mem.Allocator) !void {
    const m: c_int = 256;
    const n: c_int = 256;
    const k: c_int = 256;

    const a = try alloc.alloc(f32, @as(usize, @intCast(m * k)));
    defer alloc.free(a);
    const b = try alloc.alloc(f32, @as(usize, @intCast(k * n)));
    defer alloc.free(b);
    const c = try alloc.alloc(f32, @as(usize, @intCast(m * n)));
    defer alloc.free(c);

    for (a, 0..) |*v, i| v.* = @as(f32, @floatFromInt(@as(u32, @intCast(i % 13)))) / 13.0;
    for (b, 0..) |*v, i| v.* = @as(f32, @floatFromInt(@as(u32, @intCast((i * 7) % 17)))) / 17.0;
    @memset(c, 0);

    var t = try std.time.Timer.start();
    cblas_sgemm(
        CblasRowMajor,
        CblasNoTrans,
        CblasNoTrans,
        m,
        n,
        k,
        1.0,
        a.ptr,
        k,
        b.ptr,
        n,
        0.0,
        c.ptr,
        n,
    );
    const ns = t.read();

    const flops: f64 = 2.0 * @as(f64, @floatFromInt(m)) * @as(f64, @floatFromInt(n)) * @as(f64, @floatFromInt(k));
    const sec: f64 = @as(f64, @floatFromInt(ns)) / 1_000_000_000.0;
    const gflops: f64 = (flops / sec) / 1_000_000_000.0;

    try print("{s}⏺{s} SGEMM 256x256: {d:.2} GFLOP/s (C[0]={d:.3})\n", .{ C, R, gflops, c[0] });
}
