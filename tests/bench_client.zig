const std = @import("std");
const net = std.net;
const posix = std.posix;

const DEFAULT_PORT: u16 = 19283;
const DEFAULT_ITERATIONS: usize = 100;

const request_body = "{\"model\":\"mock\",\"max_tokens\":1024,\"messages\":[{\"role\":\"user\",\"content\":\"test\"}]}";

fn makeRequest(port: u16) !f64 {
    const address = net.Address.initIp4(.{ 127, 0, 0, 1 }, port);
    const socket = try posix.socket(posix.AF.INET, posix.SOCK.STREAM, 0);
    defer posix.close(socket);

    // Build HTTP headers with dynamic port
    var header_buf: [256]u8 = undefined;
    const headers = std.fmt.bufPrint(&header_buf,
        "POST /v1/messages HTTP/1.1\r\n" ++
        "Host: localhost:{}\r\n" ++
        "Content-Type: application/json\r\n" ++
        "anthropic-version: 2023-06-01\r\n" ++
        "x-api-key: mock-key\r\n" ++
        "Content-Length: {}\r\n" ++
        "Connection: close\r\n\r\n"
    , .{ port, request_body.len }) catch return error.BufferTooSmall;

    const start = std.time.nanoTimestamp();

    try posix.connect(socket, &address.any, address.getOsSockLen());
    _ = try posix.send(socket, headers, 0);
    _ = try posix.send(socket, request_body, 0);

    // Read response
    var buf: [4096]u8 = undefined;
    while (true) {
        const n = posix.recv(socket, &buf, 0) catch break;
        if (n == 0) break;
    }

    const end = std.time.nanoTimestamp();
    return @as(f64, @floatFromInt(end - start)) / 1_000_000.0;
}

fn mean(values: []const f64) f64 {
    var sum: f64 = 0;
    for (values) |v| sum += v;
    return sum / @as(f64, @floatFromInt(values.len));
}

fn stddev(values: []const f64, avg: f64) f64 {
    var sum: f64 = 0;
    for (values) |v| {
        const diff = v - avg;
        sum += diff * diff;
    }
    return @sqrt(sum / @as(f64, @floatFromInt(values.len)));
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Usage: bench_client [iterations] [port] [server_name]
    const iterations: usize = if (args.len > 1)
        std.fmt.parseInt(usize, args[1], 10) catch DEFAULT_ITERATIONS
    else
        DEFAULT_ITERATIONS;

    const port: u16 = if (args.len > 2)
        std.fmt.parseInt(u16, args[2], 10) catch DEFAULT_PORT
    else
        DEFAULT_PORT;

    const server_name = if (args.len > 3) args[3] else "Mock";

    // Warmup
    const warmup = @min(5, iterations / 10);
    for (0..warmup) |_| {
        _ = makeRequest(port) catch continue;
    }

    // Benchmark
    const times = try allocator.alloc(f64, iterations);
    defer allocator.free(times);

    const total_start = std.time.nanoTimestamp();

    for (0..iterations) |i| {
        times[i] = makeRequest(port) catch 0;
    }

    const total_end = std.time.nanoTimestamp();
    const total_ms = @as(f64, @floatFromInt(total_end - total_start)) / 1_000_000.0;

    // Sort for percentiles
    std.mem.sort(f64, times, {}, std.sort.asc(f64));

    const avg = mean(times);
    const median = times[iterations / 2];
    const min_t = times[0];
    const max_t = times[iterations - 1];
    const std_dev = stddev(times, avg);
    const p95 = times[@as(usize, @intFromFloat(@as(f64, @floatFromInt(iterations)) * 0.95))];
    const p99 = times[@as(usize, @intFromFloat(@as(f64, @floatFromInt(iterations)) * 0.99))];
    const rps = @as(f64, @floatFromInt(iterations)) / (total_ms / 1000.0);

    // Output JSON - build manually to avoid brace escaping issues
    var out_buf: [512]u8 = undefined;
    const output = std.fmt.bufPrint(&out_buf,
        "{{\"language\":\"{s}\",\"iterations\":{},\"total_ms\":{d:.2},\"avg_ms\":{d:.3},\"median_ms\":{d:.3},\"min_ms\":{d:.3},\"max_ms\":{d:.3},\"stddev_ms\":{d:.3},\"p95_ms\":{d:.3},\"p99_ms\":{d:.3},\"requests_per_sec\":{d:.2}}}\n"
    , .{ server_name, iterations, total_ms, avg, median, min_t, max_t, std_dev, p95, p99, rps }) catch return;
    _ = posix.write(1, output) catch {};
}
