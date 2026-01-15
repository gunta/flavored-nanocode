// Benchmark mock server - always returns end_turn (no tool calls)
// This allows measuring single request/response cycles
// Usage: ./bench_server [port]

const std = @import("std");
const net = std.net;
const posix = std.posix;

const DEFAULT_PORT: u16 = 19283;

// Simple response with end_turn - no tool calls
const mock_response =
    \\{"id":"msg_bench","type":"message","role":"assistant","content":[{"type":"text","text":"Benchmark response."}],"model":"mock","stop_reason":"end_turn","usage":{"input_tokens":10,"output_tokens":5}}
;

const http_200 = "HTTP/1.1 200 OK\r\n" ++
    "Content-Type: application/json\r\n" ++
    "Connection: close\r\n" ++
    "Content-Length: {}\r\n\r\n";

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const port: u16 = if (args.len > 1)
        std.fmt.parseInt(u16, args[1], 10) catch DEFAULT_PORT
    else
        DEFAULT_PORT;

    const address = net.Address.initIp4(.{ 127, 0, 0, 1 }, port);
    const socket = try posix.socket(posix.AF.INET, posix.SOCK.STREAM, 0);
    defer posix.close(socket);

    try posix.setsockopt(socket, posix.SOL.SOCKET, posix.SO.REUSEADDR, &std.mem.toBytes(@as(c_int, 1)));
    try posix.bind(socket, &address.any, address.getOsSockLen());
    try posix.listen(socket, 128);

    while (true) {
        const conn = posix.accept(socket, null, null, 0) catch continue;
        handleConnection(conn) catch {};
        posix.close(conn);
    }
}

fn handleConnection(conn: posix.socket_t) !void {
    var buf: [65536]u8 = undefined;
    var total: usize = 0;

    // Read request
    while (total < buf.len) {
        const n = posix.recv(conn, buf[total..], 0) catch break;
        if (n == 0) break;
        total += n;

        if (std.mem.indexOf(u8, buf[0..total], "\r\n\r\n")) |header_end| {
            const headers = buf[0..header_end];
            if (std.mem.indexOf(u8, headers, "Content-Length: ")) |cl_start| {
                const cl_value_start = cl_start + 16;
                const cl_end = std.mem.indexOfPos(u8, headers, cl_value_start, "\r\n") orelse header_end;
                const content_length = std.fmt.parseInt(usize, headers[cl_value_start..cl_end], 10) catch 0;
                if (total >= header_end + 4 + content_length) break;
            } else break;
        }
    }

    if (total == 0) return;

    // Send response
    var header_buf: [256]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf, http_200, .{mock_response.len}) catch return;
    _ = try posix.send(conn, header, 0);
    _ = try posix.send(conn, mock_response, 0);
}
