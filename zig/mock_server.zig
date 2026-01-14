const std = @import("std");
const net = std.net;
const posix = std.posix;

const PORT: u16 = 8080;

const mock_response =
    \\{"id":"msg_mock","type":"message","role":"assistant","content":[{"type":"text","text":"Mock response from Zig server"}],"model":"mock-model","stop_reason":"end_turn","stop_sequence":null,"usage":{"input_tokens":10,"output_tokens":5}}
;

const http_200 = "HTTP/1.1 200 OK\r\n" ++
    "Content-Type: application/json\r\n" ++
    "Access-Control-Allow-Origin: *\r\n" ++
    "Connection: close\r\n" ++
    "Content-Length: {d}\r\n\r\n";

const http_404 = "HTTP/1.1 404 Not Found\r\n" ++
    "Content-Type: text/plain\r\n" ++
    "Connection: close\r\n" ++
    "Content-Length: 9\r\n\r\nNot Found";

const cors_response = "HTTP/1.1 204 No Content\r\n" ++
    "Access-Control-Allow-Origin: *\r\n" ++
    "Access-Control-Allow-Methods: POST, OPTIONS\r\n" ++
    "Access-Control-Allow-Headers: content-type, authorization, x-api-key, anthropic-version\r\n" ++
    "Connection: close\r\n\r\n";

pub fn main() !void {
    const address = net.Address.initIp4(.{ 127, 0, 0, 1 }, PORT);
    const socket = try posix.socket(posix.AF.INET, posix.SOCK.STREAM, 0);
    defer posix.close(socket);

    try posix.setsockopt(socket, posix.SOL.SOCKET, posix.SO.REUSEADDR, &std.mem.toBytes(@as(c_int, 1)));
    try posix.bind(socket, &address.any, address.getOsSockLen());
    try posix.listen(socket, 128);

    std.debug.print("Mock server running on http://localhost:{d}/v1/messages\n", .{PORT});

    while (true) {
        const conn = posix.accept(socket, null, null, 0) catch |err| {
            std.debug.print("Accept error: {}\n", .{err});
            continue;
        };

        handleConnection(conn) catch |err| {
            std.debug.print("Handle error: {}\n", .{err});
        };
        posix.close(conn);
    }
}

fn handleConnection(conn: posix.socket_t) !void {
    var buf: [8192]u8 = undefined;
    const n = try posix.recv(conn, &buf, 0);
    if (n == 0) return;

    const request = buf[0..n];

    // Parse method and path
    const method_end = std.mem.indexOf(u8, request, " ") orelse return;
    const method = request[0..method_end];

    const path_start = method_end + 1;
    const path_end = std.mem.indexOfPos(u8, request, path_start, " ") orelse return;
    const path = request[path_start..path_end];

    if (std.mem.eql(u8, method, "POST") and std.mem.eql(u8, path, "/v1/messages")) {
        // Send mock response
        var header_buf: [256]u8 = undefined;
        const header = std.fmt.bufPrint(&header_buf, http_200, .{mock_response.len}) catch return;
        _ = try posix.send(conn, header, 0);
        _ = try posix.send(conn, mock_response, 0);
    } else if (std.mem.eql(u8, method, "OPTIONS")) {
        _ = try posix.send(conn, cors_response, 0);
    } else {
        _ = try posix.send(conn, http_404, 0);
    }
}
