const std = @import("std");
const net = std.net;
const posix = std.posix;
const json = std.json;

const PORT: u16 = 19283;

// Tool use responses - each tests a different tool
const tool_responses = [_][]const u8{
    // 1. glob - find files
    \\{"id":"msg_1","type":"message","role":"assistant","content":[{"type":"text","text":"Let me find the test files."},{"type":"tool_use","id":"tool_1","name":"glob","input":{"pat":"*.zig","path":"."}}],"model":"mock","stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":20}}
    ,
    // 2. read - read a file
    \\{"id":"msg_2","type":"message","role":"assistant","content":[{"type":"text","text":"Now let me read the mock server."},{"type":"tool_use","id":"tool_2","name":"read","input":{"path":"mock_server.zig"}}],"model":"mock","stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":20}}
    ,
    // 3. grep - search for pattern
    \\{"id":"msg_3","type":"message","role":"assistant","content":[{"type":"text","text":"Searching for PORT definition."},{"type":"tool_use","id":"tool_3","name":"grep","input":{"pat":"PORT","path":"."}}],"model":"mock","stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":20}}
    ,
    // 4. write - create a file
    \\{"id":"msg_4","type":"message","role":"assistant","content":[{"type":"text","text":"Creating a test file."},{"type":"tool_use","id":"tool_4","name":"write","input":{"path":"_test_output.txt","content":"Hello from mock server test!"}}],"model":"mock","stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":20}}
    ,
    // 5. edit - modify the file
    \\{"id":"msg_5","type":"message","role":"assistant","content":[{"type":"text","text":"Editing the test file."},{"type":"tool_use","id":"tool_5","name":"edit","input":{"path":"_test_output.txt","old":"Hello","new":"Goodbye"}}],"model":"mock","stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":20}}
    ,
    // 6. bash - run a command
    \\{"id":"msg_6","type":"message","role":"assistant","content":[{"type":"text","text":"Running a shell command."},{"type":"tool_use","id":"tool_6","name":"bash","input":{"cmd":"echo 'Mock test complete!' && rm -f _test_output.txt"}}],"model":"mock","stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":20}}
    ,
    // 7. Final response - no more tools
    \\{"id":"msg_7","type":"message","role":"assistant","content":[{"type":"text","text":"All tools tested successfully! The mock server exercised: glob, read, grep, write, edit, and bash."}],"model":"mock","stop_reason":"end_turn","usage":{"input_tokens":10,"output_tokens":30}}
    ,
};

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

var request_count: usize = 0;

pub fn main() !void {
    const address = net.Address.initIp4(.{ 127, 0, 0, 1 }, PORT);
    const socket = try posix.socket(posix.AF.INET, posix.SOCK.STREAM, 0);
    defer posix.close(socket);

    try posix.setsockopt(socket, posix.SOL.SOCKET, posix.SO.REUSEADDR, &std.mem.toBytes(@as(c_int, 1)));
    try posix.bind(socket, &address.any, address.getOsSockLen());
    try posix.listen(socket, 128);

    std.debug.print("Mock server running on http://localhost:{d}/v1/messages\n", .{PORT});
    std.debug.print("Will cycle through {d} responses testing all tools\n", .{tool_responses.len});

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
    var buf: [65536]u8 = undefined;
    var total: usize = 0;

    // Read full request
    while (total < buf.len) {
        const n = posix.recv(conn, buf[total..], 0) catch break;
        if (n == 0) break;
        total += n;

        // Check if we have complete HTTP request (headers + body based on Content-Length)
        if (std.mem.indexOf(u8, buf[0..total], "\r\n\r\n")) |header_end| {
            // Find Content-Length
            const headers = buf[0..header_end];
            if (std.mem.indexOf(u8, headers, "Content-Length: ")) |cl_start| {
                const cl_value_start = cl_start + 16;
                const cl_end = std.mem.indexOfPos(u8, headers, cl_value_start, "\r\n") orelse header_end;
                const content_length = std.fmt.parseInt(usize, headers[cl_value_start..cl_end], 10) catch 0;
                const body_start = header_end + 4;
                if (total >= body_start + content_length) break;
            } else break;
        }
    }

    if (total == 0) return;
    const request = buf[0..total];

    // Parse method and path
    const method_end = std.mem.indexOf(u8, request, " ") orelse return;
    const method = request[0..method_end];

    const path_start = method_end + 1;
    const path_end = std.mem.indexOfPos(u8, request, path_start, " ") orelse return;
    const path = request[path_start..path_end];

    if (std.mem.eql(u8, method, "POST") and std.mem.eql(u8, path, "/v1/messages")) {
        // Get response for current request
        const response = tool_responses[@min(request_count, tool_responses.len - 1)];
        request_count += 1;

        std.debug.print("Request {d}: returning response {d}/{d}\n", .{ request_count, @min(request_count, tool_responses.len), tool_responses.len });

        var header_buf: [256]u8 = undefined;
        const header = std.fmt.bufPrint(&header_buf, http_200, .{response.len}) catch return;
        _ = try posix.send(conn, header, 0);
        _ = try posix.send(conn, response, 0);
    } else if (std.mem.eql(u8, method, "OPTIONS")) {
        _ = try posix.send(conn, cors_response, 0);
    } else {
        _ = try posix.send(conn, http_404, 0);
    }
}
