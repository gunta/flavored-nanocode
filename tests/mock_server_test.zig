const std = @import("std");
const net = std.net;
const posix = std.posix;

const PORT: u16 = 19283;

fn sendRequest(method: []const u8, path: []const u8, body: ?[]const u8) ![]u8 {
    const allocator = std.testing.allocator;

    const address = net.Address.initIp4(.{ 127, 0, 0, 1 }, PORT);
    const socket = try posix.socket(posix.AF.INET, posix.SOCK.STREAM, 0);
    defer posix.close(socket);

    try posix.connect(socket, &address.any, address.getOsSockLen());

    // Build request
    var request_buf: [4096]u8 = undefined;
    const content_length = if (body) |b| b.len else 0;
    const request = try std.fmt.bufPrint(&request_buf, "{s} {s} HTTP/1.1\r\n" ++
        "Host: localhost:{d}\r\n" ++
        "Content-Type: application/json\r\n" ++
        "Content-Length: {d}\r\n\r\n", .{ method, path, PORT, content_length });

    _ = try posix.send(socket, request, 0);
    if (body) |b| {
        _ = try posix.send(socket, b, 0);
    }

    // Read response (loop until connection closes)
    var response_buf = try allocator.alloc(u8, 8192);
    var total: usize = 0;
    while (true) {
        const n = try posix.recv(socket, response_buf[total..], 0);
        if (n == 0) break;
        total += n;
    }
    return allocator.realloc(response_buf, total);
}

fn getResponseBody(response: []const u8) ?[]const u8 {
    const header_end = std.mem.indexOf(u8, response, "\r\n\r\n") orelse return null;
    return response[header_end + 4 ..];
}

fn getStatusCode(response: []const u8) ?u16 {
    const first_space = std.mem.indexOf(u8, response, " ") orelse return null;
    const second_space = std.mem.indexOfPos(u8, response, first_space + 1, " ") orelse return null;
    return std.fmt.parseInt(u16, response[first_space + 1 .. second_space], 10) catch null;
}

fn getJsonString(obj: std.json.ObjectMap, key: []const u8) ?[]const u8 {
    const val = obj.get(key) orelse return null;
    return if (val == .string) val.string else null;
}

test "1. glob tool_use response" {
    const response = try sendRequest("POST", "/v1/messages", "{}");
    defer std.testing.allocator.free(response);

    try std.testing.expectEqual(@as(?u16, 200), getStatusCode(response));

    const body = getResponseBody(response) orelse return error.NoBody;
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, body, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("tool_use", getJsonString(obj, "stop_reason").?);

    const content = obj.get("content").?.array;
    try std.testing.expect(content.items.len >= 2);

    // Find tool_use block
    for (content.items) |item| {
        const item_obj = item.object;
        if (std.mem.eql(u8, getJsonString(item_obj, "type").?, "tool_use")) {
            try std.testing.expectEqualStrings("glob", getJsonString(item_obj, "name").?);
            return;
        }
    }
    return error.NoToolUse;
}

test "2. read tool_use response" {
    const response = try sendRequest("POST", "/v1/messages", "{}");
    defer std.testing.allocator.free(response);

    const body = getResponseBody(response) orelse return error.NoBody;
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, body, .{});
    defer parsed.deinit();

    const content = parsed.value.object.get("content").?.array;
    for (content.items) |item| {
        const item_obj = item.object;
        if (std.mem.eql(u8, getJsonString(item_obj, "type").?, "tool_use")) {
            try std.testing.expectEqualStrings("read", getJsonString(item_obj, "name").?);
            return;
        }
    }
    return error.NoToolUse;
}

test "3. grep tool_use response" {
    const response = try sendRequest("POST", "/v1/messages", "{}");
    defer std.testing.allocator.free(response);

    const body = getResponseBody(response) orelse return error.NoBody;
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, body, .{});
    defer parsed.deinit();

    const content = parsed.value.object.get("content").?.array;
    for (content.items) |item| {
        const item_obj = item.object;
        if (std.mem.eql(u8, getJsonString(item_obj, "type").?, "tool_use")) {
            try std.testing.expectEqualStrings("grep", getJsonString(item_obj, "name").?);
            return;
        }
    }
    return error.NoToolUse;
}

test "4. write tool_use response" {
    const response = try sendRequest("POST", "/v1/messages", "{}");
    defer std.testing.allocator.free(response);

    const body = getResponseBody(response) orelse return error.NoBody;
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, body, .{});
    defer parsed.deinit();

    const content = parsed.value.object.get("content").?.array;
    for (content.items) |item| {
        const item_obj = item.object;
        if (std.mem.eql(u8, getJsonString(item_obj, "type").?, "tool_use")) {
            try std.testing.expectEqualStrings("write", getJsonString(item_obj, "name").?);
            return;
        }
    }
    return error.NoToolUse;
}

test "5. edit tool_use response" {
    const response = try sendRequest("POST", "/v1/messages", "{}");
    defer std.testing.allocator.free(response);

    const body = getResponseBody(response) orelse return error.NoBody;
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, body, .{});
    defer parsed.deinit();

    const content = parsed.value.object.get("content").?.array;
    for (content.items) |item| {
        const item_obj = item.object;
        if (std.mem.eql(u8, getJsonString(item_obj, "type").?, "tool_use")) {
            try std.testing.expectEqualStrings("edit", getJsonString(item_obj, "name").?);
            return;
        }
    }
    return error.NoToolUse;
}

test "6. bash tool_use response" {
    const response = try sendRequest("POST", "/v1/messages", "{}");
    defer std.testing.allocator.free(response);

    const body = getResponseBody(response) orelse return error.NoBody;
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, body, .{});
    defer parsed.deinit();

    const content = parsed.value.object.get("content").?.array;
    for (content.items) |item| {
        const item_obj = item.object;
        if (std.mem.eql(u8, getJsonString(item_obj, "type").?, "tool_use")) {
            try std.testing.expectEqualStrings("bash", getJsonString(item_obj, "name").?);
            return;
        }
    }
    return error.NoToolUse;
}

test "7. final end_turn response" {
    const response = try sendRequest("POST", "/v1/messages", "{}");
    defer std.testing.allocator.free(response);

    const body = getResponseBody(response) orelse return error.NoBody;
    const parsed = try std.json.parseFromSlice(std.json.Value, std.testing.allocator, body, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    try std.testing.expectEqualStrings("end_turn", getJsonString(obj, "stop_reason").?);

    // Should only have text, no tool_use
    const content = obj.get("content").?.array;
    for (content.items) |item| {
        const item_type = getJsonString(item.object, "type").?;
        try std.testing.expect(!std.mem.eql(u8, item_type, "tool_use"));
    }
}

test "OPTIONS returns CORS headers" {
    const response = try sendRequest("OPTIONS", "/v1/messages", null);
    defer std.testing.allocator.free(response);

    try std.testing.expectEqual(@as(?u16, 204), getStatusCode(response));
    try std.testing.expect(std.mem.indexOf(u8, response, "Access-Control-Allow-Origin") != null);
}

test "GET /unknown returns 404" {
    const response = try sendRequest("GET", "/unknown", null);
    defer std.testing.allocator.free(response);

    try std.testing.expectEqual(@as(?u16, 404), getStatusCode(response));
}
