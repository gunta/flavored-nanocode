const std = @import("std");
const posix = std.posix;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const openrouter_key = std.process.getEnvVarOwned(alloc, "OPENROUTER_API_KEY") catch null;
    const anthropic_key = std.process.getEnvVarOwned(alloc, "ANTHROPIC_API_KEY") catch null;
    const api_default: []const u8 = if (openrouter_key != null) "openrouter.ai" else "api.anthropic.com";
    const api_host = std.process.getEnvVarOwned(alloc, "API_HOST") catch @constCast(api_default);
    const api_url_env = std.process.getEnvVarOwned(alloc, "API_URL") catch null;
    const model_default: []const u8 = if (openrouter_key != null) "anthropic/claude-sonnet-4-20250514" else "claude-sonnet-4-20250514";
    const model = std.process.getEnvVarOwned(alloc, "MODEL") catch @constCast(model_default);
    const key = openrouter_key orelse anthropic_key orelse "";
    const use_bearer = openrouter_key != null;

    // Parse API_URL if set (for test mode: http://localhost:PORT)
    var host: []const u8 = api_host;
    var port: u16 = 443;
    var use_tls = true;
    if (api_url_env) |url| {
        if (std.mem.startsWith(u8, url, "http://")) {
            use_tls = false;
            const rest = url[7..];
            if (std.mem.indexOf(u8, rest, ":")) |colon| {
                host = rest[0..colon];
                const port_end = std.mem.indexOf(u8, rest[colon + 1 ..], "/") orelse (rest.len - colon - 1);
                port = std.fmt.parseInt(u16, rest[colon + 1 .. colon + 1 + port_end], 10) catch 80;
            }
        }
    }

    try print("\x1b[1mnanocode\x1b[0m | \x1b[2m{s}\x1b[0m | Zig 0.15\n\n", .{model});

    var messages: std.ArrayListUnmanaged(u8) = .{};
    defer messages.deinit(alloc);
    try messages.appendSlice(alloc, "[]");

    var line_buf: [4096]u8 = undefined;

    while (true) {
        try print("\x1b[1m\x1b[34m❯\x1b[0m ", .{});
        // Read line from stdin (fd 0)
        var total: usize = 0;
        while (total < line_buf.len - 1) {
            const n = posix.read(0, line_buf[total..total+1]) catch break;
            if (n == 0) break;
            if (line_buf[total] == '\n') break;
            total += 1;
        }
        if (total == 0) break;
        const input = std.mem.trim(u8, line_buf[0..total], " \r\n");
        if (input.len == 0) break; // Exit on empty (for benchmarks)
        if (std.mem.eql(u8, input, "/q")) break;
        if (std.mem.eql(u8, input, "/c")) {
            messages.clearRetainingCapacity();
            try messages.appendSlice(alloc, "[]");
            try print("\x1b[32m⏺ Cleared\x1b[0m\n", .{});
            continue;
        }

        try appendUserMessage(&messages, alloc, input);
        while (true) {
            const resp = try callApi(alloc, host, port, use_tls, key, model, messages.items, use_bearer);
            defer alloc.free(resp);
            var content = try parseContent(alloc, resp);
            defer content.deinit();

            var tool_results: std.ArrayListUnmanaged(ToolResult) = .{};
            defer {
                for (tool_results.items) |tr| alloc.free(tr.content);
                tool_results.deinit(alloc);
            }

            for (content.items) |block| {
                switch (block.kind) {
                    .text => try print("\n\x1b[36m⏺\x1b[0m {s}\n", .{block.text}),
                    .tool_use => {
                        try print("\n\x1b[32m⏺ {s}\x1b[0m\n", .{block.tool_name});
                        const result = try runTool(alloc, block.tool_name, block.input);
                        try print("  \x1b[2m⎿ {s}\x1b[0m\n", .{preview(result)});
                        try tool_results.append(alloc, .{ .tool_use_id = block.tool_id, .content = result });
                    },
                }
            }

            try appendAssistant(&messages, alloc, content.raw);
            if (tool_results.items.len == 0) break;
            try appendToolResults(&messages, alloc, tool_results.items);
        }
        try print("\n", .{});
    }
}

fn print(comptime fmt: []const u8, args: anytype) !void {
    var buf: [4096]u8 = undefined;
    const out = std.fmt.bufPrint(&buf, fmt, args) catch &buf;
    _ = posix.write(1, out) catch {};
}

fn preview(s: []const u8) []const u8 {
    return if (s.len > 80) s[0..80] else s;
}

const ToolResult = struct { tool_use_id: []const u8, content: []u8 };

fn appendUserMessage(messages: *std.ArrayListUnmanaged(u8), alloc: std.mem.Allocator, input: []const u8) !void {
    try grow(messages);
    const escaped = try jsonEscape(alloc, input);
    defer alloc.free(escaped);
    var w = messages.writer(alloc);
    try w.print("{{\"role\":\"user\",\"content\":\"{s}\"}}]", .{escaped});
}

fn appendAssistant(messages: *std.ArrayListUnmanaged(u8), alloc: std.mem.Allocator, raw_content: []const u8) !void {
    try grow(messages);
    var w = messages.writer(alloc);
    try w.print("{{\"role\":\"assistant\",\"content\":{s}}}]", .{raw_content});
}

fn appendToolResults(messages: *std.ArrayListUnmanaged(u8), alloc: std.mem.Allocator, trs: []const ToolResult) !void {
    try grow(messages);
    try messages.appendSlice(alloc, "{\"role\":\"user\",\"content\":[");
    for (trs, 0..) |tr, i| {
        if (i != 0) try messages.appendSlice(alloc, ",");
        const content_esc = try jsonEscape(alloc, tr.content);
        defer alloc.free(content_esc);
        var w = messages.writer(alloc);
        try w.print("{{\"type\":\"tool_result\",\"tool_use_id\":\"{s}\",\"content\":\"{s}\"}}", .{ tr.tool_use_id, content_esc });
    }
    try messages.appendSlice(alloc, "]}]");
}

fn grow(messages: *std.ArrayListUnmanaged(u8)) !void {
    if (messages.items.len <= 2) return;
    messages.items[messages.items.len - 1] = ',';
}

fn jsonEscape(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    for (input) |c| switch (c) {
        '"' => try buf.appendSlice(alloc, "\\\""),
        '\\' => try buf.appendSlice(alloc, "\\\\"),
        '\n' => try buf.appendSlice(alloc, "\\n"),
        '\r' => try buf.appendSlice(alloc, "\\r"),
        '\t' => try buf.appendSlice(alloc, "\\t"),
        else => try buf.append(alloc, c),
    };
    return buf.toOwnedSlice(alloc);
}

const Block = struct {
    kind: enum { text, tool_use },
    text: []const u8 = "",
    tool_name: []const u8 = "",
    tool_id: []const u8 = "",
    input: []const u8 = "",
    raw: []const u8,
};

const ContentList = struct {
    items: []Block,
    raw: []const u8,
    allocator: std.mem.Allocator,
    fn deinit(self: *ContentList) void {
        self.allocator.free(self.items);
        self.allocator.free(self.raw);
    }
};

fn parseContent(alloc: std.mem.Allocator, resp: []const u8) !ContentList {
    const raw = try alloc.dupe(u8, resp);
    var items: std.ArrayListUnmanaged(Block) = .{};
    var iter = std.mem.splitSequence(u8, resp, "{\"type\":");
    _ = iter.next();
    while (iter.next()) |chunk| {
        if (chunk.len == 0) continue;
        const kind_char = chunk[1];
        if (kind_char == 't') {
            const text_start = std.mem.indexOf(u8, chunk, "\"text\":\"") orelse continue;
            const after = chunk[text_start + 8 ..];
            const text_end = std.mem.indexOfScalar(u8, after, '"') orelse continue;
            try items.append(alloc, .{ .kind = .text, .text = after[0..text_end], .raw = raw });
        } else if (kind_char == 'o') {
            const name_start = std.mem.indexOf(u8, chunk, "\"name\":\"") orelse continue;
            const after_name = chunk[name_start + 8 ..];
            const name_end = std.mem.indexOfScalar(u8, after_name, '"') orelse continue;
            const id_start = std.mem.indexOf(u8, chunk, "\"id\":\"") orelse continue;
            const after_id = chunk[id_start + 6 ..];
            const id_end = std.mem.indexOfScalar(u8, after_id, '"') orelse continue;
            const input_start = std.mem.indexOf(u8, chunk, "\"input\":") orelse continue;
            const input_slice = chunk[input_start + 8 ..];
            const input_end = std.mem.indexOfScalar(u8, input_slice, '}') orelse continue;
            try items.append(alloc, .{ .kind = .tool_use, .tool_name = after_name[0..name_end], .tool_id = after_id[0..id_end], .input = input_slice[0 .. input_end + 1], .raw = raw });
        }
    }
    return .{ .items = try items.toOwnedSlice(alloc), .raw = raw, .allocator = alloc };
}

fn callApi(alloc: std.mem.Allocator, host: []const u8, port: u16, use_tls: bool, key: []const u8, model: []const u8, messages: []const u8, use_bearer: bool) ![]u8 {
    _ = use_tls; // For now, always use plain HTTP for simplicity
    const tools =
        \\[{"name":"read","description":"Read file","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
        \\{"name":"write","description":"Write file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
        \\{"name":"bash","description":"Run command","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]
    ;

    var body: std.ArrayListUnmanaged(u8) = .{};
    defer body.deinit(alloc);
    var bw = body.writer(alloc);
    try bw.print("{{\"model\":\"{s}\",\"max_tokens\":4096,\"system\":\"Concise coding assistant\",\"messages\":{s},\"tools\":{s}}}", .{ model, messages, tools });

    // Build HTTP request
    var req: std.ArrayListUnmanaged(u8) = .{};
    defer req.deinit(alloc);
    var w = req.writer(alloc);
    try req.appendSlice(alloc, "POST /v1/messages HTTP/1.1\r\n");
    try w.print("Host: {s}\r\n", .{host});
    try req.appendSlice(alloc, "Content-Type: application/json\r\n");
    try req.appendSlice(alloc, "anthropic-version: 2023-06-01\r\n");
    if (use_bearer) {
        try w.print("Authorization: Bearer {s}\r\n", .{key});
    } else {
        try w.print("x-api-key: {s}\r\n", .{key});
    }
    try w.print("Content-Length: {d}\r\n", .{body.items.len});
    try req.appendSlice(alloc, "Connection: close\r\n\r\n");
    try req.appendSlice(alloc, body.items);

    // Connect via TCP
    // Handle localhost specially
    const ip: [4]u8 = if (std.mem.eql(u8, host, "localhost"))
        .{ 127, 0, 0, 1 }
    else blk: {
        // Try to parse as IP address
        var result: [4]u8 = undefined;
        var iter = std.mem.splitScalar(u8, host, '.');
        var i: usize = 0;
        while (iter.next()) |part| : (i += 1) {
            if (i >= 4) break :blk .{ 127, 0, 0, 1 };
            result[i] = std.fmt.parseInt(u8, part, 10) catch break :blk .{ 127, 0, 0, 1 };
        }
        break :blk result;
    };
    const address = std.net.Address.initIp4(ip, port);
    const sock = try posix.socket(address.any.family, posix.SOCK.STREAM, 0);
    defer posix.close(sock);
    try posix.connect(sock, &address.any, address.getOsSockLen());

    // Send request
    _ = try posix.write(sock, req.items);

    // Read response
    var resp: std.ArrayListUnmanaged(u8) = .{};
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = posix.read(sock, &buf) catch break;
        if (n == 0) break;
        try resp.appendSlice(alloc, buf[0..n]);
    }

    // Skip HTTP headers, find body
    const body_start = std.mem.indexOf(u8, resp.items, "\r\n\r\n") orelse 0;
    const result = try alloc.dupe(u8, resp.items[body_start + 4 ..]);
    resp.deinit(alloc);
    return result;
}

fn runTool(alloc: std.mem.Allocator, name: []const u8, input: []const u8) ![]u8 {
    if (std.mem.eql(u8, name, "read")) return try toolRead(alloc, input);
    if (std.mem.eql(u8, name, "write")) return try toolWrite(alloc, input);
    if (std.mem.eql(u8, name, "bash")) return try toolBash(alloc, input);
    return alloc.dupe(u8, "unknown tool");
}

fn parseField(input: []const u8, comptime field: []const u8) ?[]const u8 {
    const pat = "\"" ++ field ++ "\":\"";
    const start = std.mem.indexOf(u8, input, pat) orelse return null;
    const after = input[start + pat.len ..];
    const end = std.mem.indexOfScalar(u8, after, '"') orelse return null;
    return after[0..end];
}

fn toolRead(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    const path = parseField(input, "path") orelse return alloc.dupe(u8, "error: no path");
    const file = std.fs.cwd().openFile(path, .{}) catch return alloc.dupe(u8, "error: open failed");
    defer file.close();
    const contents = file.readToEndAlloc(alloc, 128 * 1024) catch return alloc.dupe(u8, "error: read failed");
    defer alloc.free(contents);
    var out: std.ArrayListUnmanaged(u8) = .{};
    var iter = std.mem.splitScalar(u8, contents, '\n');
    var idx: usize = 1;
    var w = out.writer(alloc);
    while (iter.next()) |line| {
        try w.print("{d}| {s}\n", .{ idx, line });
        idx += 1;
        if (idx > 100) {
            try out.appendSlice(alloc, "...(truncated)\n");
            break;
        }
    }
    return out.toOwnedSlice(alloc);
}

fn toolWrite(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    const path = parseField(input, "path") orelse return alloc.dupe(u8, "error: no path");
    const content = parseField(input, "content") orelse "";
    var file = std.fs.cwd().createFile(path, .{ .truncate = true }) catch return alloc.dupe(u8, "error: create failed");
    defer file.close();
    file.writeAll(content) catch return alloc.dupe(u8, "error: write failed");
    return alloc.dupe(u8, "ok");
}

fn toolBash(alloc: std.mem.Allocator, input: []const u8) ![]u8 {
    const cmd = parseField(input, "cmd") orelse return alloc.dupe(u8, "error: no cmd");
    var child = std.process.Child.init(&.{ "sh", "-c", cmd }, alloc);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    try child.spawn();
    
    // Read stdout
    var stdout_buf: [64 * 1024]u8 = undefined;
    var stdout_data: std.ArrayListUnmanaged(u8) = .{};
    while (true) {
        const n = child.stdout.?.read(&stdout_buf) catch break;
        if (n == 0) break;
        try stdout_data.appendSlice(alloc, stdout_buf[0..n]);
    }
    defer stdout_data.deinit(alloc);
    
    // Read stderr
    var stderr_buf: [8 * 1024]u8 = undefined;
    var stderr_data: std.ArrayListUnmanaged(u8) = .{};
    while (true) {
        const n = child.stderr.?.read(&stderr_buf) catch break;
        if (n == 0) break;
        try stderr_data.appendSlice(alloc, stderr_buf[0..n]);
    }
    defer stderr_data.deinit(alloc);
    
    _ = child.wait() catch {};
    
    var out: std.ArrayListUnmanaged(u8) = .{};
    try out.appendSlice(alloc, stdout_data.items);
    if (stderr_data.items.len > 0) {
        try out.appendSlice(alloc, "\n[stderr] ");
        try out.appendSlice(alloc, stderr_data.items);
    }
    return out.toOwnedSlice(alloc);
}
