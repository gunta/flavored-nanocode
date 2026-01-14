// nanocode - minimal claude code alternative (Zig)
// zig build-exe nanocode.zig && ./nanocode
const std = @import("std");

const R = "\x1b[0m";
const B = "\x1b[1m";
const D = "\x1b[2m";
const C = "\x1b[36m";
const G = "\x1b[32m";
const BL = "\x1b[34m";
const RD = "\x1b[31m";

const Allocator = std.mem.Allocator;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    const or_key = std.posix.getenv("OPENROUTER_API_KEY");
    const an_key = std.posix.getenv("ANTHROPIC_API_KEY");
    const key = or_key orelse an_key orelse @panic("ANTHROPIC_API_KEY or OPENROUTER_API_KEY required");
    const api = if (or_key != null) "https://openrouter.ai/api/v1/messages" else "https://api.anthropic.com/v1/messages";
    const default_model = if (or_key != null) "anthropic/claude-opus-4" else "claude-sonnet-4-20250514";
    const model = std.posix.getenv("MODEL") orelse default_model;
    const auth_header = if (or_key != null) try std.fmt.allocPrint(alloc, "Authorization: Bearer {s}", .{key}) else try std.fmt.allocPrint(alloc, "x-api-key: {s}", .{key});

    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var cwd_buf: [std.fs.max_path_bytes]u8 = undefined;
    const cwd = std.fs.cwd().realpath(".", &cwd_buf) catch ".";

    try stdout.print("{s}nanocode{s} | {s}{s} | {s}{s}\n\n", .{ B, R, D, model, cwd, R });

    var messages = std.ArrayList([]const u8).init(alloc);
    var buf: [8192]u8 = undefined;

    while (true) {
        try stdout.print("{s}{s}❯{s} ", .{ B, BL, R });

        const line = stdin.readUntilDelimiterOrEof(&buf, '\n') catch break;
        if (line == null) break;

        const input = std.mem.trim(u8, line.?, " \t\r\n");
        if (input.len == 0) continue;
        if (std.mem.eql(u8, input, "/q") or std.mem.eql(u8, input, "exit")) break;
        if (std.mem.eql(u8, input, "/c")) {
            messages.clearRetainingCapacity();
            try stdout.print("{s}⏺ Cleared{s}\n", .{ G, R });
            continue;
        }

        try messages.append(try std.fmt.allocPrint(alloc, "{{\"role\":\"user\",\"content\":{s}}}", .{try jsonStr(alloc, input)}));

        while (true) {
            const msgs_json = try std.mem.join(alloc, ",", messages.items);
            const body = try std.fmt.allocPrint(alloc,
                \\{{"model":"{s}","max_tokens":8192,"system":"Concise coding assistant. cwd: {s}","messages":[{s}],"tools":[{s},{s},{s},{s},{s},{s}]}}
            , .{ model, cwd, msgs_json, tool_read, tool_write, tool_edit, tool_glob, tool_grep, tool_bash });

            const result = try std.process.Child.run(.{
                .allocator = alloc,
                .argv = &.{ "curl", "-s", api, "-H", "Content-Type: application/json", "-H", "anthropic-version: 2023-06-01", "-H", auth_header, "-d", body },
            });

            const resp = result.stdout;
            var has_tool_use = false;
            var tool_results = std.ArrayList([]const u8).init(alloc);
            var content_parts = std.ArrayList([]const u8).init(alloc);

            // Parse content blocks
            var pos: usize = 0;
            while (std.mem.indexOfPos(u8, resp, pos, "\"type\":\"")) |type_start| {
                const t_start = type_start + 8;
                if (std.mem.indexOfPos(u8, resp, t_start, "\"")) |t_end| {
                    const block_type = resp[t_start..t_end];

                    if (std.mem.eql(u8, block_type, "text")) {
                        if (extractField(resp, t_end, "text")) |text| {
                            try stdout.print("\n{s}⏺{s} {s}", .{ C, R, unescape(alloc, text) catch text });
                            try content_parts.append(try std.fmt.allocPrint(alloc, "{{\"type\":\"text\",\"text\":{s}}}", .{try jsonStr(alloc, text)}));
                        }
                    } else if (std.mem.eql(u8, block_type, "tool_use")) {
                        has_tool_use = true;
                        const id = extractField(resp, t_end, "id") orelse "unknown";
                        const name = extractField(resp, t_end, "name") orelse "unknown";
                        const input_json = extractInput(resp, t_end) orelse "{}";

                        try stdout.print("\n{s}⏺ {s}{s}", .{ G, name, R });
                        const tool_result = runTool(alloc, name, input_json) catch |e| try std.fmt.allocPrint(alloc, "error: {}", .{e});
                        const first_line = std.mem.sliceTo(tool_result, '\n');
                        const preview = if (first_line.len > 60) first_line[0..60] else first_line;
                        try stdout.print("\n  {s}⎿ {s}{s}", .{ D, preview, R });

                        try content_parts.append(try std.fmt.allocPrint(alloc, "{{\"type\":\"tool_use\",\"id\":\"{s}\",\"name\":\"{s}\",\"input\":{s}}}", .{ id, name, input_json }));
                        try tool_results.append(try std.fmt.allocPrint(alloc, "{{\"type\":\"tool_result\",\"tool_use_id\":\"{s}\",\"content\":{s}}}", .{ id, try jsonStr(alloc, tool_result) }));
                    }
                    pos = t_end + 1;
                } else break;
            }

            if (content_parts.items.len > 0) {
                const content_json = try std.mem.join(alloc, ",", content_parts.items);
                try messages.append(try std.fmt.allocPrint(alloc, "{{\"role\":\"assistant\",\"content\":[{s}]}}", .{content_json}));
            }

            if (has_tool_use and tool_results.items.len > 0) {
                const results_json = try std.mem.join(alloc, ",", tool_results.items);
                try messages.append(try std.fmt.allocPrint(alloc, "{{\"role\":\"user\",\"content\":[{s}]}}", .{results_json}));
            } else {
                break;
            }
        }
        try stdout.print("\n\n", .{});
    }
}

const tool_read =
    \\{"name":"read","description":"Read file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer"},"limit":{"type":"integer"}},"required":["path"]}}
;
const tool_write =
    \\{"name":"write","description":"Write file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}}
;
const tool_edit =
    \\{"name":"edit","description":"Edit file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"},"all":{"type":"boolean"}},"required":["path","old","new"]}}
;
const tool_glob =
    \\{"name":"glob","description":"Find files","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}},"required":["pat"]}}
;
const tool_grep =
    \\{"name":"grep","description":"Search files","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}},"required":["pat"]}}
;
const tool_bash =
    \\{"name":"bash","description":"Run command","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}
;

fn extractField(resp: []const u8, start: usize, field: []const u8) ?[]const u8 {
    const search = std.fmt.allocPrint(std.heap.page_allocator, "\"{s}\":\"", .{field}) catch return null;
    if (std.mem.indexOfPos(u8, resp, start, search)) |f_start| {
        const v_start = f_start + search.len;
        var end = v_start;
        while (end < resp.len) : (end += 1) {
            if (resp[end] == '"' and (end == v_start or resp[end - 1] != '\\')) break;
        }
        return resp[v_start..end];
    }
    return null;
}

fn extractInput(resp: []const u8, start: usize) ?[]const u8 {
    if (std.mem.indexOfPos(u8, resp, start, "\"input\":")) |i_start| {
        const v_start = i_start + 8;
        if (v_start < resp.len and resp[v_start] == '{') {
            var depth: i32 = 1;
            var end = v_start + 1;
            while (end < resp.len and depth > 0) : (end += 1) {
                if (resp[end] == '{') depth += 1 else if (resp[end] == '}') depth -= 1;
            }
            return resp[v_start..end];
        }
    }
    return null;
}

fn runTool(alloc: Allocator, name: []const u8, input_json: []const u8) ![]const u8 {
    if (std.mem.eql(u8, name, "bash")) {
        const cmd = extractJsonField(input_json, "cmd") orelse return "error: no cmd";
        const result = try std.process.Child.run(.{ .allocator = alloc, .argv = &.{ "sh", "-c", cmd } });
        return if (result.stdout.len > 0) result.stdout else if (result.stderr.len > 0) result.stderr else "(empty)";
    } else if (std.mem.eql(u8, name, "read")) {
        const path = extractJsonField(input_json, "path") orelse return "error: no path";
        const file = std.fs.cwd().openFile(path, .{}) catch return "error: file not found";
        defer file.close();
        return file.readToEndAlloc(alloc, 1024 * 1024) catch "error: read failed";
    } else if (std.mem.eql(u8, name, "write")) {
        const path = extractJsonField(input_json, "path") orelse return "error: no path";
        const content = extractJsonField(input_json, "content") orelse return "error: no content";
        const file = std.fs.cwd().createFile(path, .{}) catch return "error: create failed";
        defer file.close();
        file.writeAll(try unescape(alloc, content)) catch return "error: write failed";
        return "ok";
    } else if (std.mem.eql(u8, name, "edit")) {
        const path = extractJsonField(input_json, "path") orelse return "error: no path";
        const old = try unescape(alloc, extractJsonField(input_json, "old") orelse return "error: no old");
        const new = try unescape(alloc, extractJsonField(input_json, "new") orelse return "error: no new");
        const file = std.fs.cwd().openFile(path, .{}) catch return "error: file not found";
        const content = file.readToEndAlloc(alloc, 1024 * 1024) catch return "error: read failed";
        file.close();
        if (std.mem.indexOf(u8, content, old) == null) return "error: not found";
        const replaced = try std.mem.replaceOwned(u8, alloc, content, old, new);
        const out = std.fs.cwd().createFile(path, .{}) catch return "error: create failed";
        defer out.close();
        out.writeAll(replaced) catch return "error: write failed";
        return "ok";
    } else if (std.mem.eql(u8, name, "glob")) {
        const pat = extractJsonField(input_json, "pat") orelse return "error: no pat";
        const result = try std.process.Child.run(.{ .allocator = alloc, .argv = &.{ "sh", "-c", try std.fmt.allocPrint(alloc, "find . -name '{s}' 2>/dev/null | head -50", .{pat}) } });
        return if (result.stdout.len > 0) result.stdout else "none";
    } else if (std.mem.eql(u8, name, "grep")) {
        const pat = extractJsonField(input_json, "pat") orelse return "error: no pat";
        const result = try std.process.Child.run(.{ .allocator = alloc, .argv = &.{ "sh", "-c", try std.fmt.allocPrint(alloc, "grep -rn '{s}' . 2>/dev/null | head -50", .{pat}) } });
        return if (result.stdout.len > 0) result.stdout else "none";
    }
    return "error: unknown tool";
}

fn extractJsonField(json: []const u8, field: []const u8) ?[]const u8 {
    const search = std.fmt.allocPrint(std.heap.page_allocator, "\"{s}\":\"", .{field}) catch return null;
    if (std.mem.indexOf(u8, json, search)) |start| {
        const v_start = start + search.len;
        var end = v_start;
        while (end < json.len) : (end += 1) {
            if (json[end] == '"' and (end == v_start or json[end - 1] != '\\')) break;
        }
        return json[v_start..end];
    }
    return null;
}

fn jsonStr(alloc: Allocator, s: []const u8) ![]const u8 {
    var result = std.ArrayList(u8).init(alloc);
    try result.append('"');
    for (s) |c| {
        switch (c) {
            '"' => try result.appendSlice("\\\""),
            '\\' => try result.appendSlice("\\\\"),
            '\n' => try result.appendSlice("\\n"),
            '\r' => try result.appendSlice("\\r"),
            '\t' => try result.appendSlice("\\t"),
            else => try result.append(c),
        }
    }
    try result.append('"');
    return result.items;
}

fn unescape(alloc: Allocator, s: []const u8) ![]const u8 {
    var result = std.ArrayList(u8).init(alloc);
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        if (s[i] == '\\' and i + 1 < s.len) {
            switch (s[i + 1]) {
                'n' => try result.append('\n'),
                'r' => try result.append('\r'),
                't' => try result.append('\t'),
                '"' => try result.append('"'),
                '\\' => try result.append('\\'),
                else => try result.append(s[i + 1]),
            }
            i += 1;
        } else try result.append(s[i]);
    }
    return result.items;
}
