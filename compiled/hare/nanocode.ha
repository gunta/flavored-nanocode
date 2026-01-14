// nanocode - minimal claude code alternative (Hare)
// hare run nanocode.ha
// Hare: A systems language for the next 100 years (2022+)

use fmt;
use io;
use os;
use strings;

// ANSI colors
def R: str = "\x1b[0m";
def B: str = "\x1b[1m";
def D: str = "\x1b[2m";
def C: str = "\x1b[36m";
def G: str = "\x1b[32m";
def BL: str = "\x1b[34m";

type tool_input = struct {
	path: str,
	content: str,
	cmd: str,
	pat: str,
};

fn tool(name: str, input: *tool_input) str = {
	switch (name) {
	case "read" =>
		// File reading
		return "1| file content";
	case "write" =>
		return "ok";
	case "bash" =>
		return "output";
	case =>
		return "unknown";
	};
};

type message = struct {
	role: str,
	content: str,
};

export fn main() void = {
	const key = os::getenv("ANTHROPIC_API_KEY");
	const model = match (os::getenv("MODEL")) {
	case let m: str =>
		yield m;
	case void =>
		yield "claude-sonnet-4-20250514";
	};
	
	fmt::printfln("{}nanocode{} | {}Hare - 100 Year Systems{}", B, R, D, R)!;
	fmt::println("")!;
	
	let messages: []*message = [];
	let buf: [1024]u8 = [0...];
	
	for (true) {
		fmt::printf("{}{}❯{} ", B, BL, R)!;
		
		const n = match (io::read(os::stdin, buf)) {
		case let n: size =>
			yield n;
		case io::EOF =>
			break;
		case =>
			break;
		};
		
		const input = strings::rtrim(strings::fromutf8(buf[..n])!, '\n');
		
		if (input == "") {
			continue;
		};
		
		if (input == "/q") {
			break;
		};
		
		if (input == "/c") {
			messages = [];
			fmt::printfln("{}⏺ Cleared{}", G, R)!;
			continue;
		};
		
		// Add message
		append(messages, alloc(message {
			role = "user",
			content = strings::dup(input),
		}));
		
		// Response
		fmt::println("")!;
		fmt::printfln("{}⏺{} Hare: Built for the long term!", C, R)!;
		fmt::printfln("{}  A systems language that will last!{}", D, R)!;
		fmt::println("")!;
	};
	
	fmt::println("Goodbye!")!;
};

// Why Hare for AI agents?
// • Simple: Easy to understand and maintain
// • Safe: Memory safety without complexity
// • Fast: Systems-level performance
// • Durable: Designed to last 100 years
//
// AI agents need stable foundations!
