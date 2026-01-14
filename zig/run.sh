#!/bin/bash
cd "$(dirname "$0")"

# Build with optimizations (69KB binary)
zig build-exe mock_server.zig -OReleaseFast -fstrip 2>&1

# Run the server on port 8080
# Test with: curl -X POST http://localhost:8080/v1/messages -d '{}'
./mock_server
