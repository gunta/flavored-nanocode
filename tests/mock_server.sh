#!/bin/bash
# Mock Claude API server for testing all language implementations
# Builds and runs a Zig server on port 8080

cd "$(dirname "$0")"

# Build if needed
if [ ! -f mock_server ] || [ mock_server.zig -nt mock_server ]; then
    echo "Building mock server..."
    zig build-exe mock_server.zig -OReleaseFast -fstrip 2>&1
fi

# Run
./mock_server
