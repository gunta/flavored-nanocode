#!/bin/bash
# Run mock server tests
cd "$(dirname "$0")"

# Build server if needed
if [ ! -f mock_server ] || [ mock_server.zig -nt mock_server ]; then
    echo "Building mock server..."
    zig build-exe mock_server.zig -OReleaseFast -fstrip
fi

# Start server in background
./mock_server &
SERVER_PID=$!
sleep 0.5

# Run tests
echo "Running tests..."
zig test mock_server_test.zig 2>&1
TEST_EXIT=$?

# Cleanup
kill $SERVER_PID 2>/dev/null
wait $SERVER_PID 2>/dev/null

exit $TEST_EXIT
