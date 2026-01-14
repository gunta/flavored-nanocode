#!/bin/bash
# Run Go Fiber web server

cd "$(dirname "$0")"

# Initialize module if needed
if [ ! -f "go.mod" ]; then
    go mod init nanocode-fiber
    go get github.com/gofiber/fiber/v2
fi

go run nanocode.go
