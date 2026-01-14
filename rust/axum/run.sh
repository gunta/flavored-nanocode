#!/bin/bash
# Build and run Rust Axum web server

cd "$(dirname "$0")"

# Run with cargo (creates temp project if needed)
if [ ! -f "Cargo.toml" ]; then
    cargo init --name nanocode-axum
    echo 'axum = "0.7"' >> Cargo.toml
    echo 'tokio = { version = "1", features = ["full"] }' >> Cargo.toml
    mv src/main.rs src/main.rs.bak 2>/dev/null
    cp nanocode.rs src/main.rs
fi

cargo run
