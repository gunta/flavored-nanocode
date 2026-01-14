# Rust Nanocode

## Vanilla (with rust-script)

```bash
# Using rust-script
cargo install rust-script
rust-script nanocode.rs
```

Or compile directly:

```bash
cargo init --name nanocode
cargo add ureq serde serde_json
# Copy nanocode.rs to src/main.rs
cargo run
```

## Axum (Web UI)

```bash
cargo add axum tokio serde serde_json reqwest
cargo run
# Open http://localhost:3000
```
