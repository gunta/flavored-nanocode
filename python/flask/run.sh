#!/bin/bash
# Run Python Flask web server

cd "$(dirname "$0")"

# Install dependencies if needed
pip install flask -q 2>/dev/null

python3 nanocode.py
