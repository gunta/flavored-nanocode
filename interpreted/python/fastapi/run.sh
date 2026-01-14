#!/bin/bash
# Run Python FastAPI web server

cd "$(dirname "$0")"

# Install dependencies if needed
pip install fastapi uvicorn -q 2>/dev/null

uvicorn nanocode:app --reload
