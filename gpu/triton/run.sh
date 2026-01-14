#!/bin/bash
# Run Triton (OpenAI) nanocode

cd "$(dirname "$0")"

# Triton requires PyTorch
pip install triton torch -q 2>/dev/null

python3 nanocode.py
