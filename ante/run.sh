#!/bin/bash
# Run Ante nanocode

cd "$(dirname "$0")"

# Ante is a functional language focusing on lifetime inference
ante run nanocode.ante 2>/dev/null || ante nanocode.ante
