#!/bin/bash
# Check Isabelle proof

cd "$(dirname "$0")"

isabelle build -D .
