#!/bin/bash
# Run Coq nanocode

cd "$(dirname "$0")"

coqc nanocode.v
