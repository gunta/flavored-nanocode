#!/bin/bash
# Run Org-mode nanocode with Emacs

cd "$(dirname "$0")"

emacs --batch -l org --eval "(org-babel-execute-buffer)" nanocode.org
