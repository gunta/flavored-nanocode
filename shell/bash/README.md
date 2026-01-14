# Shell Nanocode

Various shell implementations.

## Bash

```bash
chmod +x nanocode.sh
./nanocode.sh
```

## Zsh

```bash
chmod +x ../zsh/nanocode.zsh
../zsh/nanocode.zsh
```

## Just curl + jq (minimal)

The most minimal version - just 37 lines:

```bash
chmod +x ../shell/nanocode.sh
../shell/nanocode.sh
```

## Fish

```bash
fish ../fish/nanocode.fish
```

## Nushell

```bash
nu ../nushell/nanocode.nu
```

## PowerShell

```bash
pwsh ../powershell/nanocode.ps1
```

## Requirements

Most shell versions require `jq` for JSON parsing:

```bash
# macOS
brew install jq

# Ubuntu/Debian
apt install jq
```
