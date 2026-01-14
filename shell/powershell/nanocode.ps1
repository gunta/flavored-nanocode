#!/usr/bin/env pwsh
# nanocode - minimal claude code alternative (PowerShell)
$KEY = $env:ANTHROPIC_API_KEY
$MODEL = if ($env:MODEL) { $env:MODEL } else { "claude-sonnet-4-20250514" }
$R = "`e[0m"; $B = "`e[1m"; $D = "`e[2m"; $C = "`e[36m"; $G = "`e[32m"; $BL = "`e[34m"

function Invoke-Tool($name, $input) {
    switch ($name) {
        "read" { (Get-Content $input.path | ForEach-Object -Begin { $i = 1 } { "$i| $_"; $i++ }) -join "`n" }
        "write" { Set-Content -Path $input.path -Value $input.content; "ok" }
        "edit" { 
            $t = Get-Content $input.path -Raw
            if (-not $t.Contains($input.old)) { return "error: not found" }
            Set-Content -Path $input.path -Value $t.Replace($input.old, $input.new); "ok"
        }
        "glob" { (Get-ChildItem -Recurse -Name $input.pat | Select-Object -First 50) -join "`n" }
        "grep" { (Select-String -Path * -Pattern $input.pat -Recurse | Select-Object -First 50 | ForEach-Object { "$($_.Path):$($_.LineNumber):$($_.Line)" }) -join "`n" }
        "bash" { & sh -c $input.cmd 2>&1 }
        default { "unknown" }
    }
}

$schema = @"
[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]
"@

function Invoke-API($messages) {
    $body = @{ model = $MODEL; max_tokens = 4096; system = "Concise assistant"; messages = $messages; tools = $schema | ConvertFrom-Json } | ConvertTo-Json -Depth 10
    $headers = @{ "Content-Type" = "application/json"; "anthropic-version" = "2023-06-01"; "x-api-key" = $KEY }
    (Invoke-RestMethod -Uri "https://api.anthropic.com/v1/messages" -Method Post -Headers $headers -Body $body)
}

Write-Host "${B}nanocode${R} | ${D}${MODEL}${R}`n"
$messages = @()

while ($true) {
    Write-Host "${B}${BL}❯${R} " -NoNewline
    $input = Read-Host
    if (-not $input) { continue }
    if ($input -eq "/q") { break }
    if ($input -eq "/c") { $messages = @(); Write-Host "${G}⏺ Cleared${R}"; continue }
    
    $messages += @{ role = "user"; content = $input }
    
    while ($true) {
        $resp = Invoke-API $messages
        $content = $resp.content
        $results = @()
        
        foreach ($block in $content) {
            if ($block.type -eq "text") { Write-Host "`n${C}⏺${R} $($block.text)" }
            if ($block.type -eq "tool_use") {
                $name = $block.name
                Write-Host "`n${G}⏺ ${name}${R}"
                $result = Invoke-Tool $name $block.input
                Write-Host "  ${D}⎿ $($result.Split("`n")[0])${R}"
                $results += @{ type = "tool_result"; tool_use_id = $block.id; content = $result }
            }
        }
        
        $messages += @{ role = "assistant"; content = $content }
        if ($results.Count -eq 0) { break }
        $messages += @{ role = "user"; content = $results }
    }
    Write-Host
}
