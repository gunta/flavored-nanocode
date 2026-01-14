#!/usr/bin/env tclsh
# nanocode - minimal claude code alternative (Tcl)
package require http
package require tls
package require json
package require json::write

::http::register https 443 [list ::tls::socket -autoservername true]

set KEY $::env(ANTHROPIC_API_KEY)
set MODEL [expr {[info exists ::env(MODEL)] ? $::env(MODEL) : "claude-sonnet-4-20250514"}]
set R "\033\[0m"; set B "\033\[1m"; set D "\033\[2m"; set C "\033\[36m"; set G "\033\[32m"; set BL "\033\[34m"

proc tool {name input} {
    switch $name {
        read {
            set f [open [dict get $input path] r]
            set lines [split [read $f] \n]; close $f
            set i 1; set result {}
            foreach l $lines { lappend result "$i| $l"; incr i }
            return [join $result \n]
        }
        write { set f [open [dict get $input path] w]; puts -nonewline $f [dict get $input content]; close $f; return "ok" }
        bash { return [exec sh -c [dict get $input cmd] 2>@1] }
        glob { return [exec find . -name [dict get $input pat] | head -50] }
        grep { return [exec grep -rn [dict get $input pat] . | head -50] }
        default { return "unknown" }
    }
}

set schema {[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}},{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}}]}

proc ask {messages} {
    global KEY MODEL schema
    set body [json::write object model [json::write string $MODEL] max_tokens 4096 \
        system [json::write string "Concise assistant"] messages $messages tools $schema]
    set tok [::http::geturl "https://api.anthropic.com/v1/messages" -method POST \
        -headers [list Content-Type application/json anthropic-version 2023-06-01 x-api-key $KEY] \
        -query $body]
    set data [::http::data $tok]; ::http::cleanup $tok
    return [json::json2dict $data]
}

puts "${B}nanocode${R} | ${D}${MODEL}${R}\n"
set messages \[\]

while 1 {
    puts -nonewline "${B}${BL}❯${R} "; flush stdout
    if {[gets stdin input] < 0} break
    set input [string trim $input]
    if {$input eq ""} continue
    if {$input eq "/q"} break
    if {$input eq "/c"} { set messages \[\]; puts "${G}⏺ Cleared${R}"; continue }
    
    append messages [json::write object role [json::write string user] content [json::write string $input]] ,
    
    while 1 {
        set resp [ask "\[$messages\]"]
        set content [dict get $resp content]
        set results {}
        
        foreach block $content {
            if {[dict get $block type] eq "text"} { puts "\n${C}⏺${R} [dict get $block text]" }
            if {[dict get $block type] eq "tool_use"} {
                set name [dict get $block name]
                puts "\n${G}⏺ ${name}${R}"
                set result [tool $name [dict get $block input]]
                puts "  ${D}⎿ [lindex [split $result \n] 0]${R}"
                lappend results [json::write object type [json::write string tool_result] \
                    tool_use_id [json::write string [dict get $block id]] content [json::write string $result]]
            }
        }
        
        append messages [json::write object role [json::write string assistant] content [json::write array {*}$content]] ,
        if {[llength $results] == 0} break
        append messages [json::write object role [json::write string user] content [json::write array {*}$results]] ,
    }
    puts ""
}
