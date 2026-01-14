;; nanocode - minimal claude code alternative (WebAssembly Text Format)
;; wat2wasm nanocode.wat -o nanocode.wasm
;; Needs JS/browser host for I/O

(module
  ;; Import JavaScript functions for I/O
  (import "env" "print" (func $print (param i32 i32)))
  (import "env" "read_line" (func $read_line (result i32)))
  (import "env" "api_call" (func $api_call (param i32) (result i32)))
  (import "env" "run_tool" (func $run_tool (param i32 i32) (result i32)))

  ;; Memory for strings and data
  (memory (export "memory") 1)

  ;; String constants
  (data (i32.const 0) "nanocode | WebAssembly\00")
  (data (i32.const 30) "❯ \00")
  (data (i32.const 40) "Cleared\00")
  (data (i32.const 50) "/q\00")
  (data (i32.const 55) "/c\00")

  ;; Global state
  (global $msg_count (mut i32) (i32.const 0))
  (global $running (mut i32) (i32.const 1))

  ;; Print a null-terminated string
  (func $print_str (param $ptr i32)
    (local $len i32)
    (local $i i32)
    ;; Calculate string length
    (local.set $len (i32.const 0))
    (block $done
      (loop $count
        (br_if $done
          (i32.eqz (i32.load8_u (i32.add (local.get $ptr) (local.get $len)))))
        (local.set $len (i32.add (local.get $len) (i32.const 1)))
        (br $count)
      )
    )
    (call $print (local.get $ptr) (local.get $len))
  )

  ;; String comparison
  (func $str_eq (param $a i32) (param $b i32) (result i32)
    (local $i i32)
    (local.set $i (i32.const 0))
    (block $done
      (loop $cmp
        (if (i32.ne
              (i32.load8_u (i32.add (local.get $a) (local.get $i)))
              (i32.load8_u (i32.add (local.get $b) (local.get $i))))
          (then (return (i32.const 0))))
        (br_if $done
          (i32.eqz (i32.load8_u (i32.add (local.get $a) (local.get $i)))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $cmp)
      )
    )
    (i32.const 1)
  )

  ;; Tool execution
  (func $execute_tool (param $name i32) (param $input i32) (result i32)
    ;; Delegate to JavaScript host
    (call $run_tool (local.get $name) (local.get $input))
  )

  ;; Main loop
  (func $main_loop
    (local $input_ptr i32)
    
    (block $exit
      (loop $loop
        ;; Check if still running
        (br_if $exit (i32.eqz (global.get $running)))
        
        ;; Print prompt
        (call $print_str (i32.const 30))
        
        ;; Read input
        (local.set $input_ptr (call $read_line))
        
        ;; Check for /q
        (if (call $str_eq (local.get $input_ptr) (i32.const 50))
          (then
            (global.set $running (i32.const 0))
            (br $exit)))
        
        ;; Check for /c
        (if (call $str_eq (local.get $input_ptr) (i32.const 55))
          (then
            (global.set $msg_count (i32.const 0))
            (call $print_str (i32.const 40))
            (br $loop)))
        
        ;; Call API (through JS host)
        (drop (call $api_call (local.get $input_ptr)))
        
        ;; Continue loop
        (br $loop)
      )
    )
  )

  ;; Entry point
  (func (export "main")
    ;; Print header
    (call $print_str (i32.const 0))
    
    ;; Run main loop
    (call $main_loop)
  )

  ;; Export for tool calls from JS
  (func (export "handle_tool") (param $name i32) (param $input i32) (result i32)
    (call $execute_tool (local.get $name) (local.get $input))
  )
)
