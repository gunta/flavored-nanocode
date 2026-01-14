// nanocode - minimal claude code alternative (ARM64 Assembly)
// as -o nanocode.o nanocode.s && ld -o nanocode nanocode.o
// ARM64 Linux

.global _start

.section .data
header:     .asciz "\033[1mnanocode\033[0m | ARM64 Assembly\n\n"
prompt:     .asciz "\033[1m\033[34m❯\033[0m "
cleared:    .asciz "\033[32m⏺ Cleared\033[0m\n"
quit_cmd:   .asciz "/q"
clear_cmd:  .asciz "/c"
newline:    .asciz "\n"

.section .bss
input:      .skip 1024
messages:   .skip 65536

.section .text

// Print null-terminated string
// x0 = string pointer
print_str:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    
    mov x1, x0          // Save string pointer
    mov x2, #0          // Length counter
1:
    ldrb w3, [x0, x2]
    cbz w3, 2f
    add x2, x2, #1
    b 1b
2:
    mov x0, #1          // stdout
    mov x8, #64         // sys_write
    svc #0
    
    ldp x29, x30, [sp], #16
    ret

// Read line from stdin
// Returns length in x0
read_line:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    
    mov x0, #0          // stdin
    adr x1, input
    mov x2, #1024
    mov x8, #63         // sys_read
    svc #0
    
    // Null terminate
    sub x0, x0, #1
    adr x1, input
    strb wzr, [x1, x0]
    
    ldp x29, x30, [sp], #16
    ret

// Compare strings
// x0 = str1, x1 = str2
// Returns 1 if equal, 0 otherwise
strcmp:
    mov x2, #0
1:
    ldrb w3, [x0, x2]
    ldrb w4, [x1, x2]
    cmp w3, w4
    bne 2f
    cbz w3, 3f
    add x2, x2, #1
    b 1b
2:
    mov x0, #0
    ret
3:
    mov x0, #1
    ret

_start:
    // Print header
    adr x0, header
    bl print_str

main_loop:
    // Print prompt
    adr x0, prompt
    bl print_str
    
    // Read input
    bl read_line
    cbz x0, main_loop
    
    // Check for /q
    adr x0, input
    adr x1, quit_cmd
    bl strcmp
    cbnz x0, quit
    
    // Check for /c
    adr x0, input
    adr x1, clear_cmd
    bl strcmp
    cbnz x0, clear
    
    // Echo input (would be API call)
    adr x0, input
    bl print_str
    adr x0, newline
    bl print_str
    
    b main_loop

clear:
    adr x0, cleared
    bl print_str
    b main_loop

quit:
    mov x0, #0
    mov x8, #93         // sys_exit
    svc #0
