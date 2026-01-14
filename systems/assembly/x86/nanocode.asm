; nanocode - minimal claude code alternative (x86-64 Assembly)
; nasm -f elf64 nanocode.asm && ld -o nanocode nanocode.o
; Linux x86-64

section .data
    header:     db 0x1b, '[1mnanocode', 0x1b, '[0m | x86-64 Assembly', 10, 10, 0
    prompt:     db 0x1b, '[1m', 0x1b, '[34m❯', 0x1b, '[0m ', 0
    cleared:    db 0x1b, '[32m⏺ Cleared', 0x1b, '[0m', 10, 0
    quit_cmd:   db '/q', 0
    clear_cmd:  db '/c', 0
    newline:    db 10, 0

section .bss
    input:      resb 1024
    messages:   resb 65536

section .text
    global _start

; Print null-terminated string
; rdi = string pointer
print_str:
    push rdi
    xor rdx, rdx
.count:
    cmp byte [rdi + rdx], 0
    je .write
    inc rdx
    jmp .count
.write:
    pop rsi
    mov rax, 1          ; sys_write
    mov rdi, 1          ; stdout
    syscall
    ret

; Read line from stdin
; Returns length in rax
read_line:
    mov rax, 0          ; sys_read
    mov rdi, 0          ; stdin
    mov rsi, input
    mov rdx, 1024
    syscall
    ; Null terminate
    dec rax
    mov byte [input + rax], 0
    ret

; Compare strings
; rdi = str1, rsi = str2
; Returns 1 if equal, 0 otherwise
strcmp:
    xor rax, rax
.loop:
    mov cl, [rdi]
    cmp cl, [rsi]
    jne .not_equal
    test cl, cl
    jz .equal
    inc rdi
    inc rsi
    jmp .loop
.equal:
    mov rax, 1
.not_equal:
    ret

_start:
    ; Print header
    mov rdi, header
    call print_str

.main_loop:
    ; Print prompt
    mov rdi, prompt
    call print_str
    
    ; Read input
    call read_line
    test rax, rax
    jz .main_loop       ; Empty input, continue
    
    ; Check for /q
    mov rdi, input
    mov rsi, quit_cmd
    call strcmp
    test rax, rax
    jnz .quit
    
    ; Check for /c
    mov rdi, input
    mov rsi, clear_cmd
    call strcmp
    test rax, rax
    jnz .clear
    
    ; Would call API here
    ; For now, just echo back
    mov rdi, input
    call print_str
    mov rdi, newline
    call print_str
    
    jmp .main_loop

.clear:
    mov rdi, cleared
    call print_str
    jmp .main_loop

.quit:
    ; Exit
    mov rax, 60         ; sys_exit
    xor rdi, rdi        ; exit code 0
    syscall
