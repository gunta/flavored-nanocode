# nanocode - minimal claude code alternative (RISC-V Assembly)
# riscv64-linux-gnu-as -o nanocode.o nanocode.s
# riscv64-linux-gnu-ld -o nanocode nanocode.o

.section .data
header:     .string "\033[1mnanocode\033[0m | RISC-V Assembly\n\n"
prompt:     .string "\033[1m\033[34m❯\033[0m "
cleared:    .string "\033[32m⏺ Cleared\033[0m\n"
quit_cmd:   .string "/q"
clear_cmd:  .string "/c"
newline:    .string "\n"

.section .bss
input:      .skip 1024
messages:   .skip 65536

.section .text
.global _start

# Print null-terminated string
# a0 = string pointer
print_str:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    
    mv s0, a0           # Save string pointer
    li t0, 0            # Length counter
1:
    add t1, s0, t0
    lb t2, 0(t1)
    beqz t2, 2f
    addi t0, t0, 1
    j 1b
2:
    li a7, 64           # sys_write
    li a0, 1            # stdout
    mv a1, s0           # buffer
    mv a2, t0           # length
    ecall
    
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    ret

# Read line from stdin
# Returns length in a0
read_line:
    addi sp, sp, -16
    sd ra, 8(sp)
    
    li a7, 63           # sys_read
    li a0, 0            # stdin
    la a1, input        # buffer
    li a2, 1024         # size
    ecall
    
    # Null terminate
    addi a0, a0, -1
    la t0, input
    add t0, t0, a0
    sb zero, 0(t0)
    
    ld ra, 8(sp)
    addi sp, sp, 16
    ret

# Compare strings
# a0 = str1, a1 = str2
# Returns 1 if equal, 0 otherwise  
strcmp:
    li t0, 0
1:
    add t1, a0, t0
    add t2, a1, t0
    lb t3, 0(t1)
    lb t4, 0(t2)
    bne t3, t4, 2f
    beqz t3, 3f
    addi t0, t0, 1
    j 1b
2:
    li a0, 0
    ret
3:
    li a0, 1
    ret

_start:
    # Print header
    la a0, header
    call print_str

main_loop:
    # Print prompt
    la a0, prompt
    call print_str
    
    # Read input
    call read_line
    beqz a0, main_loop
    
    # Check for /q
    la a0, input
    la a1, quit_cmd
    call strcmp
    bnez a0, quit
    
    # Check for /c
    la a0, input
    la a1, clear_cmd
    call strcmp
    bnez a0, clear
    
    # Echo input
    la a0, input
    call print_str
    la a0, newline
    call print_str
    
    j main_loop

clear:
    la a0, cleared
    call print_str
    j main_loop

quit:
    li a7, 93           # sys_exit
    li a0, 0
    ecall
