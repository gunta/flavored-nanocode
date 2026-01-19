// nanocode - Apple Silicon Assembly + BLAS (Accelerate)
//
// This "BLAS flavor" demonstrates using Accelerate's optimized CBLAS SDOT
// from hand-written ARM64 assembly. It's intentionally minimal (like other
// conceptual flavors in this repo): it echoes input and computes a toy score.
//
// Build (macOS arm64):
//   as -o nanocode.o nanocode.s
//   ld -o nanocode nanocode.o -lSystem -framework Accelerate \
//      -syslibroot "$(xcrun -sdk macosx --show-sdk-path)" -e _main -arch arm64
//
// Run:
//   ./nanocode
//
// Commands:
//   /q    quit
//   /c    clear (no-op)
//   /blas run a dot-product (SDOT) demo

.global _main

//=============================================================================
// DATA
//=============================================================================
.section __DATA,__data
.p2align 4

hdr:     .asciz "\033[1mnanocode\033[0m | Apple Silicon ASM + BLAS (Accelerate)\n\n"
pmt:     .asciz "\033[1m\033[34m❯\033[0m "
clrmsg:  .asciz "\033[32m⏺ Cleared\033[0m\n"
scfmt:   .asciz "\033[36m⏺\033[0m score=%d | %s\n"
blasfmt: .asciz "\033[36m⏺\033[0m SDOT 4: dot=%d\n"

// Toy weight vector w (1x4) for score = w · x, where x depends on input length.
.p2align 4
wvec:    .float 0.10, 0.01, 1.0, -0.50

// Demo vectors (4 floats each): dot = 1*5 + 2*6 + 3*7 + 4*8 = 70
.p2align 4
a2:      .float 1.0, 2.0, 3.0, 4.0
.p2align 4
b2:      .float 5.0, 6.0, 7.0, 8.0

//=============================================================================
// BSS
//=============================================================================
.section __DATA,__bss
.p2align 4

inp:   .skip 1024   // input buffer (multiple of 16 bytes)
.p2align 4
xvec:  .skip 16     // 4 floats
.p2align 2
score: .skip 4      // 1 float

//=============================================================================
// TEXT
//=============================================================================
.section __TEXT,__text
.p2align 4

//------------------------------------------------------------------------------
// externs
//------------------------------------------------------------------------------
// int printf(const char *fmt, ...);
// ssize_t read(int fd, void *buf, size_t n);
// ssize_t write(int fd, const void *buf, size_t n);
// void cblas_sgemm(...);

//------------------------------------------------------------------------------
// strlen: x0 = ptr -> x0 = len
//------------------------------------------------------------------------------
strlen:
    mov     x1, x0
    mov     x0, #0
1:
    ldrb    w2, [x1, x0]
    cbz     w2, 2f
    add     x0, x0, #1
    b       1b
2:
    ret

//------------------------------------------------------------------------------
// puts: x0 = ptr (null-terminated), prints to stdout via write()
//------------------------------------------------------------------------------
puts:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    sub     sp, sp, #16
    str     x0, [sp, #0]         // save ptr

    bl      strlen               // x0 = len
    mov     x2, x0               // count
    ldr     x1, [sp, #0]         // buf
    mov     x0, #1               // fd=stdout
    add     sp, sp, #16
    bl      _write

    ldp     x29, x30, [sp], #16
    ret

//------------------------------------------------------------------------------
// trim_newline: replace first '\n' or '\r' with 0
// x0 = buf, x1 = bytes_read
//------------------------------------------------------------------------------
trim_newline:
    mov     x2, #0
1:
    cmp     x2, x1
    b.ge    2f
    ldrb    w3, [x0, x2]
    cmp     w3, #10              // '\n'
    b.eq    3f
    cmp     w3, #13              // '\r'
    b.eq    3f
    add     x2, x2, #1
    b       1b
3:
    strb    wzr, [x0, x2]
2:
    // always ensure termination at end (safe: buffer is 1024, read <= 1023)
    strb    wzr, [x0, x1]
    ret

//------------------------------------------------------------------------------
// score_input: compute score using BLAS SGEMM (1x1x4) and printf it.
// x0 = inp pointer
//------------------------------------------------------------------------------
score_input:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // len = strlen(inp)
    mov     x19, x0
    bl      strlen
    mov     x20, x0              // len

    // x = [len, len^2, 1, 0]
    adrp    x8, xvec@PAGE
    add     x8, x8, xvec@PAGEOFF

    scvtf   s2, x20
    mul     x21, x20, x20
    scvtf   s3, x21
    fmov    s4, #1.0
    fmov    s5, wzr

    str     s2, [x8, #0]
    str     s3, [x8, #4]
    str     s4, [x8, #8]
    str     s5, [x8, #12]

    // BLAS: score = dot(wvec, xvec)
    // float cblas_sdot(int N, const float* X, int incX, const float* Y, int incY)
    mov     w0, #4
    adrp    x1, wvec@PAGE
    add     x1, x1, wvec@PAGEOFF
    mov     w2, #1
    mov     x3, x8
    mov     w4, #1
    bl      _cblas_sdot

    // s0 = float score -> int
    fcvtzs  w1, s0

    // printf(scfmt, score_int, inp)
    // NOTE: On macOS/arm64, variadic args are passed on the stack.
    sxtw    x10, w1              // promote int to 64-bit
    sub     sp, sp, #16
    str     x10, [sp, #0]        // arg1: int
    str     x19, [sp, #8]        // arg2: const char*
    adrp    x0, scfmt@PAGE
    add     x0, x0, scfmt@PAGEOFF
    bl      _printf
    add     sp, sp, #16

    ldp     x29, x30, [sp], #16
    ret

//------------------------------------------------------------------------------
// demo_blas_dot: run SDOT demo and print int
//------------------------------------------------------------------------------
demo_blas_dot:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    // dot(a2, b2) = 70
    mov     w0, #4
    adrp    x1, a2@PAGE
    add     x1, x1, a2@PAGEOFF
    mov     w2, #1
    adrp    x3, b2@PAGE
    add     x3, x3, b2@PAGEOFF
    mov     w4, #1
    bl      _cblas_sdot
    fcvtzs  w1, s0

    // printf(blasfmt, dot_int)
    // NOTE: variadic args on stack (macOS/arm64).
    sxtw    x10, w1
    sub     sp, sp, #16
    str     x10, [sp, #0]
    adrp    x0, blasfmt@PAGE
    add     x0, x0, blasfmt@PAGEOFF
    bl      _printf
    add     sp, sp, #16

    ldp     x29, x30, [sp], #16
    ret

//------------------------------------------------------------------------------
// _main
//------------------------------------------------------------------------------
_main:
    stp     x29, x30, [sp, #-16]!
    mov     x29, sp

    adrp    x0, hdr@PAGE
    add     x0, x0, hdr@PAGEOFF
    bl      puts

.loop:
    adrp    x0, pmt@PAGE
    add     x0, x0, pmt@PAGEOFF
    bl      puts

    // read(0, inp, 1023)
    adrp    x1, inp@PAGE
    add     x1, x1, inp@PAGEOFF
    mov     x0, #0
    mov     x2, #1023
    bl      _read
    cmp     x0, #0
    b.le    .quit

    // trim newlines + null terminate
    mov     x19, x0              // bytes_read
    adrp    x0, inp@PAGE
    add     x0, x0, inp@PAGEOFF
    mov     x1, x19
    bl      trim_newline

    // if empty, continue
    adrp    x19, inp@PAGE
    add     x19, x19, inp@PAGEOFF
    ldrb    w0, [x19]
    cbz     w0, .loop

    // Commands start with '/'
    cmp     w0, #'/'
    b.ne    .score

    ldrb    w1, [x19, #1]
    cmp     w1, #'q'
    b.ne    1f
    ldrb    w2, [x19, #2]
    cbz     w2, .quit
1:
    cmp     w1, #'c'
    b.ne    2f
    ldrb    w2, [x19, #2]
    cbz     w2, .clear
2:
    // /blas
    cmp     w1, #'b'
    b.ne    .score
    ldrb    w2, [x19, #2]
    cmp     w2, #'l'
    b.ne    .score
    ldrb    w2, [x19, #3]
    cmp     w2, #'a'
    b.ne    .score
    ldrb    w2, [x19, #4]
    cmp     w2, #'s'
    b.ne    .score
    ldrb    w2, [x19, #5]
    cbnz    w2, .score
    bl      demo_blas_dot
    b       .loop

.clear:
    adrp    x0, clrmsg@PAGE
    add     x0, x0, clrmsg@PAGEOFF
    bl      puts
    b       .loop

.score:
    mov     x0, x19
    bl      score_input
    b       .loop

.quit:
    mov     w0, #0
    ldp     x29, x30, [sp], #16
    ret
