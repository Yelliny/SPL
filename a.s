%include "scheme.s"


section .data
msg: db "MOR TUR", 0
format_s: db "%s", 10, 0
format_d: db "%d", 10, 0

format_p: db "%p", 10, 0

section .data
L0:
	dq SOB_UNDEFINED
L1:
	dq SOB_VOID
L2:
	dq SOB_NIL
L3:
	dq SOB_TRUE
L4:
	dq SOB_FALSE
L5:
	dq MAKE_LITERAL(T_INTEGER, 2)
L6:
	dq MAKE_LITERAL(T_INTEGER, 3)

plus: dq 0
cons: dq 0
mult: dq 0
divi: dq 0

section .text
global main
main:
# ----- gen-lambda-7 depth: 0 -----
closure_7:

gen_closure 0, 0, 24, 32, 16, plus_6
jmp plus_6_end

plus_6:
push rbp
mov rbp, rsp
pushall

mov rax, 0
mov r15, 0
mov r14, [rbp + 3*8]
mov r10, 0
mov r11, 1

.loop:
cmp r14, r15
je .endloop
mov r8, [rbp + 4*8 + r15*8]
mov rbx, r8
TYPE rbx
cmp rbx, T_INTEGER
jne .fraction
DATA_LOWER r8
mov r9, 1
jmp .after_all

.fraction:
mov r9, r8
DATA_UPPER r8
DATA_LOWER r9
jmp .after_all



.after_all:
mov rax, r11
mul r9
mov r13, rax
mov rax, r10
mul r9
mov r10, rax
mov rax, r8
mul r11
add r10, rax
mov r11, r13
inc r15
jmp .loop

.endloop:
mov rdx, 0
mov rax, r10
div r11
cmp rdx, 0
je .is_int

.is_frac:
make_lit_frac_runtime r10, r11
mov rax, r10
jmp .end

.is_int:
mov rdx, rax
make_lit_int_runtime rdx
mov rax, rdx
.end:
popall
leave
ret

plus_6_end:

mov [plus], rax
# ----- gen-lambda-5 depth: 0 -----
closure_5:

gen_closure 0, 0, 24, 32, 16, cons_4
jmp cons_4_end

cons_4:
our_cons
cons_4_end:

mov [cons], rax
# ----- gen-lambda-3 depth: 0 -----
closure_3:

gen_closure 0, 0, 24, 32, 16, mult_2
jmp mult_2_end

mult_2:
our_mult
mult_2_end:

mov [mult], rax
# ----- gen-lambda-1 depth: 0 -----
closure_1:

gen_closure 0, 0, 24, 32, 16, divi_0
jmp divi_0_end

divi_0:
our_divi
divi_0_end:

mov [divi], rax
# ----- gen-applic 2 -----
mov rax, [cons]
mov r8, rax
mov r9, r8
CLOSURE_CODE r8
CLOSURE_ENV r9
push qword 0
mov rax, [L6]
push rax
mov rax, [L5]
push rax
push 2
push r9
call r8
mov r8, [rsp + 1*8]
add rsp, 8*2
mov r9, rax
mov rax, 8
mul r8
add rsp, rax
mov rax, r9
add rsp, 8

push qword rax
call write_sob_if_not_void
add rsp, 1*8

push qword rax
call write_sob_if_not_void
add rsp, 1*8
