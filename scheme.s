;;; scheme.s
;;; Support for the Scheme compiler
;;; 
;;; Programmer: Mayer Goldberg, 2018

%define T_UNDEFINED 0
%define T_VOID 1
%define T_NIL 2
%define T_INTEGER 3
%define T_FRACTION 4
%define T_BOOL 5
%define T_CHAR 6
%define T_STRING 7
%define T_SYMBOL 8
%define T_CLOSURE 9
%define T_PAIR 10
%define T_VECTOR 11

%define CHAR_NUL 0
%define CHAR_TAB 9
%define CHAR_NEWLINE 10
%define CHAR_PAGE 12
%define CHAR_RETURN 13
%define CHAR_SPACE 32

%define TYPE_BITS 4
%define WORD_SIZE 64
%define NUMERATOR_BITS 30

%define MAKE_LITERAL(type, lit) ((lit << TYPE_BITS) | type)

%define MAKE_LITERAL_FRACTION(numerator, denominator) ((((numerator << NUMERATOR_BITS) | denominator) << TYPE_BITS) | T_FRACTION)

%macro make_lit_frac_runtime 2
	shl %1, NUMERATOR_BITS
	or %1, %2
	shl %1, TYPE_BITS
	or %1, T_FRACTION
%endmacro

%macro make_lit_int_runtime 1
	shl %1, TYPE_BITS
	or %1, T_INTEGER
%endmacro

%macro make_lit_char_runtime 1
	shl %1, TYPE_BITS
	or %1, T_CHAR
%endmacro

%macro make_lit_string_runtime 2
    shl %1, 34
    sub %2, start_of_data
    shl %2, TYPE_BITS 
	or %1, %2
	or %1, T_STRING
%endmacro

%macro make_lit_vector_runtime 2
    shl %1, 34
    sub %2, start_of_data
    shl %2, TYPE_BITS 
	or %1, %2
	or %1, T_VECTOR
%endmacro


%macro TYPE 1
	and %1, ((1 << TYPE_BITS) - 1) 
%endmacro


%macro DATA 1
	sar %1, TYPE_BITS
%endmacro

%macro DATA_UPPER 1
	sar %1, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
%endmacro

%macro DATA_LOWER 1
	sal %1, ((WORD_SIZE - TYPE_BITS) >> 1)
	DATA_UPPER %1
%endmacro

%define MAKE_LITERAL_PAIR(car, cdr) (((((car - start_of_data) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (cdr - start_of_data)) << TYPE_BITS) | T_PAIR)

%macro make_lit_pair_runtime 3
	pushall
	mov rax, %1
	sub %2, start_of_data
	shl %2, ((WORD_SIZE - TYPE_BITS) >> 1)
	sub %3, start_of_data
	or %2, %3
	shl %2, TYPE_BITS
	or %2, T_PAIR
	mov [rax], %2
	popall
%endmacro

%macro CAR 1
	DATA_UPPER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

%macro CDR 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro

;;; MAKE_LITERAL_CLOSURE target, env, code #lea rbx, [rax + 8 - start_of_data]
%macro MAKE_LITERAL_CLOSURE 3
	push rax
	push rbx
	mov rax, %1
	mov qword [rax], %2
	sub qword [rax], start_of_data
	shl qword [rax], ((WORD_SIZE - TYPE_BITS) >> 1)
	
	
	lea rbx, [rax + 8]
	sub rbx, start_of_data

	or qword [rax], rbx
	shl qword [rax], TYPE_BITS
	or qword [rax], T_CLOSURE
	mov qword [rax + 8], %3
	pop rbx
	pop rax
%endmacro

%macro CLOSURE_ENV 1
	DATA_UPPER %1
	add %1, start_of_data
%endmacro

%macro CLOSURE_CODE 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
%endmacro


%macro MAKE_LITERAL_STRING 1+
	dq (((((%%LstrEnd - %%Lstr) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Lstr - start_of_data)) << TYPE_BITS) | T_STRING)
	%%Lstr:
	db %1
	%%LstrEnd:
%endmacro

%macro STRING_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro STRING_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; STRING_REF dest, src, index
;;; dest cannot be RAX! (fix this!)
%macro STRING_REF 3
	push rax
	mov rax, %2
	STRING_ELEMENTS rax
	add rax, %3
	mov %1, byte [rax]
	pop rax
%endmacro

%macro MAKE_LITERAL_VECTOR 1+
	dq ((((((%%VecEnd - %%Vec) >> 3) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Vec - start_of_data)) << TYPE_BITS) | T_VECTOR)
	%%Vec:
	dq %1
	%%VecEnd:
%endmacro

%macro VECTOR_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro VECTOR_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; VECTOR_REF dest, src, index
;;; dest cannot be RAX! (fix this!)
%macro VECTOR_REF 3
	mov %1, %2
	VECTOR_ELEMENTS %1
	lea %1, [%1 + %3*8]
	mov %1, qword [%1]
	mov %1, qword [%1]
%endmacro

%define SOB_UNDEFINED MAKE_LITERAL(T_UNDEFINED, 0)
%define SOB_VOID MAKE_LITERAL(T_VOID, 0)
%define SOB_FALSE MAKE_LITERAL(T_BOOL, 0)
%define SOB_TRUE MAKE_LITERAL(T_BOOL, 1)
%define SOB_NIL MAKE_LITERAL(T_NIL, 0)



section .data
start_of_data:

frac:
	dq MAKE_LITERAL_FRACTION(2, 4)

sobNil:
	dq SOB_NIL
sobInt3:
	dq MAKE_LITERAL(T_INTEGER, 3)
sobInt2:
	dq MAKE_LITERAL(T_INTEGER, 2)
sobInt1:
	dq MAKE_LITERAL(T_INTEGER, 1)
sobPair3N:
	dq MAKE_LITERAL_PAIR(sobInt3, sobNil)
sobPair23N:
	dq MAKE_LITERAL_PAIR(sobInt2, sobPair3N)
sobPair123N:
	dq MAKE_LITERAL_PAIR(sobInt1, sobPair23N)
sobPair12:
	dq MAKE_LITERAL_PAIR(sobInt1, sobInt2)
sobPairA:
	dq MAKE_LITERAL_PAIR(sobPair12, sobNil)
sobPairB:
	dq MAKE_LITERAL_PAIR(sobPair123N, sobPairA)
sobPairC:
	dq MAKE_LITERAL_PAIR(sobInt3, sobPair12)
sobPairNN:
	dq MAKE_LITERAL_PAIR(sobNil, sobNil)
sob1:
	dq MAKE_LITERAL_PAIR(sobInt1, sobPairNN)
sob2:
	dq MAKE_LITERAL_PAIR(sobInt2, sob1)
sob3:
	dq MAKE_LITERAL_PAIR(sob2, sob2)
sob4:
	dq MAKE_LITERAL_PAIR(sobInt1, sobNil)
sob5:
	dq MAKE_LITERAL_PAIR(sob4, sobNil)
sob6:
	dq 0, 0 		; closure: wait for later!
sob7:
	MAKE_LITERAL_STRING "Mayer", CHAR_NEWLINE, "Goldberg", CHAR_TAB, "<=="
sob8:
	dq MAKE_LITERAL_PAIR(sob7, sobPairB)
sobVec1:
	MAKE_LITERAL_VECTOR sob8, sob7, sobInt1, sobInt2, sobInt3, sob4 

section .bss

extern exit, printf, scanf, malloc
global write_sob, write_sob_if_not_void
section .text

;;;main:
;;;	mov rax, [frac]
;;;	push qword rax
;;;	call write_sob_if_not_void
;;;	add rsp, 8
;;;	ret

jmp END_ERROR
ERROR:
    call exit
    
END_ERROR:

gcd:
	push rbp
	mov rbp, rsp
	push r8
	push r9
	push rdx
	mov r8, qword [rbp + 8 + 1*8]
	mov r9, qword [rbp + 8 + 2*8]

	.loop:
		mov rdx, 0
		cmp r9, 0
		je .end_loop
		mov rax, r8
		div r9
		mov r8, r9
		mov r9, rdx
		jmp .loop

	.end_loop:
		mov rax, r8
		pop rdx
		pop r9
		pop r8

	leave
	ret

write_sob_fraction:
	push rbp
	mov rbp, rsp
	push r8
	push rax
	push r10
	push rdx
	push rsi
	push rdi

	mov r8, qword [rbp + 8 + 1*8]
	
	mov rsi, r8
	DATA_UPPER rsi
	mov r10, r8
	DATA_LOWER r10

	push qword rsi
	push qword r10
	call gcd

	push r9

	mov r9, rax

	mov rdx, 0
	mov rax, rsi
	div r9
	mov rsi, rax

	mov rdx, 0
	mov rax, r10
	div r9
	mov r10, rax

	pop r9
	mov rdx, r10

	mov rdi, .frac_format_string
	mov rax, 0
	call printf

	pop rdi
	pop rsi
	pop rdx
	pop r10
	pop rax
	pop r8
	leave
	ret

section .data
.frac_format_string:
	db "%ld/%ld", 0

write_sob_undefined:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .undefined
	call printf

	leave
	ret

section .data

.undefined:
	db "#<undefined>", 0


write_sob_integer:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	sar rsi, TYPE_BITS
	mov rdi, .int_format_string
	mov rax, 0
	call printf

	leave
	ret

section .data
.int_format_string:
	db "%ld", 0

write_sob_char:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	DATA rsi

	cmp rsi, CHAR_NUL
	je .Lnul

	cmp rsi, CHAR_TAB
	je .Ltab

	cmp rsi, CHAR_NEWLINE
	je .Lnewline

	cmp rsi, CHAR_PAGE
	je .Lpage

	cmp rsi, CHAR_RETURN
	je .Lreturn

	cmp rsi, CHAR_SPACE
	je .Lspace
	jg .Lregular

	mov rdi, .special
	jmp .done	

.Lnul:
	mov rdi, .nul
	jmp .done

.Ltab:
	mov rdi, .tab
	jmp .done

.Lnewline:
	mov rdi, .newline
	jmp .done

.Lpage:
	mov rdi, .page
	jmp .done

.Lreturn:
	mov rdi, .return
	jmp .done

.Lspace:
	mov rdi, .space
	jmp .done

.Lregular:
	mov rdi, .regular
	jmp .done

.done:
	mov rax, 0
	call printf

	leave
	ret

section .data
.space:
	db "#\space", 0
.newline:
	db "#\newline", 0
.return:
	db "#\return", 0
.tab:
	db "#\tab", 0
.page:
	db "#\page", 0
.nul:
	db "#\nul", 0
.special:
	db "#\x%02x", 0
.regular:
	db "#\%c", 0

write_sob_void:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .void
	call printf

	leave
	ret

section .data
.void:
	db "#<void>", 0
	
write_sob_bool:
	push rbp
	mov rbp, rsp

	mov rax, qword [rbp + 8 + 1*8]
	cmp rax, SOB_FALSE
	je .sobFalse
	
	mov rdi, .true
	jmp .continue

.sobFalse:
	mov rdi, .false

.continue:
	mov rax, 0
	call printf	

	leave
	ret

section .data			
.false:
	db "#f", 0
.true:
	db "#t", 0

write_sob_nil:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .nil
	call printf

	leave
	ret

section .data
.nil:
	db "()", 0

write_sob_string:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .double_quote
	call printf

	mov rax, qword [rbp + 8 + 1*8]
	mov rcx, rax
	STRING_LENGTH rcx
	STRING_ELEMENTS rax

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rbx, CHAR_TAB
	je .ch_tab
	cmp rbx, CHAR_NEWLINE
	je .ch_newline
	cmp rbx, CHAR_PAGE
	je .ch_page
	cmp rbx, CHAR_RETURN
	je .ch_return
	cmp rbx, CHAR_SPACE
	jl .ch_hex
	
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx
	jmp .printf
	
.ch_tab:
	mov rdi, .fs_tab
	mov rsi, rbx
	jmp .printf
	
.ch_page:
	mov rdi, .fs_page
	mov rsi, rbx
	jmp .printf
	
.ch_return:
	mov rdi, .fs_return
	mov rsi, rbx
	jmp .printf

.ch_newline:
	mov rdi, .fs_newline
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .double_quote
	call printf

	leave
	ret
section .data
.double_quote:
	db '"', 0
.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%02x;", 0	
.fs_tab:
	db "\t", 0
.fs_page:
	db "\f", 0
.fs_return:
	db "\r", 0
.fs_newline:
	db "\n", 0

write_sob_pair:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .open_paren
	call printf
	mov rax, qword [rbp + 8 + 1*8]
	CAR rax
	push rax
	call write_sob
	add rsp, 1*8
	mov rax, qword [rbp + 8 + 1*8]
	CDR rax
	push rax
	call write_sob_pair_on_cdr
	add rsp, 1*8
	mov rdi, .close_paren
	mov rax, 0
	call printf

	leave
	ret

section .data
.open_paren:
	db "(", 0
.close_paren:
	db ")", 0

write_sob_pair_on_cdr:
	push rbp
	mov rbp, rsp

	mov rbx, qword [rbp + 8 + 1*8]
	mov rax, rbx
	TYPE rbx
	cmp rbx, T_NIL
	je .done
	cmp rbx, T_PAIR
	je .cdrIsPair
	push rax
	mov rax, 0
	mov rdi, .dot
	call printf
	call write_sob
	add rsp, 1*8
	jmp .done

.cdrIsPair:
	mov rbx, rax
	CDR rbx
	push rbx
	CAR rax
	push rax
	mov rax, 0
	mov rdi, .space
	call printf
	call write_sob
	add rsp, 1*8
	call write_sob_pair_on_cdr
	add rsp, 1*8

.done:
	leave
	ret

section .data
.space:
	db " ", 0
.dot:
	db " . ", 0

write_sob_vector:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .fs_open_vector
	call printf

	mov rax, qword [rbp + 8 + 1*8]
	mov rcx, rax
	VECTOR_LENGTH rcx
	cmp rcx, 0
	je .done
	VECTOR_ELEMENTS rax

	push rcx
	push rax
	mov rax, qword [rax]
	push qword [rax]
	call write_sob
	add rsp, 1*8
	pop rax
	pop rcx
	dec rcx
	add rax, 8

.loop:
	cmp rcx, 0
	je .done

	push rcx
	push rax
	mov rax, 0
	mov rdi, .fs_space
	call printf
	
	pop rax
	push rax
	mov rax, qword [rax]
	push qword [rax]
	call write_sob
	add rsp, 1*8
	pop rax
	pop rcx
	dec rcx
	add rax, 8
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .fs_close_vector
	call printf

	leave
	ret

section	.data
.fs_open_vector:
	db "#(", 0
.fs_close_vector:
	db ")", 0
.fs_space:
	db " ", 0

write_sob_symbol:
	push rbp
	mov rbp, rsp

	leave
	ret
	

write_sob_closure:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	mov rdx, rsi
	CLOSURE_ENV rsi
	CLOSURE_CODE rdx
	mov rdi, .closure
	mov rax, 0
	call printf

	leave
	ret
section .data
.closure:
	db "#<closure [env:%p, code:%p]>", 0

write_sob:
	mov rax, qword [rsp + 1*8]
	TYPE rax
	jmp qword [.jmp_table + rax * 8]

section .data
.jmp_table:
	dq write_sob_undefined, write_sob_void, write_sob_nil
	dq write_sob_integer, write_sob_fraction, write_sob_bool
	dq write_sob_char, write_sob_string, write_sob_symbol
	dq write_sob_closure, write_sob_pair, write_sob_vector

section .text
write_sob_if_not_void:
	mov rax, qword [rsp + 1*8]
	cmp rax, SOB_VOID
	je .continue

	push rax
	call write_sob
	add rsp, 1*8
	mov rax, 0
	mov rdi, .newline
	call printf
	
.continue:
	ret

%macro print 2
	push rax
	push rsi
	push rdi
	mov rsi, %1
	mov rdi, %2
	mov rax, 0
	call printf
	pop rdi
	pop rsi
	pop rax
%endmacro

%macro pushall 0
	push rbx
	push rcx
	push rdx
	push rsi
	push rdi
	push r8
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15
%endmacro

%macro popall 0
	pop r15
	pop r14
	pop r13
	pop r12
	pop r11
	pop r10
	pop r9
	pop r8
	pop rdi
	pop rsi
	pop rdx
	pop rcx
	pop rbx
%endmacro

%macro our_plus 0
	push rbp
	mov rbp, rsp
	pushall

	;;; r15 - counter, r14 - n
	mov rax, 0
	mov r15, 0
	mov r14, [rbp + 3*8]
	mov r10, 0
	mov r11, 1

	.loop:
	cmp r14, r15
	je .endloop
	
	;;; r8 - curr param , rbx-type

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

	;;; r13 - common denominator

	mov r13, rax
	mov rax, r10
	mul r9
	mov r10, rax
	mov rax, r8
	mul r11
	add r10, rax

	;;; r10 - final numerator before gcd
	
	mov r11, r13
	
	;;; r11 - final denominator before gcd

	;;; reduce r10/r11 with gcd
	push r10
	push r11
	call gcd
	add rsp, 2*8
	
	;;; rbx - gcd of r10 and r11

	mov rbx, rax

	mov rdx, 0
	;;; r10 - divide numerator by gcd
	mov rax, r10
	div rbx
	mov r10, rax

	;;; r11 - divide denominator by gcd
	mov rax, r11
	div rbx
	mov r11, rax

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

%endmacro

%macro our_minus 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r15 - counter, r14 - n
	mov rax, 0
	mov r15, 0
	mov r14, [rbp + 3*8]

	;;; r10,r11 - first parameter

	mov r10, [rbp + 4*8]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	jne .fraction_1
	DATA_LOWER r10
	mov r11, 1
	jmp .after_1

	.fraction_1:
	mov r11, r10
	DATA_UPPER r10
	DATA_LOWER r11
	jmp .after_1

	.after_1:

	inc r15

	;;; handle special case when n=1: return -1 * param
	cmp r14, 1
	jne .cont

	neg r10
	jmp .endloop

	.cont:

	.loop:
	cmp r14, r15
	je .endloop
	
	;;; r8 - curr param , rbx - type

	mov r8, [rbp + 4*8 + r15*8]
	mov rbx, r8
	TYPE rbx
	cmp rbx, T_INTEGER
	jne .fraction_2
	DATA_LOWER r8
	mov r9, 1
	jmp .after_2

	.fraction_2:
	mov r9, r8
	DATA_UPPER r8
	DATA_LOWER r9
	jmp .after_2


	.after_2:

	mov rax, r11
	mul r9

	;;; r13 - common denominator

	mov r13, rax
	mov rax, r10
	mul r9
	mov r10, rax
	mov rax, r8
	mul r11
	sub r10, rax

	;;; r10 - final numerator before gcd
	
	mov r11, r13
	
	;;; r11 - final denominator before gcd

	;;; reduce r10/r11 with gcd
	push r10
	push r11
	call gcd
	add rsp, 2*8
	
	;;; rbx - gcd of r10 and r11

	mov rbx, rax

	mov rdx, 0
	;;; r10 - numerator divided by gcd
	mov rax, r10
	div rbx
	mov r10, rax

	;;; r11 - denominator divided by gcd
	mov rax, r11
	div rbx
	mov r11, rax

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

%endmacro

%macro our_mult 0
    push rbp 
    mov rbp, rsp
    pushall
    
    
    mov rax, 0 ; r15 - counter, r14 - n
    mov r15, 0
    mov r14, [rbp + 3*8]
    mov r10, 1
    mov r11, 1 
    

    .loop: ; loop
    cmp r14, r15
    je .endloop
    
    mov r8, [rbp + 4*8 + r15*8] ; r8 - curr param, rbx - type
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
    mov rax, r8 ; r10 <- r10 * r8
    mul r10
    mov r10, rax
    mov rax, r9 ; r11 <- r11 * r9
    mul r11
    mov r11, rax
    

    push r10 ; gcd r10/r11 , r12 - gcd result
    push r11
    call gcd
    add rsp, 2*8
    mov rdx, 0
    mov r12, rax
    mov rax, r10
    div r12
    mov r10, rax
    mov rax, r11
    mov rdx, 0
    div r12
    mov r11, rax
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
%endmacro

%macro our_divi 0
    push rbp 
    mov rbp, rsp
    pushall
    
    
    mov rax, 0 ; r15 - counter, r14 - n
    mov r14, [rbp + 3*8]
    
    ; if n == 1 , special case of one param
    cmp r14, 1
    je .one_param
    
    ; r10 - curr param, rbx - type
    mov r10, [rbp + 4*8 + r15*8]
    mov rbx, r10
    TYPE rbx
    cmp rbx, T_INTEGER
    jne .fraction_0
    DATA_LOWER r10
    mov r11, 1
    mov r15, 1
    jmp .loop
    
    .fraction_0:
    mov r11, r10
    DATA_UPPER r10
    DATA_LOWER r11
    mov r15, 1
    jmp .loop
    
    .one_param:
    mov r10, 1
    mov r11, 1
    

    .loop: ; loop
    cmp r14, r15
    je .endloop
    
    mov r8, [rbp + 4*8 + r15*8] ; r8 - curr param, rbx - type
    mov rbx, r8
    TYPE rbx
    cmp rbx, T_INTEGER
    jne .fraction
    mov r9, r8
    DATA_LOWER r9
    mov r8, 1
    jmp .after_all
    
    .fraction:
    mov r9, r8
    DATA_UPPER r9
    DATA_LOWER r8
    jmp .after_all
    
    .after_all:
    mov rax, r8 ; r10 <- r10 * r8
    mul r10
    mov r10, rax
    mov rax, r9 ; r11 <- r11 * r9
    mul r11
    mov r11, rax
    

    push r10 ; gcd r10/r11 , r12 - gcd result
    push r11
    call gcd
    add rsp, 2*8
    mov rdx, 0
    mov r12, rax
    mov rax, r10
    div r12
    mov r10, rax
    mov rax, r11
    mov rdx, 0
    div r12
    mov r11, rax
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
%endmacro

%macro gen_closure 6
    pushall

    ; r12 = pointer to closure
    mov rdi, 16
    call malloc
    mov r12, rax

    ; r11 = address of new environment
    mov rdi, %1 ;  %1 = env_size (depth * 8)
    call malloc
    mov r11, rax

    ; if depth == 0 build a closure with an empty env
    mov r8, %2 ; %2 = depth

    cmp r8, 0
    je .make_closure
    ; r8 = n (number of parameters)
    mov r8, [rbp + %3] ; %3 = n_offset (8 * 3)

    ; r9 = size of extended environment (bytes)
    mov rax, r8
    mov r13, 8
    mul r13
    mov r9, rax

    ; r10 = address of extended environment 
    mov rdi, r9
    call malloc
    mov r10, rax

    ; build the extend env
    cmp r8, 0
    je .ext_env_done

    ; r14 - counter. 
    mov r14, 0

    .ext_env:

    ; r15 = current parameter
    mov r15, [rbp + %4 + 8*r14] ; %4 = params_offset (8 * 4)

    ; insert curr param to extended env
    mov [r10 + r14*8], r15 

    ; check if reached end of params
    inc r14
    cmp r8, r14
    jne .ext_env
    
    .ext_env_done:

    ;put the extend env in the first cell of the new env
    mov [r11], r10

    ; copy the prev env to the new one
    mov r14, 0
    mov r15, 1

    .cpy_prev_env:
    
    inc r14
    cmp r14, %2 ; %2 = depth
    je .cpy_prev_env_done
    dec r14 
    
    ; put the next element of prev env in r8
    mov r8, [rbp + %5] ; %5 = prev_env_offset (8 * 2)
    mov r8, [r8 + 8*r14]


    ; put the next element of prev env in the next cell of the new env
    mov [r11 + r15*8], r8

    inc r14
    inc r15


    jmp .cpy_prev_env

    .cpy_prev_env_done:

    .make_closure:

    MAKE_LITERAL_CLOSURE r12, r11, %6 ; %6 = code-label
    mov rax, [r12]

    popall
%endmacro

%macro our_cons 0
    push rbp
    mov rbp, rsp
    pushall

    ; r8 - first argument, r9 - second argument
    mov r8, [rbp + 4*8]
    mov r9, [rbp + 5*8]

    ; r12 - address of new pair
    mov rdi, 8
    call malloc
    mov r12, rax

    ; r10 - address of first arg
    mov rdi, 8
    call malloc
    mov r10, rax
    mov [r10], r8

    ; r11 - address of second arg
    mov rdi, 8
    call malloc
    mov r11, rax
    mov [r11], r9

    make_lit_pair_runtime r12, r10, r11

    mov rax, [r12]

    popall
    leave
    ret
%endmacro

%macro our_lower 0
    push rbp
    mov rbp, rsp
    pushall
    
    ;;; r15 - counter, r14 - n
	mov rax, 0
	mov r15, 1
	mov r14, [rbp + 3*8]
	
	
	mov r10, [rbp + 4*8]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	jne .fraction_0
	DATA_LOWER r10
	mov r11, 1
	jmp .loop
	
	.fraction_0:
	mov r11, r10
	DATA_UPPER r10
	DATA_LOWER r11

	.loop:
	cmp r14, r15
	je .endloop
	
	;;; r8 - curr param , rbx-type

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

	; r13 - common denominator
    ; chech if r10/r11 >= r8/r9 , if true- jump to ret_false
	mov r13, rax
	mov rax, r10
	mul r9
	mov r10, rax
	mov rax, r8
	mul r11
    mov r8, rax
    cmp r10, r8
    jge .ret_false
    
    mov r10, r8
    mov r11, r9


	;;; reduce r10/r11 with gcd
	push r10
	push r11
	call gcd
	add rsp, 2*8
	
	;;; rbx - gcd of r10 and r11

	mov rbx, rax

	mov rdx, 0
	;;; r10 - divide numerator by gcd
	mov rax, r10
	div rbx
	mov r10, rax

	;;; r11 - divide denominator by gcd
	mov rax, r11
	div rbx
	mov r11, rax

	inc r15
	jmp .loop
	.endloop:
    
    mov rax, [L3]
    jmp .end
            
	.ret_false:
	mov rax, [L4]
	
	.end:
	popall
	leave
	ret
%endmacro

%macro our_greater 0
    push rbp
    mov rbp, rsp
    pushall
    
    ;;; r15 - counter, r14 - n
	mov rax, 0
	mov r15, 1
	mov r14, [rbp + 3*8]
	
	
	mov r10, [rbp + 4*8]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	jne .fraction_0
	DATA_LOWER r10
	mov r11, 1
	jmp .loop
	
	.fraction_0:
	mov r11, r10
	DATA_UPPER r10
	DATA_LOWER r11

	.loop:
	cmp r14, r15
	je .endloop
	
	;;; r8 - curr param , rbx-type

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

	; r13 - common denominator
    ; chech if r10/r11 >= r8/r9 , if true- jump to ret_false
	mov r13, rax
	mov rax, r10
	mul r9
	mov r10, rax
	mov rax, r8
	mul r11
    mov r8, rax
    cmp r10, r8
    jle .ret_false
    
    mov r10, r8
    mov r11, r9


	;;; reduce r10/r11 with gcd
	push r10
	push r11
	call gcd
	add rsp, 2*8
	
	;;; rbx - gcd of r10 and r11

	mov rbx, rax

	mov rdx, 0
	;;; r10 - divide numerator by gcd
	mov rax, r10
	div rbx
	mov r10, rax

	;;; r11 - divide denominator by gcd
	mov rax, r11
	div rbx
	mov r11, rax

	inc r15
	jmp .loop
	.endloop:
    
    mov rax, [L3]
    jmp .end
            
	.ret_false:
	mov rax, [L4]
	
	.end:
	popall
	leave
	ret
%endmacro

%macro our_equali 0
    push rbp
    mov rbp, rsp
    pushall
    
    ;;; r15 - counter, r14 - n
	mov rax, 0
	mov r15, 1
	mov r14, [rbp + 3*8]
	
	
	mov r10, [rbp + 4*8]
	mov rbx, r10
	TYPE rbx
	cmp rbx, T_INTEGER
	jne .fraction_0
	DATA_LOWER r10
	mov r11, 1
	jmp .loop
	
	.fraction_0:
	mov r11, r10
	DATA_UPPER r10
	DATA_LOWER r11

	.loop:
	cmp r14, r15
	je .endloop
	
	;;; r8 - curr param , rbx-type

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

	; r13 - common denominator
    ; chech if r10/r11 >= r8/r9 , if true- jump to ret_false
	mov r13, rax
	mov rax, r10
	mul r9
	mov r10, rax
	mov rax, r8
	mul r11
    mov r8, rax
    cmp r10, r8
    jne .ret_false
    
    mov r10, r8
    mov r11, r9


	;;; reduce r10/r11 with gcd
	push r10
	push r11
	call gcd
	add rsp, 2*8
	
	;;; rbx - gcd of r10 and r11

	mov rbx, rax

	mov rdx, 0
	;;; r10 - divide numerator by gcd
	mov rax, r10
	div rbx
	mov r10, rax

	;;; r11 - divide denominator by gcd
	mov rax, r11
	div rbx
	mov r11, rax

	inc r15
	jmp .loop
	.endloop:
    
    mov rax, [L3]
    jmp .end
            
	.ret_false:
	mov rax, [L4]
	
	.end:
	popall
	leave
	ret
%endmacro

%macro our_car 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r8 - the pair (parameter)
	mov r8, [rbp + 4*8]
	CAR r8
	mov rax, r8

	popall
	leave
	ret

%endmacro

%macro our_cdr 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r8 - the pair (parameter)
	mov r8, [rbp + 4*8]
	CDR r8
	mov rax, r8

	popall
	leave
	ret
%endmacro


%macro our_number? 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r8 - param
	mov r8, [rbp + 4*8]
	TYPE r8
	cmp r8, T_INTEGER
	je .ret_true
	cmp r8, T_FRACTION
	je .ret_true
	
	
    mov rax, [L4]
    jmp .end
    
    .ret_true:
    mov rax, [L3]
	
    .end:
	popall
	leave
	ret
	
%endmacro

%macro our_not 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r8 - param
	mov r8, [rbp + 4*8]

	; r9 - false SOB
	mov r9, [L4]

	sub r8, r9
	cmp r8, 0

	je .ret_true
	mov rax, [L4]
	jmp .end

	.ret_true:
	mov rax, [L3]

	.end:
	popall
	leave
	ret

%endmacro


%macro our_pred? 1

	push rbp
	mov rbp, rsp
	pushall

	;;; r8 - param
	mov r8, [rbp + 4*8]
	TYPE r8
	cmp r8, %1
	jne .ret_false
	
	
    mov rax, [L3]
    jmp .end
    
    .ret_false:
    mov rax, [L4]
	
    .end:
	popall
	leave
	ret
	
%endmacro

%macro our_char_to_integer 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r9 - param , r8 - type
	mov r8, [rbp + 4*8]
	mov r9, r8
	TYPE r8
	cmp r8, T_CHAR
	jne ERROR

	DATA r9
	make_lit_int_runtime r9
    mov rax, r9

	
	popall
	leave
	ret
	
%endmacro

%macro our_denominator 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r9 - param , r8 - type
	mov r8, [rbp + 4*8]
	mov r9, r8
	TYPE r8
	cmp r8, T_INTEGER
	je .integer
    
    cmp r8, T_FRACTION
    je .fraction
    
    jmp ERROR
	.integer:
	mov r9, 1
	make_lit_int_runtime r9
	mov rax, r9
	jmp .end

	.fraction:
	DATA_LOWER r9
	make_lit_int_runtime r9
	mov rax, r9
	
	.end:
	popall
	leave
	ret
	
%endmacro

%macro our_numerator 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r9 - param , r8 - type
	mov r8, [rbp + 4*8]
	mov r9, r8
	TYPE r8
	cmp r8, T_INTEGER
	je .integer
    
    cmp r8, T_FRACTION
    je .fraction
    
    jmp ERROR
	.integer:
	DATA r9
	make_lit_int_runtime r9
	mov rax, r9
	jmp .end

	.fraction:
	DATA_UPPER r9
	make_lit_int_runtime r9
	mov rax, r9
	
	.end:
	popall
	leave
	ret
	
%endmacro

%macro our_integer_to_char 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r9 - param , r8 - type
	mov r8, [rbp + 4*8]
	mov r9, r8
	TYPE r8
	cmp r8, T_INTEGER
	jne ERROR

	DATA r9
	make_lit_char_runtime r9
    mov rax, r9

	
	popall
	leave
	ret
	
%endmacro

%macro our_make_string 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r8 - number , r9 - char, r10 - type, r11- nparams
	mov r11, [rbp + 3*8]
	cmp r11, 2
	jg ERROR
	
	cmp r11, 1
	jl ERROR
	
	
	mov r8, [rbp + 4*8]
	mov r10, r8
	TYPE r10
	cmp r10, T_INTEGER
	jne ERROR
	
	;; check if length < 0
	DATA r8
	cmp r8, 0
	jl ERROR
	
	cmp r11, 1
	je .one_param
	
    ;; two params - r9 holds the char
	mov r9, [rbp + 5*8]
	mov r10, r9
	TYPE r10
	cmp r10, T_CHAR
	jne ERROR
	DATA r9
	jmp .malloc
	
	;; one param - r9 holds 0
	.one_param:
    mov r9, 0
	
	.malloc:
	; malloc size of string length
    mov rdi, r8
	call malloc
	
	;; r11 - pointer to malloc , r12 - counter
	mov r11, rax
	mov r12, 0
	
	;; fill string with chars
	.loop:
	cmp r12, r8
	je .endloop
	mov [r11 + r12], r9
	inc r12
	jmp .loop
	
	.endloop:
	make_lit_string_runtime r8, r11
    mov rax, r8

	
	popall
	leave
	ret
	
%endmacro


%macro our_length 1

	push rbp
	mov rbp, rsp
	pushall

	mov r10, [rbp + 3*8]
	cmp r10, 1
	jne ERROR
	
	mov r8, [rbp + 4*8]
	mov r10, r8
	TYPE r10
	cmp r10, %1
	jne ERROR
	
	;  - length
	DATA_UPPER r8
	make_lit_int_runtime r8
    mov rax, r8

	
	popall
	leave
	ret
	
%endmacro

%macro our_string_ref 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r8 - string , r9 - index, r10 - type
	mov r10, [rbp + 3*8]
	cmp r10, 2
	jne ERROR
	
	mov r8, [rbp + 4*8]
	mov r10, r8
	TYPE r10
	cmp r10, T_STRING
	jne ERROR
	
	mov r9, [rbp + 5*8]
	mov r10, r9
	TYPE r10
	cmp r10, T_INTEGER
	jne ERROR
	DATA r9
	
	;; check index < n
	mov r10, r8
	DATA_UPPER r10
	cmp r10, r9
	jle ERROR
	
	
	;; get the char at index r9
	DATA_LOWER r8
	add r8, start_of_data
	mov r10, [r8 + r9]
	make_lit_char_runtime r10
    mov rax, r10

	
	popall
	leave
	ret
	
%endmacro     

%macro our_vector_ref 0

	push rbp
	mov rbp, rsp
	pushall

	;;; r8 - vector , r9 - index, r10 - type
	mov r10, [rbp + 3*8]
	cmp r10, 2
	jne ERROR
	
	mov r8, [rbp + 4*8]
	mov r10, r8
	TYPE r10
	cmp r10, T_VECTOR
	jne ERROR
	
	mov r9, [rbp + 5*8]
	mov r10, r9
	TYPE r10
	cmp r10, T_INTEGER
	jne ERROR
	DATA r9
	
	;; check index < n
	mov r10, r8
	DATA_UPPER r10
	cmp r10, r9
	jle ERROR
	
	
	;; get the char at index r9
	DATA_LOWER r8
	add r8, start_of_data
	mov r10, [r8 + r9*8]
    mov rax, [r10]

	
	popall
	leave
	ret
	
%endmacro  

%macro our_make_vector 0
  	push rbp
	mov rbp, rsp
	pushall
	mov r11, [rbp + 3*8]
	cmp r11, 2
	jg ERROR
	
	cmp r11, 1
	jl ERROR
	
	mov r8, [rbp + 4*8]
	mov r10, r8
	TYPE r10
	cmp r10, T_INTEGER
	jne ERROR
	
	;; check if length < 0
	DATA r8
	cmp r8, 0
	jl ERROR
	
	cmp r11, 1
	je .one_param
	
	;; two params - r9 holds param
	mov r9, [rbp + 5*8]
	jmp .malloc
	
	;; one param, r9  = (0, T_INTEGER)
	.one_param:
	mov r9, 0
	make_lit_int_runtime r9
	
	;; malloc address to param
	.malloc:
	mov rdi, 8
	call malloc
	mov r13, rax
	mov [r13], r9

    ; malloc size of vector length
	mov rax, 8
	mul r8
	mov rdi, rax
	;;add rdi, 8
	call malloc
	
	;; r11 - pointer to malloc , r12 - counter
	mov r11, rax
	mov r12, 0
	
	;; fill vector with addresses
	.loop:
	cmp r12, r8
	je .endloop
	mov [r11 + r12*8], r13
	inc r12
	jmp .loop
	
	.endloop:
	make_lit_vector_runtime r8, r11
    mov rax, r8

	
	popall
	leave
	ret
	
%endmacro
	
%macro our_set_car 0
  	push rbp
	mov rbp, rsp
	pushall
	;; r8 - pointer to pair
	mov r8, [rbp + 4*8]

	;; r9 - future car
	mov r9, [rbp + 5*8]

	mov rdi, 8
	call malloc
	mov r10, rax

	; r10 - pointer to future car
	mov [r10], r9

	sub r10, start_of_data
	shl r10, 34

	; r11 - value of the pair
	mov r11, [r8]
	; r11 - reset 30 left bits
	shl r11, 30
	shr r11, 30

	or r11, r10
	mov [r8], r11

	mov rax, [L1]

	popall
	leave
	ret

%endmacro

%macro our_remainder 0

	push rbp
	mov rbp, rsp
	pushall

	mov r8, [rbp + 4*8]
	mov r9, [rbp + 5*8]

	DATA r8
	DATA r9

	cmp r9, 0
	jge .sec_arg_positive
	neg r9

	.sec_arg_positive:

	mov rdx, 0
	mov rax, r8

	cmp r8, 0
	jge .positive
	neg rax

	.positive:

	div r9
	mov rax, rdx

	cmp r8, 0
	jge .end

	neg rax

	.end:
	make_lit_int_runtime rax

	popall
	leave
	ret
	
%endmacro

section .data
.newline:
	db CHAR_NEWLINE, 0