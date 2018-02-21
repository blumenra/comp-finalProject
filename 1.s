%include "scheme.s"

%macro print 2 

push rdi 
push rsi 
push rax 
mov rdi,%1 
mov rsi,%2 
mov rax,0 
call printf 
pop rax 
pop rsi 
pop rdi 

%endmacro 

consts_table:
L_const0:
	dq MAKE_LITERAL(0, 0)
L_const1:
	dq MAKE_LITERAL(1, 0)
L_const2:
	dq MAKE_LITERAL(2, 0)
L_const3:
	dq MAKE_LITERAL(5, 1)
L_const5:
	dq MAKE_LITERAL(5, 0)
L_const7:
	dq MAKE_LITERAL(3, 1)
L_const9:
	dq MAKE_LITERAL(3, 2)
L_const11:
	dq MAKE_LITERAL(3, 3)
L_const13:
	dq MAKE_LITERAL_PAIR(L_const11, L_const2)
L_const16:
	dq MAKE_LITERAL_PAIR(L_const9, L_const13)
L_const19:
	dq MAKE_LITERAL_PAIR(L_const7, L_const16)

global_table:
L_glob22:
	 dq SOB_UNDEFINED 
L_glob23:
	 dq SOB_UNDEFINED 
L_glob24:
	 dq SOB_UNDEFINED 
L_glob25:
	 dq SOB_UNDEFINED 
L_glob26:
	 dq SOB_UNDEFINED 
L_glob27:
	 dq SOB_UNDEFINED 
L_glob28:
	 dq SOB_UNDEFINED 
L_glob29:
	 dq SOB_UNDEFINED 
L_glob30:
	 dq SOB_UNDEFINED 
L_glob31:
	 dq SOB_UNDEFINED 
L_glob32:
	 dq SOB_UNDEFINED 
L_glob33:
	 dq SOB_UNDEFINED 
L_glob34:
	 dq SOB_UNDEFINED 
L_glob35:
	 dq SOB_UNDEFINED 
L_glob36:
	 dq SOB_UNDEFINED 
L_glob37:
	 dq SOB_UNDEFINED 
L_glob38:
	 dq SOB_UNDEFINED 
L_glob39:
	 dq SOB_UNDEFINED 
L_glob40:
	 dq SOB_UNDEFINED 
L_glob41:
	 dq SOB_UNDEFINED 
L_glob42:
	 dq SOB_UNDEFINED 
L_glob43:
	 dq SOB_UNDEFINED 
L_glob44:
	 dq SOB_UNDEFINED 
L_glob45:
	 dq SOB_UNDEFINED 
L_glob46:
	 dq SOB_UNDEFINED 
L_glob47:
	 dq SOB_UNDEFINED 
L_glob48:
	 dq SOB_UNDEFINED 
L_glob49:
	 dq SOB_UNDEFINED 
L_glob50:
	 dq SOB_UNDEFINED 
L_glob51:
	 dq SOB_UNDEFINED 
L_glob52:
	 dq SOB_UNDEFINED 
L_glob53:
	 dq SOB_UNDEFINED 
L_glob54:
	 dq SOB_UNDEFINED 
L_glob55:
	 dq SOB_UNDEFINED 
L_glob56:
	 dq SOB_UNDEFINED 
L_glob57:
	 dq SOB_UNDEFINED 
L_glob58:
	 dq SOB_UNDEFINED 
L_glob59:
	 dq SOB_UNDEFINED 
L_glob60:
	 dq SOB_UNDEFINED 
L_glob61:
	 dq SOB_UNDEFINED 
L_glob62:
	 dq SOB_UNDEFINED 
L_glob63:
	 dq SOB_UNDEFINED 
L_glob64:
	 dq SOB_UNDEFINED 
L_glob65:
	 dq SOB_UNDEFINED 
L_glob66:
	 dq SOB_UNDEFINED 
L_glob67:
	 dq SOB_UNDEFINED 
L_glob68:
	 dq SOB_UNDEFINED 
L_glob69:
	 dq SOB_UNDEFINED 

global main
section .text
main:

mov rax, malloc_pointer 
mov qword [rax], start_of_malloc 

jmp L_make_car 
L_car: 
push rbp 
mov rbp, rsp 
mov rbx, [rbp + 8*3] 
cmp rbx , 1 
jne L_incorrect_num_of_args 
mov rbx, [rbp + 8*4] 
mov rbx, [rbx] 
mov rax, rbx 
TYPE rbx 
cmp rbx, T_PAIR 
jne L_incorrect_type 
 MY_CAR rax 
leave 
ret 
L_make_car: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_car 
mov rax, [rax] 
mov [L_glob33], rax 

jmp L_make_cdr 
L_cdr: 
push rbp 
mov rbp, rsp 
mov rbx, [rbp + 8*3] 
cmp rbx , 1 
jne L_incorrect_num_of_args 
mov rbx, [rbp + 8*4] 
mov rbx, [rbx] 
mov rax, rbx 
TYPE rbx 
cmp rbx, T_PAIR 
jne L_incorrect_type 
 MY_CDR rax 
leave 
ret 
L_make_cdr: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_cdr 
mov rax, [rax] 
mov [L_glob34], rax 

jmp L_make_cons 
L_cons: 
push rbp 
mov rbp, rsp 
mov rbx, [rbp + 8*3] 
cmp rbx , 2 
jne L_incorrect_num_of_args 
mov r8, [malloc_pointer] 
my_malloc 8 
mov rbx,[rbp + 8*4] 
mov rbx, [rbx] 
mov [r8], rbx  ; here stored car 
mov r9, [malloc_pointer] 
my_malloc 8 
mov rbx,[rbp + 8*5] 
mov rbx, [rbx] 
mov [r9], rbx ; here stored cdr 

;allocate memory for pair in heap in rax 
mov r10, [malloc_pointer] 
my_malloc 8 
MAKE_MALLOC_LITERAL_PAIR r10, r8, r9
mov rax, r10 
leave 
ret 

L_make_cons: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_cons 
mov rax, [rax] 
mov [L_glob37], rax 

jmp L_make_plus_bin 
L_plus_bin: 
push rbp 
mov rbp, rsp 
mov rbx, [rbp + 8*3] 
cmp rbx , 2 
jne L_incorrect_num_of_args 
mov rax, [rbp + 8*4] 
mov rax, [rax] 
mov rbx, rax 
TYPE rbx 
cmp rbx, T_INTEGER 
je L_make_frac4 
 cmp rbx, T_FRACTION 
jne L_incorrect_type 
 mov rbx, rax 
CAR rax 
DATA rax 
mov r8, rax 
CDR rbx 
DATA rbx 
mov r9, rbx 
jmp L_next_arg4 
 L_make_frac4: 
DATA rax 
int_to_frac rax, r8, r9 
L_next_arg4: 
;At this point the first argument is stored as fraction in r8, r9 
mov rcx, [rbp + 8*5] 
mov rcx, [rcx] 
mov rbx, rcx 
TYPE rbx 
cmp rbx, T_INTEGER 
je L_make_frac42 
 cmp rbx, T_FRACTION 
jne L_incorrect_type 
 mov rbx, rcx 
CAR rcx 
DATA rcx 
mov r10, rcx 
CDR rbx 
DATA rbx 
mov r11, rbx 
jmp L_start_plus_bin 
 L_make_frac42: 
DATA rcx 
int_to_frac rcx, r10, r11 
L_start_plus_bin: 
;At this point the first argument is stored as fraction in r8, r9 
;At this point the second argument is stored as fraction in r10, r11 
mov rax, r9 
imul r11 
mov r13, rax 
mov rax, r8 
imul r11 
mov r14, rax 
mov rax, r9 
imul r10 
mov rsi, rax 
add rsi, r14 
push r13 
push rsi 
push r13 
push rsi 
call gcd 
add rsp, 8*2 
pop rsi 
pop r13 
mov rdi, rax 
my_idiv r13, rdi 
mov r13, rax 
my_idiv rsi, rdi 
mov rsi, rax 
mov rax, [malloc_pointer] 
my_malloc 8 
mov qword [rax], rsi 
shl qword [rax], 4 
or qword [rax], T_INTEGER 
mov rsi, rax 
mov rax, [malloc_pointer] 
my_malloc 8 
mov qword [rax], r13 
shl qword [rax], 4 
or qword [rax], T_INTEGER 
mov r13, rax 
mov rax, [malloc_pointer] 
my_malloc 8 
mov r8, [r13] 
DATA r8 
cmp r8, 1 
je .L_make_integer 
mov r10, rax 
MAKE_MALLOC_LITERAL_FRACTION r10, rsi, r13 
mov rax, r10 
jmp L_end_plus_bin 
.L_make_integer: 
mov rax, rsi 
L_end_plus_bin: 
leave 
ret 
L_make_plus_bin: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_plus_bin 
mov rax, [rax] 
mov [L_glob28], rax 

mov rax, L_const19 
mov rax, [rax] 
mov [L_glob22], rax 
mov rax, L_const1 
push qword [rax]
call write_sob_if_not_void
add rsp, 1*8
cmp qword [rsp], L_const2 
jne END_of_program 
add rsp, 8 
END_of_program: 

ret


section .rodata 
	 format_str: DB "%s", 10,0 
	 format_num: DD "%d", 10,0 
	 newline: DB 10, 0 
	 error_msg: DB "ERROR!!!", 10, 0 
	 error_num_args_msg: DB "incorrect number of arguments", 10, 0 
	 error_type_msg: DB "incorrect type", 10, 0 

	 error_division_by_0_msg: DB "Error: Divided by 0", 10, 0 

L_error: 
	print format_str, error_msg 
   jmp L_END 

L_incorrect_num_of_args: 
	print format_str, error_num_args_msg 
   jmp L_END 

L_incorrect_type: 
	print format_str, error_type_msg 
   jmp L_END 

L_deivision_by_0_error: 
	print format_str, error_division_by_0_msg 
   jmp L_END 

L_END: 
