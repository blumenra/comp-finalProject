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
	dq MAKE_LITERAL_FRACTION(L_const7, L_const9)
L_const14:
	dq MAKE_LITERAL_PAIR(L_const7, L_const9)

global_table:
L_glob17:
	 dq SOB_UNDEFINED 
L_glob18:
	 dq SOB_UNDEFINED 
L_glob19:
	 dq SOB_UNDEFINED 
L_glob20:
	 dq SOB_UNDEFINED 
L_glob21:
	 dq SOB_UNDEFINED 
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
mov [L_glob27], rax 

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
mov [L_glob28], rax 

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
mov [L_glob31], rax 

jmp L_make_string_ref 
L_string_ref: 
push rbp 
mov rbp, rsp 
mov rbx, [rbp + 8*3] 
cmp rbx , 2 
jne L_incorrect_num_of_args 
mov rax, [rbp + 8*4] 
mov rax, [rax] 
mov rbx, rax 
TYPE rbx 
cmp rbx, T_STRING 
jne L_incorrect_type 
 mov rcx, [rbp + 8*5] 
mov rcx, [rcx] 
mov rbx, rcx 
TYPE rbx 
cmp rbx, T_INTEGER 
jne L_incorrect_type 
 DATA rcx 
mov rdx, 0 
STRING_REF dl, rax, rcx 
mov rax, [malloc_pointer] 
my_malloc 8 
mov qword [rax],  rdx 
shl qword [rax], 4 
or qword [rax], T_CHAR 
leave 
ret 
L_make_string_ref: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_string_ref 
mov rax, [rax] 
mov [L_glob51], rax 

jmp L_make_make_string 
L_make_string: 
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
jne L_incorrect_type 
 mov rbx, rax 
DATA rbx 
mov rcx, [rbp + 8*5] 
mov rcx, [rcx] 
mov rdx, rcx 
TYPE rdx 
cmp rdx, T_CHAR 
jne L_incorrect_type 
 mov r10, rbx ;save the length 
mov r9, rbx                        ;use r9 as size to malloc 
mov r8 , 64                         ;space for length 
add r9, r8                         ;make space for the length
add r9, 4                          ;make space for the type 
push r10 
push r8 
push r9 
DATA rcx 
pop r9                             ;restore r9 
mov rax, [malloc_pointer] 
mov rdx, rax 
my_malloc r9 
L_make_string_loop: 
cmp rbx, 0 
je L_end_loop 
mov [rdx],rcx 
add rdx, 1 
sub rbx, 1 
jmp L_make_string_loop 
L_end_loop: 
pop r8 
pop r10 
shl qword [rax], 4 
or qword [rax], T_STRING 
shl qword [rax], 64 
or qword [rax], r10 
leave 
ret 
L_make_make_string: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_make_string 
mov rax, [rax] 
mov [L_glob36], rax 

jmp L_make_vector_ref 
L_vector_ref: 
push rbp 
mov rbp, rsp 
mov rbx, [rbp + 8*3] 
cmp rbx , 2 
jne L_incorrect_num_of_args 
mov rax, [rbp + 8*4] 
mov rax, [rax] 
mov rbx, rax 
TYPE rbx 
cmp rbx, T_VECTOR 
jne L_incorrect_type 
 mov rcx, [rbp + 8*5] 
mov rcx, [rcx] 
mov rbx, rcx 
TYPE rbx 
cmp rbx, T_INTEGER 
jne L_incorrect_type 
 DATA rcx 
VECTOR_ELEMENTS rax 
mov rax, [rax + rcx*8] 
leave 
ret 
L_make_vector_ref: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_vector_ref 
mov rax, [rax] 
mov [L_glob59], rax 

jmp L_make_vector_set 
L_vector_set: 
push rbp 
mov rbp, rsp 
mov rbx, [rbp + 8*3] 
cmp rbx , 3 
jne L_incorrect_num_of_args 
mov rax, [rbp + 8*4] 
mov rax, [rax] 
mov rbx, rax 
TYPE rbx 
cmp rbx, T_VECTOR 
jne L_incorrect_type 
 mov rcx, [rbp + 8*5] 
mov rcx, [rcx] 
mov rbx, rcx 
TYPE rbx 
cmp rbx, T_INTEGER 
jne L_incorrect_type 
 DATA rcx 
mov rdx, [rbp + 8*6] 
VECTOR_ELEMENTS rax 
shl rcx, 3 
add rax, rcx 
mov [rax], rdx 
mov rax, L_const1 
leave 
ret 
L_make_vector_set: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_vector_set 
mov rax, [rax] 
mov [L_glob60], rax 

jmp L_make_set_car 
L_set_car: 
push rbp 
mov rbp, rsp 
mov rbx, [rbp + 8*3] 
cmp rbx , 2 
jne L_incorrect_num_of_args 
mov rax, [rbp + 8*4] 
mov rcx, [rbp + 8*5] 
mov rcx, [rcx] 
mov rax, [rax] 
mov rbx, rax 
TYPE rbx 
cmp rbx, T_PAIR 
jne L_incorrect_type 
 MY_CAR rax 
mov qword [rax], rcx 
mov rax, L_const1 
leave 
ret 
L_make_set_car: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_set_car 
mov rax, [rax] 
mov [L_glob48], rax 

jmp L_make_set_cdr 
L_set_cdr: 
push rbp 
mov rbp, rsp 
mov rbx, [rbp + 8*3] 
cmp rbx , 2 
jne L_incorrect_num_of_args 
mov rax, [rbp + 8*4] 
mov rcx, [rbp + 8*5] 
mov rcx, [rcx] 
mov rax, [rax] 
mov rbx, rax 
TYPE rbx 
cmp rbx, T_PAIR 
jne L_incorrect_type 
 MY_CDR rax 
mov qword [rax], rcx 
mov rax, L_const1 
leave 
ret 
L_make_set_cdr: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_set_cdr 
mov rax, [rax] 
mov [L_glob49], rax 

jmp L_make_remainder 
L_remainder: 
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
jne L_incorrect_type 
 mov rcx, [rbp + 8*5] 
mov rcx, [rcx] 
mov rbx, rcx 
TYPE rbx 
cmp rbx, T_INTEGER 
jne L_incorrect_type 
 mov rdx, 0 
DATA rax 
DATA rcx 
cmp rax, 0 
jge L_CONT 
mov r8, rax 
sar rax, 31      ; -1 or 0 (sign of rax) 
xor r8, rax 
sub r8, rax 
mov rax, r8 
idiv rcx 
mov r8, rdx 
mov rdx, 0 
sub rdx, r8 
jmp L_CONT2 
L_CONT: 
idiv rcx 
L_CONT2: 
mov rax, [malloc_pointer] 
my_malloc 8 
mov qword [rax],  rdx 
shl qword [rax], 4 
or qword [rax], T_INTEGER 
leave 
ret 
L_make_remainder: 
mov rax, [malloc_pointer] 
my_malloc 16 
MAKE_LITERAL_CLOSURE rax, L_const2, L_remainder 
mov rax, [rax] 
mov [L_glob47], rax 

mov rax, L_const11 
mov rax, L_const14 
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

L_error: 
	print format_str, error_msg 
   jmp L_END 

L_incorrect_num_of_args: 
	print format_str, error_num_args_msg 
   jmp L_END 

L_incorrect_type: 
	print format_str, error_type_msg 
   jmp L_END 

L_END: 
