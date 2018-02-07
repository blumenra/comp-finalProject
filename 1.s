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

section .rodata 
	 format_str: DB "%s", 10,0 
	 format_num: DD "%d", 10,0 
	 newline: DB 10, 0 
	 error_msg: DB "ERROR!!!", 10, 0 

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

global_table:

global main
section .text
main:

mov rax, malloc_pointer 
mov qword [rax], start_of_malloc 

push L_const2 
mov rax, [L_const9] 
push rax 
push 1
push L_const2 
mov rax, [L_const7] 
push rax 
push 1
mov rax, [malloc_pointer] 
my_malloc 8 
mov rbx, [malloc_pointer] 
my_malloc (8*1) 
mov rcx, [malloc_pointer] 
my_malloc (8*1) 
add rcx, 8*0 
mov qword [rcx], 0
mov qword [rbx], rcx 
MAKE_LITERAL_CLOSURE rax, rbx, L_lambda_code1
mov rax, [rax] 
jmp END_L_lambda_code1
L_lambda_code1: 
push rbp 
mov rbp, rsp 
mov rax, [malloc_pointer] 
my_malloc 8 
mov rbx, [malloc_pointer] 
my_malloc (8*2) 
mov rcx, [malloc_pointer] 
my_malloc (8*1) 
add rcx, 8*0 
mov qword [rcx], 0
mov qword [rbx], rcx 
add rbx, 8*1 
mov qword [rbx], 0
MAKE_LITERAL_CLOSURE rax, rbx, L_lambda_code2
mov rax, [rax] 
jmp END_L_lambda_code2
L_lambda_code2: 
push rbp 
mov rbp, rsp 
mov rax, qword [rbp +  2 * 8] 
mov rax, qword [rax + 0 * 8] 
mov rax, qword [rax + 0 * 8] 
leave 
ret 
END_L_lambda_code2: 
leave 
ret 
END_L_lambda_code1: 
mov rbx, rax 
TYPE rbx 
cmp rbx, T_CLOSURE 
jne L_error 
mov rbx, rax 
CLOSURE_ENV rbx 
push rbx 
CLOSURE_CODE rax 
call rax 
add rsp, 8 * (3 + 1) 
mov rbx, rax 
TYPE rbx 
cmp rbx, T_CLOSURE 
jne L_error 
mov rbx, rax 
CLOSURE_ENV rbx 
push rbx 
CLOSURE_CODE rax 
call rax 
add rsp, 8 * (3 + 1) 
push qword rax
call write_sob_if_not_void
add rsp, 1*8

ret


L_error: 
	 print format_str, error_msg 

