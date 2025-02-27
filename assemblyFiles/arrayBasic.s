.intel_syntax noprefix
.globl main
.section .rodata
.text
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov edi, 42 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov edi, -1 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov edi, 7 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov edi, 256 
and rsp, -16 
call exit@plt
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 0 
mov rax, 0 
add rsp, 0 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 8 
mov edi, 4 
call _malloc
mov dword ptr [rax], 0 
add eax, 4 
mov qword ptr [rbp - 8], rax 
mov rax, 0 
add rsp, 8 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 9 
mov edi, 4 
call _malloc
mov dword ptr [rax], 0 
add eax, 4 
mov qword ptr [rbp - 8], rax 
mov byte ptr [rbp - 9], 1 
mov rax, 0 
add rsp, 9 
pop rbp
ret
main:
push rbp
mov rbp, rsp 
sub rsp, 8 
mov edi, 12 
call _malloc
mov dword ptr [rax], 1 
add eax, 4 
mov qword ptr [rax], 0 
mov qword ptr [rbp - 8], rax 
mov rax, 0 
add rsp, 8 
pop rbp
ret
