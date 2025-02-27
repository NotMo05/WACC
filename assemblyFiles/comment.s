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
