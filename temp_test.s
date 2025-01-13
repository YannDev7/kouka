	.text
	.globl	main
main:
	movq %rsp, %rbp
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	pushq $0
	cmpq %rbx, %rax
	je lbl_eq
	popq %rdi
	pushq $1
lbl_eq:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	pushq $0
	cmpq %rbx, %rax
	je lbl_eq
	popq %rdi
	pushq $1
lbl_eq:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	pushq $0
	cmpq %rbx, %rax
	je lbl_eq
	popq %rdi
	pushq $1
lbl_eq:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	popq %rdi
	call print_int
	movq $0, %rax
	ret
print_int:
	movq %rdi, %rsi
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	ret
print_string:
	movq %rdi, %rsi
	movq $message, %rdi
	movq $0, %rax
	call printf
	ret
	.data
.Sprint_int:
	.string "%d\n"
message:
	.string "%s\n"
