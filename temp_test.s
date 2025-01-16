	.text
	.globl	main
main:
	movq %rsp, %rbp
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	je label_1
	popq %rdi
	pushq $0
label_1:
	popq %rax
	pushq %rax
	popq %rdi
	movq $0, %rcx
	cmpq %rdi, %rcx
	je print_false
	cmpq %rdi, %rcx
	jne print_true
	movq $0, %rax
	ret
print_int:
	movq %rdi, %rsi
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	ret
print_false:
	movq %rdi, %rsi
	movq $.false, %rdi
	movq $0, %rax
	call puts
	ret
print_true:
	movq %rdi, %rsi
	movq $.true, %rdi
	movq $0, %rax
	call puts
	ret
	.data
.Sprint_int:
	.string "%d\n"
.false:
	.string "False"
.true:
	.string "True"
