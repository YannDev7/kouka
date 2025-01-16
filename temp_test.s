	.text
	.globl	main
main:
	movq %rsp, %rbp
	pushq $0
	popq %rax
	notq %rax
	pushq %rax
	popq %rdi
	movq $0, %rcx
	movq %rdi, %r9
	cmpq %r9, %rcx
	je print_false
	cmpq %r9, %rcx
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
