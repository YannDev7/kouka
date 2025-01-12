	.text
	.globl	main
main:
	movq %rsp, %rbp
	pushq $42
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
	.data
.Sprint_int:
	.string "%d\n"
