	.text
	.globl	main
main:
	movq %rsp, %rbp
	pushq $9
	popq %rdi
	call print_string
	pushq $21
	popq %rdi
	call print_string
	pushq $15
	popq %rdi
	call print_string
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
