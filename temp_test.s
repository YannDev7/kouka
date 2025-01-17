	.text
	.globl	main
main:
	movq %rsp, %rbp
	pushq $12
	popq %rdi
	call print_int
	movq $0, %rax
	ret
print_int:
	movq %rdi, %rsi
	movq $.Sprint_int, %rdi
	movq $0, %rax
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call printf
	movq %rbp, %rsp
	popq %rbp
	ret
print_false:
	movq %rdi, %rsi
	movq $.false, %rdi
	movq $0, %rax
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call puts
	movq %rbp, %rsp
	popq %rbp
	ret
print_true:
	movq %rdi, %rsi
	movq $.true, %rdi
	movq $0, %rax
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	call puts
	movq %rbp, %rsp
	popq %rbp
	ret
print_bool:
	movq $print_false, %r10
	cmpq $0, %rdi
	je chg_b_to_print
	movq $print_true, %r10
chg_b_to_print:
	call *%r10
	ret
	.data
.Sprint_int:
	.string "%d\n"
.false:
	.string "False"
.true:
	.string "True"
