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
	je label_18
	popq %rdi
	pushq $0
label_18:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	je label_17
	popq %rdi
	pushq $0
label_17:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	je label_16
	popq %rdi
	pushq $0
label_16:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jl label_15
	popq %rdi
	pushq $0
label_15:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jl label_14
	popq %rdi
	pushq $0
label_14:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jl label_13
	popq %rdi
	pushq $0
label_13:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	ja label_12
	popq %rdi
	pushq $0
label_12:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	ja label_11
	popq %rdi
	pushq $0
label_11:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	ja label_10
	popq %rdi
	pushq $0
label_10:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jne label_9
	popq %rdi
	pushq $0
label_9:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jne label_8
	popq %rdi
	pushq $0
label_8:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jne label_7
	popq %rdi
	pushq $0
label_7:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jle label_6
	popq %rdi
	pushq $0
label_6:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jle label_5
	popq %rdi
	pushq $0
label_5:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jle label_4
	popq %rdi
	pushq $0
label_4:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jae label_3
	popq %rdi
	pushq $0
label_3:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jae label_2
	popq %rdi
	pushq $0
label_2:
	popq %rax
	pushq %rax
	popq %rdi
	call print_int
	pushq $2
	pushq $1
	popq %rbx
	popq %rax
	pushq $1
	cmpq %rbx, %rax
	jae label_1
	popq %rdi
	pushq $0
label_1:
	popq %rax
	pushq %rax
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
