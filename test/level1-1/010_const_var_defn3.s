	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 10
.global b
b:
	.word 5
	.text
	.global main
	.type main , %function
main:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L3:
	mov r0, #5
	add sp, sp, #0
	pop {fp, lr }
	bx lr
