	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 10
	.text
	.global main
	.type main , %function
main:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L3:
	mov r0, #10
	add sp, sp, #0
	pop {fp, lr }
	bx lr
