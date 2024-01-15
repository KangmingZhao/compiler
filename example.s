	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global n
n:
	.word 0
	.text
	.global main
	.type main , %function
main:
	push {r4, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L5:
	bl getint
	mov r4, r0
	str r4, [fp, #-4]
	mov r0, #0
	add sp, sp, #4
	pop {r4, fp, lr }
	bx lr
