	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 0
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L4:
	bl getint
	mov r4, r0
	ldr r5, =a
	str r4, [r5]
	mov r0, #0
	add sp, sp, #0
	pop {r4, r5, fp, lr }
	bx lr
