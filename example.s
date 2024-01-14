	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global main
	.type main , %function
main:
	push {r4, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L5:
	ldr r4, [fp, #-4]
	cmp r4, #0
	mov  r4, #1
	mov  r4, #0
	str r4, [fp, #-8]
	mov  r0, #0
	add sp, sp, #8
	pop {r4, fp, lr }
	bx lr
