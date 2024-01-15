	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global main
	.type main , %function
main:
	push {r4, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L3:
	ldr r4, =1
	cmp r4, #0
	movne r4, #1
	moveq r4, #0
	bne .L4
	b .L5
.L4:
	ldr r0, =69
	bl putch
	b .L5
.L5:
	mov r0, #0
	add sp, sp, #0
	pop {r4, fp, lr }
	bx lr
