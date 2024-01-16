	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L24:
	ldr r4, =56
	str r4, [fp, #-8]
	ldr r4, =4
	str r4, [fp, #-4]
	ldr r4, [fp, #-8]
	sub r5, r4, #-4
	ldr r4, [fp, #-4]
	add r6, r5, r4
	str r6, [fp, #-8]
	ldr r4, [fp, #-8]
	cmp r4, #0
	moveq r4, #1
	movne r4, #0
	ldr r5, =-1
	eor r6, r4, r5
	cmp r6, #0
	moveq r4, #1
	movne r4, #0
	ldr r5, =0
	sub r5, r5, r4
	beq .L27
	b .L28
.L27:
	ldr r4, =-1
	str r4, [fp, #-8]
	b .L29
.L28:
	ldr r4, [fp, #-4]
	ldr r5, =0
	add r6, r5, r4
	str r6, [fp, #-8]
	b .L29
.L29:
	ldr r4, [fp, #-8]
	mov r0, r4
	bl putint
	mov r0, #0
	add sp, sp, #8
	pop {r4, r5, r6, fp, lr }
	bx lr
