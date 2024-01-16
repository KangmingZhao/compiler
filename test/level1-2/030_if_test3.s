	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global ififElse
	.type ififElse , %function
ififElse:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L14:
	ldr r4, =5
	str r4, [fp, #-8]
	ldr r4, =10
	str r4, [fp, #-4]
	ldr r4, [fp, #-8]
	cmp r4, #5
	moveq r4, #1
	movne r4, #0
	beq .L17
	b .L18
.L17:
	ldr r4, [fp, #-4]
	cmp r4, #10
	moveq r4, #1
	movne r4, #0
	beq .L19
	b .L20
.L18:
	ldr r4, [fp, #-8]
	mov r0, r4
	add sp, sp, #8
	pop {r4, r5, fp, lr }
	bx lr
.L19:
	ldr r4, =25
	str r4, [fp, #-8]
	b .L21
.L20:
	ldr r4, [fp, #-8]
	add r5, r4, #15
	str r5, [fp, #-8]
	b .L21
.L21:
	b .L18
	.global main
	.type main , %function
main:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L22:
	bl ififElse
	mov r0, r0
	add sp, sp, #0
	pop {fp, lr }
	bx lr
