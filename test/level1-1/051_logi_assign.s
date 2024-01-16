	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 0
.global b
b:
	.word 0
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L18:
	bl getint
	mov r4, r0
	ldr r5, =a
	str r4, [r5]
	bl getint
	mov r4, r0
	ldr r5, =b
	str r4, [r5]
	ldr r4, =a
	ldr r5, [r4]
	ldr r4, =b
	ldr r6, [r4]
	cmp r5, r6
	moveq r4, #1
	movne r4, #0
	beq .L23
	b .L21
.L20:
	ldr r4, =1
	str r4, [fp, #-4]
	b .L22
.L21:
	ldr r4, =0
	str r4, [fp, #-4]
	b .L22
.L22:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #4
	pop {r4, r5, r6, fp, lr }
	bx lr
.L23:
	ldr r4, =a
	ldr r5, [r4]
	cmp r5, #3
	movne r4, #1
	moveq r4, #0
	bne .L20
	b .L21
