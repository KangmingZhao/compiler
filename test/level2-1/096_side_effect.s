	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word -1
.global b
b:
	.word 1
	.text
	.global inc_a
	.type inc_a , %function
inc_a:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L53:
	ldr r4, =a
	ldr r5, [r4]
	str r5, [fp, #-4]
	ldr r4, [fp, #-4]
	add r5, r4, #1
	str r5, [fp, #-4]
	ldr r4, [fp, #-4]
	ldr r5, =a
	str r4, [r5]
	ldr r4, =a
	ldr r5, [r4]
	mov r0, r5
	add sp, sp, #4
	pop {r4, r5, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L55:
	ldr r4, =5
	str r4, [fp, #-4]
	b .L57
.L57:
	ldr r4, [fp, #-4]
	cmp r4, #0
	movge r4, #1
	movlt r4, #0
	bge .L58
	b .L59
.L58:
	bl inc_a
	cmp v14, #0
	movne r4, #1
	moveq r4, #0
	bne .L63
	b .L61
.L59:
	ldr r4, =a
	ldr r5, [r4]
	mov r0, r5
	bl putint
	ldr r0, =32
	bl putch
	ldr r4, =b
	ldr r5, [r4]
	mov r0, r5
	bl putint
	ldr r0, =10
	bl putch
	ldr r4, =a
	ldr r5, [r4]
	mov r0, r5
	add sp, sp, #4
	pop {r4, r5, r6, fp, lr }
	bx lr
.L60:
	ldr r4, =a
	ldr r5, [r4]
	mov r0, r5
	bl putint
	ldr r0, =32
	bl putch
	ldr r4, =b
	ldr r5, [r4]
	mov r0, r5
	bl putint
	ldr r0, =10
	bl putch
	b .L61
.L61:
	bl inc_a
	cmp v25, #14
	movlt r4, #1
	movge r4, #0
	blt .L67
	b .L70
.L62:
	bl inc_a
	cmp v17, #0
	movne r4, #1
	moveq r4, #0
	bne .L60
	b .L61
.L63:
	bl inc_a
	cmp v15, #0
	movne r4, #1
	moveq r4, #0
	bne .L62
	b .L61
.L67:
	ldr r4, =a
	ldr r5, [r4]
	mov r0, r5
	bl putint
	ldr r0, =10
	bl putch
	ldr r4, =b
	ldr r5, [r4]
	ldr r6, =2
	mul r4, r5, r6
	ldr r5, =b
	str r4, [r5]
	b .L69
.L68:
	bl inc_a
	b .L69
.L69:
	ldr r4, [fp, #-4]
	sub r5, r4, #1
	str r5, [fp, #-4]
	b .L57
.L70:
	bl inc_a
	cmp v27, #0
	movne r4, #1
	moveq r4, #0
	bne .L71
	b .L68
.L71:
	bl inc_a
	bl inc_a
	sub r4, v28, v29
	add r5, r4, #1
	mov r0, #0
	add sp, sp, #4
	pop {r4, r5, r6, fp, lr }
	bx lr
