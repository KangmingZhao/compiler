	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #20
.L44:
	ldr r4, =5
	str r4, [fp, #-20]
	ldr r4, =5
	str r4, [fp, #-16]
	ldr r4, =1
	str r4, [fp, #-12]
	ldr r4, =-2
	str r4, [fp, #-8]
	ldr r4, =2
	str r4, [fp, #-4]
	ldr r4, [fp, #-8]
	ldr r6, =1
	mul r5, r4, r6
	ldr r6, =2
	sdiv r4, r5, r6
	cmp r4, #0
	movlt r4, #1
	movge r4, #0
	blt .L50
	b .L52
.L50:
	ldr r4, [fp, #-4]
	mov r0, r4
	bl putint
	b .L51
.L51:
	ldr r4, [fp, #-8]
	ldr r6, =2
	sdiv r5, r4, r6
	mul r6, r5, r6
	sub r5, r4, r6
	add r4, r5, #67
	cmp r4, #0
	movlt r4, #1
	movge r4, #0
	blt .L54
	b .L56
.L52:
	ldr r4, [fp, #-20]
	ldr r5, [fp, #-16]
	sub r6, r4, r5
	cmp r6, #0
	movne r4, #1
	moveq r4, #0
	bne .L53
	b .L51
.L53:
	ldr r4, [fp, #-12]
	add r5, r4, #3
	ldr r6, =2
	sdiv r4, r5, r6
	mul r6, r4, r6
	sub r4, r5, r6
	cmp r4, #0
	movne r4, #1
	moveq r4, #0
	bne .L50
	b .L51
.L54:
	ldr r4, =4
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	mov r0, r4
	bl putint
	b .L55
.L55:
	mov r0, #0
	add sp, sp, #20
	pop {r4, r5, r6, fp, lr }
	bx lr
.L56:
	ldr r4, [fp, #-20]
	ldr r5, [fp, #-16]
	sub r6, r4, r5
	cmp r6, #0
	movne r4, #1
	moveq r4, #0
	bne .L57
	b .L55
.L57:
	ldr r4, [fp, #-12]
	add r5, r4, #2
	ldr r6, =2
	sdiv r4, r5, r6
	mul r6, r4, r6
	sub r4, r5, r6
	cmp r4, #0
	movne r4, #1
	moveq r4, #0
	bne .L54
	b .L55
