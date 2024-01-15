	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global n
n:
	.word 0
	.text
	.global gcd
	.type gcd , %function
gcd:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #16
.L49:
	mov r4, r0
	str r4, [fp, #-16]
	mov r4, r1
	str r4, [fp, #-12]
	mov r4, r0
	mov r5, r1
	cmp r4, r5
	movlt r4, #1
	movge r4, #0
	blt .L58
	b .L59
.L58:
	mov r4, r0
	str r4, [fp, #-8]
	mov r4, r1
	mov r0, r4
	ldr r4, [fp, #-8]
	mov r1, r4
	b .L59
.L59:
	mov r4, r0
	mov r5, r1
	sdiv r6, r4, r5
	mul r6, r6, r5
	sub r5, r4, r6
	str r5, [fp, #-4]
	b .L60
.L60:
	ldr r4, [fp, #-4]
	cmp r4, #0
	movne r4, #1
	moveq r4, #0
	bne .L61
	b .L62
.L61:
	mov r4, r1
	mov r0, r4
	ldr r4, [fp, #-4]
	mov r1, r4
	mov r4, r0
	mov r5, r1
	sdiv r6, r4, r5
	mul r6, r6, r5
	sub r5, r4, r6
	str r5, [fp, #-4]
	b .L60
.L62:
	ldr r4, [fp, #-16]
	ldr r5, [fp, #-12]
	mul r6, r4, r5
	mov r4, r1
	sdiv r5, r6, r4
	mov r0, r5
	add sp, sp, #16
	pop {r4, r5, r6, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L63:
	bl getint
	mov r4, r0
	str r4, [fp, #-8]
	bl getint
	mov r4, r0
	str r4, [fp, #-4]
	ldr r4, [fp, #-8]
	ldr r5, [fp, #-4]
	mov r0, r4
	mov r1, r5
	bl gcd
	mov r0, r0
	add sp, sp, #8
	pop {r4, r5, fp, lr }
	bx lr
