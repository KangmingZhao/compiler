	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global gcd
	.type gcd , %function
gcd:
	push {r4, r5, r6, r7, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L26:
	mov r4, r1
	cmp r4, #0
	moveq r4, #1
	movne r4, #0
	beq .L31
	b .L32
.L31:
	mov r4, r0
	mov r0, r4
	add sp, sp, #0
	pop {r4, r5, r6, r7, fp, lr }
	bx lr
	b .L32
.L32:
	mov r4, r1
	mov r5, r0
	mov r6, r1
	sdiv r7, r5, r6
	mul r7, r7, r6
	sub r6, r5, r7
	mov r0, r4
	mov r1, r6
	bl gcd
	mov r0, r0
	add sp, sp, #0
	pop {r4, r5, r6, r7, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #12
.L33:
	bl getint
	mov r4, r0
	str r4, [fp, #-4]
	b .L35
.L35:
	ldr r4, [fp, #-4]
	cmp r4, #0
	movgt r4, #1
	movle r4, #0
	bgt .L36
	b .L37
.L36:
	bl getint
	mov r4, r0
	str r4, [fp, #-12]
	bl getint
	mov r4, r0
	str r4, [fp, #-8]
	ldr r4, [fp, #-12]
	ldr r5, [fp, #-8]
	mov r0, r4
	mov r1, r5
	bl gcd
	mov r0, r0
	bl putint
	ldr r0, =10
	bl putch
	ldr r4, [fp, #-4]
	sub r5, r4, #1
	str r5, [fp, #-4]
	b .L35
.L37:
	mov r0, #0
	add sp, sp, #12
	pop {r4, r5, fp, lr }
	bx lr
