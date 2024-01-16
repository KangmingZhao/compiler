	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global ifWhile
	.type ifWhile , %function
ifWhile:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L24:
	ldr r4, =0
	str r4, [fp, #-8]
	ldr r4, =3
	str r4, [fp, #-4]
	ldr r4, [fp, #-8]
	cmp r4, #5
	moveq r4, #1
	movne r4, #0
	beq .L27
	b .L28
.L27:
	b .L30
.L28:
	b .L33
.L29:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #8
	pop {r4, r5, r6, fp, lr }
	bx lr
.L30:
	ldr r4, [fp, #-4]
	cmp r4, #2
	moveq r4, #1
	movne r4, #0
	beq .L31
	b .L32
.L31:
	ldr r4, [fp, #-4]
	add r5, r4, #2
	str r5, [fp, #-4]
	b .L30
.L32:
	ldr r4, [fp, #-4]
	add r5, r4, #25
	str r5, [fp, #-4]
	b .L29
.L33:
	ldr r4, [fp, #-8]
	cmp r4, #5
	movlt r4, #1
	movge r4, #0
	blt .L34
	b .L35
.L34:
	ldr r4, [fp, #-4]
	ldr r6, =2
	mul r5, r4, r6
	str r5, [fp, #-4]
	ldr r4, [fp, #-8]
	add r5, r4, #1
	str r5, [fp, #-8]
	b .L33
.L35:
	b .L29
	.global main
	.type main , %function
main:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L36:
	bl ifWhile
	mov r0, r0
	add sp, sp, #0
	pop {fp, lr }
	bx lr
