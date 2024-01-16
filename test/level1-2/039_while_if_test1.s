	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global whileIf
	.type whileIf , %function
whileIf:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L20:
	ldr r4, =0
	str r4, [fp, #-8]
	ldr r4, =0
	str r4, [fp, #-4]
	b .L23
.L23:
	ldr r4, [fp, #-8]
	cmp r4, #100
	movlt r4, #1
	movge r4, #0
	blt .L24
	b .L25
.L24:
	ldr r4, [fp, #-8]
	cmp r4, #5
	moveq r4, #1
	movne r4, #0
	beq .L26
	b .L27
.L25:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #8
	pop {r4, r5, r6, fp, lr }
	bx lr
.L26:
	ldr r4, =25
	str r4, [fp, #-4]
	b .L28
.L27:
	ldr r4, [fp, #-8]
	cmp r4, #10
	moveq r4, #1
	movne r4, #0
	beq .L29
	b .L30
.L28:
	ldr r4, [fp, #-8]
	add r5, r4, #1
	str r5, [fp, #-8]
	b .L23
.L29:
	ldr r4, =42
	str r4, [fp, #-4]
	b .L31
.L30:
	ldr r4, [fp, #-8]
	ldr r6, =2
	mul r5, r4, r6
	str r5, [fp, #-4]
	b .L31
.L31:
	b .L28
	.global main
	.type main , %function
main:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L32:
	bl whileIf
	mov r0, r0
	add sp, sp, #0
	pop {fp, lr }
	bx lr
