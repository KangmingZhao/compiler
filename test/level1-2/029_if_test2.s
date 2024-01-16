	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global ifElseIf
	.type ifElseIf , %function
ifElseIf:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L31:
	ldr r4, =5
	str r4, [fp, #-8]
	ldr r4, =10
	str r4, [fp, #-4]
	ldr r4, [fp, #-8]
	cmp r4, #6
	moveq r4, #1
	movne r4, #0
	beq .L34
	b .L37
.L34:
	ldr r4, [fp, #-8]
	mov r0, r4
	add sp, sp, #8
	pop {r4, r5, r6, fp, lr }
	bx lr
	b .L36
.L35:
	ldr r4, [fp, #-4]
	cmp r4, #10
	moveq r4, #1
	movne r4, #0
	beq .L41
	b .L39
.L36:
	ldr r4, [fp, #-8]
	mov r0, r4
	add sp, sp, #8
	pop {r4, r5, r6, fp, lr }
	bx lr
.L37:
	ldr r4, [fp, #-4]
	cmp r4, #11
	moveq r4, #1
	movne r4, #0
	beq .L34
	b .L35
.L38:
	ldr r4, =25
	str r4, [fp, #-8]
	b .L40
.L39:
	ldr r4, [fp, #-4]
	cmp r4, #10
	moveq r4, #1
	movne r4, #0
	beq .L45
	b .L43
.L40:
	b .L36
.L41:
	ldr r4, [fp, #-8]
	cmp r4, #1
	moveq r4, #1
	movne r4, #0
	beq .L38
	b .L39
.L42:
	ldr r4, [fp, #-8]
	add r5, r4, #15
	str r5, [fp, #-8]
	b .L44
.L43:
	ldr r4, [fp, #-8]
	ldr r6, =0
	sub r5, r6, r4
	str r5, [fp, #-8]
	b .L44
.L44:
	b .L40
.L45:
	ldr r4, [fp, #-8]
	cmp r4, #-5
	moveq r4, #1
	movne r4, #0
	beq .L42
	b .L43
	.global main
	.type main , %function
main:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L46:
	bl ifElseIf
	mov r0, r0
	bl putint
	mov r0, #0
	add sp, sp, #0
	pop {fp, lr }
	bx lr
