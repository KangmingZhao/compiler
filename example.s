	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global ifElse
	.type ifElse , %function
ifElse:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L10:
	ldr r4, =5
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	cmp r4, #5
	moveq r4, #1
	movne r4, #0
	beq .L12
	b .L13
.L12:
	ldr r4, =25
	str r4, [fp, #-4]
	b .L14
.L13:
	ldr r4, [fp, #-4]
	ldr r6, =2
	mul r5, r4, r6
	str r5, [fp, #-4]
	b .L14
.L14:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #4
	pop {r4, r5, r6, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L15:
	bl ifElse
	mov r0, r0
	add sp, sp, #0
	pop {fp, lr }
	bx lr
