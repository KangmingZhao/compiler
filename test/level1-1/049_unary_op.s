	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L13:
	ldr r4, =10
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	cmp r4, #0
	moveq r4, #1
	movne r4, #0
	ldr r6, =-1
	eor r5, r4, r6
	cmp r5, #0
	moveq r4, #1
	movne r4, #0
	ldr r5, =0
	sub r5, r5, r4
	beq .L15
	b .L16
.L15:
	ldr r4, =-1
	str r4, [fp, #-4]
	b .L17
.L16:
	ldr r4, =0
	str r4, [fp, #-4]
	b .L17
.L17:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #4
	pop {r4, r5, r6, fp, lr }
	bx lr
