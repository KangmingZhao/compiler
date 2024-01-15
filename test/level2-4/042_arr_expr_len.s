	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global N
N:
	.word -1
.global arr
arr:
	.word 0
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L22:
	ldr r4, =0
	str r4, [fp, #-8]
	ldr r4, =0
	str r4, [fp, #-4]
	b .L25
.L25:
	ldr r4, [fp, #-8]
	cmp r4, #6
	movlt r4, #1
	movge r4, #0
	blt .L26
	b .L27
.L26:
	ldr r4, [fp, #-4]
	ldr r5, =arr
	ldr r6, [r5]
	add r5, r4, r6
	str r5, [fp, #-4]
	ldr r4, [fp, #-8]
	add r5, r4, #1
	str r5, [fp, #-8]
	b .L25
.L27:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #8
	pop {r4, r5, r6, fp, lr }
	bx lr
