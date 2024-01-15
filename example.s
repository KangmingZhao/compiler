	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global k
k:
	.word 0
	.size k, 4
.global n
n:
	.word 10
	.size 0, 4
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L22:
	ldr r4, =0
	str r4, [fp, #-4]
	ldr r4, =1
	ldr r5, =k
	str r4, [r5]
	b .L24
.L24:
	ldr r4, [fp, #-4]
	cmp r4, #9
	movle r4, #1
	movgt r4, #0
	ble .L25
	b .L26
.L25:
	ldr r4, [fp, #-4]
	add r5, r4, #1
	str r5, [fp, #-4]
	ldr r4, =k
	ldr r5, [r4]
	add r4, r5, #1
	ldr r4, =k
	ldr r5, [r4]
	ldr r4, =k
	ldr r6, [r4]
	add r4, r5, r6
	ldr r5, =k
	str r4, [r5]
	b .L24
.L26:
	ldr r4, =k
	ldr r5, [r4]
	mov r0, r5
	bl putint
	ldr r4, =k
	ldr r5, [r4]
	mov r0, r5
	add sp, sp, #4
	pop {r4, r5, r6, fp, lr }
	bx lr
