	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 0
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L5:
	ldr r4, =10
	ldr r5, =a
	str r4, [r5]
	ldr r4, =a
	ldr r5, [r4]
	cmp r5, #0
	movgt r4, #1
	movle r4, #0
	bgt .L6
	b .L7
.L6:
	mov r0, #1
	add sp, sp, #0
	pop {r4, r5, fp, lr }
	bx lr
	b .L7
.L7:
	mov r0, #0
	add sp, sp, #0
	pop {r4, r5, fp, lr }
	bx lr
