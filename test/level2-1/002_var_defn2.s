	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 3
.global b
b:
	.word 5
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L8:
	ldr r4, =5
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	ldr r5, =b
	ldr r6, [r5]
	add r5, r4, r6
	mov r0, r5
	add sp, sp, #4
	pop {r4, r5, r6, fp, lr }
	bx lr
