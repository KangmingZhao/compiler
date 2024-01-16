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
	sub sp, sp, #0
.L7:
	ldr r4, =a
	ldr r5, [r4]
	ldr r4, =b
	ldr r6, [r4]
	add r4, r5, r6
	mov r0, r4
	add sp, sp, #0
	pop {r4, r5, r6, fp, lr }
	bx lr
