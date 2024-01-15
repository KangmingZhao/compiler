	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L11:
	ldr r4, =10
	str r4, [fp, #-8]
	ldr r4, =30
	str r4, [fp, #-4]
	ldr r4, [fp, #-8]
	ldr r6, =0
	sub r5, r6, #5
	sub r6, r4, r5
	ldr r4, [fp, #-4]
	add r5, r6, r4
	ldr r6, =0
	sub r4, r6, #5
	add r6, r5, r4
	mov r0, r6
	add sp, sp, #8
	pop {r4, r5, r6, fp, lr }
	bx lr
