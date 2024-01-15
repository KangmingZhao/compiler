	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global fuck
	.type fuck , %function
fuck:
	push {r4, fp, lr}
	mov fp, sp
	sub sp, sp, #16
	str r0, [sp, #-16]
	str r1, [sp, #-12]
	str r2, [sp, #-8]
	str r3, [sp, #-4]
.L10:
	ldr r4, =0
	str r4, [fp, #-16]
	ldr r4, =1
	str r4, [fp, #-12]
	ldr r4, =2
	str r4, [fp, #-8]
	ldr r4, =3
	str r4, [fp, #-4]
	ldr r4, =4
	add sp, sp, #16
	pop {r4, fp, lr }
	bx lr
