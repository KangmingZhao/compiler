	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global k
k:
	.word 0
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #16
.L33:
	ldr r4, =3389
	ldr r4, =k
	str r4, [r4]
	ldr r4, =k
	ldr r5, [r4]
	ldr r4, =10000
	cmp r5, r4
	movlt r4, #1
	movge r4, #0
	blt .L34
	b .L35
.L34:
	ldr r4, =k
	ldr r5, [r4]
	add r4, r5, #1
	ldr r5, =k
	str r4, [r5]
	ldr r4, =112
	str r4, [fp, #-4]
	b .L37
.L35:
	ldr r4, =k
	ldr r5, [r4]
	mov r0, r5
	add sp, sp, #4
	pop {r4, r5, r6, fp, lr }
	bx lr
.L37:
	ldr r4, [fp, #-4]
	cmp r4, #10
	movgt r4, #1
	movle r4, #0
	bgt .L38
	b .L39
.L38:
	ldr r4, [fp, #-4]
	sub r5, r4, #88
	str r5, [fp, #-4]
	ldr r4, [fp, #-4]
	ldr r5, =1000
	cmp r4, r5
	movlt r4, #1
	movge r4, #0
	blt .L40
	b .L41
.L39:
	ldr r4, [fp, #-4]
	mov r0, r4
	bl putint
	b .L35
.L40:
	ldr r4, =9
	str r4, [fp, #-16]
	ldr r4, =11
	str r4, [fp, #-12]
	ldr r4, =10
	str r4, [fp, #-16]
	ldr r4, [fp, #-4]
	ldr r5, [fp, #-16]
	sub r6, r4, r5
	str r6, [fp, #-4]
	ldr r4, =11
	str r4, [fp, #-8]
	ldr r4, [fp, #-4]
	ldr r5, [fp, #-8]
	add r6, r4, r5
	ldr r4, [fp, #-12]
	add r5, r6, r4
	str r5, [fp, #-4]
	b .L41
.L41:
	b .L37
