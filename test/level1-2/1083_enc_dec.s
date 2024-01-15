	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global enc
	.type enc , %function
enc:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L34:
	mov r4, r0
	cmp r4, #25
	movgt r4, #1
	movle r4, #0
	bgt .L37
	b .L38
.L37:
	mov r4, r0
	add r5, r4, #60
	mov r0, r5
	b .L39
.L38:
	mov r4, r0
	sub r5, r4, #15
	mov r0, r5
	b .L39
.L39:
	mov r4, r0
	mov r0, r4
	add sp, sp, #0
	pop {r4, r5, fp, lr }
	bx lr
	.global dec
	.type dec , %function
dec:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L40:
	mov r4, r0
	cmp r4, #85
	movgt r4, #1
	movle r4, #0
	bgt .L43
	b .L44
.L43:
	mov r4, r0
	sub r5, r4, #59
	mov r0, r5
	b .L45
.L44:
	mov r4, r0
	add r5, r4, #14
	mov r0, r5
	b .L45
.L45:
	mov r4, r0
	mov r0, r4
	add sp, sp, #0
	pop {r4, r5, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {r4, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L46:
	ldr r4, =400
	str r4, [fp, #-8]
	ldr r4, [fp, #-8]
	mov r0, r4
	bl enc
	mov r4, r0
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	mov r0, r4
	bl dec
	mov r4, r0
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	mov r0, r4
	bl putint
	ldr r4, =10
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	mov r0, r4
	bl putch
	mov r0, #0
	add sp, sp, #8
	pop {r4, fp, lr }
	bx lr
