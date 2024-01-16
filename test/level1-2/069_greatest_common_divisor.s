	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global fun
	.type fun , %function
fun:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L27:
	b .L33
.L33:
	mov r4, r1
	cmp r4, #0
	movgt r4, #1
	movle r4, #0
	bgt .L34
	b .L35
.L34:
	mov r4, r0
	mov r5, r1
	sdiv r6, r4, r5
	mul r6, r6, r5
	sub r5, r4, r6
	str r5, [fp, #-4]
	mov r4, r1
	mov r0, r4
	ldr r4, [fp, #-4]
	mov r1, r4
	b .L33
.L35:
	mov r4, r0
	mov r0, r4
	add sp, sp, #4
	pop {r4, r5, r6, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #12
.L36:
	bl getint
	mov r4, r0
	str r4, [fp, #-8]
	bl getint
	mov r4, r0
	str r4, [fp, #-12]
	ldr r4, [fp, #-8]
	ldr r5, [fp, #-12]
	mov r0, r4
	mov r1, r5
	bl fun
	mov r4, r0
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	mov r0, r4
	bl putint
	mov r0, #0
	add sp, sp, #12
	pop {r4, r5, fp, lr }
	bx lr
