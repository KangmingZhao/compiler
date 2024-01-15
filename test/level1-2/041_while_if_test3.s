	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global deepWhileBr
	.type deepWhileBr , %function
deepWhileBr:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #12
.L33:
	mov r4, r0
	mov r5, r1
	add r6, r4, r5
	str r6, [fp, #-4]
	b .L39
.L39:
	ldr r4, [fp, #-4]
	cmp r4, #75
	movlt r4, #1
	movge r4, #0
	blt .L40
	b .L41
.L40:
	ldr r4, =42
	str r4, [fp, #-12]
	ldr r4, [fp, #-4]
	cmp r4, #100
	movlt r4, #1
	movge r4, #0
	blt .L43
	b .L44
.L41:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #12
	pop {r4, r5, r6, fp, lr }
	bx lr
.L43:
	ldr r4, [fp, #-4]
	ldr r5, [fp, #-12]
	add r6, r4, r5
	str r6, [fp, #-4]
	ldr r4, [fp, #-4]
	cmp r4, #99
	movgt r4, #1
	movle r4, #0
	bgt .L45
	b .L46
.L44:
	b .L39
.L45:
	ldr r4, [fp, #-12]
	ldr r6, =2
	mul r5, r4, r6
	str r5, [fp, #-8]
	ldr r4, =1
	cmp r4, #0
	movne r4, #1
	moveq r4, #0
	bne .L48
	b .L49
.L46:
	b .L44
.L48:
	ldr r4, [fp, #-8]
	ldr r6, =2
	mul r5, r4, r6
	str r5, [fp, #-4]
	b .L49
.L49:
	b .L46
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L51:
	ldr r4, =2
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	ldr r5, [fp, #-4]
	mov r0, r4
	mov r1, r5
	bl deepWhileBr
	mov r0, r0
	add sp, sp, #4
	pop {r4, r5, fp, lr }
	bx lr
