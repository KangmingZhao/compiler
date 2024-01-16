	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global get_one
	.type get_one , %function
get_one:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L38:
	mov r0, #1
	add sp, sp, #0
	pop {fp, lr }
	bx lr
	.global deepWhileBr
	.type deepWhileBr , %function
deepWhileBr:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #12
.L41:
	mov r4, r0
	mov r5, r1
	add r6, r4, r5
	str r6, [fp, #-4]
	b .L47
.L47:
	ldr r4, [fp, #-4]
	cmp r4, #75
	movlt r4, #1
	movge r4, #0
	blt .L48
	b .L49
.L48:
	ldr r4, =42
	str r4, [fp, #-12]
	ldr r4, [fp, #-4]
	cmp r4, #100
	movlt r4, #1
	movge r4, #0
	blt .L51
	b .L52
.L49:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #12
	pop {r4, r5, r6, fp, lr }
	bx lr
.L51:
	ldr r4, [fp, #-4]
	ldr r5, [fp, #-12]
	add r6, r4, r5
	str r6, [fp, #-4]
	ldr r4, [fp, #-4]
	cmp r4, #99
	movgt r4, #1
	movle r4, #0
	bgt .L53
	b .L54
.L52:
	b .L47
.L53:
	ldr r4, [fp, #-12]
	ldr r6, =2
	mul r5, r4, r6
	str r5, [fp, #-8]
	ldr r0, =0
	bl get_one
	cmp v24, #1
	moveq r4, #1
	movne r4, #0
	beq .L56
	b .L57
.L54:
	b .L52
.L56:
	ldr r4, [fp, #-8]
	ldr r6, =2
	mul r5, r4, r6
	str r5, [fp, #-4]
	b .L57
.L57:
	b .L54
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L58:
	ldr r4, =2
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	ldr r5, [fp, #-4]
	mov r0, r4
	mov r1, r5
	bl deepWhileBr
	mov r4, r0
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	mov r0, r4
	bl putint
	mov r0, #0
	add sp, sp, #4
	pop {r4, r5, fp, lr }
	bx lr
