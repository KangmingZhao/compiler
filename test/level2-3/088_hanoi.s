	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global move
	.type move , %function
move:
	push {r4, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L43:
	mov r4, r0
	mov r0, r4
	bl putint
	ldr r0, =32
	bl putch
	mov r4, r1
	mov r0, r4
	bl putint
	ldr r0, =44
	bl putch
	ldr r0, =32
	bl putch
	add sp, sp, #0
	pop {r4, fp, lr }
	bx lr
	.global hanoi
	.type hanoi , %function
hanoi:
	push {r4, r5, r6, r7, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L48:
	mov r4, r0
	cmp r4, #1
	moveq r4, #1
	movne r4, #0
	beq .L57
	b .L58
.L57:
	mov r4, r1
	mov r5, r3
	mov r0, r4
	mov r1, r5
	bl move
	b .L59
.L58:
	mov r4, r0
	sub r5, r4, #1
	mov r4, r1
	mov r6, r3
	mov r7, r2
	mov r0, r5
	mov r1, r4
	mov r2, r6
	mov r3, r7
	bl hanoi
	mov r4, r1
	mov r5, r3
	mov r0, r4
	mov r1, r5
	bl move
	mov r4, r0
	sub r5, r4, #1
	mov r4, r2
	mov r6, r1
	mov r7, r3
	mov r0, r5
	mov r1, r4
	mov r2, r6
	mov r3, r7
	bl hanoi
	b .L59
.L59:
	add sp, sp, #0
	pop {r4, r5, r6, r7, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L60:
	bl getint
	mov r4, r0
	str r4, [fp, #-4]
	b .L62
.L62:
	ldr r4, [fp, #-4]
	cmp r4, #0
	movgt r4, #1
	movle r4, #0
	bgt .L63
	b .L64
.L63:
	bl getint
	mov r0, r0
	ldr r1, =1
	ldr r2, =2
	ldr r3, =3
	bl hanoi
	ldr r0, =10
	bl putch
	ldr r4, [fp, #-4]
	sub r5, r4, #1
	str r5, [fp, #-4]
	b .L62
.L64:
	mov r0, #0
	add sp, sp, #4
	pop {r4, r5, fp, lr }
	bx lr
