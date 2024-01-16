	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 0
.global b
b:
	.word 0
.global c
c:
	.word 0
.global d
d:
	.word 0
.global e
e:
	.word 0
	.text
	.global main
	.type main , %function
main:
	push {r4, r5, r6, r7, r8, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L54:
	bl getint
	mov r4, r0
	ldr r4, =a
	str r4, [r4]
	bl getint
	mov r4, r0
	ldr r4, =b
	str r4, [r4]
	bl getint
	mov r4, r0
	ldr r5, =c
	str r4, [r5]
	bl getint
	mov r4, r0
	ldr r5, =d
	str r4, [r5]
	bl getint
	mov r4, r0
	ldr r5, =e
	str r4, [r5]
	ldr r4, =0
	str r4, [fp, #-4]
	ldr r4, =a
	ldr r5, [r4]
	ldr r4, =b
	ldr r6, [r4]
	ldr r4, =c
	ldr r7, [r4]
	mul r4, r6, r7
	sub r6, r5, r4
	ldr r4, =d
	ldr r5, [r4]
	ldr r4, =a
	ldr r7, [r4]
	ldr r4, =c
	ldr r8, [r4]
	sdiv r4, r7, r8
	sub r7, r5, r4
	cmp r6, r7
	movne r4, #1
	moveq r4, #0
	bne .L56
	b .L59
.L56:
	ldr r4, =1
	str r4, [fp, #-4]
	b .L57
.L57:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #4
	pop {r4, r5, r6, r7, r8, fp, lr }
	bx lr
.L58:
	ldr r4, =a
	ldr r5, [r4]
	ldr r4, =b
	ldr r6, [r4]
	add r4, r5, r6
	ldr r5, =c
	ldr r6, [r5]
	add r5, r4, r6
	ldr r4, =d
	ldr r6, [r4]
	ldr r4, =e
	ldr r7, [r4]
	add r4, r6, r7
	cmp r5, r4
	moveq r4, #1
	movne r4, #0
	beq .L56
	b .L57
.L59:
	ldr r4, =a
	ldr r5, [r4]
	ldr r4, =b
	ldr r6, [r4]
	mul r4, r5, r6
	ldr r5, =c
	ldr r6, [r5]
	sdiv r5, r4, r6
	ldr r4, =e
	ldr r6, [r4]
	ldr r4, =d
	ldr r7, [r4]
	add r4, r6, r7
	cmp r5, r4
	moveq r4, #1
	movne r4, #0
	beq .L56
	b .L58
