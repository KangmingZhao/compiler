	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global N
N:
	.word 0
.global newline
newline:
	.word 0
	.text
	.global factor
	.type factor , %function
factor:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L33:
	ldr r4, =0
	str r4, [fp, #-4]
	ldr r4, =1
	str r4, [fp, #-8]
	b .L38
.L38:
	ldr r4, [fp, #-8]
	mov r5, r0
	add r6, r5, #1
	cmp r4, r6
	movlt r4, #1
	movge r4, #0
	blt .L39
	b .L40
.L39:
	mov r4, r0
	ldr r5, [fp, #-8]
	sdiv r6, r4, r5
	mul r6, r6, r5
	sub r5, r4, r6
	cmp r5, #0
	moveq r4, #1
	movne r4, #0
	beq .L41
	b .L42
.L40:
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #8
	pop {r4, r5, r6, fp, lr }
	bx lr
.L41:
	ldr r4, [fp, #-4]
	ldr r5, [fp, #-8]
	add r6, r4, r5
	str r6, [fp, #-4]
	b .L42
.L42:
	ldr r4, [fp, #-8]
	add r5, r4, #1
	str r5, [fp, #-8]
	b .L38
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #12
.L43:
	ldr r4, =4
	ldr r5, =N
	str r4, [r5]
	ldr r4, =10
	ldr r5, =newline
	str r4, [r5]
	ldr r4, =1478
	str r4, [fp, #-8]
	ldr r4, [fp, #-8]
	mov r0, r4
	bl factor
	mov r0, r0
	add sp, sp, #12
	pop {r4, r5, fp, lr }
	bx lr
