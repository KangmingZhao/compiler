	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 7
	.text
	.global func
	.type func , %function
func:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L27:
	ldr r4, =a
	ldr r5, [r4]
	str r5, [fp, #-8]
	ldr r4, =1
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	ldr r5, [fp, #-8]
	cmp r4, r5
	moveq r4, #1
	movne r4, #0
	beq .L30
	b .L31
.L30:
	ldr r4, [fp, #-4]
	add r5, r4, #1
	str r5, [fp, #-4]
	mov r0, #1
	add sp, sp, #8
	pop {r4, r5, fp, lr }
	bx lr
	b .L32
.L31:
	mov r0, #0
	add sp, sp, #8
	pop {r4, r5, fp, lr }
	bx lr
	b .L32
.L32:
	mov r0, #0
	add sp, sp, #8
	pop {r4, r5, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #8
.L33:
	ldr r4, =0
	str r4, [fp, #-8]
	ldr r4, =0
	str r4, [fp, #-4]
	b .L36
.L36:
	ldr r4, [fp, #-4]
	cmp r4, #100
	movlt r4, #1
	movge r4, #0
	blt .L37
	b .L38
.L37:
	bl func
	cmp v14, #1
	moveq r4, #1
	movne r4, #0
	beq .L39
	b .L40
.L38:
	ldr r4, [fp, #-8]
	cmp r4, #100
	movlt r4, #1
	movge r4, #0
	blt .L41
	b .L42
.L39:
	ldr r4, [fp, #-8]
	add r5, r4, #1
	str r5, [fp, #-8]
	b .L40
.L40:
	ldr r4, [fp, #-4]
	add r5, r4, #1
	str r5, [fp, #-4]
	b .L36
.L41:
	ldr r0, =1
	bl putint
	b .L43
.L42:
	ldr r0, =0
	bl putint
	b .L43
.L43:
	mov r0, #0
	add sp, sp, #8
	pop {r4, r5, fp, lr }
	bx lr
