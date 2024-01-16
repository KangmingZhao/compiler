	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 0
	.text
	.global func
	.type func , %function
func:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L13:
	mov r4, r0
	sub r5, r4, #1
	mov r0, r5
	mov r4, r0
	mov r0, r4
	add sp, sp, #0
	pop {r4, r5, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #4
.L16:
	ldr r4, =10
	ldr r5, =a
	str r4, [r5]
	ldr r4, =a
	ldr r5, [r4]
	mov r0, r5
	bl func
	mov r4, r0
	str r4, [fp, #-4]
	ldr r4, [fp, #-4]
	mov r0, r4
	add sp, sp, #4
	pop {r4, r5, fp, lr }
	bx lr
