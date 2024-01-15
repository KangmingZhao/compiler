	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global ifElse
	.type ifElse , %function
ifElse:
	push {r4, r5, r6, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L8:
	mov r4, r0
	mov r5, r0
	add r6, r4, r5
	mov r0, r6
	mov r4, r0
	mov r0, r4
	add sp, sp, #0
	pop {r4, r5, r6, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L11:
	ldr r0, =0
	bl ifElse
	mov r0, r0
	bl ifElse
	mov r0, #0
	add sp, sp, #0
	pop {fp, lr }
	bx lr
