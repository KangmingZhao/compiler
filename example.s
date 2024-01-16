	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global k0
k0:
	.word -2147483648
	.text
	.global fuck
	.type fuck , %function
fuck:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L5:
	mov r0, #0
	add sp, sp, #0
	pop {fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {r4, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L8:
	ldr r4, =k0
	mov r0, r4
	bl fuck
	mov r0, r0
	add sp, sp, #0
	pop {r4, fp, lr }
	bx lr
