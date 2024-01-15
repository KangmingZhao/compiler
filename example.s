	.arch armv8-a
	.arch_extension crc
	.arm
	.text
	.global fuck
	.type fuck , %function
fuck:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L4:
	mov r4, r0
	add r5, r4, #1
	mov r0, r5
	add sp, sp, #0
	pop {r4, r5, fp, lr }
	bx lr
	.global main
	.type main , %function
main:
	push {fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L7:
	mov r0, #0
	add sp, sp, #0
	pop {fp, lr }
	bx lr
