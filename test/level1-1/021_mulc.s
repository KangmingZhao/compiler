	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global a
a:
	.word 5
	.text
	.global main
	.type main , %function
main:
	push {r4, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L4:
	mov  r0, #25
	add sp, sp, #0
	pop {r4, fp, lr }
	bx lr
