declare i32 @getint()
declare void @putint(i32)
declare i32 @getch()
declare void @putch(i32)
declare void @putf(i32)

	.arch armv8-a
	.arch_extension crc
	.arm
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L15:
	mov  r0, r0
	mov  r0, r0
	b .L18
.L18:
	mov  r0, r0
	cmp r4, #0
	mov  r5, #1
	mov  r5, #0
	blt .L19
	b .L20
.L19:
	mov  r0, r0
	mov  r0, r0
	add r6, r5, r5
	mov  r0, r0
	mov  r0, r0
	add r4, r6, #0
	mov  r0, r0
	b .L18
.L20:
	mov  r0, r0
	mov  r0, r5
	bl putint
	mov  r0, #0
	add sp, sp, #8
	POP {pc} 
	POP { }
	bx lr
