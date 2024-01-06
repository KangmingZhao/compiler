@n = global i32 0, align 4 
declare i32 @getint()
declare void @putint(i32)
declare i32 @getch()
declare void @putch(i32)
declare void @putf(i32)

	.arch armv8-a
	.arch_extension crc
	.arm
.global k
k:
	.space 8
.global n
n:
	.word 0
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L22:
	mov  r0, r0
	mov  r0, r0
	b .L24
.L24:
	mov  r0, r0
	cmp r4, #0
	mov  r5, #1
	mov  r5, #0
	bge .L25
	b .L26
.L25:
	mov  r0, r0
	add r6, r5, #0
	mov  r0, r0
	mov  r0, r0
	add r7, r6, #0
	mov  r0, r0
	mov  r0, r0
	add r4, r7, r7
	mov  r0, r0
	b .L24
.L26:
	mov  r0, r0
	mov  r0, r5
	bl putint
	mov  r0, r0
	mov  r0, r5
	add sp, sp, #4
	POP {pc} 
	POP { }
	bx lr
