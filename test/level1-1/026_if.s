declare i32 @getint()
declare void @putint(i32)
declare i32 @getch()
declare void @putch(i32)
declare void @putf(i32)

	.arch armv8-a
	.arch_extension crc
	.arm
.global a
a:
	.space 8
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L5:
	mov  r0, r0
	mov  r0, r0
	cmp v0, #0
	mov  r4, #1
	mov  r4, #0
	bgt .L6
	b .L7
.L6:
	mov  r0, #0
	add sp, sp, #0
	POP {pc} 
	POP { }
	bx lr
	b .L7
.L7:
	mov  r0, #0
	add sp, sp, #0
	POP {pc} 
	POP { }
	bx lr
