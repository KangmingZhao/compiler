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
.L3:
	mov  r0, r0
	mov  r0, r0
	mov  r0, v0
	add sp, sp, #4
	POP {pc} 
	POP { }
	bx lr
