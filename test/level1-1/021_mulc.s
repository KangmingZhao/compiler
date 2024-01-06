@a = global i32 0, align 4 
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
	.word 0
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L4:
	mov  r0, #0
	add sp, sp, #0
	POP {pc} 
	POP { }
	bx lr
