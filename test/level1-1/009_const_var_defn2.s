@a = global i32 0, align 4 
@b = global i32 5, align 4 
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
.global b
b:
	.word 5
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L5:
