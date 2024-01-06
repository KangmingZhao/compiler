declare i32 @getint()
declare void @putint(i32)
declare i32 @getch()
declare void @putch(i32)
declare void @putf(i32)

	.arch armv8-a
	.arch_extension crc
	.arm
.global n
n:
	.space 8
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L25:
	bl getint
	mov r0, r0
	bl getint
	mov r0, r0
	mov r0, r0
	mov r0, r0
	mov r0, r0
	mov r0, r0
	mov r0, r0
	mov r0, r0
	mov r0, r0
	mov r0, v0
	bl putint
	mov r0, r0
	mov r0, r0
	mov r0, v0
	bl putch
	mov r0, r0
	mov r0, v0
	bl putint
	mov r0, r0
	mov r0, r0
	mov r0, v0
	bl putch
