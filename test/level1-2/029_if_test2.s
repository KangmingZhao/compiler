declare i32 @getint()
declare void @putint(i32)
declare i32 @getch()
declare void @putch(i32)
declare void @putf(i32)

	.arch armv8-a
	.arch_extension crc
	.arm
	.global ifElseIf
	.type ifElseIf , %function
ifElseIf:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L31:
	mov r0, r0
	mov r0, r0
	mov r0, r0
.L34:
	mov r0, r0
.L35:
	mov r0, r0
.L36:
	mov r0, r0
.L37:
	mov r0, r0
.L38:
	mov r0, r0
.L39:
	mov r0, r0
.L40:
.L41:
	mov r0, r0
.L42:
	mov r0, r0
	add r4, v0, #0
	mov r0, r0
.L43:
	mov r0, r0
	mov r0, r0
.L44:
.L45:
	mov r0, r0
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L46:
	bl ifElseIf
	mov r0, v0
	bl putint
