declare i32 @getint()
declare void @putint(i32)
declare i32 @getch()
declare void @putch(i32)
declare void @putf(i32)

	.arch armv8-a
	.arch_extension crc
	.arm
	.global if_if_Else
	.type if_if_Else , %function
if_if_Else:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L14:
	mov r0, r0
	mov r0, r0
	mov r0, r0
.L17:
	mov r0, r0
.L18:
	mov r0, r0
	add r4, v0, #0
	mov r0, r0
.L19:
	mov r0, r0
.L20:
	mov r0, r0
.L21:
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L22:
	bl if_if_Else
