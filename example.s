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
.L9:
	ldr r0, [fp, #-8]
	ldr r0, [fp, #-4]
	add r0, r0, r0
