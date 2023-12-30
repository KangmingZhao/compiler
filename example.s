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
.L4:
	ldr r4, =0
	ldr r4, =1
	ldr r4, [fp, #-4]
