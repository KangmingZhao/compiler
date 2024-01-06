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
.L13:
	mov  r0, r0
	mov  r0, r0
	cmp v0, #0
	mov  r4, #1
	mov  r4, #0
	cmp r4, #0
	mov  r4, #1
	mov  r4, #0
	beq .L15
	b .L16
.L15:
	mov  r0, r0
	b .L17
.L16:
	mov  r0, r0
	b .L17
.L17:
	mov  r0, r0
	mov  r0, r4
	add sp, sp, #4
	POP {pc} 
	POP { }
	bx lr
