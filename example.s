@shitfuck = global i32 0, align 4 
@shitconst = global i32 0, align 4 
declare i32 @getint()
declare void @putint(i32)
declare i32 @getch()
declare void @putch(i32)
declare void @putf(i32)

	.arch armv8-a
	.arch_extension crc
	.arm
.global shityou
shityou:
	.space 8
.global shitfuck
shitfuck:
	.word 0
.global shitconst
shitconst:
	.word 0
	.global fuck
	.type fuck , %function
fuck:
	str fp, [sp, #8]!
	mov fp, sp
	sub sp, sp, #114514
.L23:
	mov  r21857, r0
	mov  r0, r0
	mov  r21857, r0
	mov  r0, r0
	mov  r1, r0
	mov  r2, r0
	mov  r3, r0
	mov  r4, r0
	mov  r4, r0
	add sp, sp, #4
	pop { }
	pop {pc} 
	bx lr
	mov  r0, r0
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L39:
	mov  r21857, r0
	mov  r0, r21857
	cmp v0, #0
	mov  r4, #1
	mov  r4, #0
	bgt .L41
	b .L42
.L41:
	ldr r0, =0
	push {r0}
	ldr r0, =0
	push {r0}
	ldr r0, =0
	ldr r1, =0
	ldr r2, =0
	ldr r3, =0
	bl fuck
	b .L42
.L42:
	mov  r0, #0
	add sp, sp, #4
	pop { }
	pop {pc} 
	bx lr
