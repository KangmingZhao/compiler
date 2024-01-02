@shitfuck = global i32 10, align 4 
@shitconst = global i32 114514, align 4 
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
	.word 10
.global shitconst
shitconst:
	.word 114514
	.global fuck
	.type fuck , %function
fuck:
	str fp, [sp, #8]!
	mov fp, sp
	sub sp, sp, #114514
.L20:
	ldr r4, =0
	str r4, [fp, #-4]
	mov r4, r0
	str r4, [fp, #-4]
	mov r0, #1
	mov r1, #2
	mov r2, #3
	mov r3, #4
	ldr r4, =5
	str r4, [fp, #4]
	ldr r4, =6
	str r4, [fp, #8]
	ldr r4, =10
	ldr r4, addr_@shitfuck
	str r4, [r4]
	.global main
	.type main , %function
main:
	str fp, [sp, #4]!
	mov fp, sp
	sub sp, sp, #114514
.L36:
	ldr r0, =6
	push {r0}
	ldr r0, =5
	push {r0}
	ldr r0, =1
	ldr r1, =2
	ldr r2, =3
	ldr r3, =4
	bl fuck
