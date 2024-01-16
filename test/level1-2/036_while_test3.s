	.arch armv8-a
	.arch_extension crc
	.arm
.data
.global g
g:
	.word 0
.global h
h:
	.word 0
.global f
f:
	.word 0
.global e
e:
	.word 0
	.text
	.global EightWhile
	.type EightWhile , %function
EightWhile:
	push {r4, r5, r6, r7, fp, lr}
	mov fp, sp
	sub sp, sp, #16
.L97:
	ldr r4, =5
	str r4, [fp, #-16]
	ldr r4, =6
	str r4, [fp, #-12]
	ldr r4, =7
	str r4, [fp, #-8]
	ldr r4, =10
	str r4, [fp, #-4]
	b .L102
.L102:
	ldr r4, [fp, #-16]
	cmp r4, #20
	movlt r4, #1
	movge r4, #0
	blt .L103
	b .L104
.L103:
	ldr r4, [fp, #-16]
	add r5, r4, #3
	str r5, [fp, #-16]
	b .L105
.L104:
	ldr r4, [fp, #-16]
	ldr r5, [fp, #-12]
	ldr r6, [fp, #-4]
	add r7, r5, r6
	add r5, r4, r7
	ldr r4, [fp, #-8]
	add r6, r5, r4
	ldr r4, =e
	ldr r5, [r4]
	ldr r4, [fp, #-4]
	add r7, r5, r4
	ldr r4, =g
	ldr r5, [r4]
	sub r4, r7, r5
	ldr r5, =h
	ldr r7, [r5]
	add r5, r4, r7
	sub r4, r6, r5
	mov r0, r4
	add sp, sp, #16
	pop {r4, r5, r6, r7, fp, lr }
	bx lr
.L105:
	ldr r4, [fp, #-12]
	cmp r4, #10
	movlt r4, #1
	movge r4, #0
	blt .L106
	b .L107
.L106:
	ldr r4, [fp, #-12]
	add r5, r4, #1
	str r5, [fp, #-12]
	b .L108
.L107:
	ldr r4, [fp, #-12]
	sub r5, r4, #2
	str r5, [fp, #-12]
	b .L102
.L108:
	ldr r4, [fp, #-8]
	cmp r4, #7
	moveq r4, #1
	movne r4, #0
	beq .L109
	b .L110
.L109:
	ldr r4, [fp, #-8]
	sub r5, r4, #1
	str r5, [fp, #-8]
	b .L111
.L110:
	ldr r4, [fp, #-8]
	add r5, r4, #1
	str r5, [fp, #-8]
	b .L105
.L111:
	ldr r4, [fp, #-4]
	cmp r4, #20
	movlt r4, #1
	movge r4, #0
	blt .L112
	b .L113
.L112:
	ldr r4, [fp, #-4]
	add r5, r4, #3
	str r5, [fp, #-4]
	b .L114
.L113:
	ldr r4, [fp, #-4]
	sub r5, r4, #1
	str r5, [fp, #-4]
	b .L108
.L114:
	ldr r4, =e
	ldr r5, [r4]
	cmp r5, #1
	movgt r4, #1
	movle r4, #0
	bgt .L115
	b .L116
.L115:
	ldr r4, =e
	ldr r5, [r4]
	sub r4, r5, #1
	ldr r5, =e
	str r4, [r5]
	b .L117
.L116:
	ldr r4, =e
	ldr r5, [r4]
	add r4, r5, #1
	ldr r5, =e
	str r4, [r5]
	b .L111
.L117:
	ldr r4, =f
	ldr r5, [r4]
	cmp r5, #2
	movgt r4, #1
	movle r4, #0
	bgt .L118
	b .L119
.L118:
	ldr r4, =f
	ldr r5, [r4]
	sub r4, r5, #2
	ldr r5, =f
	str r4, [r5]
	b .L120
.L119:
	ldr r4, =f
	ldr r5, [r4]
	add r4, r5, #1
	ldr r5, =f
	str r4, [r5]
	b .L114
.L120:
	ldr r4, =g
	ldr r5, [r4]
	cmp r5, #3
	movlt r4, #1
	movge r4, #0
	blt .L121
	b .L122
.L121:
	ldr r4, =g
	ldr r5, [r4]
	add r4, r5, #10
	ldr r5, =g
	str r4, [r5]
	b .L123
.L122:
	ldr r4, =g
	ldr r5, [r4]
	sub r4, r5, #8
	ldr r5, =g
	str r4, [r5]
	b .L117
.L123:
	ldr r4, =h
	ldr r5, [r4]
	cmp r5, #10
	movlt r4, #1
	movge r4, #0
	blt .L124
	b .L125
.L124:
	ldr r4, =h
	ldr r5, [r4]
	add r4, r5, #8
	ldr r5, =h
	str r4, [r5]
	b .L123
.L125:
	ldr r4, =h
	ldr r5, [r4]
	sub r4, r5, #1
	ldr r5, =h
	str r4, [r5]
	b .L120
	.global main
	.type main , %function
main:
	push {r4, r5, fp, lr}
	mov fp, sp
	sub sp, sp, #0
.L126:
	ldr r4, =1
	ldr r5, =g
	str r4, [r5]
	ldr r4, =2
	ldr r4, =h
	str r4, [r4]
	ldr r4, =4
	ldr r4, =e
	str r4, [r4]
	ldr r4, =6
	ldr r5, =f
	str r4, [r5]
	bl EightWhile
	mov r0, r0
	add sp, sp, #0
	pop {r4, r5, fp, lr }
	bx lr
