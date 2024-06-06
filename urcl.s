	.text
	.file	"urcl"
	.globl	urcl_main                       // -- Begin function urcl_main
	.p2align	2
	.type	urcl_main,@function
urcl_main:                              // @urcl_main
	.cfi_startproc
// %bb.0:                               // %entry
	sub	sp, sp, #64
	stp	x30, x19, [sp, #48]             // 16-byte Folded Spill
	.cfi_def_cfa_offset 64
	.cfi_offset w19, -8
	.cfi_offset w30, -16
	mov	w8, #69                         // =0x45
	mov	w9, #5                          // =0x5
	mov	w0, #2                          // =0x2
	mov	w1, wzr
	strb	wzr, [sp, #47]
	strb	w8, [sp, #39]
	str	x9, [sp]
	bl	urcl_out
	ldr	x8, [sp]
	mov	w0, #24                         // =0x18
	mov	w1, #1                          // =0x1
	add	x8, x8, #1
	str	x8, [sp]
	bl	urcl_out
	ldr	x8, [sp]
	mov	w0, #1                          // =0x1
	mov	w1, #10                         // =0xa
	add	x8, x8, #1
	str	x8, [sp]
	bl	urcl_out
	ldr	x8, [sp]
	mov	w9, #8                          // =0x8
	mov	w0, #2                          // =0x2
	add	x8, x8, #4
	strb	w9, [sp, #47]
	str	x8, [sp]
	cbz	wzr, .LBB0_2
// %bb.1:                               // %inst_12
	mov	w1, #1                          // =0x1
	bl	urcl_out
	b	.LBB0_3
.LBB0_2:                                // %inst_10
	mov	w1, wzr
	bl	urcl_out
	ldr	x8, [sp]
	add	x8, x8, #1
	str	x8, [sp]
.LBB0_3:                                // %inst_13
	ldr	x8, [sp]
	mov	w0, #24                         // =0x18
	mov	w1, #1                          // =0x1
	mov	w19, #1                         // =0x1
	add	x8, x8, #1
	str	x8, [sp]
	bl	urcl_out
	ldr	x8, [sp]
	mov	w0, #1                          // =0x1
	mov	w1, #10                         // =0xa
	add	x8, x8, #1
	str	x8, [sp]
	bl	urcl_out
	ldr	x8, [sp]
	mov	w0, #2                          // =0x2
	mov	w1, wzr
	strb	w19, [sp, #47]
	add	x8, x8, #3
	str	x8, [sp]
	bl	urcl_out
	ldr	x8, [sp]
	mov	w0, #24                         // =0x18
	mov	w1, #1                          // =0x1
	add	x8, x8, #1
	str	x8, [sp]
	bl	urcl_out
	ldr	x8, [sp]
	mov	w0, #1                          // =0x1
	mov	w1, #10                         // =0xa
	add	x8, x8, #1
	str	x8, [sp]
	bl	urcl_out
	ldr	x8, [sp]
	strb	w19, [sp, #47]
	ldp	x30, x19, [sp, #48]             // 16-byte Folded Reload
	add	x0, x8, #1
	str	x0, [sp]
	add	sp, sp, #64
	ret
.Lfunc_end0:
	.size	urcl_main, .Lfunc_end0-urcl_main
	.cfi_endproc
                                        // -- End function
	.section	".note.GNU-stack","",@progbits
