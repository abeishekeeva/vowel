	.text
	.file	"MicroC"
	.globl	main                    // -- Begin function main
	.p2align	2
	.type	main,@function
main:                                   // @main
	.cfi_startproc
// %bb.0:                               // %entry
	str	x30, [sp, #-16]!        // 8-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -16
	mov	w0, #72
	bl	printbig
	mov	w0, #69
	bl	printbig
	mov	w0, #76
	bl	printbig
	mov	w0, #76
	bl	printbig
	mov	w0, #79
	bl	printbig
	mov	w0, #32
	bl	printbig
	mov	w0, #87
	bl	printbig
	mov	w0, #79
	bl	printbig
	mov	w0, #82
	bl	printbig
	mov	w0, #76
	bl	printbig
	mov	w0, #68
	bl	printbig
	mov	w0, wzr
	ldr	x30, [sp], #16          // 8-byte Folded Reload
	ret
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        // -- End function
	.type	.Lfmt,@object           // @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         // @fmt.1
.Lfmt.1:
	.asciz	"%g\n"
	.size	.Lfmt.1, 4

	.type	.Lfmt.2,@object         // @fmt.2
.Lfmt.2:
	.asciz	"%s\n"
	.size	.Lfmt.2, 4

	.section	".note.GNU-stack","",@progbits
