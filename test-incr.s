	.text
	.file	"MicroC"
	.globl	test_incr               // -- Begin function test_incr
	.p2align	2
	.type	test_incr,@function
test_incr:                              // @test_incr
	.cfi_startproc
// %bb.0:                               // %entry
	str	x30, [sp, #-16]!        // 8-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -16
	adrp	x0, .Lfmt
	mov	w8, #45
	add	x0, x0, :lo12:.Lfmt
	mov	w1, #45
	str	w8, [sp, #12]
	bl	printf
	mov	w0, wzr
	ldr	x30, [sp], #16          // 8-byte Folded Reload
	ret
.Lfunc_end0:
	.size	test_incr, .Lfunc_end0-test_incr
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
