	.text
	.file	"MicroC"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	.Lstring(%rip), %rdi
	movq	%rdi, 16(%rsp)
	leaq	.Lstring.3(%rip), %rsi
	movq	%rsi, 8(%rsp)
	callq	string_equality@PLT
	xorl	%esi, %esi
	testl	%eax, %eax
	setne	%sil
	leaq	.Lfmt(%rip), %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	xorl	%eax, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         # @fmt.1
.Lfmt.1:
	.asciz	"%g\n"
	.size	.Lfmt.1, 4

	.type	.Lfmt.2,@object         # @fmt.2
.Lfmt.2:
	.asciz	"%s\n"
	.size	.Lfmt.2, 4

	.type	.Lstring,@object        # @string
.Lstring:
	.asciz	"\"Hell\""
	.size	.Lstring, 7

	.type	.Lstring.3,@object      # @string.3
.Lstring.3:
	.asciz	"\"Hello\""
	.size	.Lstring.3, 8

	.section	".note.GNU-stack","",@progbits
