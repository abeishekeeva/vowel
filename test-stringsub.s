	.text
	.file	"MicroC"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -16
	leaq	.Lstring(%rip), %rdi
	movq	%rdi, 24(%rsp)
	leaq	.Lstring.3(%rip), %rsi
	movq	%rsi, 16(%rsp)
	callq	string_sub@PLT
	movq	%rax, 8(%rsp)
	movl	$0, 4(%rsp)
	leaq	.Lfmt.2(%rip), %rbx
	cmpl	$2, 4(%rsp)
	jg	.LBB0_3
	.p2align	4, 0x90
.LBB0_2:                                # %while_body
                                        # =>This Inner Loop Header: Depth=1
	movslq	4(%rsp), %rax
	movq	8(%rsp), %rcx
	movq	(%rcx,%rax,8), %rsi
	movq	%rbx, %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	incl	4(%rsp)
	cmpl	$2, 4(%rsp)
	jle	.LBB0_2
.LBB0_3:                                # %merge
	xorl	%eax, %eax
	addq	$32, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
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
	.asciz	"\"hello this is a string\""
	.size	.Lstring, 25

	.type	.Lstring.3,@object      # @string.3
.Lstring.3:
	.asciz	"\"hello a\""
	.size	.Lstring.3, 10

	.section	".note.GNU-stack","",@progbits
