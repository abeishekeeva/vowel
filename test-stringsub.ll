; ModuleID = 'MicroC'
source_filename = "MicroC"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@string = private unnamed_addr constant [25 x i8] c"\22hello this is a string\22\00", align 1
@string.3 = private unnamed_addr constant [10 x i8] c"\22hello a\22\00", align 1

declare i8* @string_concat(i8*, i8*)

declare i32 @string_inequality(i8*, i8*)

declare i8** @string_sub(i8*, i8*)

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %s1 = alloca i8*
  %s2 = alloca i8*
  %arr = alloca i8**
  %i = alloca i32
  store i8* getelementptr inbounds ([25 x i8], [25 x i8]* @string, i32 0, i32 0), i8** %s1
  store i8* getelementptr inbounds ([10 x i8], [10 x i8]* @string.3, i32 0, i32 0), i8** %s2
  %s11 = load i8*, i8** %s1
  %s22 = load i8*, i8** %s2
  %string_sub = call i8** @string_sub(i8* %s11, i8* %s22)
  store i8** %string_sub, i8*** %arr
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %while_body, %entry
  %i6 = load i32, i32* %i
  %tmp7 = icmp slt i32 %i6, 3
  br i1 %tmp7, label %while_body, label %merge

while_body:                                       ; preds = %while
  %i3 = load i32, i32* %i
  %accpos = add i32 %i3, 0
  %arr4 = load i8**, i8*** %arr
  %acceltptr = getelementptr i8*, i8** %arr4, i32 %accpos
  %accelt = load i8*, i8** %acceltptr
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i8* %accelt)
  %i5 = load i32, i32* %i
  %tmp = add i32 %i5, 1
  store i32 %tmp, i32* %i
  br label %while

merge:                                            ; preds = %while
  ret i32 0
}
