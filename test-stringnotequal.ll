; ModuleID = 'MicroC'
source_filename = "MicroC"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@string = private unnamed_addr constant [7 x i8] c"\22Hell\22\00", align 1
@string.3 = private unnamed_addr constant [8 x i8] c"\22Hello\22\00", align 1

declare i8* @string_concat(i8*, i8*)

declare i1 @string_equality(i8*, i8*)

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %a = alloca i8*
  %b = alloca i8*
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @string, i32 0, i32 0), i8** %a
  store i8* getelementptr inbounds ([8 x i8], [8 x i8]* @string.3, i32 0, i32 0), i8** %b
  %a1 = load i8*, i8** %a
  %b2 = load i8*, i8** %b
  %string_equality = call i1 @string_equality(i8* %a1, i8* %b2)
  %tmp = icmp ne i1 false, %string_equality
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i1 %tmp)
  ret i32 0
}
