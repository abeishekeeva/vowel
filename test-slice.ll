; ModuleID = 'MicroC'
source_filename = "MicroC"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@string = private unnamed_addr constant [17 x i8] c"\22thisismystring\22\00", align 1

declare i8* @string_concat(i8*, i8*)

declare i8* @Slice(i8*, i32, i32)

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %r = alloca i8*
  %k = alloca i8*
  store i8* getelementptr inbounds ([17 x i8], [17 x i8]* @string, i32 0, i32 0), i8** %r
  %r1 = load i8*, i8** %r
  %Slice = call i8* @Slice(i8* %r1, i32 2, i32 7)
  store i8* %Slice, i8** %k
  %k2 = load i8*, i8** %k
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i8* %k2)
  ret i32 0
}
