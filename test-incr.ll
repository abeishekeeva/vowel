; ModuleID = 'MicroC'
source_filename = "MicroC"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @test_incr() {
entry:
  %a = alloca i32
  store i32 42, i32* %a
  %a1 = load i32, i32* %a
  %a2 = add i32 3, %a1
  store i32 %a2, i32* %a
  %a3 = load i32, i32* %a
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %a3)
  ret i32 0
}
