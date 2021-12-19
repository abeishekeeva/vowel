; ModuleID = 'MicroC'
source_filename = "MicroC"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i8* @string_concat(i8*, i8*)

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %a = alloca i32*
  %arr = alloca i32*, i32 4
  %arrptr = bitcast i32** %arr to i32*
  %arrelt = getelementptr i32, i32* %arrptr, i32 0
  store i32 1, i32* %arrelt
  %arrelt1 = getelementptr i32, i32* %arrptr, i32 1
  store i32 2, i32* %arrelt1
  %arrelt2 = getelementptr i32, i32* %arrptr, i32 2
  store i32 3, i32* %arrelt2
  %arrlast = getelementptr i32, i32* %arrptr, i32 3
  store i32 0, i32* %arrlast
  store i32* %arrptr, i32** %a
  ret i32 0
}
