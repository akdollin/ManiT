; ModuleID = 'ManiT'

%test = type { i32 }

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@a = global i32 0
@b = global i8* null
@0 = private unnamed_addr constant [7 x i8] c"string\00"
@c = global i1 false
@d = global double 0.000000e+00
@1 = private unnamed_addr constant [6 x i8] c"hello\00"
@tester = global %test zeroinitializer
@arr = global [3 x i32] zeroinitializer

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  store i32 4, i32* @a
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @0, i32 0, i32 0), i8** @b
  store i1 true, i1* @c
  store double 1.100000e+00, double* @d
  store i32 5, i32* @a
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @1, i32 0, i32 0), i8** @b
  store i1 false, i1* @c
  store double 2.100000e+00, double* @d
  store i32 4, i32* getelementptr inbounds (%test, %test* @tester, i32 0, i32 0)
  store [3 x i32] [i32 3, i32 2, i32 1], [3 x i32]* @arr
  store i32 10, i32* getelementptr inbounds ([3 x i32], [3 x i32]* @arr, i32 0, i32 0)
  %struct_access = load i32, i32* getelementptr inbounds (%test, %test* @tester, i32 0, i32 0)
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %struct_access)
  %loaded = load [3 x i32], [3 x i32]* @arr
  %array_access = load i32, i32* getelementptr inbounds ([3 x i32], [3 x i32]* @arr, i32 0, i32 0)
  %printf1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %array_access)
  ret i32 0
}
