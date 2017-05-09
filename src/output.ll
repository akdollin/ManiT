; ModuleID = 'ManiT'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@0 = private unnamed_addr constant [9 x i8] c"demo2.mt\00"
@1 = private unnamed_addr constant [4 x i8] c"cat\00"
@2 = private unnamed_addr constant [4 x i8] c"cat\00"

declare i32 @printf(i8*, ...)

declare i8* @fopen(i8*, i8*)

declare i32 @fclose(i8*)

declare i32 @fputs(i32, i8*)

declare i8* @fgets(i8*, i32, i8*)

declare i32 @fwrite(i8*, i32, i32, i8*)

declare i32 @fread(i8*, i32, i32, i8*)

declare i32 @strlen(i8*)

declare i32 @fork()

declare i32 @sleep(i32)

declare i32 @execlp(i8*, i8*, i8*, i32)

define i32 @main() {
entry:
  %sleep = call i32 @sleep(i32 1)
  %execlp = call i32 @execlp(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @2, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @1, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @0, i32 0, i32 0), i32 0)
  %sleep1 = call i32 @sleep(i32 1)
  ret i32 0
}
