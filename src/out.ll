; ModuleID = 'ManiT'

@a = global i32 0
@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  store i32 2, i32* @a
  ret i32 0
}
