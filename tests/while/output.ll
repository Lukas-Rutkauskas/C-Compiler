; ModuleID = 'mini-c'
source_filename = "mini-c"

@0 = common global i32 0
@1 = common global float 0.000000e+00
@2 = common global i1 false

declare i32 @print_int(i32)

define i32 @While(i32 %n) {
entry:
  %result = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 12, ptr @0, align 4
  store i32 0, ptr %result, align 4
  %test = load i32, ptr @0, align 4
  %calltmp = call i32 @print_int(i32 %test)
  br label %before

before:                                           ; preds = %loop, %entry
  %result2 = load i32, ptr %result, align 4
  %lttmp = icmp slt i32 %result2, 10
  %whilecond = icmp ne i1 %lttmp, false
  br i1 %whilecond, label %loop, label %end

loop:                                             ; preds = %before
  %result3 = load i32, ptr %result, align 4
  %addtmp = add i32 %result3, 1
  store i32 %addtmp, ptr %result, align 4
  br label %before
  br label %end

end:                                              ; preds = %loop, %before
  %result4 = load i32, ptr %result, align 4
  ret i32 %result4
  ret i32 0
}
