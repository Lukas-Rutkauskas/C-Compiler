; ModuleID = 'mini-c'
source_filename = "mini-c"

define float @pi() {
entry:
  %i = alloca i32, align 4
  %PI = alloca float, align 4
  %flag = alloca i1, align 1
  store i1 true, ptr %flag, align 1
  store float 3.000000e+00, ptr %PI, align 4
  store i32 2, ptr %i, align 4
  br label %before

before:                                           ; preds = %end, %entry
  %i1 = load i32, ptr %i, align 4
  %lttmp = icmp slt i32 %i1, 100
  %whilecond = icmp ne i1 %lttmp, false
  br i1 %whilecond, label %loop, label %end23

loop:                                             ; preds = %before
  %flag2 = load i1, ptr %flag, align 1
  %ifcond = icmp ne i1 %flag2, false
  br i1 %ifcond, label %then, label %"else then"

then:                                             ; preds = %loop
  %PI3 = load float, ptr %PI, align 4
  %i4 = load i32, ptr %i, align 4
  %i5 = load i32, ptr %i, align 4
  %addtmp = add i32 %i5, 2
  %multmp = mul i32 %i4, %addtmp
  %i6 = load i32, ptr %i, align 4
  %addtmp7 = add i32 %i6, 1
  %multmp8 = mul i32 %multmp, %addtmp7
  %inttofp = sitofp i32 %multmp8 to float
  %divtmp = fdiv float 4.000000e+00, %inttofp
  %addtmp9 = fadd float %PI3, %divtmp
  store float %addtmp9, ptr %PI, align 4
  br label %end

"else then":                                      ; preds = %loop
  %PI10 = load float, ptr %PI, align 4
  %i11 = load i32, ptr %i, align 4
  %i12 = load i32, ptr %i, align 4
  %addtmp13 = add i32 %i12, 2
  %multmp14 = mul i32 %i11, %addtmp13
  %i15 = load i32, ptr %i, align 4
  %addtmp16 = add i32 %i15, 1
  %multmp17 = mul i32 %multmp14, %addtmp16
  %inttofp18 = sitofp i32 %multmp17 to float
  %divtmp19 = fdiv float 4.000000e+00, %inttofp18
  %subtmp = fsub float %PI10, %divtmp19
  store float %subtmp, ptr %PI, align 4
  br label %end

end:                                              ; preds = %"else then", %then
  %flag20 = load i1, ptr %flag, align 1
  %0 = xor i1 %flag20, true
  store i1 %0, ptr %flag, align 1
  %i21 = load i32, ptr %i, align 4
  %addtmp22 = add i32 %i21, 2
  store i32 %addtmp22, ptr %i, align 4
  br label %before
  br label %end23

end23:                                            ; preds = %end, %before
  %PI24 = load float, ptr %PI, align 4
  ret float %PI24
  ret float 0.000000e+00
}
