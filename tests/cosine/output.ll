; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_float(i32)

define float @cosine(float %x) {
entry:
  %alt = alloca float, align 4
  %eps = alloca float, align 4
  %term = alloca float, align 4
  %n = alloca float, align 4
  %cos = alloca float, align 4
  %x1 = alloca float, align 4
  store float %x, ptr %x1, align 4
  store float 0x3EB0C6F7A0000000, ptr %eps, align 4
  store float 1.000000e+00, ptr %n, align 4
  store float 1.000000e+00, ptr %cos, align 4
  store float 1.000000e+00, ptr %term, align 4
  store float -1.000000e+00, ptr %alt, align 4
  br label %before

before:                                           ; preds = %loop, %entry
  %term2 = load float, ptr %term, align 4
  %eps3 = load float, ptr %eps, align 4
  %gttmp = fcmp ogt float %term2, %eps3
  %whilecond = icmp ne i1 %gttmp, false
  br i1 %whilecond, label %loop, label %end

loop:                                             ; preds = %before
  %term4 = load float, ptr %term, align 4
  %n5 = load float, ptr %n, align 4
  %addtmp = fadd float %n5, 1.000000e+00
  %divtmp = fdiv float %term4, %addtmp
  %n6 = load float, ptr %n, align 4
  %divtmp7 = fdiv float %divtmp, %n6
  %x8 = load float, ptr %x1, align 4
  %multmp = fmul float %divtmp7, %x8
  %x9 = load float, ptr %x1, align 4
  %multmp10 = fmul float %multmp, %x9
  store float %multmp10, ptr %term, align 4
  %cos11 = load float, ptr %cos, align 4
  %alt12 = load float, ptr %alt, align 4
  %term13 = load float, ptr %term, align 4
  %multmp14 = fmul float %alt12, %term13
  %addtmp15 = fadd float %cos11, %multmp14
  store float %addtmp15, ptr %cos, align 4
  %alt16 = load float, ptr %alt, align 4
  %0 = fneg float %alt16
  store float %0, ptr %alt, align 4
  %n17 = load float, ptr %n, align 4
  %addtmp18 = fadd float %n17, 2.000000e+00
  store float %addtmp18, ptr %n, align 4
  br label %before
  br label %end

end:                                              ; preds = %loop, %before
  %cos19 = load float, ptr %cos, align 4
  %calltmp = call i32 @print_float(float %cos19)
  %cos20 = load float, ptr %cos, align 4
  ret float %cos20
  ret float 0.000000e+00
}
