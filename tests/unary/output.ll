; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

declare i32 @print_float(i32)

define float @unary(i32 %n, float %m) {
entry:
  %sum = alloca float, align 4
  %result = alloca float, align 4
  %m2 = alloca float, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store float %m, ptr %m2, align 4
  store float 0.000000e+00, ptr %sum, align 4
  %n3 = load i32, ptr %n1, align 4
  %m4 = load float, ptr %m2, align 4
  %inttofp = sitofp i32 %n3 to float
  %addtmp = fadd float %inttofp, %m4
  store float %addtmp, ptr %result, align 4
  %result5 = load float, ptr %result, align 4
  %calltmp = call i32 @print_float(float %result5)
  %sum6 = load float, ptr %sum, align 4
  %result7 = load float, ptr %result, align 4
  %addtmp8 = fadd float %sum6, %result7
  store float %addtmp8, ptr %sum, align 4
  %n9 = load i32, ptr %n1, align 4
  %m10 = load float, ptr %m2, align 4
  %0 = fneg float %m10
  %inttofp11 = sitofp i32 %n9 to float
  %addtmp12 = fadd float %inttofp11, %0
  store float %addtmp12, ptr %result, align 4
  %result13 = load float, ptr %result, align 4
  %calltmp14 = call i32 @print_float(float %result13)
  %sum15 = load float, ptr %sum, align 4
  %result16 = load float, ptr %result, align 4
  %addtmp17 = fadd float %sum15, %result16
  store float %addtmp17, ptr %sum, align 4
  %n18 = load i32, ptr %n1, align 4
  %m19 = load float, ptr %m2, align 4
  %1 = fneg float %m19
  %2 = fneg float %1
  %inttofp20 = sitofp i32 %n18 to float
  %addtmp21 = fadd float %inttofp20, %2
  store float %addtmp21, ptr %result, align 4
  %result22 = load float, ptr %result, align 4
  %calltmp23 = call i32 @print_float(float %result22)
  %sum24 = load float, ptr %sum, align 4
  %result25 = load float, ptr %result, align 4
  %addtmp26 = fadd float %sum24, %result25
  store float %addtmp26, ptr %sum, align 4
  %n27 = load i32, ptr %n1, align 4
  %3 = sub i32 0, %n27
  %m28 = load float, ptr %m2, align 4
  %4 = fneg float %m28
  %inttofp29 = sitofp i32 %3 to float
  %addtmp30 = fadd float %inttofp29, %4
  store float %addtmp30, ptr %result, align 4
  %result31 = load float, ptr %result, align 4
  %calltmp32 = call i32 @print_float(float %result31)
  %sum33 = load float, ptr %sum, align 4
  %result34 = load float, ptr %result, align 4
  %addtmp35 = fadd float %sum33, %result34
  store float %addtmp35, ptr %sum, align 4
  %sum36 = load float, ptr %sum, align 4
  ret float %sum36
  ret float 0.000000e+00
}
