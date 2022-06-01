; ModuleID = '/home/faisal/code/eg_c/eg_while/SlacData/LLVM/eg_while.c.bc'
source_filename = "/home/faisal/code/eg_c/eg_while//eg_while.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @g() #0 {
entry:
  %i = alloca i32, align 4
  %n = alloca i32, align 4
  store i32 0, i32* %i, align 4
  store i32 10, i32* %n, align 4
  br label %while.cond

while.cond:                                       ; preds = %if.end, %entry
  %0 = load i32, i32* %i, align 4
  %1 = load i32, i32* %n, align 4
  %cmp = icmp slt i32 %0, %1
  br i1 %cmp, label %while.body, label %while.end

while.body:                                       ; preds = %while.cond
  %2 = load i32, i32* %i, align 4
  %inc = add nsw i32 %2, 1
  store i32 %inc, i32* %i, align 4
  %3 = load i32, i32* %i, align 4
  %cmp1 = icmp sgt i32 %3, 3
  br i1 %cmp1, label %if.then, label %if.end

if.then:                                          ; preds = %while.body
  %4 = load i32, i32* %i, align 4
  %add = add nsw i32 %4, 2
  store i32 %add, i32* %i, align 4
  br label %while.end

if.end:                                           ; preds = %while.body
  %5 = load i32, i32* %i, align 4
  %add2 = add nsw i32 %5, 4
  store i32 %add2, i32* %i, align 4
  br label %while.cond, !llvm.loop !4

while.end:                                        ; preds = %if.then, %while.cond
  %6 = load i32, i32* %n, align 4
  %dec = add nsw i32 %6, -1
  store i32 %dec, i32* %n, align 4
  %7 = load i32, i32* %i, align 4
  ret i32 %7
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 1}
!2 = !{i32 7, !"frame-pointer", i32 2}
!3 = !{!"clang version 13.0.0"}
!4 = distinct !{!4, !5}
!5 = !{!"llvm.loop.mustprogress"}
