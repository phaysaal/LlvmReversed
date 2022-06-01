; ModuleID = '/home/faisal/code/eg_c/eg1/SlacData/LLVM/eg1.c.bc'
source_filename = "/home/faisal/code/eg_c/eg1//eg1.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @cond_in_fcall(i32 %n) #0 {
entry:
  %n.addr = alloca i32, align 4
  %i = alloca i32, align 4
  store i32 %n, i32* %n.addr, align 4
  store i32 0, i32* %i, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %0 = load i32, i32* %i, align 4
  %cmp = icmp slt i32 %0, 10
  br i1 %cmp, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %1 = load i32, i32* %n.addr, align 4
  %cmp1 = icmp sgt i32 %1, 10
  br i1 %cmp1, label %land.rhs, label %land.end

land.rhs:                                         ; preds = %for.body
  %2 = load i32, i32* %n.addr, align 4
  %cmp2 = icmp slt i32 %2, 100000
  br label %land.end

land.end:                                         ; preds = %land.rhs, %for.body
  %3 = phi i1 [ false, %for.body ], [ %cmp2, %land.rhs ]
  %land.ext = zext i1 %3 to i32
  %call = call i32 (i32, ...) bitcast (i32 (...)* @assume_abort_if_not to i32 (i32, ...)*)(i32 %land.ext)
  br label %for.inc

for.inc:                                          ; preds = %land.end
  %4 = load i32, i32* %i, align 4
  %inc = add nsw i32 %4, 1
  store i32 %inc, i32* %i, align 4
  br label %for.cond, !llvm.loop !4

for.end:                                          ; preds = %for.cond
  ret i32 0
}

declare dso_local i32 @assume_abort_if_not(...) #1

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 1}
!2 = !{i32 7, !"frame-pointer", i32 2}
!3 = !{!"clang version 13.0.0"}
!4 = distinct !{!4, !5}
!5 = !{!"llvm.loop.mustprogress"}
