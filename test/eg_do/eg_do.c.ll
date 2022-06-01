; ModuleID = '/home/faisal/code/eg_c/eg_do/SlacData/LLVM/eg_do.c.bc'
source_filename = "/home/faisal/code/eg_c/eg_do//eg_do.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f() #0 {
entry:
  %i = alloca i32, align 4
  %n = alloca i32, align 4
  store i32 0, i32* %i, align 4
  store i32 10, i32* %n, align 4
  br label %do.body

do.body:                                          ; preds = %do.cond, %entry
  %0 = load i32, i32* %i, align 4
  %inc = add nsw i32 %0, 1
  store i32 %inc, i32* %i, align 4
  br label %do.cond

do.cond:                                          ; preds = %do.body
  %1 = load i32, i32* %i, align 4
  %2 = load i32, i32* %n, align 4
  %cmp = icmp slt i32 %1, %2
  br i1 %cmp, label %do.body, label %do.end, !llvm.loop !4

do.end:                                           ; preds = %do.cond
  %3 = load i32, i32* %i, align 4
  ret i32 %3
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @g() #0 {
entry:
  %i = alloca i32, align 4
  %n = alloca i32, align 4
  store i32 0, i32* %i, align 4
  store i32 10, i32* %n, align 4
  br label %do.body

do.body:                                          ; preds = %do.cond, %entry
  %0 = load i32, i32* %i, align 4
  %inc = add nsw i32 %0, 1
  store i32 %inc, i32* %i, align 4
  %1 = load i32, i32* %i, align 4
  %cmp = icmp sgt i32 %1, 3
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %do.body
  br label %do.end

if.end:                                           ; preds = %do.body
  br label %do.cond

do.cond:                                          ; preds = %if.end
  %2 = load i32, i32* %i, align 4
  %3 = load i32, i32* %n, align 4
  %cmp1 = icmp slt i32 %2, %3
  br i1 %cmp1, label %do.body, label %do.end, !llvm.loop !6

do.end:                                           ; preds = %do.cond, %if.then
  %4 = load i32, i32* %i, align 4
  ret i32 %4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @g1() #0 {
entry:
  %i = alloca i32, align 4
  %n = alloca i32, align 4
  store i32 0, i32* %i, align 4
  store i32 10, i32* %n, align 4
  br label %do.body

do.body:                                          ; preds = %do.cond, %entry
  %0 = load i32, i32* %i, align 4
  %inc = add nsw i32 %0, 1
  store i32 %inc, i32* %i, align 4
  %1 = load i32, i32* %i, align 4
  %cmp = icmp sgt i32 %1, 3
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %do.body
  br label %do.end

if.else:                                          ; preds = %do.body
  %2 = load i32, i32* %i, align 4
  %add = add nsw i32 %2, 2
  store i32 %add, i32* %i, align 4
  br label %if.end

if.end:                                           ; preds = %if.else
  br label %do.cond

do.cond:                                          ; preds = %if.end
  %3 = load i32, i32* %i, align 4
  %4 = load i32, i32* %n, align 4
  %cmp1 = icmp slt i32 %3, %4
  br i1 %cmp1, label %do.body, label %do.end, !llvm.loop !7

do.end:                                           ; preds = %do.cond, %if.then
  %5 = load i32, i32* %i, align 4
  ret i32 %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @g2() #0 {
entry:
  %i = alloca i32, align 4
  %n = alloca i32, align 4
  store i32 0, i32* %i, align 4
  store i32 10, i32* %n, align 4
  br label %do.body

do.body:                                          ; preds = %do.cond, %entry
  %0 = load i32, i32* %i, align 4
  %inc = add nsw i32 %0, 1
  store i32 %inc, i32* %i, align 4
  %1 = load i32, i32* %i, align 4
  %cmp = icmp sgt i32 %1, 3
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %do.body
  %2 = load i32, i32* %n, align 4
  %add = add nsw i32 %2, 2
  store i32 %add, i32* %n, align 4
  %3 = load i32, i32* %i, align 4
  %add1 = add nsw i32 %3, 3
  store i32 %add1, i32* %i, align 4
  br label %if.end

if.else:                                          ; preds = %do.body
  %4 = load i32, i32* %n, align 4
  %inc2 = add nsw i32 %4, 1
  store i32 %inc2, i32* %n, align 4
  br label %do.end

if.end:                                           ; preds = %if.then
  br label %do.cond

do.cond:                                          ; preds = %if.end
  %5 = load i32, i32* %i, align 4
  %6 = load i32, i32* %n, align 4
  %cmp3 = icmp slt i32 %5, %6
  br i1 %cmp3, label %do.body, label %do.end, !llvm.loop !8

do.end:                                           ; preds = %do.cond, %if.else
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
!6 = distinct !{!6, !5}
!7 = distinct !{!7, !5}
!8 = distinct !{!8, !5}
