; ModuleID = '/home/faisal/code/eg_c/eg_and/SlacData/LLVM/egand.c.bc'
source_filename = "/home/faisal/code/eg_c/eg_and//egand.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f1() #0 {
entry:
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 2, i32* %x, align 4
  store i32 3, i32* %y, align 4
  %0 = load i32, i32* %x, align 4
  %cmp = icmp slt i32 %0, 3
  br i1 %cmp, label %land.lhs.true, label %if.else

land.lhs.true:                                    ; preds = %entry
  %1 = load i32, i32* %y, align 4
  %cmp1 = icmp slt i32 %1, 5
  br i1 %cmp1, label %land.lhs.true2, label %if.else

land.lhs.true2:                                   ; preds = %land.lhs.true
  %2 = load i32, i32* %x, align 4
  %cmp3 = icmp sgt i32 %2, 0
  br i1 %cmp3, label %if.then, label %if.else

if.then:                                          ; preds = %land.lhs.true2
  store i32 9, i32* %x, align 4
  br label %if.end

if.else:                                          ; preds = %land.lhs.true2, %land.lhs.true, %entry
  store i32 9, i32* %y, align 4
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret i32 0
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f2() #0 {
entry:
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 2, i32* %x, align 4
  store i32 3, i32* %y, align 4
  %0 = load i32, i32* %x, align 4
  %cmp = icmp slt i32 %0, 3
  br i1 %cmp, label %if.then, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %entry
  %1 = load i32, i32* %y, align 4
  %cmp1 = icmp slt i32 %1, 5
  br i1 %cmp1, label %if.then, label %if.else

if.then:                                          ; preds = %lor.lhs.false, %entry
  store i32 9, i32* %x, align 4
  br label %if.end

if.else:                                          ; preds = %lor.lhs.false
  store i32 9, i32* %y, align 4
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret i32 0
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f3() #0 {
entry:
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 2, i32* %x, align 4
  store i32 3, i32* %y, align 4
  %0 = load i32, i32* %x, align 4
  %cmp = icmp slt i32 %0, 3
  br i1 %cmp, label %if.then, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %entry
  %1 = load i32, i32* %y, align 4
  %cmp1 = icmp slt i32 %1, 5
  br i1 %cmp1, label %if.then, label %lor.lhs.false2

lor.lhs.false2:                                   ; preds = %lor.lhs.false
  %2 = load i32, i32* %x, align 4
  %cmp3 = icmp sgt i32 %2, 0
  br i1 %cmp3, label %if.then, label %if.else

if.then:                                          ; preds = %lor.lhs.false2, %lor.lhs.false, %entry
  store i32 9, i32* %x, align 4
  br label %if.end

if.else:                                          ; preds = %lor.lhs.false2
  store i32 9, i32* %y, align 4
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret i32 0
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f4() #0 {
entry:
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 2, i32* %x, align 4
  store i32 3, i32* %y, align 4
  %0 = load i32, i32* %x, align 4
  %cmp = icmp slt i32 %0, 3
  br i1 %cmp, label %land.lhs.true, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %entry
  %1 = load i32, i32* %y, align 4
  %cmp1 = icmp slt i32 %1, 5
  br i1 %cmp1, label %land.lhs.true, label %if.else

land.lhs.true:                                    ; preds = %lor.lhs.false, %entry
  %2 = load i32, i32* %x, align 4
  %cmp2 = icmp sgt i32 %2, 0
  br i1 %cmp2, label %if.then, label %if.else

if.then:                                          ; preds = %land.lhs.true
  store i32 9, i32* %x, align 4
  br label %if.end

if.else:                                          ; preds = %land.lhs.true, %lor.lhs.false
  store i32 9, i32* %y, align 4
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret i32 0
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f5() #0 {
entry:
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  store i32 2, i32* %x, align 4
  store i32 3, i32* %y, align 4
  %0 = load i32, i32* %x, align 4
  %cmp = icmp slt i32 %0, 3
  br i1 %cmp, label %land.lhs.true, label %lor.lhs.false

land.lhs.true:                                    ; preds = %entry
  %1 = load i32, i32* %y, align 4
  %cmp1 = icmp slt i32 %1, 5
  br i1 %cmp1, label %if.then, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %land.lhs.true, %entry
  %2 = load i32, i32* %x, align 4
  %cmp2 = icmp sgt i32 %2, 0
  br i1 %cmp2, label %if.then, label %if.else

if.then:                                          ; preds = %lor.lhs.false, %land.lhs.true
  store i32 9, i32* %x, align 4
  br label %if.end

if.else:                                          ; preds = %lor.lhs.false
  store i32 9, i32* %y, align 4
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 1}
!2 = !{i32 7, !"frame-pointer", i32 2}
!3 = !{!"clang version 13.0.0"}
