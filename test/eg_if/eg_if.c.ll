; ModuleID = '/home/faisal/code/eg_c/eg_if/SlacData/LLVM/eg_if.c.bc'
source_filename = "/home/faisal/code/eg_c/eg_if//eg_if.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @ifonly(i32 %x, i32 %y) #0 {
entry:
  %x.addr = alloca i32, align 4
  %y.addr = alloca i32, align 4
  %n = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  store i32 %y, i32* %y.addr, align 4
  store i32 0, i32* %n, align 4
  %0 = load i32, i32* %n, align 4
  %inc = add nsw i32 %0, 1
  store i32 %inc, i32* %n, align 4
  %1 = load i32, i32* %x.addr, align 4
  %2 = load i32, i32* %y.addr, align 4
  %cmp = icmp slt i32 %1, %2
  br i1 %cmp, label %if.then, label %if.end

if.then:                                          ; preds = %entry
  store i32 1, i32* %n, align 4
  br label %if.end

if.end:                                           ; preds = %if.then, %entry
  %3 = load i32, i32* %x.addr, align 4
  %4 = load i32, i32* %n, align 4
  %add = add nsw i32 %4, %3
  store i32 %add, i32* %n, align 4
  %5 = load i32, i32* %n, align 4
  ret i32 %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @ifelse(i32 %x, i32 %y) #0 {
entry:
  %x.addr = alloca i32, align 4
  %y.addr = alloca i32, align 4
  %n = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  store i32 %y, i32* %y.addr, align 4
  store i32 0, i32* %n, align 4
  %0 = load i32, i32* %x.addr, align 4
  %1 = load i32, i32* %y.addr, align 4
  %cmp = icmp slt i32 %0, %1
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  store i32 1, i32* %n, align 4
  br label %if.end

if.else:                                          ; preds = %entry
  store i32 0, i32* %n, align 4
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %2 = load i32, i32* %n, align 4
  ret i32 %2
}

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 1}
!2 = !{i32 7, !"frame-pointer", i32 2}
!3 = !{!"clang version 13.0.0"}
