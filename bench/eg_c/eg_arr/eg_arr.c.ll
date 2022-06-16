; ModuleID = 'eg_arr.c.bc'
source_filename = "test/eg_arr//eg_arr.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@bb = dso_local global [3 x i32] zeroinitializer, align 4

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @f1() #0 {
entry:
  %b = alloca [3 x i32], align 4
  %c = alloca i32, align 4
  %p = alloca i32*, align 8
  %arrayidx = getelementptr inbounds [3 x i32], [3 x i32]* %b, i64 0, i64 0
  store i32 3, i32* %arrayidx, align 4
  %arrayidx1 = getelementptr inbounds [3 x i32], [3 x i32]* %b, i64 0, i64 2
  store i32 4, i32* %arrayidx1, align 4
  %arrayidx2 = getelementptr inbounds [3 x i32], [3 x i32]* %b, i64 0, i64 2
  %0 = load i32, i32* %arrayidx2, align 4
  store i32 %0, i32* %c, align 4
  %arrayidx3 = getelementptr inbounds [3 x i32], [3 x i32]* %b, i64 0, i64 0
  %1 = load i32, i32* %arrayidx3, align 4
  %2 = load i32, i32* %c, align 4
  %cmp = icmp slt i32 %1, %2
  %conv = zext i1 %cmp to i32
  %call = call i32 (i32, ...) bitcast (i32 (...)* @assert to i32 (i32, ...)*)(i32 %conv)
  %call4 = call align 16 i8* @malloc(i64 4)
  %3 = bitcast i8* %call4 to i32*
  store i32* %3, i32** %p, align 8
  %4 = load i32*, i32** %p, align 8
  store i32 10, i32* %4, align 4
  %5 = load i32*, i32** %p, align 8
  %6 = load i32, i32* %5, align 4
  store i32 %6, i32* %c, align 4
  %7 = load i32, i32* %c, align 4
  %cmp5 = icmp eq i32 %7, 10
  %conv6 = zext i1 %cmp5 to i32
  %call7 = call i32 (i32, ...) bitcast (i32 (...)* @assert to i32 (i32, ...)*)(i32 %conv6)
  ret i32 0
}

declare i32 @assert(...) #1

declare align 16 i8* @malloc(i64) #1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  %x = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  %call = call i32 @f1()
  store i32 %call, i32* %x, align 4
  ret i32 0
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 13.0.1"}
