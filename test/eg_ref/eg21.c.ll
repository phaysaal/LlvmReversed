; ModuleID = '/home/faisal/code/eg_c/eg21/SlacData/LLVM/eg21.c.bc'
source_filename = "/home/faisal/code/eg_c/eg21//eg21.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@.str = private unnamed_addr constant [5 x i8] c"m==n\00", align 1
@.str.1 = private unnamed_addr constant [36 x i8] c"/home/faisal/code/eg_c/eg21//eg21.c\00", align 1
@__PRETTY_FUNCTION__.main = private unnamed_addr constant [11 x i8] c"int main()\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f1(i32* %z) #0 {
entry:
  %z.addr = alloca i32*, align 8
  store i32* %z, i32** %z.addr, align 8
  %0 = load i32*, i32** %z.addr, align 8
  %1 = load i32, i32* %0, align 4
  %add = add nsw i32 %1, 1
  %2 = load i32*, i32** %z.addr, align 8
  store i32 %add, i32* %2, align 4
  %3 = load i32*, i32** %z.addr, align 8
  %4 = load i32, i32* %3, align 4
  ret i32 %4
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f2(i32 %x) #0 {
entry:
  %x.addr = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  %0 = load i32, i32* %x.addr, align 4
  %add = add nsw i32 %0, 1
  store i32 %add, i32* %x.addr, align 4
  %1 = load i32, i32* %x.addr, align 4
  ret i32 %1
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  %n = alloca i32, align 4
  %m = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  store i32 8, i32* %n, align 4
  %call = call i32 @f1(i32* %n)
  store i32 %call, i32* %m, align 4
  %0 = load i32, i32* %m, align 4
  %1 = load i32, i32* %n, align 4
  %cmp = icmp eq i32 %0, %1
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  br label %if.end

if.else:                                          ; preds = %entry
  call void @__assert_fail(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i64 0, i64 0), i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.1, i64 0, i64 0), i32 17, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @__PRETTY_FUNCTION__.main, i64 0, i64 0)) #2
  unreachable

if.end:                                           ; preds = %if.then
  ret i32 0
}

; Function Attrs: noreturn nounwind
declare dso_local void @__assert_fail(i8*, i8*, i32, i8*) #1

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { noreturn nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 1}
!2 = !{i32 7, !"frame-pointer", i32 2}
!3 = !{!"clang version 13.0.0"}
