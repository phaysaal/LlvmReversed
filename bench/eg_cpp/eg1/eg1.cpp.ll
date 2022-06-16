; ModuleID = '/home/faisal/code/eg_cpp/eg1_LlvmRev/SlacData/LLVM/eg1.cpp.bc'
source_filename = "/home/faisal/code/eg_cpp/eg1//eg1.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.A = type { i32 }

$_ZN1A3setEi = comdat any

$_ZN1A3getEv = comdat any

@.str = private unnamed_addr constant [5 x i8] c"p==q\00", align 1
@.str.1 = private unnamed_addr constant [38 x i8] c"/home/faisal/code/eg_cpp/eg1//eg1.cpp\00", align 1
@__PRETTY_FUNCTION__.main = private unnamed_addr constant [11 x i8] c"int main()\00", align 1

; Function Attrs: mustprogress noinline norecurse optnone sspstrong uwtable
define dso_local i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  %p = alloca i32, align 4
  %q = alloca i32, align 4
  %a = alloca %class.A, align 4
  store i32 0, i32* %retval, align 4
  store i32 9, i32* %p, align 4
  %0 = load i32, i32* %p, align 4
  call void @_ZN1A3setEi(%class.A* nonnull align 4 dereferenceable(4) %a, i32 %0)
  %call = call i32 @_ZN1A3getEv(%class.A* nonnull align 4 dereferenceable(4) %a)
  store i32 %call, i32* %q, align 4
  %1 = load i32, i32* %p, align 4
  %2 = load i32, i32* %q, align 4
  %cmp = icmp eq i32 %1, %2
  br i1 %cmp, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  br label %cond.end

cond.false:                                       ; preds = %entry
  call void @__assert_fail(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i64 0, i64 0), i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.1, i64 0, i64 0), i32 19, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @__PRETTY_FUNCTION__.main, i64 0, i64 0)) #3
  unreachable

3:                                                ; No predecessors!
  br label %cond.end

cond.end:                                         ; preds = %3, %cond.true
  ret i32 0
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local void @_ZN1A3setEi(%class.A* nonnull align 4 dereferenceable(4) %this, i32 %y) #1 comdat align 2 {
entry:
  %this.addr = alloca %class.A*, align 8
  %y.addr = alloca i32, align 4
  store %class.A* %this, %class.A** %this.addr, align 8
  store i32 %y, i32* %y.addr, align 4
  %this1 = load %class.A*, %class.A** %this.addr, align 8
  %0 = load i32, i32* %y.addr, align 4
  %x = getelementptr inbounds %class.A, %class.A* %this1, i32 0, i32 0
  store i32 %0, i32* %x, align 4
  ret void
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local i32 @_ZN1A3getEv(%class.A* nonnull align 4 dereferenceable(4) %this) #1 comdat align 2 {
entry:
  %this.addr = alloca %class.A*, align 8
  store %class.A* %this, %class.A** %this.addr, align 8
  %this1 = load %class.A*, %class.A** %this.addr, align 8
  %x = getelementptr inbounds %class.A, %class.A* %this1, i32 0, i32 0
  %0 = load i32, i32* %x, align 4
  ret i32 %0
}

; Function Attrs: noreturn nounwind
declare void @__assert_fail(i8*, i8*, i32, i8*) #2

attributes #0 = { mustprogress noinline norecurse optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { mustprogress noinline nounwind optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { noreturn nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #3 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 13.0.1"}
