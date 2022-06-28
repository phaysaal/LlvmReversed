; ModuleID = 'bench/eg_cpp/eg_inheritence_LlvmRev/SlacData/LLVM/eg_inheritence.cpp.bc'
source_filename = "bench/eg_cpp/eg_inheritence//eg_inheritence.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.C = type { %class.D, i32 }
%class.D = type { i32, i32 }

$_ZN1D4setDEi = comdat any

$_ZN1D4getDEv = comdat any

@.str = private unnamed_addr constant [8 x i8] c"q == 10\00", align 1
@.str.1 = private unnamed_addr constant [48 x i8] c"bench/eg_cpp/eg_inheritence//eg_inheritence.cpp\00", align 1
@__PRETTY_FUNCTION__.main = private unnamed_addr constant [11 x i8] c"int main()\00", align 1

; Function Attrs: mustprogress noinline norecurse optnone sspstrong uwtable
define dso_local i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  %c = alloca %class.C, align 4
  %q = alloca i32, align 4
  store i32 0, i32* %retval, align 4
  %0 = bitcast %class.C* %c to %class.D*
  call void @_ZN1D4setDEi(%class.D* nonnull align 4 dereferenceable(8) %0, i32 10)
  %1 = bitcast %class.C* %c to %class.D*
  %call = call i32 @_ZN1D4getDEv(%class.D* nonnull align 4 dereferenceable(8) %1)
  store i32 %call, i32* %q, align 4
  %2 = load i32, i32* %q, align 4
  %cmp = icmp eq i32 %2, 10
  br i1 %cmp, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  br label %cond.end

cond.false:                                       ; preds = %entry
  call void @__assert_fail(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str, i64 0, i64 0), i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.1, i64 0, i64 0), i32 31, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @__PRETTY_FUNCTION__.main, i64 0, i64 0)) #3
  unreachable

3:                                                ; No predecessors!
  br label %cond.end

cond.end:                                         ; preds = %3, %cond.true
  ret i32 0
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local void @_ZN1D4setDEi(%class.D* nonnull align 4 dereferenceable(8) %this, i32 %x) #1 comdat align 2 {
entry:
  %this.addr = alloca %class.D*, align 8
  %x.addr = alloca i32, align 4
  store %class.D* %this, %class.D** %this.addr, align 8
  store i32 %x, i32* %x.addr, align 4
  %this1 = load %class.D*, %class.D** %this.addr, align 8
  %0 = load i32, i32* %x.addr, align 4
  %d2 = getelementptr inbounds %class.D, %class.D* %this1, i32 0, i32 0
  store i32 %0, i32* %d2, align 4
  ret void
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local i32 @_ZN1D4getDEv(%class.D* nonnull align 4 dereferenceable(8) %this) #1 comdat align 2 {
entry:
  %this.addr = alloca %class.D*, align 8
  store %class.D* %this, %class.D** %this.addr, align 8
  %this1 = load %class.D*, %class.D** %this.addr, align 8
  %d2 = getelementptr inbounds %class.D, %class.D* %this1, i32 0, i32 0
  %0 = load i32, i32* %d2, align 4
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
