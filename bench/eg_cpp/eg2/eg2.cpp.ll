; ModuleID = '/home/faisal/code/eg_cpp/eg2_LlvmRev/SlacData/LLVM/eg2.cpp.bc'
source_filename = "/home/faisal/code/eg_cpp/eg2//eg2.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.B = type { i32 }

$_ZN1B3setEi = comdat any

$_ZN1B3getEv = comdat any

@.str = private unnamed_addr constant [5 x i8] c"p==q\00", align 1
@.str.1 = private unnamed_addr constant [38 x i8] c"/home/faisal/code/eg_cpp/eg2//eg2.cpp\00", align 1
@__PRETTY_FUNCTION__.main = private unnamed_addr constant [11 x i8] c"int main()\00", align 1

; Function Attrs: mustprogress noinline norecurse optnone sspstrong uwtable
define dso_local i32 @main() #0 {
entry:
  %retval = alloca i32, align 4
  %p = alloca i32, align 4
  %q = alloca i32, align 4
  %b = alloca %class.B*, align 8
  store i32 0, i32* %retval, align 4
  store i32 8, i32* %p, align 4
  %call = call noalias nonnull i8* @_Znwm(i64 4) #5
  %0 = bitcast i8* %call to %class.B*
  %1 = bitcast %class.B* %0 to i8*
  call void @llvm.memset.p0i8.i64(i8* align 4 %1, i8 0, i64 4, i1 false)
  store %class.B* %0, %class.B** %b, align 8
  %2 = load %class.B*, %class.B** %b, align 8
  %3 = load i32, i32* %p, align 4
  call void @_ZN1B3setEi(%class.B* nonnull align 4 dereferenceable(4) %2, i32 %3)
  %4 = load %class.B*, %class.B** %b, align 8
  %call1 = call i32 @_ZN1B3getEv(%class.B* nonnull align 4 dereferenceable(4) %4)
  store i32 %call1, i32* %q, align 4
  %5 = load i32, i32* %p, align 4
  %6 = load i32, i32* %q, align 4
  %cmp = icmp eq i32 %5, %6
  br i1 %cmp, label %cond.true, label %cond.false

cond.true:                                        ; preds = %entry
  br label %cond.end

cond.false:                                       ; preds = %entry
  call void @__assert_fail(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i64 0, i64 0), i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.1, i64 0, i64 0), i32 21, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @__PRETTY_FUNCTION__.main, i64 0, i64 0)) #6
  unreachable

7:                                                ; No predecessors!
  br label %cond.end

cond.end:                                         ; preds = %7, %cond.true
  %8 = load i32, i32* %p, align 4
  %9 = load i32, i32* %q, align 4
  %cmp2 = icmp eq i32 %8, %9
  %conv = zext i1 %cmp2 to i32
  ret i32 %conv
}

; Function Attrs: nobuiltin allocsize(0)
declare nonnull i8* @_Znwm(i64) #1

; Function Attrs: argmemonly nofree nounwind willreturn writeonly
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #2

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local void @_ZN1B3setEi(%class.B* nonnull align 4 dereferenceable(4) %this, i32 %y) #3 comdat align 2 {
entry:
  %this.addr = alloca %class.B*, align 8
  %y.addr = alloca i32, align 4
  store %class.B* %this, %class.B** %this.addr, align 8
  store i32 %y, i32* %y.addr, align 4
  %this1 = load %class.B*, %class.B** %this.addr, align 8
  %0 = load i32, i32* %y.addr, align 4
  %x = getelementptr inbounds %class.B, %class.B* %this1, i32 0, i32 0
  store i32 %0, i32* %x, align 4
  ret void
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local i32 @_ZN1B3getEv(%class.B* nonnull align 4 dereferenceable(4) %this) #3 comdat align 2 {
entry:
  %this.addr = alloca %class.B*, align 8
  store %class.B* %this, %class.B** %this.addr, align 8
  %this1 = load %class.B*, %class.B** %this.addr, align 8
  %x = getelementptr inbounds %class.B, %class.B* %this1, i32 0, i32 0
  %0 = load i32, i32* %x, align 4
  ret i32 %0
}

; Function Attrs: noreturn nounwind
declare void @__assert_fail(i8*, i8*, i32, i8*) #4

attributes #0 = { mustprogress noinline norecurse optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nobuiltin allocsize(0) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { argmemonly nofree nounwind willreturn writeonly }
attributes #3 = { mustprogress noinline nounwind optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { noreturn nounwind "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #5 = { builtin allocsize(0) }
attributes #6 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 1}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 13.0.1"}
