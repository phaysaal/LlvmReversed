; ModuleID = 'bench/eg_cpp/eg_box2d_LlvmRev/SlacData/LLVM/eg_box2d.cpp.bc'
source_filename = "bench/eg_cpp/eg_box2d//eg_box2d.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.Box2D = type { i32 (...)**, %class.BasicData, %class.Point2D, %class.Point2D, double }
%class.BasicData = type { i32, i32 }
%class.Point2D = type { i32 (...)**, %class.BasicData, double, double }

$_ZNK7Point2D4getXEv = comdat any

$_ZNK7Point2D4getYEv = comdat any

$_ZN7Point2D4setXEd = comdat any

$_ZN7Point2D4setYEd = comdat any

@.str = private unnamed_addr constant [13 x i8] c"correctRange\00", align 1
@.str.1 = private unnamed_addr constant [36 x i8] c"bench/eg_cpp/eg_box2d//eg_box2d.cpp\00", align 1
@__PRETTY_FUNCTION__._ZN5Box2D19verifyNumericRangesEv = private unnamed_addr constant [34 x i8] c"void Box2D::verifyNumericRanges()\00", align 1

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define dso_local i32 @_Z3absi(i32 %x) #0 {
entry:
  %retval = alloca i32, align 4
  %x.addr = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  %0 = load i32, i32* %x.addr, align 4
  %cmp = icmp slt i32 %0, 0
  br i1 %cmp, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  %1 = load i32, i32* %x.addr, align 4
  %sub = sub nsw i32 0, %1
  store i32 %sub, i32* %retval, align 4
  br label %return

if.else:                                          ; preds = %entry
  %2 = load i32, i32* %x.addr, align 4
  store i32 %2, i32* %retval, align 4
  br label %return

return:                                           ; preds = %if.else, %if.then
  %3 = load i32, i32* %retval, align 4
  ret i32 %3
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define dso_local zeroext i1 @_Z5isNaNi(i32 %x) #0 {
entry:
  %x.addr = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  ret i1 false
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define dso_local zeroext i1 @_Z12fuzzyCompareii(i32 %x, i32 %y) #0 {
entry:
  %x.addr = alloca i32, align 4
  %y.addr = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  store i32 %y, i32* %y.addr, align 4
  ret i1 true
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define dso_local i32 @_Z16normalizeRadiansi(i32 %x) #0 {
entry:
  %x.addr = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  %0 = load i32, i32* %x.addr, align 4
  ret i32 %0
}

; Function Attrs: mustprogress noinline optnone sspstrong uwtable
define dso_local void @_ZN5Box2D19verifyNumericRangesEv(%class.Box2D* nonnull align 8 dereferenceable(88) %this) #1 align 2 {
entry:
  %this.addr = alloca %class.Box2D*, align 8
  %correctRange = alloca i8, align 1
  %normd_rot = alloca double, align 8
  store %class.Box2D* %this, %class.Box2D** %this.addr, align 8
  %this1 = load %class.Box2D*, %class.Box2D** %this.addr, align 8
  store i8 1, i8* %correctRange, align 1
  %m_size = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  %call = call double @_ZNK7Point2D4getXEv(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size)
  %cmp = fcmp olt double %call, 0.000000e+00
  br i1 %cmp, label %if.then, label %lor.lhs.false

lor.lhs.false:                                    ; preds = %entry
  %m_size2 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  %call3 = call double @_ZNK7Point2D4getYEv(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size2)
  %cmp4 = fcmp olt double %call3, 0.000000e+00
  br i1 %cmp4, label %if.then, label %if.end

if.then:                                          ; preds = %lor.lhs.false, %entry
  %m_size5 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  %m_size6 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  %call7 = call double @_ZNK7Point2D4getXEv(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size6)
  %conv = fptosi double %call7 to i32
  %call8 = call i32 @_Z3absi(i32 %conv)
  %conv9 = sitofp i32 %call8 to double
  call void @_ZN7Point2D4setXEd(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size5, double %conv9)
  %m_size10 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  %m_size11 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  %call12 = call double @_ZNK7Point2D4getYEv(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size11)
  %conv13 = fptosi double %call12 to i32
  %call14 = call i32 @_Z3absi(i32 %conv13)
  %conv15 = sitofp i32 %call14 to double
  call void @_ZN7Point2D4setYEd(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size10, double %conv15)
  store i8 0, i8* %correctRange, align 1
  br label %if.end

if.end:                                           ; preds = %if.then, %lor.lhs.false
  %m_size16 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  %call17 = call double @_ZNK7Point2D4getXEv(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size16)
  %conv18 = fptosi double %call17 to i32
  %call19 = call zeroext i1 @_Z5isNaNi(i32 %conv18)
  br i1 %call19, label %if.then20, label %if.end22

if.then20:                                        ; preds = %if.end
  %m_size21 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  call void @_ZN7Point2D4setXEd(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size21, double 0.000000e+00)
  store i8 0, i8* %correctRange, align 1
  br label %if.end22

if.end22:                                         ; preds = %if.then20, %if.end
  %m_size23 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  %call24 = call double @_ZNK7Point2D4getYEv(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size23)
  %conv25 = fptosi double %call24 to i32
  %call26 = call zeroext i1 @_Z5isNaNi(i32 %conv25)
  br i1 %call26, label %if.then27, label %if.end29

if.then27:                                        ; preds = %if.end22
  %m_size28 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 3
  call void @_ZN7Point2D4setYEd(%class.Point2D* nonnull align 8 dereferenceable(32) %m_size28, double 0.000000e+00)
  store i8 0, i8* %correctRange, align 1
  br label %if.end29

if.end29:                                         ; preds = %if.then27, %if.end22
  %m_rotation = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 4
  %0 = load double, double* %m_rotation, align 8
  %conv30 = fptosi double %0 to i32
  %call31 = call zeroext i1 @_Z5isNaNi(i32 %conv30)
  br i1 %call31, label %if.then32, label %if.end34

if.then32:                                        ; preds = %if.end29
  %m_rotation33 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 4
  store double 0.000000e+00, double* %m_rotation33, align 8
  store i8 0, i8* %correctRange, align 1
  br label %if.end34

if.end34:                                         ; preds = %if.then32, %if.end29
  %m_rotation35 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 4
  %1 = load double, double* %m_rotation35, align 8
  %conv36 = fptosi double %1 to i32
  %call37 = call i32 @_Z16normalizeRadiansi(i32 %conv36)
  %conv38 = sitofp i32 %call37 to double
  store double %conv38, double* %normd_rot, align 8
  %2 = load double, double* %normd_rot, align 8
  %conv39 = fptosi double %2 to i32
  %m_rotation40 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 4
  %3 = load double, double* %m_rotation40, align 8
  %conv41 = fptosi double %3 to i32
  %call42 = call zeroext i1 @_Z12fuzzyCompareii(i32 %conv39, i32 %conv41)
  br i1 %call42, label %if.end45, label %if.then43

if.then43:                                        ; preds = %if.end34
  %4 = load double, double* %normd_rot, align 8
  %m_rotation44 = getelementptr inbounds %class.Box2D, %class.Box2D* %this1, i32 0, i32 4
  store double %4, double* %m_rotation44, align 8
  store i8 0, i8* %correctRange, align 1
  br label %if.end45

if.end45:                                         ; preds = %if.then43, %if.end34
  %5 = load i8, i8* %correctRange, align 1
  %tobool = trunc i8 %5 to i1
  br i1 %tobool, label %cond.true, label %cond.false

cond.true:                                        ; preds = %if.end45
  br label %cond.end

cond.false:                                       ; preds = %if.end45
  call void @__assert_fail(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str, i64 0, i64 0), i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.1, i64 0, i64 0), i32 532, i8* getelementptr inbounds ([34 x i8], [34 x i8]* @__PRETTY_FUNCTION__._ZN5Box2D19verifyNumericRangesEv, i64 0, i64 0)) #3
  unreachable

6:                                                ; No predecessors!
  br label %cond.end

cond.end:                                         ; preds = %6, %cond.true
  ret void
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local double @_ZNK7Point2D4getXEv(%class.Point2D* nonnull align 8 dereferenceable(32) %this) #0 comdat align 2 {
entry:
  %this.addr = alloca %class.Point2D*, align 8
  store %class.Point2D* %this, %class.Point2D** %this.addr, align 8
  %this1 = load %class.Point2D*, %class.Point2D** %this.addr, align 8
  %m_x = getelementptr inbounds %class.Point2D, %class.Point2D* %this1, i32 0, i32 2
  %0 = load double, double* %m_x, align 8
  ret double %0
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local double @_ZNK7Point2D4getYEv(%class.Point2D* nonnull align 8 dereferenceable(32) %this) #0 comdat align 2 {
entry:
  %this.addr = alloca %class.Point2D*, align 8
  store %class.Point2D* %this, %class.Point2D** %this.addr, align 8
  %this1 = load %class.Point2D*, %class.Point2D** %this.addr, align 8
  %m_y = getelementptr inbounds %class.Point2D, %class.Point2D* %this1, i32 0, i32 3
  %0 = load double, double* %m_y, align 8
  ret double %0
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local void @_ZN7Point2D4setXEd(%class.Point2D* nonnull align 8 dereferenceable(32) %this, double %x) #0 comdat align 2 {
entry:
  %this.addr = alloca %class.Point2D*, align 8
  %x.addr = alloca double, align 8
  store %class.Point2D* %this, %class.Point2D** %this.addr, align 8
  store double %x, double* %x.addr, align 8
  %this1 = load %class.Point2D*, %class.Point2D** %this.addr, align 8
  %0 = load double, double* %x.addr, align 8
  %m_x = getelementptr inbounds %class.Point2D, %class.Point2D* %this1, i32 0, i32 2
  store double %0, double* %m_x, align 8
  ret void
}

; Function Attrs: mustprogress noinline nounwind optnone sspstrong uwtable
define linkonce_odr dso_local void @_ZN7Point2D4setYEd(%class.Point2D* nonnull align 8 dereferenceable(32) %this, double %y) #0 comdat align 2 {
entry:
  %this.addr = alloca %class.Point2D*, align 8
  %y.addr = alloca double, align 8
  store %class.Point2D* %this, %class.Point2D** %this.addr, align 8
  store double %y, double* %y.addr, align 8
  %this1 = load %class.Point2D*, %class.Point2D** %this.addr, align 8
  %0 = load double, double* %y.addr, align 8
  %m_y = getelementptr inbounds %class.Point2D, %class.Point2D* %this1, i32 0, i32 3
  store double %0, double* %m_y, align 8
  ret void
}

; Function Attrs: noreturn nounwind
declare void @__assert_fail(i8*, i8*, i32, i8*) #2

attributes #0 = { mustprogress noinline nounwind optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { mustprogress noinline optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
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
