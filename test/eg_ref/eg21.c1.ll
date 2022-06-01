; ModuleID = '/home/faisal/code/eg_c/eg21/SlacData/LLVM/eg21.c.bc'
source_filename = "/home/faisal/code/eg_c/eg21//eg21.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f1(i32* %z) #0 {
entry:
  %z.addr = alloca i32*, align 8                      ; int **z.addr = malloc(sizeof(int *) * 1);
  store i32* %z, i32** %z.addr, align 8								; *z.addr = z;	 									 				
  %0 = load i32*, i32** %z.addr, align 8							; 
  %1 = load i32, i32* %0, align 4			 								; 
  %add = add nsw i32 %1, 1														; add=**z.addr+1; ===>  t1=*z.addr; t2 = *t1; add = t2 + 1;
  %2 = load i32*, i32** %z.addr, align 8							; 
  store i32 %add, i32* %2, align 4		 								; **z.addr = add; ===>  t1 = *z.addr; *t1 = add 
  %3 = load i32*, i32** %z.addr, align 8							;  
  %4 = load i32, i32* %3, align 4			  							; 
  ret i32 %4		 																			; ret **z.addr;
}


define dso_local i32 @f1(i32* %z) #0 {
entry:
  %z.addr = alloca i32*, align 8                      ; int *z.addr; 
  store i32* %z, i32** %z.addr, align 8								; z.addr = z;	 		z is PTR_PARAM, z.addr is PTR; So, *z.addr = z or z.addr = &z; 							 				
  %0 = load i32*, i32** %z.addr, align 8							; 
  %1 = load i32, i32* %0, align 4			 								; 
  %add = add nsw i32 %1, 1														; add=z.addr+1;   LHS is int, RHS should be int
  %2 = load i32*, i32** %z.addr, align 8							; 
  store i32 %add, i32* %2, align 4		 								; z.addr = add;
  %3 = load i32*, i32** %z.addr, align 8							;  
  %4 = load i32, i32* %3, align 4			  							; 
  ret i32 %4		 																			; ret z.addr;
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
  %conv = zext i1 %cmp to i32
  %call1 = call i32 (i32, ...) bitcast (i32 (...)* @assert to i32 (i32, ...)*)(i32 %conv)
  %2 = load i32, i32* %n, align 4
  %call2 = call i32 @f2(i32 %2)
  store i32 %call2, i32* %m, align 4
  %3 = load i32, i32* %m, align 4
  %4 = load i32, i32* %n, align 4
  %add = add nsw i32 %4, 1
  %cmp3 = icmp eq i32 %3, %add
  %conv4 = zext i1 %cmp3 to i32
  %call5 = call i32 (i32, ...) bitcast (i32 (...)* @assert to i32 (i32, ...)*)(i32 %conv4)
  ret i32 0
}

declare dso_local i32 @assert(...) #1

attributes #0 = { noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"uwtable", i32 1}
!2 = !{i32 7, !"frame-pointer", i32 2}
!3 = !{!"clang version 13.0.0"}
