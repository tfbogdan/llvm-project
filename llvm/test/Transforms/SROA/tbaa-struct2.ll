; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt -S -passes='sroa<preserve-cfg>' %s | FileCheck %s --check-prefixes=CHECK,CHECK-preserve-cfg
; RUN: opt -S -passes='sroa<modify-cfg>' %s | FileCheck %s --check-prefixes=CHECK,CHECK-modify-cfg

; SROA should correctly offset `!tbaa.struct` metadata

%struct.Wishart = type { double, i32 }
declare void @llvm.memcpy.p0.p0.i64(ptr writeonly, ptr readonly, i64, i1 immarg)
declare double @subcall(double %g, i32 %m)

define double @bar(ptr %wishart) {
; CHECK-LABEL: @bar(
; CHECK-NEXT:    [[TMP_SROA_3:%.*]] = alloca [4 x i8], align 4
; CHECK-NEXT:    [[TMP_SROA_0_0_COPYLOAD:%.*]] = load double, ptr [[WISHART:%.*]], align 8, !tbaa.struct !0
; CHECK-NEXT:    [[TMP_SROA_2_0_WISHART_SROA_IDX:%.*]] = getelementptr inbounds i8, ptr [[WISHART]], i64 8
; CHECK-NEXT:    [[TMP_SROA_2_0_COPYLOAD:%.*]] = load i32, ptr [[TMP_SROA_2_0_WISHART_SROA_IDX]], align 8, !tbaa.struct !7
; CHECK-NEXT:    [[TMP_SROA_3_0_WISHART_SROA_IDX:%.*]] = getelementptr inbounds i8, ptr [[WISHART]], i64 12
; CHECK-NEXT:    call void @llvm.memcpy.p0.p0.i64(ptr align 4 [[TMP_SROA_3]], ptr align 4 [[TMP_SROA_3_0_WISHART_SROA_IDX]], i64 4, i1 false), !tbaa.struct !8
; CHECK-NEXT:    [[CALL:%.*]] = call double @subcall(double [[TMP_SROA_0_0_COPYLOAD]], i32 [[TMP_SROA_2_0_COPYLOAD]])
; CHECK-NEXT:    ret double [[CALL]]
;
  %tmp = alloca %struct.Wishart, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %tmp, ptr align 8 %wishart, i64 16, i1 false), !tbaa.struct !2
  %lg = load double, ptr %tmp, align 8, !tbaa !4
  %m = getelementptr inbounds %struct.Wishart, ptr %tmp, i32 0, i32 1
  %lm = load i32, ptr %m, align 8, !tbaa !8
  %call = call double @subcall(double %lg, i32 %lm)
  ret double %call
}

!2 = !{i64 0, i64 8, !3, i64 8, i64 4, !7}
!3 = !{!4, !4, i64 0}
!4 = !{!"double", !5, i64 0}
!5 = !{!"omnipotent char", !6, i64 0}
!6 = !{!"Simple C++ TBAA"}
!7 = !{!8, !8, i64 0}
!8 = !{!"int", !5, i64 0}
;; NOTE: These prefixes are unused and the list is autogenerated. Do not add tests below this line:
; CHECK-modify-cfg: {{.*}}
; CHECK-preserve-cfg: {{.*}}
