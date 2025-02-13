; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -passes='sroa<preserve-cfg>' -S | FileCheck %s --check-prefixes=CHECK,CHECK-preserve-cfg
; RUN: opt < %s -passes='sroa<modify-cfg>' -S | FileCheck %s --check-prefixes=CHECK,CHECK-modify-cfg

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"

define float @_Z8stdclampfff(float %x, float %lo, float %hi) {
; CHECK-LABEL: @_Z8stdclampfff(
; CHECK-NEXT:  bb:
; CHECK-NEXT:    [[I4:%.*]] = alloca float, align 4
; CHECK-NEXT:    store float [[HI:%.*]], ptr [[I4]], align 4
; CHECK-NEXT:    [[I5:%.*]] = fcmp fast olt float [[X:%.*]], [[LO:%.*]]
; CHECK-NEXT:    [[I6:%.*]] = fcmp fast olt float [[HI]], [[X]]
; CHECK-NEXT:    [[I9_SROA_SPECULATE_LOAD_FALSE_SROA_SPECULATE_LOAD_TRUE:%.*]] = load float, ptr [[I4]], align 4
; CHECK-NEXT:    [[I9_SROA_SPECULATE_LOAD_FALSE_SROA_SPECULATED:%.*]] = select i1 [[I6]], float [[I9_SROA_SPECULATE_LOAD_FALSE_SROA_SPECULATE_LOAD_TRUE]], float [[X]]
; CHECK-NEXT:    [[I9_SROA_SPECULATED:%.*]] = select i1 [[I5]], float [[LO]], float [[I9_SROA_SPECULATE_LOAD_FALSE_SROA_SPECULATED]]
; CHECK-NEXT:    ret float [[I9_SROA_SPECULATED]]
;
bb:
  %i = alloca float, align 4
  %i3 = alloca float, align 4
  %i4 = alloca float, align 4
  store float %x, ptr %i, align 4
  store float %lo, ptr %i3, align 4
  store float %hi, ptr %i4, align 4
  %i5 = fcmp fast olt float %x, %lo
  %i6 = fcmp fast olt float %hi, %x
  %i7 = select i1 %i6, ptr %i4, ptr %i
  %i8 = select i1 %i5, ptr %i3, ptr %i7
  %i9 = load float, ptr %i8, align 4
  ret float %i9
}
;; NOTE: These prefixes are unused and the list is autogenerated. Do not add tests below this line:
; CHECK-modify-cfg: {{.*}}
; CHECK-preserve-cfg: {{.*}}
