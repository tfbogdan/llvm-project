; NOTE: Assertions have been autogenerated by utils/update_test_checks.py
; RUN: opt < %s -passes='sroa<preserve-cfg>' -S | FileCheck %s --check-prefixes=CHECK,CHECK-preserve-cfg
; RUN: opt < %s -passes='sroa<modify-cfg>' -S | FileCheck %s --check-prefixes=CHECK,CHECK-modify-cfg

target datalayout = "E-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-n8:16:32:64"

define i8 @test1() {
; We fully promote these to the i24 load or store size, resulting in just masks
; and other operations that instcombine will fold, but no alloca. Note this is
; the same as test12 in basictest.ll, but here we assert big-endian byte
; ordering.
;
; CHECK-LABEL: @test1(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[A_SROA_3_0_INSERT_EXT:%.*]] = zext i8 0 to i24
; CHECK-NEXT:    [[A_SROA_3_0_INSERT_MASK:%.*]] = and i24 undef, -256
; CHECK-NEXT:    [[A_SROA_3_0_INSERT_INSERT:%.*]] = or i24 [[A_SROA_3_0_INSERT_MASK]], [[A_SROA_3_0_INSERT_EXT]]
; CHECK-NEXT:    [[A_SROA_2_0_INSERT_EXT:%.*]] = zext i8 0 to i24
; CHECK-NEXT:    [[A_SROA_2_0_INSERT_SHIFT:%.*]] = shl i24 [[A_SROA_2_0_INSERT_EXT]], 8
; CHECK-NEXT:    [[A_SROA_2_0_INSERT_MASK:%.*]] = and i24 [[A_SROA_3_0_INSERT_INSERT]], -65281
; CHECK-NEXT:    [[A_SROA_2_0_INSERT_INSERT:%.*]] = or i24 [[A_SROA_2_0_INSERT_MASK]], [[A_SROA_2_0_INSERT_SHIFT]]
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_EXT:%.*]] = zext i8 0 to i24
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_SHIFT:%.*]] = shl i24 [[A_SROA_0_0_INSERT_EXT]], 16
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_MASK:%.*]] = and i24 [[A_SROA_2_0_INSERT_INSERT]], 65535
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_INSERT:%.*]] = or i24 [[A_SROA_0_0_INSERT_MASK]], [[A_SROA_0_0_INSERT_SHIFT]]
; CHECK-NEXT:    [[B_SROA_0_0_EXTRACT_SHIFT:%.*]] = lshr i24 [[A_SROA_0_0_INSERT_INSERT]], 16
; CHECK-NEXT:    [[B_SROA_0_0_EXTRACT_TRUNC:%.*]] = trunc i24 [[B_SROA_0_0_EXTRACT_SHIFT]] to i8
; CHECK-NEXT:    [[B_SROA_2_0_EXTRACT_SHIFT:%.*]] = lshr i24 [[A_SROA_0_0_INSERT_INSERT]], 8
; CHECK-NEXT:    [[B_SROA_2_0_EXTRACT_TRUNC:%.*]] = trunc i24 [[B_SROA_2_0_EXTRACT_SHIFT]] to i8
; CHECK-NEXT:    [[B_SROA_3_0_EXTRACT_TRUNC:%.*]] = trunc i24 [[A_SROA_0_0_INSERT_INSERT]] to i8
; CHECK-NEXT:    [[BSUM0:%.*]] = add i8 [[B_SROA_0_0_EXTRACT_TRUNC]], [[B_SROA_2_0_EXTRACT_TRUNC]]
; CHECK-NEXT:    [[BSUM1:%.*]] = add i8 [[BSUM0]], [[B_SROA_3_0_EXTRACT_TRUNC]]
; CHECK-NEXT:    ret i8 [[BSUM1]]
;
entry:
  %a = alloca [3 x i8]
  %b = alloca [3 x i8]

  store i8 0, ptr %a
  %a1ptr = getelementptr [3 x i8], ptr %a, i64 0, i32 1
  store i8 0, ptr %a1ptr
  %a2ptr = getelementptr [3 x i8], ptr %a, i64 0, i32 2
  store i8 0, ptr %a2ptr
  %ai = load i24, ptr %a

  store i24 %ai, ptr %b
  %b0 = load i8, ptr %b
  %b1ptr = getelementptr [3 x i8], ptr %b, i64 0, i32 1
  %b1 = load i8, ptr %b1ptr
  %b2ptr = getelementptr [3 x i8], ptr %b, i64 0, i32 2
  %b2 = load i8, ptr %b2ptr

  %bsum0 = add i8 %b0, %b1
  %bsum1 = add i8 %bsum0, %b2
  ret i8 %bsum1
}

define i64 @test2() {
; Test for various mixed sizes of integer loads and stores all getting
; promoted.
;
; CHECK-LABEL: @test2(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[A_SROA_2_SROA_4_0_INSERT_EXT:%.*]] = zext i8 1 to i40
; CHECK-NEXT:    [[A_SROA_2_SROA_4_0_INSERT_MASK:%.*]] = and i40 undef, -256
; CHECK-NEXT:    [[A_SROA_2_SROA_4_0_INSERT_INSERT:%.*]] = or i40 [[A_SROA_2_SROA_4_0_INSERT_MASK]], [[A_SROA_2_SROA_4_0_INSERT_EXT]]
; CHECK-NEXT:    [[A_SROA_2_SROA_3_0_INSERT_EXT:%.*]] = zext i24 0 to i40
; CHECK-NEXT:    [[A_SROA_2_SROA_3_0_INSERT_SHIFT:%.*]] = shl i40 [[A_SROA_2_SROA_3_0_INSERT_EXT]], 8
; CHECK-NEXT:    [[A_SROA_2_SROA_3_0_INSERT_MASK:%.*]] = and i40 [[A_SROA_2_SROA_4_0_INSERT_INSERT]], -4294967041
; CHECK-NEXT:    [[A_SROA_2_SROA_3_0_INSERT_INSERT:%.*]] = or i40 [[A_SROA_2_SROA_3_0_INSERT_MASK]], [[A_SROA_2_SROA_3_0_INSERT_SHIFT]]
; CHECK-NEXT:    [[A_SROA_2_SROA_0_0_INSERT_EXT:%.*]] = zext i8 0 to i40
; CHECK-NEXT:    [[A_SROA_2_SROA_0_0_INSERT_SHIFT:%.*]] = shl i40 [[A_SROA_2_SROA_0_0_INSERT_EXT]], 32
; CHECK-NEXT:    [[A_SROA_2_SROA_0_0_INSERT_MASK:%.*]] = and i40 [[A_SROA_2_SROA_3_0_INSERT_INSERT]], 4294967295
; CHECK-NEXT:    [[A_SROA_2_SROA_0_0_INSERT_INSERT:%.*]] = or i40 [[A_SROA_2_SROA_0_0_INSERT_MASK]], [[A_SROA_2_SROA_0_0_INSERT_SHIFT]]
; CHECK-NEXT:    [[A_SROA_2_0_INSERT_EXT:%.*]] = zext i40 [[A_SROA_2_SROA_0_0_INSERT_INSERT]] to i56
; CHECK-NEXT:    [[A_SROA_2_0_INSERT_MASK:%.*]] = and i56 undef, -1099511627776
; CHECK-NEXT:    [[A_SROA_2_0_INSERT_INSERT:%.*]] = or i56 [[A_SROA_2_0_INSERT_MASK]], [[A_SROA_2_0_INSERT_EXT]]
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_EXT:%.*]] = zext i16 1 to i56
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_SHIFT:%.*]] = shl i56 [[A_SROA_0_0_INSERT_EXT]], 40
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_MASK:%.*]] = and i56 [[A_SROA_2_0_INSERT_INSERT]], 1099511627775
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_INSERT:%.*]] = or i56 [[A_SROA_0_0_INSERT_MASK]], [[A_SROA_0_0_INSERT_SHIFT]]
; CHECK-NEXT:    [[RET:%.*]] = zext i56 [[A_SROA_0_0_INSERT_INSERT]] to i64
; CHECK-NEXT:    ret i64 [[RET]]
;
entry:
  %a = alloca [7 x i8]

  %a1ptr = getelementptr [7 x i8], ptr %a, i64 0, i32 1
  %a2ptr = getelementptr [7 x i8], ptr %a, i64 0, i32 2
  %a3ptr = getelementptr [7 x i8], ptr %a, i64 0, i32 3


  store i16 1, ptr %a

  store i8 1, ptr %a2ptr

  store i24 1, ptr %a3ptr

  store i40 1, ptr %a2ptr

; the alloca is splitted into multiple slices
; Here, i8 1 is for %a[6]

; Here, i24 0 is for %a[3] to %a[5]

; Here, i8 0 is for %a[2]



  %ai = load i56, ptr %a
  %ret = zext i56 %ai to i64
  ret i64 %ret
; Here, i16 1 is for %a[0] to %a[1]
}

define i64 @PR14132(i1 %flag) {
; Here we form a PHI-node by promoting the pointer alloca first, and then in
; order to promote the other two allocas, we speculate the load of the
; now-phi-node-pointer. In doing so we end up loading a 64-bit value from an i8
; alloca. While this is a bit dubious, we were asserting on trying to
; rewrite it. The trick is that the code using the value may carefully take
; steps to only use the not-undef bits, and so we need to at least loosely
; support this. This test is particularly interesting because how we handle
; a load of an i64 from an i8 alloca is dependent on endianness.
; CHECK-LABEL: @PR14132(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    br i1 [[FLAG:%.*]], label [[IF_THEN:%.*]], label [[IF_END:%.*]]
; CHECK:       if.then:
; CHECK-NEXT:    [[B_0_LOAD_EXT:%.*]] = zext i8 1 to i64
; CHECK-NEXT:    [[B_0_ENDIAN_SHIFT:%.*]] = shl i64 [[B_0_LOAD_EXT]], 56
; CHECK-NEXT:    br label [[IF_END]]
; CHECK:       if.end:
; CHECK-NEXT:    [[PTR_0_SROA_SPECULATED:%.*]] = phi i64 [ [[B_0_ENDIAN_SHIFT]], [[IF_THEN]] ], [ 0, [[ENTRY:%.*]] ]
; CHECK-NEXT:    ret i64 [[PTR_0_SROA_SPECULATED]]
;
entry:
  %a = alloca i64, align 8
  %b = alloca i8, align 8
  %ptr = alloca ptr, align 8

  store i64 0, ptr %a
  store i8 1, ptr %b
  store ptr %a, ptr %ptr
  br i1 %flag, label %if.then, label %if.end

if.then:
  store ptr %b, ptr %ptr
  br label %if.end

if.end:
  %tmp = load ptr, ptr %ptr
  %result = load i64, ptr %tmp

  ret i64 %result
}

declare void @f(i64 %x, i32 %y)

define void @test3() {
; This is a test that specifically exercises the big-endian lowering because it
; ends up splitting a 64-bit integer into two smaller integers and has a number
; of tricky aspects (the i24 type) that make that hard. Historically, SROA
; would miscompile this by either dropping a most significant byte or least
; significant byte due to shrinking the [4,8) slice to an i24, or by failing to
; move the bytes around correctly.
;
; The magical number 34494054408 is used because it has bits set in various
; bytes so that it is clear if those bytes fail to be propagated.
;
; If you're debugging this, rather than using the direct magical numbers, run
; the IR through '-sroa -instcombine'. With '-instcombine' these will be
; constant folded, and if the i64 doesn't round-trip correctly, you've found
; a bug!
;
; CHECK-LABEL: @test3(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[A_SROA_3_0_INSERT_EXT:%.*]] = zext i32 134316040 to i64
; CHECK-NEXT:    [[A_SROA_3_0_INSERT_MASK:%.*]] = and i64 undef, -4294967296
; CHECK-NEXT:    [[A_SROA_3_0_INSERT_INSERT:%.*]] = or i64 [[A_SROA_3_0_INSERT_MASK]], [[A_SROA_3_0_INSERT_EXT]]
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_EXT:%.*]] = zext i32 8 to i64
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_SHIFT:%.*]] = shl i64 [[A_SROA_0_0_INSERT_EXT]], 32
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_MASK:%.*]] = and i64 [[A_SROA_3_0_INSERT_INSERT]], 4294967295
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_INSERT:%.*]] = or i64 [[A_SROA_0_0_INSERT_MASK]], [[A_SROA_0_0_INSERT_SHIFT]]
; CHECK-NEXT:    call void @f(i64 [[A_SROA_0_0_INSERT_INSERT]], i32 8)
; CHECK-NEXT:    ret void
;
entry:
  %a = alloca { i32, i24 }, align 4

  store i64 34494054408, ptr %a
  %tmp1 = load i64, ptr %a, align 4
  %tmp3 = load i32, ptr %a, align 4

  call void @f(i64 %tmp1, i32 %tmp3)
  ret void
}

define void @test4() {
; Much like @test3, this is specifically testing big-endian management of data.
; Also similarly, it uses constants with particular bits set to help track
; whether values are corrupted, and can be easily evaluated by running through
; -passes=instcombine to see that the i64 round-trips.
;
; CHECK-LABEL: @test4(
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[A_SROA_0_0_EXTRACT_SHIFT:%.*]] = lshr i64 34494054408, 32
; CHECK-NEXT:    [[A_SROA_0_0_EXTRACT_TRUNC:%.*]] = trunc i64 [[A_SROA_0_0_EXTRACT_SHIFT]] to i32
; CHECK-NEXT:    [[A_SROA_3_0_EXTRACT_TRUNC:%.*]] = trunc i64 34494054408 to i32
; CHECK-NEXT:    [[A_SROA_3_0_INSERT_EXT:%.*]] = zext i32 [[A_SROA_3_0_EXTRACT_TRUNC]] to i64
; CHECK-NEXT:    [[A_SROA_3_0_INSERT_MASK:%.*]] = and i64 undef, -4294967296
; CHECK-NEXT:    [[A_SROA_3_0_INSERT_INSERT:%.*]] = or i64 [[A_SROA_3_0_INSERT_MASK]], [[A_SROA_3_0_INSERT_EXT]]
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_EXT:%.*]] = zext i32 [[A_SROA_0_0_EXTRACT_TRUNC]] to i64
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_SHIFT:%.*]] = shl i64 [[A_SROA_0_0_INSERT_EXT]], 32
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_MASK:%.*]] = and i64 [[A_SROA_3_0_INSERT_INSERT]], 4294967295
; CHECK-NEXT:    [[A_SROA_0_0_INSERT_INSERT:%.*]] = or i64 [[A_SROA_0_0_INSERT_MASK]], [[A_SROA_0_0_INSERT_SHIFT]]
; CHECK-NEXT:    call void @f(i64 [[A_SROA_0_0_INSERT_INSERT]], i32 [[A_SROA_0_0_EXTRACT_TRUNC]])
; CHECK-NEXT:    ret void
;
entry:
  %a = alloca { i32, i24 }, align 4
  %a2 = alloca i64, align 4

  store i64 34494054408, ptr %a2
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %a, ptr align 4 %a2, i64 8, i1 false)

  %tmp3 = load i64, ptr %a, align 4
  %tmp5 = load i32, ptr %a, align 4

  call void @f(i64 %tmp3, i32 %tmp5)
  ret void
}

declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
;; NOTE: These prefixes are unused and the list is autogenerated. Do not add tests below this line:
; CHECK-modify-cfg: {{.*}}
; CHECK-preserve-cfg: {{.*}}
