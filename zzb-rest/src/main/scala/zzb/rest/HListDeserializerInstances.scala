package zzb.rest

import zzb.rest.unmarshalling.{ MalformedContent, DeserializationError, Deserializer }
import shapeless._

abstract class HListDeserializerInstances { self: HListDeserializer.type ⇒
  implicit def hld1[Z, T1, R1](construct: (R1) ⇒ Z)(implicit q1: DS[T1, R1]) =
    create[T1 :: HNil, Z] {
      case t1 :: HNil ⇒ construct(
        get(q1(t1)))
    }

  implicit def hld2[Z, T1, R1, T2, R2](construct: (R1, R2) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2]) =
    create[T1 :: T2 :: HNil, Z] {
      case t1 :: t2 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)))
    }

  implicit def hld3[Z, T1, R1, T2, R2, T3, R3](construct: (R1, R2, R3) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3]) =
    create[T1 :: T2 :: T3 :: HNil, Z] {
      case t1 :: t2 :: t3 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)))
    }

  implicit def hld4[Z, T1, R1, T2, R2, T3, R3, T4, R4](construct: (R1, R2, R3, R4) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4]) =
    create[T1 :: T2 :: T3 :: T4 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)))
    }

  implicit def hld5[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5](construct: (R1, R2, R3, R4, R5) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)))
    }

  implicit def hld6[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6](construct: (R1, R2, R3, R4, R5, R6) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)))
    }

  implicit def hld7[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7](construct: (R1, R2, R3, R4, R5, R6, R7) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)))
    }

  implicit def hld8[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8](construct: (R1, R2, R3, R4, R5, R6, R7, R8) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)))
    }

  implicit def hld9[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)))
    }

  implicit def hld10[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)))
    }

  implicit def hld11[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)))
    }

  implicit def hld12[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)))
    }

  implicit def hld13[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)))
    }

  implicit def hld14[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13, T14, R14](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13], q14: DS[T14, R14]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: T14 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: t14 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)),
        get(q14(t14)))
    }

  implicit def hld15[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13, T14, R14, T15, R15](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13], q14: DS[T14, R14], q15: DS[T15, R15]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: T14 :: T15 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: t14 :: t15 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)),
        get(q14(t14)),
        get(q15(t15)))
    }

  implicit def hld16[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13, T14, R14, T15, R15, T16, R16](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13], q14: DS[T14, R14], q15: DS[T15, R15], q16: DS[T16, R16]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: T14 :: T15 :: T16 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: t14 :: t15 :: t16 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)),
        get(q14(t14)),
        get(q15(t15)),
        get(q16(t16)))
    }

  implicit def hld17[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13, T14, R14, T15, R15, T16, R16, T17, R17](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13], q14: DS[T14, R14], q15: DS[T15, R15], q16: DS[T16, R16], q17: DS[T17, R17]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: T14 :: T15 :: T16 :: T17 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: t14 :: t15 :: t16 :: t17 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)),
        get(q14(t14)),
        get(q15(t15)),
        get(q16(t16)),
        get(q17(t17)))
    }

  implicit def hld18[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13, T14, R14, T15, R15, T16, R16, T17, R17, T18, R18](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13], q14: DS[T14, R14], q15: DS[T15, R15], q16: DS[T16, R16], q17: DS[T17, R17], q18: DS[T18, R18]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: T14 :: T15 :: T16 :: T17 :: T18 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: t14 :: t15 :: t16 :: t17 :: t18 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)),
        get(q14(t14)),
        get(q15(t15)),
        get(q16(t16)),
        get(q17(t17)),
        get(q18(t18)))
    }

  implicit def hld19[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13, T14, R14, T15, R15, T16, R16, T17, R17, T18, R18, T19, R19](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13], q14: DS[T14, R14], q15: DS[T15, R15], q16: DS[T16, R16], q17: DS[T17, R17], q18: DS[T18, R18], q19: DS[T19, R19]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: T14 :: T15 :: T16 :: T17 :: T18 :: T19 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: t14 :: t15 :: t16 :: t17 :: t18 :: t19 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)),
        get(q14(t14)),
        get(q15(t15)),
        get(q16(t16)),
        get(q17(t17)),
        get(q18(t18)),
        get(q19(t19)))
    }

  implicit def hld20[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13, T14, R14, T15, R15, T16, R16, T17, R17, T18, R18, T19, R19, T20, R20](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13], q14: DS[T14, R14], q15: DS[T15, R15], q16: DS[T16, R16], q17: DS[T17, R17], q18: DS[T18, R18], q19: DS[T19, R19], q20: DS[T20, R20]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: T14 :: T15 :: T16 :: T17 :: T18 :: T19 :: T20 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: t14 :: t15 :: t16 :: t17 :: t18 :: t19 :: t20 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)),
        get(q14(t14)),
        get(q15(t15)),
        get(q16(t16)),
        get(q17(t17)),
        get(q18(t18)),
        get(q19(t19)),
        get(q20(t20)))
    }

  implicit def hld21[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13, T14, R14, T15, R15, T16, R16, T17, R17, T18, R18, T19, R19, T20, R20, T21, R21](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, R21) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13], q14: DS[T14, R14], q15: DS[T15, R15], q16: DS[T16, R16], q17: DS[T17, R17], q18: DS[T18, R18], q19: DS[T19, R19], q20: DS[T20, R20], q21: DS[T21, R21]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: T14 :: T15 :: T16 :: T17 :: T18 :: T19 :: T20 :: T21 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: t14 :: t15 :: t16 :: t17 :: t18 :: t19 :: t20 :: t21 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)),
        get(q14(t14)),
        get(q15(t15)),
        get(q16(t16)),
        get(q17(t17)),
        get(q18(t18)),
        get(q19(t19)),
        get(q20(t20)),
        get(q21(t21)))
    }

  implicit def hld22[Z, T1, R1, T2, R2, T3, R3, T4, R4, T5, R5, T6, R6, T7, R7, T8, R8, T9, R9, T10, R10, T11, R11, T12, R12, T13, R13, T14, R14, T15, R15, T16, R16, T17, R17, T18, R18, T19, R19, T20, R20, T21, R21, T22, R22](construct: (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, R21, R22) ⇒ Z)(implicit q1: DS[T1, R1], q2: DS[T2, R2], q3: DS[T3, R3], q4: DS[T4, R4], q5: DS[T5, R5], q6: DS[T6, R6], q7: DS[T7, R7], q8: DS[T8, R8], q9: DS[T9, R9], q10: DS[T10, R10], q11: DS[T11, R11], q12: DS[T12, R12], q13: DS[T13, R13], q14: DS[T14, R14], q15: DS[T15, R15], q16: DS[T16, R16], q17: DS[T17, R17], q18: DS[T18, R18], q19: DS[T19, R19], q20: DS[T20, R20], q21: DS[T21, R21], q22: DS[T22, R22]) =
    create[T1 :: T2 :: T3 :: T4 :: T5 :: T6 :: T7 :: T8 :: T9 :: T10 :: T11 :: T12 :: T13 :: T14 :: T15 :: T16 :: T17 :: T18 :: T19 :: T20 :: T21 :: T22 :: HNil, Z] {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: t10 :: t11 :: t12 :: t13 :: t14 :: t15 :: t16 :: t17 :: t18 :: t19 :: t20 :: t21 :: t22 :: HNil ⇒ construct(
        get(q1(t1)),
        get(q2(t2)),
        get(q3(t3)),
        get(q4(t4)),
        get(q5(t5)),
        get(q6(t6)),
        get(q7(t7)),
        get(q8(t8)),
        get(q9(t9)),
        get(q10(t10)),
        get(q11(t11)),
        get(q12(t12)),
        get(q13(t13)),
        get(q14(t14)),
        get(q15(t15)),
        get(q16(t16)),
        get(q17(t17)),
        get(q18(t18)),
        get(q19(t19)),
        get(q20(t20)),
        get(q21(t21)),
        get(q22(t22)))
    }
}