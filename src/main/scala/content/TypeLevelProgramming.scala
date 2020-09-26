package content

object TypeLevelProgramming {

  import scala.reflect.runtime.universe._

  private def show[T](value: T)(implicit tag: TypeTag[T]): Unit =
    println(tag.toString()
    .replace("content.TypeLevelProgramming.", "")
    .replace("TypeTag", ""))


  sealed trait NaturalNumber

  class Successor[N <: NaturalNumber] extends NaturalNumber

  class _0 extends NaturalNumber

  type _1 = Successor[_0]
  type _2 = Successor[_1]
  type _3 = Successor[_2]
  type _4 = Successor[_3]
  type _5 = Successor[_4]

  sealed trait <=[A <: NaturalNumber, B <: NaturalNumber]

  object <= {
    implicit def lte[B <: NaturalNumber]: <=[_0, B] = new <=[_0, B] {}

    implicit def inductive[A <: NaturalNumber, B <: NaturalNumber]
    (implicit lt: <=[A, B]): <=[Successor[A], Successor[B]] = new <=[Successor[A], Successor[B]] {}

    def apply[A <: NaturalNumber, B <: NaturalNumber](implicit lt: <=[A, B]) = lt
  }

  sealed trait <[A <: NaturalNumber, B <: NaturalNumber]

  object < {
    implicit def lt[B <: NaturalNumber]: <[_0, Successor[B]] = new <[_0, Successor[B]] {}

    implicit def inductive[A <: NaturalNumber, B <: NaturalNumber]
    (implicit lt: <[A, B]): <[Successor[A], Successor[B]] = new <[Successor[A], Successor[B]] {}

    def apply[A <: NaturalNumber, B <: NaturalNumber](implicit lt: <[A, B]) = lt
  }

  sealed trait +[A <: NaturalNumber, B <: NaturalNumber] {
    type Result <: NaturalNumber
  }

  object + {
    type Plus[A <: NaturalNumber, B <: NaturalNumber, R <: NaturalNumber] = +[A, B] { type Result = R }
    // sum of Zeros is _0
    implicit val _0: Plus[_0, _0, _0] = new +[_0, _0] { type Result = _0 }

    // for every A <: NaturalNumber such as A > 0 , we have A + 0 = A
    implicit def left[A <: NaturalNumber](implicit lt: _0 < A): Plus[_0, A, A] = new +[_0, A] { type Result = A }

    // for every A <: NaturalNumber such as A > 0 , we have 0 + A = A
    implicit def right[A <: NaturalNumber](implicit lt: _0 < A): Plus[A, _0, A] = new +[A, _0] { type Result = A }

    // if A + B = R, then Successor[A] + Successor[B] = Successor[Successor[R]]
    implicit def inductive[A <: NaturalNumber, B <: NaturalNumber, R <: NaturalNumber]
    (implicit plus: Plus[A, B, R]): Plus[Successor[A], Successor[B], Successor[Successor[R]]]
    = new +[Successor[A], Successor[B]] { type Result = Successor[Successor[R]] }

    def apply[A <: NaturalNumber, B <: NaturalNumber](implicit plus: +[A, B]): Plus[A, B, plus.Result] = plus
  }

  trait HList
  trait HNil extends HList
  // prepend a number (H head) to a list (T tail)
  class :: [N <: NaturalNumber, L <: HList] extends HList

  /**
   * implement MergeSort with types:
   * 1. split
   * 2. sort the halves
   * 3. merge the halves back
   */
  trait Split[HL <: HList, L <: HList, R <: HList]
  object Split {
    // splitting an empty list will result in an empty list
    implicit val emptyList: Split[HNil, HNil, HNil] = new Split[HNil, HNil, HNil] {}

    // splitting a singleton set (1 element list) results in the left-hand side of the split
    implicit def left[N <: NaturalNumber]: Split[N :: HNil, N :: HNil, HNil] = new Split[N :: HNil, N :: HNil, HNil] {}

    implicit def inductive[N1 <: NaturalNumber, N2 <: NaturalNumber, T <: HList, L <: HList, R <: HList]
      (implicit split: Split[T, L, R])                // if the compiler has an instance where the tail (T) is split into left (L) and right (R)
      : Split[N1 :: N2 :: T, N1 :: L, N2 :: R]        // then
      = new Split[N1 :: N2 :: T, N1 :: L, N2 :: R] {} // the first list can be split into N1 :: L and N2 :: R

    def apply[HL <: HList, L <: HList, R <: HList](implicit split: Split[HL, L, R]) = split
  }

  trait Merge[A <: HList, B <: HList, R <: HList]
  object Merge {
    implicit def left[L <: HList]: Merge[HNil, L, L] = new Merge[HNil, L, L] {}
    implicit def right[L <: HList]: Merge[L, HNil, L] = new Merge[L, HNil, L] {}
    /**
     * The inductive method will require two lists:
     * L1 => N1 :: T1 (tail)
     * L2 => N2 :: T2 (tail)
     * if N1 <= N2 => N1 :: { Merge[T1, L2] }
     * if N2 < N1 => N2 :: { Merge[T2, L1] }
     */
    // IR = intermediate result
    implicit def inductiveLte[N1 <: NaturalNumber, T1 <: HList, N2 <: NaturalNumber, T2 <: HList, IR <: HList]
      // given N1 <= N2, and given the compiler can find and implicit function that can merge T1 (tail of first list) with original second list
      // the result will be N1 :: IR (first element of the first list prepended to the intermediate result)
      (implicit merge: Merge[T1, N2 :: T2, IR], lte: N1 <= N2)
      : Merge[N1 :: T1, N2 :: T2, N1 :: IR] = new Merge[N1 :: T1, N2 :: T2, N1 :: IR] {}
    implicit def inductiveGt[N1 <: NaturalNumber, T1 <: HList, N2 <: NaturalNumber, T2 <: HList, IR <: HList]
      // given N2 < N1, we will merge T1 (tail 1) with right list)
      (implicit merge: Merge[N1 :: T1, T2, IR], lt: N2 < N1)
      : Merge[N1 :: T1, N2 :: T2, N2 :: IR] = new Merge[N1 :: T1, N2 :: T2, N2 :: IR] {}

    def apply[A <: HList, B <: HList, R <: HList](implicit merge: Merge[A, B, R]): Merge[A, B, R] = merge
  }

  trait Sort[I <: HList] { type Result <: HList}
  object Sort {
    type SortOp[L <: HList, O <: HList] = Sort[L] { type Result = O }
    // sorting and empty list will return the same list
    implicit val empty: SortOp[HNil, HNil] = new Sort[HNil] { type Result = HNil}
    // sorting a singleton list will result in the same list
    implicit def singletonList[N <: NaturalNumber]: SortOp[N :: HNil, N :: HNil] = new Sort[N :: HNil] { type Result = N :: HNil}
    implicit def inductive[I <: HList, L <: HList, R <: HList, SL <: HList, SR <: HList, O <: HList]
      (implicit
        split: Split[I, L, R],   // given the split operation on I (input) to L (left) and R (right)
        sortLeft: SortOp[L, SL],   // given the sort operation on L (left) to SL (sorted left)
        sortRight: SortOp[R, SR],  // given the sort operation on R (right) to SR (sorted right)
        merge: Merge[SL, SR, O]  // given the merge operation between SL (sorted left) and SR (sorted right) we can product O (output)
      ): SortOp[I, O] = new Sort[I] { type Result = O}

    def apply[I <: HList](implicit sort: Sort[I]): SortOp[I, sort.Result] = sort
  }

  def main(args: Array[String]): Unit = {
    val ZeroLessThanOne: <[_0, _1] = <.apply
    // val oneLessThanOne: <[_1, _1] = <.apply - WON'T COMPILE

    /**
     * What the compiler does:
     * 1. <.apply[_1, _3] -> requires an implicit of <[_1, _3]
     * 2. the compiler will run inductive[_1, _3] -> requires an implicit of <[_0, _2], which the lt method
     * 3. lt[_1] -> produces implicit <[_0, Successor[_1]] ==> <[_0, _2]
     */
    val oneLessThan_3: <[_1, _3] = <.apply
    show(oneLessThan_3)

    val twoLessThanOrEqualTwo: <=[_2, _2] = <=.apply
    //  val _3LessThanOrEqualOne: <=[_3, _1] = <=.apply - WON'T COMPILE

    show(twoLessThanOrEqualTwo)

    // uses implicit _0
    val ZeroSum: +[_0, _0] = +.apply
    show(ZeroSum)

    // uses implicit left
    val twoPlusZero: +[_2, _0] = +.apply
    show(twoPlusZero)
    // should produce Successor[Successor[_0]] (2) + _0  = Successor[Successor[_0]] (2)
    show(+.apply[_2, _0])

    // uses implicit right
    val ZeroPlus_3: +[_0, _3] = +.apply
    show(ZeroPlus_3)
    // should produce _0 (0) + Successor[Successor[Successor[_0]]] (3) = Successor[Successor[Successor[_0]]] (3)
    show(+.apply[_0, _3])

    /**
     * What the compiler does:
     * 1. +.apply[_1, _3] -> requires an implicit of +[_1, _3]
     * 2. the compiler will run inductive[_1, _3] -> requires an implicit of +[Succesor[_0], Successor[_2]], which the lt method
     * 3. left[_2] -> produces implicit +[_0, Successor[_2]] ==> +[_0, _2]
     * 4. this will result in Successor[Successor[_2]] == 4
     */
    val onePlus_3: +[_1, _3] = +.apply
    show(onePlus_3)
    // should produce Successor[_0] (1) + Successor[Successor[Successor[_0]]] (3) = Successor[Successor[Successor[Successor[_0]]]] (4)
    show(+.apply[_1, _3])

    // check the validity of splitting the list (1,2,3) ==> Left(1,3) and Right(2)
    /**
     * What the compiler does:
     * 1. the compiler requires an implicit instance of Split[One :: _2 :: _3 :: HNil, One :: _3 :: HNil, _2 :: HNil]
     * 2. the compiler calls the inductive(One, _2, _3 :: HNil) where N1 = One, N2 = 2 and T (tail) = 3::HNil
     * 3. the compiler matches N1 :: L (Left) (_3 :: HNil) and N2 :: Right (_2 :: HNil)
     * 4. the compiler requires an implicit Split[T, L, R] = Split[ _3 :: HNil, _3 :: HNil, HNil]
     * 5. the compiler will call left[_3] => Split[_3 ::  HNil, _3 ::  HNil, HNil]
     */
    val validSplit: Split[_1 :: _2 :: _3 :: HNil, _1 :: _3 :: HNil, _2 :: HNil] = Split.apply
    show(validSplit)

    /**
     * What the compiler does:
     * 1. the compiler requires an implicit instance of Merge[One :: _3 :: HNil, _2 :: _4 :: HNil, One :: _2 :: _3 :: _4 :: HNil]
     * 2. the compiler calls the inductiveLte (since 1 < 2), requires an implicit Merge where the first tail will merge with the right list, such as
     *    the result will be (3, (2,4), (2,3,4) ==> Merge[_3 :: HNil, _2 :: _4 :: HNil, _2 :: _3 :: _4 :: HNil]
     * 3. the compiler calls the inductiveGt (since 3 > 2), requires an Merge[_3 :: HNil, _4 :: _4 :: HNil, _3 :: _4 :: HNil]
     * 4. the compiler calls the inductiveLte (since 3 < 4) Merge[HNil, _4 :: HNil, _4 :: HNil]
     * 5. the compiler will call left[_4 :: HNil]
     */
    val validMerge: Merge[_1 :: _3 :: HNil, _2 :: _4 :: HNil, _1 :: _2 :: _3 :: _4 :: HNil] = Merge.apply
    show(validMerge)

    val validSort: Sort[_5 :: _1 :: _3 :: _2 :: _4 :: HNil] = Sort.apply
    show(validSort) 
    // should result in:
    // [Result = Successor[_0] :: Successor[Successor[_0]] :: Successor[Successor[Successor[_0]]] :: Successor[Successor[Successor[Successor[_0]]]] :: Successor[Successor[Successor[Successor[Successor[_0]]]]] :: HNil}]
    show(Sort.apply[_5 :: _1 :: _3 :: _2 :: _4 :: HNil])

  }
}