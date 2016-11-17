#############################################################################
##
#W  standard/cong.tst
#Y  Copyright (C) 2015                                      Michael Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/cong.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# SemigroupCongruence: Bad input
gap> S := FullTransformationSemigroup(5);;
gap> pairs := [[Transformation([2, 1, 3, 4, 1]), Transformation([2, 1])],
>              [Transformation([2, 1]), Transformation([2, 3, 4, 5, 2])]];;
gap> SemigroupCongruence(S);
Error, Semigroups: SemigroupCongruence: usage,
at least 2 arguments are required,
gap> SemigroupCongruence(42, pairs);
Error, Semigroups: SemigroupCongruence: usage,
1st argument <S> must be a semigroup,
gap> SemigroupCongruence(S, pairs[1], [Transformation([2, 1])]);
Error, Semigroups: SemigroupCongruence: usage,
<pairs> should be a list of lists of size 2,
gap> SemigroupCongruence(S, [Transformation([2, 6, 3, 4, 5, 3]),
>                            Transformation([2, 3, 1])]);
Error, Semigroups: SemigroupCongruence: usage,
each pair should contain elements from the semigroup <S>,
gap> S := FullTransformationSemigroup(6);;
gap> SemigroupCongruence(S, 12, 13, 100);
"TRY_NEXT_METHOD"

#T# SemigroupCongruence: Infinite semigroup
gap> S := FreeSemigroup(2);;
gap> SemigroupCongruence(S, [S.1, S.2]);
<semigroup congruence over <free semigroup on the generators [ s1, s2 ]> with 
1 generating pairs>

#T# SemigroupCongruence: Simple semigroup
gap> S := Semigroup(MinimalIdeal(FullTransformationMonoid(5)));;
gap> IsSimpleSemigroup(S);
true
gap> pairs := [
> [Transformation([1, 1, 1, 1, 1]), Transformation([3, 3, 3, 3, 3])]];;
gap> SemigroupCongruence(S, pairs);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 5 generators> with linked triple (1,1,4)>

#T# SemigroupCongruence: 0-simple semigroup
gap> S := Semigroup(Transformation([1, 2]), Transformation([1, 1]));;
gap> IsZeroSimpleSemigroup(S);
true
gap> IsRegularSemigroup(S);
true
gap> SemigroupCongruence(S, [S.1, S.1]);
<semigroup congruence over <commutative 0-simple regular transformation 
 monoid of degree 2 with 1 generator> with linked triple (1,1,1)>

#T# SemigroupCongruence: Inverse semigroup
# Temporarily removed - MT
#gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 4, 2]),
#>                           PartialPerm([1, 2, 3], [2, 3, 4]),
#>                           PartialPerm([1, 2, 4], [2, 1, 3])]);;
#gap> SemigroupCongruence(S, [S.1, S.2]);
#<semigroup congruence over <inverse partial perm semigroup of rank 4 with 3 
# generators> with congruence pair (116,1)>

#T# SemigroupCongruence: Pairs
gap> S := Semigroup([Transformation([3, 3, 3]),
>                    Transformation([3, 4, 3, 3])]);;
gap> pairs := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> SemigroupCongruence(S, pairs);
<semigroup congruence over <transformation semigroup of degree 4 with 2 
 generators> with 1 generating pairs>

#T# SemigroupCongruence: Giving an RMS cong
gap> S := Semigroup(MinimalIdeal(FullTransformationMonoid(5)));;
gap> iso := IsomorphismReesMatrixSemigroup(S);;
gap> R := Range(iso);;
gap> pairs := [ReesMatrixSemigroupElement(R, 1, (), 1),
>              ReesMatrixSemigroupElement(R, 1, (), 3)];;
gap> rmscong := SemigroupCongruence(R, pairs);;
gap> SemigroupCongruence(S, iso, rmscong);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 5 generators> with linked triple (1,1,4)>

#T# SemigroupCongruence: Giving an RZMS cong
gap> S := Semigroup(Transformation([1, 2]), Transformation([1, 1]));;
gap> IsRegularSemigroup(S);;
gap> iso := IsomorphismReesZeroMatrixSemigroup(S);;
gap> R := Range(iso);;
gap> pairs := [ReesZeroMatrixSemigroupElement(R, 1, (), 1),
>              ReesZeroMatrixSemigroupElement(R, 1, (), 1)];;
gap> rmscong := SemigroupCongruence(R, pairs);;
gap> SemigroupCongruence(S, iso, rmscong);
<semigroup congruence over <commutative 0-simple regular transformation 
 monoid of degree 2 with 1 generator> with linked triple (1,1,1)>

#T# SemigroupCongruence: Bad R(Z)MS Input
gap> S := Semigroup(MinimalIdeal(FullTransformationMonoid(5)));;
gap> iso := IsomorphismReesMatrixSemigroup(S);;
gap> R := Range(iso);;
gap> pairs := [ReesMatrixSemigroupElement(R, 1, (), 1),
>              ReesMatrixSemigroupElement(R, 1, (), 3)];;
gap> rmscong := SemigroupCongruence(R, pairs);;
gap> S := Semigroup(Transformation([2, 2]), Transformation([1, 1]));;
gap> SemigroupCongruence(S, iso, rmscong);
Error, Semigroups: SemigroupCongruence: usage,
<cong> should be over a Rees (0-)matrix semigroup isomorphic to <S> via <iso>,

#T# SemigroupCongruence: Rees congruence via ideal
gap> S := Semigroup(FullTransformationMonoid(5));;
gap> I := MinimalIdeal(S);;
gap> SemigroupCongruence(S, I);
<Rees congruence of <simple transformation semigroup ideal of degree 5 with
  1 generator> over <transformation monoid of degree 5 with 3 generators>>

#T# SemigroupCongruence: Kernel and Trace
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 4, 2]),
>                           PartialPerm([1, 2, 3], [2, 3, 4]),
>                           PartialPerm([1, 2, 4], [2, 1, 3])]);;
gap> ker := IdempotentGeneratedSubsemigroup(S);;
gap> trc := List(Idempotents(S), e -> [e]);;
gap> SemigroupCongruence(S, ker, trc);;

#T# LeftSemigroupCongruence: Bad input
gap> S := FullTransformationSemigroup(5);;
gap> pairs := [[Transformation([2, 1, 3, 4, 1]), Transformation([2, 1])],
>              [Transformation([2, 1]), Transformation([2, 3, 4, 5, 2])]];;
gap> LeftSemigroupCongruence(S);
Error, Semigroups: LeftSemigroupCongruence: usage,
at least 2 arguments are required,
gap> LeftSemigroupCongruence(42, pairs);
Error, Semigroups: LeftSemigroupCongruence: usage,
1st argument <S> must be a semigroup,
gap> LeftSemigroupCongruence(S, pairs[1], [Transformation([2, 1])]);
Error, Semigroups: LeftSemigroupCongruence: usage,
<pairs> should be a list of lists of size 2,
gap> LeftSemigroupCongruence(S,
> [Transformation([2, 6, 3, 4, 5, 2]), Transformation([2, 3, 1])]);
Error, Semigroups: LeftSemigroupCongruence: usage,
each pair should contain elements from the semigroup <S>,
gap> S := FullTransformationSemigroup(6);;
gap> LeftSemigroupCongruence(S, 12, 13, 100);
"TRY_NEXT_METHOD"

#T# RightSemigroupCongruence: Bad input
gap> S := FullTransformationSemigroup(5);;
gap> pairs := [[Transformation([2, 1, 3, 4, 1]), Transformation([2, 1])],
>              [Transformation([2, 1]), Transformation([2, 3, 4, 5, 2])]];;
gap> RightSemigroupCongruence(S);
Error, Semigroups: RightSemigroupCongruence: usage,
at least 2 arguments are required,
gap> RightSemigroupCongruence(42, pairs);
Error, Semigroups: RightSemigroupCongruence: usage,
1st argument <S> must be a semigroup,
gap> RightSemigroupCongruence(S, pairs[1], [Transformation([2, 2])]);
Error, Semigroups: RightSemigroupCongruence: usage,
<pairs> should be a list of lists of size 2,
gap> RightSemigroupCongruence(S,
> [Transformation([2, 6, 3, 4, 5, 4]), Transformation([2, 3, 1])]);
Error, Semigroups: RightSemigroupCongruence: usage,
each pair should contain elements from the semigroup <S>,
gap> S := FullTransformationSemigroup(6);;
gap> RightSemigroupCongruence(S, 12, 13, 100);
"TRY_NEXT_METHOD"

#T# LeftSemigroupCongruence: Pairs
gap> S := Semigroup([Transformation([3, 3, 3]),
>                      Transformation([3, 4, 3, 3])]);;
gap> pairs := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := LeftSemigroupCongruence(S, pairs);
<left semigroup congruence over <transformation semigroup of degree 4 with 2 
 generators> with 1 generating pairs>
gap> LeftCongruenceClassOfElement(cong, Transformation([3,4,3,3]));
<left congruence class of Transformation( [ 3, 4, 3, 3 ] )>

#T# RightSemigroupCongruence: Pairs
gap> S := Semigroup([Transformation([3, 3, 3]),
>                      Transformation([3, 4, 3, 3])]);;
gap> pairs := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := RightSemigroupCongruence(S, pairs);
<right semigroup congruence over <transformation semigroup of degree 4 with 2 
 generators> with 1 generating pairs>
gap> RightCongruenceClassOfElement(cong, Transformation([3,4,3,3]));
<right congruence class of Transformation( [ 3, 4, 3, 3 ] )>

#T# OnLeftCongruenceClasses
gap> S:=Semigroup(Transformation( [ 2, 1, 1, 2, 1 ] ),
>                 Transformation( [ 3, 4, 3, 4, 4 ] ),
>                 Transformation( [ 3, 4, 3, 4, 3 ] ),
>                 Transformation( [ 4, 3, 3, 4, 4 ] ));;
gap> pair1 := [Transformation( [ 3, 4, 3, 4, 3 ] ),
>              Transformation( [ 1, 2, 1, 2, 1 ] )];;
gap> pair2 := [Transformation( [ 4, 3, 4, 3, 4 ] ),
>              Transformation( [ 3, 4, 3, 4, 3 ] )];;
gap> cong := LeftSemigroupCongruence(S, [pair1, pair2]);
<left semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> x := Transformation( [ 3, 4, 3, 4, 3 ] );;
gap> class := LeftCongruenceClassOfElement(cong, x);
<left congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )>
gap> elm := Transformation( [ 1, 2, 2, 1, 2 ] );;
gap> OnLeftCongruenceClasses(class, elm);
<left congruence class of Transformation( [ 3, 4, 4, 3, 4 ] )>

#T# OnRightCongruenceClasses
gap> S:=Semigroup(Transformation( [ 2, 1, 1, 2, 1 ] ),
>                 Transformation( [ 3, 4, 3, 4, 4 ] ),
>                 Transformation( [ 3, 4, 3, 4, 3 ] ),
>                 Transformation( [ 4, 3, 3, 4, 4 ] ));;
gap> pair1 := [Transformation( [ 3, 4, 3, 4, 3 ] ),
>              Transformation( [ 1, 2, 1, 2, 1 ] )];;
gap> pair2 := [Transformation( [ 4, 3, 4, 3, 4 ] ),
>              Transformation( [ 3, 4, 3, 4, 3 ] )];;
gap> cong := RightSemigroupCongruence(S, [pair1, pair2]);
<right semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> x := Transformation( [ 3, 4, 3, 4, 3 ] );;
gap> class := RightCongruenceClassOfElement(cong, x);
<right congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )>
gap> elm := Transformation( [ 1, 2, 2, 1, 2 ] );;
gap> OnRightCongruenceClasses(class, elm);
<right congruence class of Transformation( [ 2, 1, 2, 1, 2 ] )>

#T# \* for an equivalence class and a list
gap> S := Semigroup(
> [Transformation([1, 4, 3, 4]), Transformation([2, 3, 4, 2])]);;
gap> pair := [Transformation([2, 3, 4, 2]), Transformation([4, 4, 4, 4])];;
gap> cong := SemigroupCongruence(S, pair);;
gap> class := CongruenceClassOfElement(cong, Transformation([4, 4, 4, 4]));;
gap> class * CongruenceClasses(cong);
[ <congruence class of Transformation( [ 4, 4, 4, 4 ] )>, 
  <congruence class of Transformation( [ 2, 2, 2, 2 ] )> ]
gap> CongruenceClasses(cong) * class;
[ <congruence class of Transformation( [ 4, 4, 4, 4 ] )>, 
  <congruence class of Transformation( [ 4, 4, 4, 4 ] )> ]

#T# IsSuperrelation
gap> S := Semigroup(
> [Transformation([1, 4, 3, 4]), Transformation([2, 3, 4, 2])]);;
gap> pair1 := [Transformation([2, 3, 4, 2]), Transformation([4, 4, 4, 4])];;
gap> pair2 := [Transformation([2, 3, 4, 2]), Transformation([1, 4, 3, 4])];;
gap> cong1 := SemigroupCongruence(S, pair1);;
gap> cong2 := SemigroupCongruence(S, pair2);;
gap> IsSuperrelation(cong1, cong2);
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(I);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(class);
gap> Unbind(cong);
gap> Unbind(cong1);
gap> Unbind(cong2);
gap> Unbind(elm);
gap> Unbind(iso);
gap> Unbind(ker);
gap> Unbind(pair);
gap> Unbind(pair1);
gap> Unbind(pair2);
gap> Unbind(pairs);
gap> Unbind(rmscong);
gap> Unbind(trc);
gap> Unbind(x);

#E# 
gap> STOP_TEST("Semigroups package: standard/cong.tst");
