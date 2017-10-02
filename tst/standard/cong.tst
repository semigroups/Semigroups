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
Error, Semigroups: SemigroupCongruence: usage,
the arguments are not valid for this function,

#T# \in: Bad input
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                   Transformation([2, 4, 2, 3, 5]),
>                   Transformation([3, 4, 3, 4, 3]),
>                   Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> cong := LeftSemigroupCongruence(S, pair1);;
gap> [Transformation([2, 1, 1, 2, 1])] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
gap> cong := RightSemigroupCongruence(S, pair1);;
gap> [Transformation([2, 1, 1, 2, 1])] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
gap> cong := SemigroupCongruence(S, pair1);;
gap> [Transformation([2, 1, 1, 2, 1])] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,

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
gap> cong := SemigroupCongruence(S, pairs);
<semigroup congruence over <simple transformation semigroup of degree 5 with 
 5 generators> with linked triple (1,1,4)>
gap> EquivalenceRelationCanonicalPartition(cong);
[ [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 3, 3, 3, 3, 3 ] ) 
     ] ]

#T# SemigroupCongruence: 0-simple semigroup
gap> S := Semigroup(Transformation([1, 2]), Transformation([1, 1]));;
gap> IsZeroSimpleSemigroup(S);
true
gap> IsRegularSemigroup(S);
true
gap> SemigroupCongruence(S, [S.1, S.1]);
<semigroup congruence over <commutative 0-simple inverse transformation 
 monoid of degree 2 with 1 generator> with linked triple (1,1,1)>

#T# SemigroupCongruence: Inverse semigroup
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 4, 2]),
>                           PartialPerm([1, 2, 3], [2, 3, 4]),
>                           PartialPerm([1, 2, 4], [2, 1, 3])]);;
gap> SemigroupCongruence(S, [S.1, S.2]);
<semigroup congruence over <inverse partial perm semigroup of size 116, 
 rank 4 with 3 generators> with 1 generating pairs>
gap> SemigroupCongruence(S, [S.1, S.2], rec(cong_by_ker_trace_threshold := 1024));
<semigroup congruence over <inverse partial perm semigroup of size 116, 
 rank 4 with 3 generators> with 1 generating pairs>
gap> SemigroupCongruence(S, [S.1, S.2], rec(cong_by_ker_trace_threshold := 0));
<semigroup congruence over <inverse partial perm semigroup of size 116, 
 rank 4 with 3 generators> with congruence pair (116,1)>

#T# SemigroupCongruence: Inverse semigroup (low cong_by_ker_trace_threshold)
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 4, 2]),
>                           PartialPerm([1, 2, 3], [2, 3, 4]),
>                           PartialPerm([1, 2, 4], [2, 1, 3])],
>                          rec(cong_by_ker_trace_threshold := 100));;
gap> SemigroupCongruence(S, [S.1, S.2], rec(cong_by_ker_trace_threshold := 100));
<semigroup congruence over <inverse partial perm semigroup of size 116, 
 rank 4 with 3 generators> with congruence pair (116,1)>

#T# SemigroupCongruence: Inverse semigroup (high cong_by_ker_trace_threshold)
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 4, 2]),
>                           PartialPerm([1, 2, 3], [2, 3, 4]),
>                           PartialPerm([1, 2, 4], [2, 1, 3])],
>                          rec(cong_by_ker_trace_threshold := 100));;
gap> SemigroupCongruence(S, [S.1, S.2],
>                        rec(cong_by_ker_trace_threshold := infinity));
<semigroup congruence over <inverse partial perm semigroup of size 116, 
 rank 4 with 3 generators> with 1 generating pairs>

#T# SemigroupCongruence: Pairs
gap> S := Semigroup([Transformation([3, 3, 3]),
>                    Transformation([3, 4, 3, 3])]);;
gap> pairs := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := SemigroupCongruence(S, pairs);;
gap> EquivalenceRelationCanonicalPartition(cong);
[ [ Transformation( [ 3, 3, 3, 3 ] ), Transformation( [ 3, 4, 3, 3 ] ) ] ]

#T# SemigroupCongruence: left congruence
gap> S := Semigroup([Transformation([3, 3, 3]),
>                    Transformation([3, 4, 3, 3])]);;
gap> pair := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := LeftSemigroupCongruence(S, pair);;
gap> EquivalenceRelationCanonicalPartition(cong);
[ [ Transformation( [ 3, 3, 3, 3 ] ), Transformation( [ 3, 4, 3, 3 ] ) ] ]
gap> pair in cong;
true
gap> [S.1, S.1] in cong;
true

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
<semigroup congruence over <commutative 0-simple inverse transformation 
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
Error, Semigroups: LeftSemigroupCongruence: usage,
the arguments are not valid for this function,

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
Error, Semigroups: RightSemigroupCongruence: usage,
the arguments are not valid for this function,

#T# LeftSemigroupCongruence: Pairs
gap> S := Semigroup([Transformation([3, 3, 3]),
>                      Transformation([3, 4, 3, 3])]);;
gap> pairs := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := LeftSemigroupCongruence(S, pairs);
<left semigroup congruence over <transformation semigroup of degree 4 with 2 
 generators> with 1 generating pairs>
gap> LeftCongruenceClassOfElement(cong, Transformation([3, 4, 3, 3]));
<left congruence class of Transformation( [ 3, 4, 3, 3 ] )>

#T# RightSemigroupCongruence: Pairs
gap> S := Semigroup([Transformation([3, 3, 3]),
>                      Transformation([3, 4, 3, 3])]);;
gap> pairs := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := RightSemigroupCongruence(S, pairs);
<right semigroup congruence over <transformation semigroup of degree 4 with 2 
 generators> with 1 generating pairs>
gap> RightCongruenceClassOfElement(cong, Transformation([3, 4, 3, 3]));
<right congruence class of Transformation( [ 3, 4, 3, 3 ] )>

#T# Equality of left and right congruences
gap> S := Semigroup(Matrix(IsBooleanMat, [[0, 1], [1, 0]]),
>                   Matrix(IsBooleanMat, [[1, 0], [1, 1]]),
>                   Matrix(IsBooleanMat, [[1, 0], [0, 0]]));;
gap> lcong := LeftSemigroupCongruence(S, [S.1, S.2]);;
gap> rcong := RightSemigroupCongruence(S, [S.1, S.2]);;
gap> lcong = rcong;
false
gap> rcong = lcong;
false
gap> lcong := LeftSemigroupCongruence(S, []);;
gap> rcong := RightSemigroupCongruence(S, []);;
gap> lcong = rcong;
true
gap> rcong = lcong;
true

#T# OnLeftCongruenceClasses
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                 Transformation([3, 4, 3, 4, 4]),
>                 Transformation([3, 4, 3, 4, 3]),
>                 Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> pair2 := [Transformation([4, 3, 4, 3, 4]),
>              Transformation([3, 4, 3, 4, 3])];;
gap> cong := LeftSemigroupCongruence(S, [pair1, pair2]);
<left semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> x := Transformation([3, 4, 3, 4, 3]);;
gap> class := LeftCongruenceClassOfElement(cong, x);
<left congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )>
gap> elm := Transformation([1, 2, 2, 1, 2]);;
gap> OnLeftCongruenceClasses(class, elm);
<left congruence class of Transformation( [ 3, 4, 4, 3, 4 ] )>

#T# OnRightCongruenceClasses
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                 Transformation([3, 4, 3, 4, 4]),
>                 Transformation([3, 4, 3, 4, 3]),
>                 Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> pair2 := [Transformation([4, 3, 4, 3, 4]),
>              Transformation([3, 4, 3, 4, 3])];;
gap> cong := RightSemigroupCongruence(S, [pair1, pair2]);
<right semigroup congruence over <transformation semigroup of degree 5 with 4 
 generators> with 2 generating pairs>
gap> x := Transformation([3, 4, 3, 4, 3]);;
gap> class := RightCongruenceClassOfElement(cong, x);
<right congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )>
gap> elm := Transformation([1, 2, 2, 1, 2]);;
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

#T# Equivalence classes
gap> S := Semigroup(
> [Transformation([1, 4, 3, 4]), Transformation([2, 3, 4, 2])]);;
gap> pair := [Transformation([2, 3, 4, 2]), Transformation([4, 4, 4, 4])];;
gap> cong1 := SemigroupCongruence(S, pair);;
gap> cong2 := SemigroupCongruence(S, []);;
gap> class1a := CongruenceClassOfElement(cong1, Transformation([4, 4, 4, 4]));;
gap> class1b := CongruenceClassOfElement(cong1, Transformation([2, 3, 4, 2]));;
gap> class1c := CongruenceClassOfElement(cong1, Transformation([1, 4, 3, 4]));;
gap> class2 := CongruenceClassOfElement(cong2, Transformation([4, 4, 4, 4]));;
gap> class1a * class1b;
<congruence class of Transformation( [ 2, 2, 2, 2 ] )>
gap> class1b * class1a;
<congruence class of Transformation( [ 4, 4, 4, 4 ] )>
gap> class1a * class2;
Error, Semigroups: \*: usage,
the args must be classes of the same congruence,
gap> class1a = class1b;
true
gap> class1a = class2;
false
gap> class1a < class1b;
false
gap> class1b < class1a;
false
gap> class1c < class1a;
true
gap> class1a < class2;
false

#T# IsSuperrelation
gap> S := Semigroup(
> [Transformation([1, 4, 3, 4]), Transformation([2, 3, 4, 2])]);;
gap> pair1 := [Transformation([2, 3, 4, 2]), Transformation([4, 4, 4, 4])];;
gap> pair2 := [Transformation([2, 3, 4, 2]), Transformation([1, 4, 3, 4])];;
gap> cong1 := SemigroupCongruence(S, pair1);;
gap> cong2 := SemigroupCongruence(S, pair2);;
gap> IsSuperrelation(cong1, cong2);
true

#T# Equality for different types of congruence class
gap> S := FullTransformationMonoid(4);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 2, 2]));;
gap> reescong := ReesCongruenceOfSemigroupIdeal(I);;
gap> pair := [Transformation([1, 1, 2, 2]), Transformation([1, 1, 1, 1])];;
gap> cong := SemigroupCongruence(S, pair);;
gap> reesclass := EquivalenceClassOfElement(reescong, pair[1]);
<congruence class of Transformation( [ 1, 1, 2, 2 ] )>
gap> class := EquivalenceClassOfElement(cong, Transformation([1, 1, 2, 2]));
<congruence class of Transformation( [ 1, 1, 2, 2 ] )>
gap> class = reesclass;
true
gap> cong := SemigroupCongruence(S, []);;
gap> class := EquivalenceClassOfElement(cong, Transformation([1, 1, 2, 2]));;
gap> class = reesclass;
false
gap> cong := UniversalSemigroupCongruence(S);;
gap> class := EquivalenceClassOfElement(cong, Transformation([1, 1, 2, 2]));;
gap> class = reesclass;
false

#T# EquivalenceRelation(Canonical)Lookup
gap> S := FullTransformationMonoid(3);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 2]));;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> EquivalenceRelationLookup(cong);
[ 1, 2, 3, 11, 5, 6, 11, 8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
  11, 11, 11, 11, 11, 11, 11 ]
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 4, 5, 6, 4, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
  4, 4 ]

#T# EquivalenceRelationCanonicalLookup with an fp semigroup
gap> F := FreeSemigroup(2);;
gap> S := F / [[F.1 ^ 2, F.1],
>              [F.1 * F.2 * F.1, F.2 * F.1],
>              [F.1 * F.2 ^ 2, F.2 * F.1],
>              [F.2 * F.1 * F.2, F.1 * F.2],
>              [F.2 ^ 2 * F.1, F.2 * F.1],
>              [F.2 ^ 3, F.2]];;
gap> cong1 := ReesCongruenceOfSemigroupIdeal(SemigroupIdeal(S, [S.1 * S.2]));;
gap> cong2 := SemigroupCongruenceByGeneratingPairs(S, [[S.1 * S.2,
>                                                       S.1 * S.2 * S.1]]);;
gap> EquivalenceRelationCanonicalLookup(cong1);
[ 1, 2, 3, 3, 4 ]
gap> EquivalenceRelationCanonicalLookup(cong2);
[ 1, 2, 3, 3, 4 ]

# EquivalenceRelationCanonicalLookup with an RMS cong
gap> S := ReesMatrixSemigroup(SymmetricGroup(3), [[(1, 2), ()], [(), (1, 3)]]);
<Rees matrix semigroup 2x2 over Sym( [ 1 .. 3 ] )>
gap> cong := RMSCongruenceByLinkedTriple(S, Group((1, 2, 3)),
>                                        [[1], [2]], [[1, 2]]);;
gap> ccong := AsSemigroupCongruenceByGeneratingPairs(cong);;
gap> cong := RMSCongruenceByLinkedTriple(S, Group((1, 2, 3)),
>                                        [[1], [2]], [[1, 2]]);;
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 1, 2, 2, 2, 1, 1, 3, 4, 4, 2, 1, 1, 2, 3, 4, 4, 4, 3, 3, 3, 4 ]
gap> EquivalenceRelationCanonicalLookup(ccong);
[ 1, 2, 3, 1, 2, 2, 2, 1, 1, 3, 4, 4, 2, 1, 1, 2, 3, 4, 4, 4, 3, 3, 3, 4 ]
gap> cong = ccong;
true

# This test used to return false because it fell back on a generic \= method for
# objects of different families (this is invalid and may give false negatives).
# We now use a correct method, but this test would run forever if enabled.
#gap> F := FreeSemigroup(2);;
#gap> cong1 := ReesCongruenceOfSemigroupIdeal(SemigroupIdeal(F, [F.1]));;
#gap> cong2 := SemigroupCongruence(F, [F.1, F.1 ^ 2]);;
#gap> cong1 = cong2;
#false

# This test made sure that an error was thrown when we tried anything with
# infinite semigroups.  Infinite semigroups are now supported, but this test
# would run forever if enabled.
#gap> F := FreeSemigroup(2);;
#gap> cong1 := LeftSemigroupCongruence(F, [F.1, F.2]);;
#gap> cong2 := RightSemigroupCongruence(F, [F.1, F.2]);;
#gap> cong1 = cong2;
#Error, no method found! For debugging hints type ?Recovery from NoMethodFound
#Error, no 2nd choice method found for `ImagesSet' on 2 arguments
#gap> cong2 = cong1;
#Error, no method found! For debugging hints type ?Recovery from NoMethodFound
#Error, no 2nd choice method found for `ImagesSet' on 2 arguments

# EquivalenceRelationLookup with an infinite semigroup
gap> F := FreeSemigroup(2);;
gap> cong := ReesCongruenceOfSemigroupIdeal(SemigroupIdeal(F, [F.1]));;
gap> EquivalenceRelationLookup(cong);
Error, Semigroups: EquivalenceRelationLookup: usage,
<cong> must be over a finite semigroup,
gap> EquivalenceRelationCanonicalLookup(cong);
Error, Semigroups: EquivalenceRelationLookup: usage,
<cong> must be over a finite semigroup,
gap> cong := LeftSemigroupCongruence(F, [F.1, F.2]);;
gap> EquivalenceRelationLookup(cong);
Error, Semigroups: EquivalenceRelationLookup: usage,
<cong> must be over a finite semigroup,
gap> cong := RightSemigroupCongruence(F, [F.1, F.2]);;
gap> EquivalenceRelationLookup(cong);
Error, Semigroups: EquivalenceRelationLookup: usage,
<cong> must be over a finite semigroup,

#T# Equality for congruences over different semigroups (false)
gap> S := Semigroup([Transformation([3, 2, 3]), Transformation([3, 1, 1])]);;
gap> congS := ReesCongruenceOfSemigroupIdeal(MinimalIdeal(S));;
gap> T := Semigroup([PartialPerm([1, 2], [3, 2]),
>                    PartialPerm([1, 3], [3, 1])]);;
gap> pair := [PartialPerm([], []), PartialPerm([3], [1])];;
gap> congT := SemigroupCongruence(T, pair);;
gap> congS = congT;
false

#T# Equality for different types of congruences over a maybe-infinite semigroup
gap> F := FreeSemigroup(2);;
gap> S := F / [[F.1 ^ 2, F.1],
>              [F.1 * F.2 * F.1, F.2 * F.1],
>              [F.1 * F.2 ^ 2, F.2 * F.1],
>              [F.2 * F.1 * F.2, F.1 * F.2],
>              [F.2 ^ 2 * F.1, F.2 * F.1],
>              [F.2 ^ 3, F.2]];;
gap> cong1 := ReesCongruenceOfSemigroupIdeal(SemigroupIdeal(S, [S.1 * S.2]));;
gap> cong2 := SemigroupCongruenceByGeneratingPairs(S, [[S.1 * S.2,
>                                                       S.1 * S.2 * S.1]]);;
gap> cong1 = cong2;
true

#T# Equality for different types of congruence, both with pairs
gap> S := ReesZeroMatrixSemigroup(SymmetricGroup(3),
>                                 [[(1, 2), ()], [(), (1, 3)]]);;
gap> cong1 := RMSCongruenceByLinkedTriple(S, Group((1, 2, 3)),
>                                         [[1], [2]], [[1, 2]]);;
gap> ideal := SemigroupIdeal(S, [MultiplicativeZero(S)]);;
gap> cong2 := ReesCongruenceOfSemigroupIdeal(ideal);;
gap> GeneratingPairsOfSemigroupCongruence(cong1);;
gap> GeneratingPairsOfSemigroupCongruence(cong2);;
gap> cong1 = cong2;
false

#T# EquivalenceRelation(Canonical)Partition
gap> S := FullTransformationMonoid(3);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 2]));;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> Size(EquivalenceRelationPartition(cong)[1]);
21
gap> EquivalenceRelationCanonicalPartition(cong);
[ [ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 1, 2 ] ), 
      Transformation( [ 1, 1 ] ), Transformation( [ 1, 2, 1 ] ), 
      Transformation( [ 1, 2, 2 ] ), Transformation( [ 1, 3, 1 ] ), 
      Transformation( [ 1, 3, 3 ] ), Transformation( [ 2, 1, 1 ] ), 
      Transformation( [ 2, 1, 2 ] ), Transformation( [ 2, 2, 1 ] ), 
      Transformation( [ 2, 2, 2 ] ), Transformation( [ 2, 2 ] ), 
      Transformation( [ 2, 3, 2 ] ), Transformation( [ 2, 3, 3 ] ), 
      Transformation( [ 3, 1, 1 ] ), Transformation( [ 3, 1, 3 ] ), 
      Transformation( [ 3, 2, 2 ] ), Transformation( [ 3, 2, 3 ] ), 
      Transformation( [ 3, 3, 1 ] ), Transformation( [ 3, 3, 2 ] ), 
      Transformation( [ 3, 3, 3 ] ) ] ]
gap> EquivalenceRelationCanonicalPartition(cong)
>        = Set(EquivalenceRelationPartition(cong), Set);
true

#T# A left congruence example
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 * F.2 ^ 2, F.2 ^ 2],
>              [F.2 ^ 3, F.2 ^ 2],
>              [F.1 ^ 4, F.1],
>              [F.2 * F.1 ^ 2 * F.2, F.2 ^ 2],
>              [F.2 * F.1 ^ 3 * F.2, F.2],
>              [(F.2 * F.1) ^ 2 * F.2, F.2],
>              [F.2 ^ 2 * F.1 ^ 3, F.2 ^ 2],
>              [F.2 * (F.2 * F.1) ^ 2, F.2 ^ 2 * F.1 ^ 2]];;
gap> cong1 := LeftSemigroupCongruence(M, [M.1, M.2 ^ 3]);;
gap> cong2 := LeftSemigroupCongruence(M, [M.2 ^ 2, M.1 ^ 7]);;
gap> lookup1 := EquivalenceRelationCanonicalLookup(cong1);;
gap> lookup2 := EquivalenceRelationCanonicalLookup(cong2);;
gap> Length(lookup1);
40
gap> lookup1 = lookup2;
true

#T# A right congruence example
gap> F := FreeMonoid(2);;
gap> M := F / [[F.1 * F.2 ^ 2, F.2 ^ 2],
>              [F.2 ^ 3, F.2 ^ 2],
>              [F.1 ^ 4, F.1],
>              [F.2 * F.1 ^ 2 * F.2, F.2 ^ 2],
>              [F.2 * F.1 ^ 3 * F.2, F.2],
>              [(F.2 * F.1) ^ 2 * F.2, F.2],
>              [F.2 ^ 2 * F.1 ^ 3, F.2 ^ 2],
>              [F.2 * (F.2 * F.1) ^ 2, F.2 ^ 2 * F.1 ^ 2]];;
gap> cong1 := RightSemigroupCongruence(M, [M.1, M.2 ^ 3]);;
gap> cong2 := RightSemigroupCongruence(M, [M.2 ^ 2, M.1 ^ 7]);;
gap> lookup1 := EquivalenceRelationCanonicalLookup(cong1);;
gap> lookup2 := EquivalenceRelationCanonicalLookup(cong2);;
gap> Length(lookup1);
40
gap> lookup1 = lookup2;
true

# Issue 393, missing method for NrEquivalenceClasses for a generic semigroup
# congruence
gap> f := FreeGroup("a");;
gap> g := f / [f.1 ^ 4];;
gap> phi := InjectionZeroMagma(g);;
gap> m := Range(phi);;
gap> el := Elements(m);;
gap> c := MagmaCongruenceByGeneratingPairs(m, [[el[2], el[3]]]);;
gap> EquivalenceRelationPartition(c);;
gap> IsReesCongruence(c);
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(F);
gap> Unbind(I);
gap> Unbind(M);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(ccong);
gap> Unbind(class);
gap> Unbind(class1a);
gap> Unbind(class1b);
gap> Unbind(class1c);
gap> Unbind(class2);
gap> Unbind(cong);
gap> Unbind(cong1);
gap> Unbind(cong2);
gap> Unbind(congS);
gap> Unbind(congT);
gap> Unbind(elm);
gap> Unbind(ideal);
gap> Unbind(iso);
gap> Unbind(ker);
gap> Unbind(lcong);
gap> Unbind(lookup1);
gap> Unbind(lookup2);
gap> Unbind(pair);
gap> Unbind(pair1);
gap> Unbind(pair2);
gap> Unbind(pairs);
gap> Unbind(rcong);
gap> Unbind(reesclass);
gap> Unbind(reescong);
gap> Unbind(rmscong);
gap> Unbind(trc);
gap> Unbind(x);

#E# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/cong.tst");
