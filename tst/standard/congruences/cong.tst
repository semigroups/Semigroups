#############################################################################
##
#W  standard/congruences/cong.tst
#Y  Copyright (C) 2015-2022                                 Michael Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local C, F, I, M, R, S, T, c, ccong, class, class1a, class1b, class1c, class2
#@local cong, cong1, cong2, cong3, congS, congT, cong_by_ker_trace_threshold
#@local el, elm, f, g, ideal, iso, ker, lcong, lookup1, lookup2, m, pair, pair1
#@local pair2, pairs, pairs1, pairs2, phi, rcong, reesclass, reescong, rmscong
#@local trc, x
gap> START_TEST("Semigroups package: standard/congruences/cong.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# SemigroupCongruence: Bad input
gap> S := FullTransformationSemigroup(5);;
gap> pairs := [[Transformation([2, 1, 3, 4, 1]), Transformation([2, 1])],
>              [Transformation([2, 1]), Transformation([2, 3, 4, 5, 2])]];;
gap> SemigroupCongruence(S);
Error, at least 2 arguments are required
gap> SemigroupCongruence(42, pairs);
Error, the 1st argument is not a semigroup
gap> SemigroupCongruence(S, pairs[1], [Transformation([2, 1])]);
Error, the 2nd argument (a list of lists) contains lists of size not equal to \
2
gap> SemigroupCongruence(S, [Transformation([2, 6, 3, 4, 5, 3]),
>                            Transformation([2, 3, 1])]);
Error, the 2nd argument (a list of lists) contains items that do not belong to\
 the 1st argument (a semigroup)
gap> S := FullTransformationSemigroup(6);;
gap> SemigroupCongruence(S, 12, 13, 100);
Error, the arguments are not valid for this function

# \in: Bad input
gap> S := Semigroup(Transformation([2, 1, 1, 2, 1]),
>                   Transformation([2, 4, 2, 3, 5]),
>                   Transformation([3, 4, 3, 4, 3]),
>                   Transformation([4, 3, 3, 4, 4]));;
gap> pair1 := [Transformation([3, 4, 3, 4, 3]),
>              Transformation([1, 2, 1, 2, 1])];;
gap> cong := LeftSemigroupCongruence(S, pair1);;
gap> [Transformation([2, 1, 1, 2, 1])] in cong;
Error, the 1st argument (a list) does not have length 2
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a left semigroup congruence)
gap> cong := RightSemigroupCongruence(S, pair1);;
gap> [Transformation([2, 1, 1, 2, 1])] in cong;
Error, the 1st argument (a list) does not have length 2
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a right semigroup congruence)
gap> cong := SemigroupCongruence(S, pair1);;
gap> [Transformation([2, 1, 1, 2, 1])] in cong;
Error, the 1st argument (a list) does not have length 2
gap> [Transformation([2, 1, 1, 2, 1]), Transformation([5, 2, 1, 2, 2])] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a 2-sided semigroup congruence)

# SemigroupCongruence: Infinite semigroup
gap> S := FreeSemigroup(2);;
gap> SemigroupCongruence(S, [S.1, S.2]);
<2-sided semigroup congruence over <free semigroup on the generators 
[ s1, s2 ]> with 1 generating pairs>

# SemigroupCongruence: Simple semigroup
gap> S := Semigroup(MinimalIdeal(FullTransformationMonoid(5)));;
gap> IsSimpleSemigroup(S);
true
gap> pairs := [
> [Transformation([1, 1, 1, 1, 1]), Transformation([3, 3, 3, 3, 3])]];;
gap> cong := SemigroupCongruence(S, pairs);;
gap> EquivalenceRelationCanonicalPartition(cong);
[ [ Transformation( [ 1, 1, 1, 1, 1 ] ), Transformation( [ 3, 3, 3, 3, 3 ] ) 
     ] ]

# SemigroupCongruence: 0-simple semigroup
gap> S := Semigroup(Transformation([1, 2]), Transformation([1, 1]));;
gap> IsZeroSimpleSemigroup(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsSimpleSemigroupCongruence(SemigroupCongruence(S, [S.1, S.1]));
true

# SemigroupCongruence: Inverse semigroup
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 4, 2]),
>                           PartialPerm([1, 2, 3], [2, 3, 4]),
>                           PartialPerm([1, 2, 4], [2, 1, 3])]);;
gap> SemigroupCongruence(S, [S.1, S.2]);
<2-sided semigroup congruence over <inverse partial perm semigroup 
 of size 116, rank 4 with 3 generators> with 1 generating pairs>
gap> SemigroupCongruence(S, [S.1, S.2], rec(cong_by_ker_trace_threshold := 1024));
<2-sided semigroup congruence over <inverse partial perm semigroup 
 of size 116, rank 4 with 3 generators> with 1 generating pairs>
gap> SemigroupCongruence(S, [S.1, S.2], rec(cong_by_ker_trace_threshold := 0));
<semigroup congruence over <inverse partial perm semigroup of size 116, 
 rank 4 with 3 generators> with congruence pair (116,1)>

# SemigroupCongruence: Inverse semigroup (low cong_by_ker_trace_threshold)
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 4, 2]),
>                           PartialPerm([1, 2, 3], [2, 3, 4]),
>                           PartialPerm([1, 2, 4], [2, 1, 3])],
>                          rec(cong_by_ker_trace_threshold := 100));;
gap> SemigroupCongruence(S, [S.1, S.2], rec(cong_by_ker_trace_threshold := 100));
<semigroup congruence over <inverse partial perm semigroup of size 116, 
 rank 4 with 3 generators> with congruence pair (116,1)>

# SemigroupCongruence: Inverse semigroup (high cong_by_ker_trace_threshold)
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 4, 2]),
>                           PartialPerm([1, 2, 3], [2, 3, 4]),
>                           PartialPerm([1, 2, 4], [2, 1, 3])],
>                          rec(cong_by_ker_trace_threshold := 100));;
gap> SemigroupCongruence(S, [S.1, S.2],
>                        rec(cong_by_ker_trace_threshold := infinity));
<2-sided semigroup congruence over <inverse partial perm semigroup 
 of size 116, rank 4 with 3 generators> with 1 generating pairs>

# SemigroupCongruence: Pairs
gap> S := Semigroup([Transformation([3, 3, 3]),
>                    Transformation([3, 4, 3, 3])]);;
gap> pairs := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := SemigroupCongruence(S, pairs);;
gap> EquivalenceRelationCanonicalPartition(cong);
[ [ Transformation( [ 3, 4, 3, 3 ] ), Transformation( [ 3, 3, 3, 3 ] ) ] ]

# SemigroupCongruence: left congruence
gap> S := Semigroup([Transformation([3, 3, 3]),
>                    Transformation([3, 4, 3, 3])]);;
gap> pair := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := LeftSemigroupCongruence(S, pair);;
gap> EquivalenceRelationCanonicalPartition(cong);
[ [ Transformation( [ 3, 4, 3, 3 ] ), Transformation( [ 3, 3, 3, 3 ] ) ] ]
gap> pair in cong;
true
gap> [S.1, S.1] in cong;
true

# SemigroupCongruence: Giving an RMS cong
gap> S := Semigroup(MinimalIdeal(FullTransformationMonoid(5)));;
gap> iso := IsomorphismReesMatrixSemigroup(S);;
gap> R := Range(iso);;
gap> pairs := [ReesMatrixSemigroupElement(R, 1, (), 1),
>              ReesMatrixSemigroupElement(R, 1, (), 3)];;
gap> rmscong := SemigroupCongruence(R, pairs);;
gap> IsSimpleSemigroupCongruence(SemigroupCongruence(S, iso, rmscong));
true

# SemigroupCongruence: Giving an RZMS cong
gap> S := Semigroup(Transformation([1, 2]), Transformation([1, 1]));;
gap> IsRegularSemigroup(S);;
gap> iso := IsomorphismReesZeroMatrixSemigroup(S);;
gap> R := Range(iso);;
gap> pairs := [ReesZeroMatrixSemigroupElement(R, 1, (), 1),
>              ReesZeroMatrixSemigroupElement(R, 1, (), 1)];;
gap> rmscong := SemigroupCongruence(R, pairs);;
gap> IsSimpleSemigroupCongruence(SemigroupCongruence(S, iso, rmscong));
true

# SemigroupCongruence: Bad R(Z)MS Input
gap> S := Semigroup(MinimalIdeal(FullTransformationMonoid(5)));;
gap> iso := IsomorphismReesMatrixSemigroup(S);;
gap> R := Range(iso);;
gap> pairs := [ReesMatrixSemigroupElement(R, 1, (), 1),
>              ReesMatrixSemigroupElement(R, 1, (), 3)];;
gap> rmscong := SemigroupCongruence(R, pairs);;
gap> S := Semigroup(Transformation([2, 2]), Transformation([1, 1]));;
gap> SemigroupCongruence(S, iso, rmscong);
Error, the range of the 3rd argument (a congruence) is not a Rees (0-)matrix s\
emigroup isomorphic to the 1st argument

# SemigroupCongruence: Rees congruence via ideal
gap> S := Semigroup(FullTransformationMonoid(5));;
gap> I := MinimalIdeal(S);;
gap> SemigroupCongruence(S, I);
<Rees congruence of <simple transformation semigroup ideal of degree 5 with
  1 generator> over <transformation monoid of degree 5 with 3 generators>>

# SemigroupCongruence: Kernel and Trace
gap> S := InverseSemigroup([PartialPerm([1, 2, 3], [1, 4, 2]),
>                           PartialPerm([1, 2, 3], [2, 3, 4]),
>                           PartialPerm([1, 2, 4], [2, 1, 3])]);;
gap> ker := IdempotentGeneratedSubsemigroup(S);;
gap> trc := List(Idempotents(S), e -> [e]);;
gap> SemigroupCongruence(S, ker, trc);;

# LeftSemigroupCongruence: Bad input
gap> S := FullTransformationSemigroup(5);;
gap> pairs := [[Transformation([2, 1, 3, 4, 1]), Transformation([2, 1])],
>              [Transformation([2, 1]), Transformation([2, 3, 4, 5, 2])]];;
gap> LeftSemigroupCongruence(S);
Error, at least 2 arguments are required
gap> LeftSemigroupCongruence(42, pairs);
Error, the 1st argument is not a semigroup
gap> LeftSemigroupCongruence(S, pairs[1], [Transformation([2, 1])]);
Error, the 2nd argument (a list of lists) contains lists of size not equal to \
2
gap> LeftSemigroupCongruence(S,
> [Transformation([2, 6, 3, 4, 5, 2]), Transformation([2, 3, 1])]);
Error, the 2nd argument (a list of lists) contains items that do not belong to\
 the 1st argument (a semigroup)
gap> S := FullTransformationSemigroup(6);;
gap> LeftSemigroupCongruence(S, 12, 13, 100);
Error, the arguments are not valid for this function

# RightSemigroupCongruence: Bad input
gap> S := FullTransformationSemigroup(5);;
gap> pairs := [[Transformation([2, 1, 3, 4, 1]), Transformation([2, 1])],
>              [Transformation([2, 1]), Transformation([2, 3, 4, 5, 2])]];;
gap> RightSemigroupCongruence(S);
Error, at least 2 arguments are required
gap> RightSemigroupCongruence(42, pairs);
Error, the 1st argument is not a semigroup
gap> RightSemigroupCongruence(S, pairs[1], [Transformation([2, 2])]);
Error, the 2nd argument (a list of lists) contains lists of size not equal to \
2
gap> RightSemigroupCongruence(S,
> [Transformation([2, 6, 3, 4, 5, 4]), Transformation([2, 3, 1])]);
Error, the 2nd argument (a list of lists) contains items that do not belong to\
 the 1st argument (a semigroup)
gap> S := FullTransformationSemigroup(6);;
gap> RightSemigroupCongruence(S, 12, 13, 100);
Error, the arguments are not valid for this function

# LeftSemigroupCongruence: Pairs
gap> S := Semigroup([Transformation([3, 3, 3]),
>                    Transformation([3, 4, 3, 3])]);;
gap> pairs := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := LeftSemigroupCongruence(S, pairs);
<left semigroup congruence over <transformation semigroup of degree 4 with 2 
 generators> with 1 generating pairs>
gap> C := EquivalenceClassOfElement(cong, Transformation([3, 4, 3, 3]));
<left congruence class of Transformation( [ 3, 4, 3, 3 ] )>
gap> Size(C);
2
gap> AsList(C);
[ Transformation( [ 3, 4, 3, 3 ] ), Transformation( [ 3, 3, 3, 3 ] ) ]
gap> C < C;
false
gap> EquivalenceClassOfElement(cong, Transformation([3, 4, 3, 3, 6, 6, 6]));
Error, the 2nd argument (a mult. elt.) does not belong to the range of the 1st\
 argument (a left congruence)
gap> Transformation([3, 4, 3, 3, 6, 6, 6]) in C;
false

# RightSemigroupCongruence: Pairs
gap> S := Semigroup([Transformation([3, 3, 3]),
>                      Transformation([3, 4, 3, 3])]);;
gap> pairs := [Transformation([3, 4, 3, 3]), Transformation([3, 3, 3, 3])];;
gap> cong := RightSemigroupCongruence(S, pairs);
<right semigroup congruence over <transformation semigroup of degree 4 with 2 
 generators> with 1 generating pairs>
gap> EquivalenceClassOfElement(cong, Transformation([3, 4, 3, 3]));
<right congruence class of Transformation( [ 3, 4, 3, 3 ] )>

# Equality of left and right congruences
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

# OnLeftCongruenceClasses
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
gap> class := EquivalenceClassOfElement(cong, x);
<left congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )>
gap> elm := Transformation([1, 2, 2, 1, 2]);;
gap> OnLeftCongruenceClasses(class, elm);
<left congruence class of Transformation( [ 3, 4, 4, 3, 4 ] )>

# OnRightCongruenceClasses
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
gap> class := EquivalenceClassOfElement(cong, x);
<right congruence class of Transformation( [ 3, 4, 3, 4, 3 ] )>
gap> elm := Transformation([1, 2, 2, 1, 2]);;
gap> OnRightCongruenceClasses(class, elm);
<right congruence class of Transformation( [ 2, 1, 2, 1, 2 ] )>

# \* for an equivalence class and a list
gap> S := Semigroup(
> [Transformation([1, 4, 3, 4]), Transformation([2, 3, 4, 2])]);;
gap> pair := [Transformation([2, 3, 4, 2]), Transformation([4, 4, 4, 4])];;
gap> cong := SemigroupCongruence(S, pair);;
gap> class := EquivalenceClassOfElement(cong, Transformation([4, 4, 4, 4]));;
gap> class * EquivalenceClasses(cong);
[ <2-sided congruence class of Transformation( [ 4, 4, 4, 4 ] )>, 
  <2-sided congruence class of Transformation( [ 2, 2, 2, 2 ] )> ]
gap> EquivalenceClasses(cong) * class;
[ <2-sided congruence class of Transformation( [ 4, 4, 4, 4 ] )>, 
  <2-sided congruence class of Transformation( [ 4, 4, 4, 4 ] )> ]

# Equivalence classes
gap> S := Semigroup(
> [Transformation([1, 4, 3, 4]), Transformation([2, 3, 4, 2])]);;
gap> pair := [Transformation([2, 3, 4, 2]), Transformation([4, 4, 4, 4])];;
gap> cong1 := SemigroupCongruence(S, pair);;
gap> cong2 := SemigroupCongruence(S, []);;
gap> class1a := EquivalenceClassOfElement(cong1, Transformation([4, 4, 4, 4]));;
gap> class1b := EquivalenceClassOfElement(cong1, Transformation([2, 3, 4, 2]));;
gap> class1c := EquivalenceClassOfElement(cong1, Transformation([1, 4, 3, 4]));;
gap> class2 := EquivalenceClassOfElement(cong2, Transformation([4, 4, 4, 4]));;
gap> class1a * class1b;
<2-sided congruence class of Transformation( [ 2, 2, 2, 2 ] )>
gap> class1b * class1a;
<2-sided congruence class of Transformation( [ 4, 4, 4, 4 ] )>
gap> class1a * class2;
Error, the arguments (cong. classes) are not classes of the same congruence
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

# IsSuperrelation
gap> S := Semigroup(
> [Transformation([1, 4, 3, 4]), Transformation([2, 3, 4, 2])]);;
gap> pair1 := [Transformation([2, 3, 4, 2]), Transformation([4, 4, 4, 4])];;
gap> pair2 := [Transformation([2, 3, 4, 2]), Transformation([1, 4, 3, 4])];;
gap> cong1 := SemigroupCongruence(S, pair1);;
gap> cong2 := SemigroupCongruence(S, pair2);;
gap> IsSuperrelation(cong1, cong2);
true

# Equality for different types of congruence class
gap> S := FullTransformationMonoid(4);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 2, 2]));;
gap> reescong := ReesCongruenceOfSemigroupIdeal(I);;
gap> pair := [Transformation([1, 1, 2, 2]), Transformation([1, 1, 1, 1])];;
gap> cong := SemigroupCongruence(S, pair);;
gap> reesclass := EquivalenceClassOfElement(reescong, pair[1]);
<2-sided congruence class of Transformation( [ 1, 1, 2, 2 ] )>
gap> class := EquivalenceClassOfElement(cong, Transformation([1, 1, 2, 2]));
<2-sided congruence class of Transformation( [ 1, 1, 2, 2 ] )>
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

# EquivalenceRelation(Canonical)Lookup
gap> S := FullTransformationMonoid(3);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 2]));;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> EquivalenceRelationLookup(cong);
[ 1, 2, 3, 11, 5, 6, 11, 8, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
  11, 11, 11, 11, 11, 11, 11 ]
gap> EquivalenceRelationCanonicalLookup(cong);
[ 1, 2, 3, 4, 5, 6, 4, 7, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
  4, 4 ]

# EquivalenceRelationCanonicalLookup with an fp semigroup
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
Error, the argument (a 2-sided congruence) must have finite range
gap> EquivalenceRelationCanonicalLookup(cong);
Error, the argument (a 2-sided congruence) must have finite range
gap> cong := LeftSemigroupCongruence(F, [F.1, F.2]);;
gap> EquivalenceRelationLookup(cong);
Error, the argument (a left congruence) must have finite range
gap> cong := RightSemigroupCongruence(F, [F.1, F.2]);;
gap> EquivalenceRelationLookup(cong);
Error, the argument (a right congruence) must have finite range

# Equality for congruences over different semigroups (false)
gap> S := Semigroup([Transformation([3, 2, 3]), Transformation([3, 1, 1])]);;
gap> congS := ReesCongruenceOfSemigroupIdeal(MinimalIdeal(S));;
gap> T := Semigroup([PartialPerm([1, 2], [3, 2]),
>                    PartialPerm([1, 3], [3, 1])]);;
gap> pair := [PartialPerm([], []), PartialPerm([3], [1])];;
gap> congT := SemigroupCongruence(T, pair);;
gap> congS = congT;
false

# Equality for different types of congruences over a maybe-infinite semigroup
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

# Equality for different types of congruence, both with pairs
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

# EquivalenceRelation(Canonical)Partition
gap> S := FullTransformationMonoid(3);;
gap> I := SemigroupIdeal(S, Transformation([1, 1, 2]));;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> Size(EquivalenceRelationPartition(cong)[1]);
21
gap> EquivalenceRelationCanonicalPartition(cong);
[ [ Transformation( [ 1, 2, 1 ] ), Transformation( [ 2, 1, 1 ] ), 
      Transformation( [ 2, 3, 2 ] ), Transformation( [ 2, 1, 2 ] ), 
      Transformation( [ 1, 1, 2 ] ), Transformation( [ 3, 2, 2 ] ), 
      Transformation( [ 1, 2, 2 ] ), Transformation( [ 3, 1, 3 ] ), 
      Transformation( [ 1, 3, 1 ] ), Transformation( [ 3, 2, 3 ] ), 
      Transformation( [ 2, 2 ] ), Transformation( [ 2, 2, 1 ] ), 
      Transformation( [ 1, 3, 3 ] ), Transformation( [ 3, 1, 1 ] ), 
      Transformation( [ 2, 3, 3 ] ), Transformation( [ 1, 1, 1 ] ), 
      Transformation( [ 3, 3, 1 ] ), Transformation( [ 1, 1 ] ), 
      Transformation( [ 3, 3, 2 ] ), Transformation( [ 2, 2, 2 ] ), 
      Transformation( [ 3, 3, 3 ] ) ] ]

# A left congruence example
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

# A right congruence example
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
# gap> f := FreeGroup("a");;
# gap> g := f / [f.1 ^ 4];;
# gap> phi := InjectionZeroMagma(g);;
# gap> m := Range(phi);;
# gap> el := Elements(m);;
# gap> c := MagmaCongruenceByGeneratingPairs(m, [[el[2], el[3]]]);;
# gap> EquivalenceRelationPartition(c);;
# gap> IsReesCongruence(c);
# false

# EquivalenceRelationPartitionWithSingletons non-finite
gap> S := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> C := SemigroupCongruence(S, [S.1, S.2]);
<2-sided semigroup congruence over <free semigroup on the generators 
[ s1, s2 ]> with 1 generating pairs>
gap> EquivalenceRelationPartitionWithSingletons(C);
Error, the argument (a congruence) must have finite range

# MeetSemigroupCongruences, error different ranges
gap> S := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> T := Semigroup([Transformation([2, 1, 1, 2, 1]),
>                    Transformation([3, 4, 3, 4, 4]),
>                    Transformation([3, 4, 3, 4, 3]),
>                    Transformation([4, 3, 3, 4, 4])]);;
gap> IsSimpleSemigroup(T);
true
gap> cong1 := SemigroupCongruence(T,
> [[Transformation([1, 2, 1, 2, 2]),
>   Transformation([2, 1, 2, 1, 2])],
>  [Transformation([2, 1, 1, 2, 2]),
>   Transformation([1, 2, 2, 1, 2])]]);;
gap> cong2 := SemigroupCongruence(S, []);;
gap> MeetSemigroupCongruences(cong1, cong2);
Error, cannot form the meet of congruences over different semigroups

# MeetSemigroupCongruences, contained
gap> S := PartitionMonoid(3);;
gap> pairs1 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, -1, -2, -3], [3]])]];;
gap> pairs2 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, 3, -1, -2], [-3]])],
>               [Bipartition([[1, 2, -1, -2], [3, -3]]),
>                Bipartition([[1, 2, -3], [3, -1, -2]])]];;
gap> cong1 := SemigroupCongruence(S, pairs1);;
gap> cong2 := SemigroupCongruence(S, pairs2);;
gap> cong3 := JoinSemigroupCongruences(cong1, cong2);
<2-sided semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 3 generating pairs>
gap> MeetSemigroupCongruences(cong1, cong3) = cong1;
true
gap> MeetSemigroupCongruences(cong2, cong3) = cong2;
true
gap> MeetSemigroupCongruences(cong3, cong1) = cong1;
true
gap> MeetSemigroupCongruences(cong3, cong2) = cong2;
true
gap> MeetSemigroupCongruences(cong1, cong2);
<2-sided semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 1 generating pairs>
gap> MeetSemigroupCongruences(cong3, cong3) = cong3;
true

# MeetRightSemigroupCongruences
gap> S := PartitionMonoid(3);;
gap> pairs1 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, -1, -2, -3], [3]])]];;
gap> pairs2 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, 3, -1, -2], [-3]])],
>               [Bipartition([[1, 2, -1, -2], [3, -3]]),
>                Bipartition([[1, 2, -3], [3, -1, -2]])]];;
gap> cong1 := RightSemigroupCongruence(S, pairs1);;
gap> cong2 := RightSemigroupCongruence(S, pairs2);;
gap> cong3 := JoinRightSemigroupCongruences(cong1, cong2);
<right semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 3 generating pairs>
gap> MeetRightSemigroupCongruences(cong1, cong3) = cong1;
true
gap> MeetRightSemigroupCongruences(cong2, cong3) = cong2;
true
gap> MeetRightSemigroupCongruences(cong3, cong1) = cong1;
true
gap> MeetRightSemigroupCongruences(cong3, cong2) = cong2;
true
gap> MeetRightSemigroupCongruences(cong1, cong2);
<right semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 0 generating pairs>
gap> MeetRightSemigroupCongruences(cong3, cong3) = cong3;
true

# MeetLeftSemigroupCongruences
gap> S := PartitionMonoid(3);;
gap> pairs1 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, -1, -2, -3], [3]])]];;
gap> pairs2 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, 3, -1, -2], [-3]])],
>               [Bipartition([[1, 2, -1, -2], [3, -3]]),
>                Bipartition([[1, 2, -3], [3, -1, -2]])]];;
gap> cong1 := LeftSemigroupCongruence(S, pairs1);;
gap> cong2 := LeftSemigroupCongruence(S, pairs2);;
gap> cong3 := JoinLeftSemigroupCongruences(cong1, cong2);
<left semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 3 generating pairs>
gap> MeetLeftSemigroupCongruences(cong1, cong3) = cong1;
true
gap> MeetLeftSemigroupCongruences(cong2, cong3) = cong2;
true
gap> MeetLeftSemigroupCongruences(cong3, cong1) = cong1;
true
gap> MeetLeftSemigroupCongruences(cong3, cong2) = cong2;
true
gap> MeetLeftSemigroupCongruences(cong1, cong2);
<left semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 0 generating pairs>
gap> MeetLeftSemigroupCongruences(cong3, cong3) = cong3;
true

#
gap> S := PartitionMonoid(3);;
gap> pairs1 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, -1, -2, -3], [3]])]];;
gap> pairs2 := [[Bipartition([[1, 2, 3, -1, -2, -3]]),
>                Bipartition([[1, 2, 3, -1, -2], [-3]])],
>               [Bipartition([[1, 2, -1, -2], [3, -3]]),
>                Bipartition([[1, 2, -3], [3, -1, -2]])]];;
gap> C := SemigroupCongruence(S, pairs1);
<2-sided semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 1 generating pairs>
gap> EquivalenceRelationLookup(C);
[ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 
  22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 30, 34, 35, 36, 37, 38, 39, 40, 
  41, 42, 43, 30, 45, 46, 47, 48, 36, 50, 51, 52, 53, 54, 30, 56, 30, 56, 59, 
  30, 30, 56, 63, 36, 37, 66, 67, 37, 36, 70, 30, 72, 73, 30, 75, 76, 36, 78, 
  79, 56, 30, 56, 83, 30, 85, 36, 87, 67, 37, 90, 30, 92, 93, 30, 56, 96, 56, 
  30, 30, 56, 36, 102, 36, 36, 67, 37, 67, 30, 56, 30, 56, 112, 30, 30, 56, 
  116, 117, 67, 119, 30, 121, 122, 30, 56, 36, 126, 67, 37, 30, 30, 56, 132, 
  30, 30, 56, 30, 56, 67, 36, 67, 37, 67, 30, 56, 30, 30, 36, 148, 36, 36, 
  30, 56, 153, 30, 56, 67, 56, 30, 36, 160, 36, 36, 30, 67, 37, 30, 56, 56, 
  67, 36, 67, 37, 67, 56, 36, 176, 36, 36, 56, 67, 36, 67, 37, 67, 56, 30, 
  36, 67, 37, 67, 36, 67, 37, 67, 67, 37, 56, 67, 36, 67, 37, 67, 37 ]
gap> EquivalenceRelationPartition(C);
[ [ <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, -3 ], [ 2 ], [ 3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, -1 ], [ 3 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, -2 ], [ 3 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, -3 ], [ 3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, -1 ], [ 2 ], [ 3 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 3, -3 ], [ 2 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 2, -1 ], [ 3 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, -2 ], [ 3 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, -3 ], [ 2, 3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 3, -1 ], [ 2 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 3, -2 ], [ 2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, 2, 3, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, -2 ], [ 2, 3 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, -1 ], [ 2, 3 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1 ], [ -2 ], [ -3 ]> ], 
  [ <bipartition: [ 1, 2, -2, -3 ], [ 3 ], [ -1 ]>, 
      <bipartition: [ 1, 3, -2, -3 ], [ 2 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2, -2, -3 ], [ 3 ], [ -1 ]>, 
      <bipartition: [ 1, 2, 3, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1, -2, -3 ], [ 2 ], [ 3 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2, -1 ], [ 3 ], [ -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, -1 ], [ 2 ], [ 3 ], [ -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2, -1 ], [ 3 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 3, -1 ], [ 2 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, -2, -3 ], [ 2, 3 ], [ -1 ]>, 
      <bipartition: [ 1, -1 ], [ 2, 3 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2, 3, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1 ], [ -2, -3 ]> ], 
  [ <block bijection: [ 1, 2, 3, -1, -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -1, -2, -3 ]>, 
      <bipartition: [ 1, 2, -1, -2, -3 ], [ 3 ]>, 
      <bipartition: [ 1, 3, -1, -2, -3 ], [ 2 ]>, 
      <bipartition: [ 1 ], [ 2, -1, -2, -3 ], [ 3 ]>, 
      <bipartition: [ 1, -1, -2, -3 ], [ 2 ], [ 3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1, -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1, -2, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1, -2, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1, -2, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1, -2, -3 ]>, 
      <bipartition: [ 1, -1, -2, -3 ], [ 2, 3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1, -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1, -2, -3 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1, -2, -3 ]> ], 
  [ <bipartition: [ 1 ], [ 2, 3, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ -1, -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 2, -1, -2 ], [ 3 ], [ -3 ]>, 
      <bipartition: [ 1, -3 ], [ 2 ], [ 3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 3, -1, -2 ], [ 2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, -1, -2 ], [ 3 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, -3 ], [ 3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, -1, -2 ], [ 2 ], [ 3 ], [ -3 ]>, 
      <bipartition: [ 1, 3, -3 ], [ 2 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, -3 ], [ 2, 3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, -1, -2 ], [ 2, 3 ], [ -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1, -2 ], [ -3 ]> ], 
  [ <bipartition: [ 1, 2, -1, -3 ], [ 3 ], [ -2 ]>, 
      <bipartition: [ 1, 3, -1, -3 ], [ 2 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, -1, -3 ], [ 3 ], [ -2 ]>, 
      <bipartition: [ 1, 2, 3, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, -1, -3 ], [ 2 ], [ 3 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, -2 ], [ 3 ], [ -1, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ -1, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 2, -2 ], [ 3 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 3, -2 ], [ 2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, -1, -3 ], [ 2, 3 ], [ -2 ]>, 
      <bipartition: [ 1, -2 ], [ 2, 3 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 2, 3, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1, -3 ], [ -2 ]> ] ]
gap> EquivalenceRelationPartitionWithSingletons(C);
[ [ <block bijection: [ 1, -1 ], [ 2, -2 ], [ 3, -3 ]> ], 
  [ <block bijection: [ 1, -2 ], [ 2, -3 ], [ 3, -1 ]> ], 
  [ <block bijection: [ 1, -2 ], [ 2, -1 ], [ 3, -3 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -2 ], [ 3, -3 ], [ -1 ]> ], 
  [ <block bijection: [ 1, 2, -1, -2 ], [ 3, -3 ]> ], 
  [ <block bijection: [ 1, -3 ], [ 2, -1 ], [ 3, -2 ]> ], 
  [ <block bijection: [ 1, -1 ], [ 2, -3 ], [ 3, -2 ]> ], 
  [ <bipartition: [ 1, -2 ], [ 2, -3 ], [ 3 ], [ -1 ]> ], 
  [ <block bijection: [ 1, 3, -1, -2 ], [ 2, -3 ]> ], 
  [ <block bijection: [ 1, -3 ], [ 2, -2 ], [ 3, -1 ]> ], 
  [ <bipartition: [ 1, -2 ], [ 2 ], [ 3, -3 ], [ -1 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -3 ], [ 3, -1 ], [ -2 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -1 ], [ 3, -3 ], [ -2 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -1, -2 ], [ 3, -3 ]> ], 
  [ <block bijection: [ 1, 2, -2, -3 ], [ 3, -1 ]> ], 
  [ <bipartition: [ 1, 2, -2 ], [ 3, -3 ], [ -1 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2 ], [ 3, -2 ], [ -1 ]> ], 
  [ <block bijection: [ 1, -3 ], [ 2, 3, -1, -2 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -3 ], [ 3, -2 ], [ -1 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -1 ], [ 3 ], [ -2 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, -3 ], [ 3 ], [ -2 ]> ], 
  [ <bipartition: [ 1, -1, -2 ], [ 2, -3 ], [ 3 ]> ], 
  [ <block bijection: [ 1, 3, -2, -3 ], [ 2, -1 ]> ], 
  [ <bipartition: [ 1, 3, -2 ], [ 2, -3 ], [ -1 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -2 ], [ 3 ], [ -1 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2 ], [ 3, -1 ], [ -2 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2 ], [ 3, -3 ], [ -2 ]> ], 
  [ <bipartition: [ 1, -1, -2 ], [ 2 ], [ 3, -3 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -1 ], [ 3, -2 ], [ -3 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, -3 ], [ 2 ], [ 3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, -1 ], [ 3 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, -2 ], [ 3 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, -3 ], [ 3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, -1 ], [ 2 ], [ 3 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 3, -3 ], [ 2 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 2, -1 ], [ 3 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, -2 ], [ 3 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, -3 ], [ 2, 3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 3, -1 ], [ 2 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 3, -2 ], [ 2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3, -3 ], [ -1 ], [ -2 ]>, 
      <bipartition: [ 1, 2, 3, -2 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, -2 ], [ 2, 3 ], [ -1 ], [ -3 ]>, 
      <bipartition: [ 1, -1 ], [ 2, 3 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3, -1 ], [ -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1 ], [ -2 ], [ -3 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -3 ], [ 3, -1, -2 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -2 ], [ 3, -1 ], [ -3 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -2, -3 ], [ 3, -1 ]> ], 
  [ <block bijection: [ 1, 2, -1, -3 ], [ 3, -2 ]> ], 
  [ <bipartition: [ 1, 2, -2, -3 ], [ 3 ], [ -1 ]>, 
      <bipartition: [ 1, 3, -2, -3 ], [ 2 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2, -2, -3 ], [ 3 ], [ -1 ]>, 
      <bipartition: [ 1, 2, 3, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1, -2, -3 ], [ 2 ], [ 3 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2, -1 ], [ 3 ], [ -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, -1 ], [ 2 ], [ 3 ], [ -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2, -1 ], [ 3 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 3, -1 ], [ 2 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -2, -3 ], [ -1 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, -2, -3 ], [ 2, 3 ], [ -1 ]>, 
      <bipartition: [ 1, -1 ], [ 2, 3 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2, 3, -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1 ], [ -2, -3 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1 ], [ -2, -3 ]> ], 
  [ <block bijection: [ 1, 2, 3, -1, -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -1, -2, -3 ]>, 
      <bipartition: [ 1, 2, -1, -2, -3 ], [ 3 ]>, 
      <bipartition: [ 1, 3, -1, -2, -3 ], [ 2 ]>, 
      <bipartition: [ 1 ], [ 2, -1, -2, -3 ], [ 3 ]>, 
      <bipartition: [ 1, -1, -2, -3 ], [ 2 ], [ 3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1, -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1, -2, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1, -2, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1, -2, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1, -2, -3 ]>, 
      <bipartition: [ 1, -1, -2, -3 ], [ 2, 3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1, -2, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1, -2, -3 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1, -2, -3 ]> ], 
  [ <bipartition: [ 1, 2, -3 ], [ 3, -1 ], [ -2 ]> ], 
  [ <bipartition: [ 1, 2, -1 ], [ 3, -3 ], [ -2 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2 ], [ 3, -1, -2 ]> ], 
  [ <block bijection: [ 1, -1 ], [ 2, 3, -2, -3 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, 3, -2 ], [ -1 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, -2 ], [ 3 ], [ -3 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, -1, -2 ], [ 3 ]> ], 
  [ <bipartition: [ 1, -2 ], [ 2, -1 ], [ 3 ], [ -3 ]> ], 
  [ <bipartition: [ 1, -2, -3 ], [ 2, -1 ], [ 3 ]> ], 
  [ <block bijection: [ 1, 3, -1, -3 ], [ 2, -2 ]> ], 
  [ <bipartition: [ 1, 3, -3 ], [ 2, -1 ], [ -2 ]> ], 
  [ <bipartition: [ 1, 3, -1 ], [ 2, -3 ], [ -2 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2 ], [ 3, -2 ], [ -3 ]> ], 
  [ <bipartition: [ 1, -2 ], [ 2 ], [ 3, -1 ], [ -3 ]> ], 
  [ <bipartition: [ 1, -2, -3 ], [ 2 ], [ 3, -1 ]> ], 
  [ <bipartition: [ 1 ], [ 2, 3, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, -3 ], [ 3 ], [ -1, -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 2, -1, -2 ], [ 3 ], [ -3 ]>, 
      <bipartition: [ 1, -3 ], [ 2 ], [ 3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 3, -1, -2 ], [ 2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, -1, -2 ], [ 3 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, -3 ], [ 3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, -1, -2 ], [ 2 ], [ 3 ], [ -3 ]>, 
      <bipartition: [ 1, 3, -3 ], [ 2 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, -3 ], [ 2, 3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3, -3 ], [ -1, -2 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, -1, -2 ], [ 2, 3 ], [ -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1, -2 ], [ -3 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1, -2 ], [ -3 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -1 ], [ 3, -2, -3 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -1, -3 ], [ 3, -2 ]> ], 
  [ <bipartition: [ 1, 2, -3 ], [ 3, -2 ], [ -1 ]> ], 
  [ <bipartition: [ 1, 2, -1, -3 ], [ 3 ], [ -2 ]>, 
      <bipartition: [ 1, 3, -1, -3 ], [ 2 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, -1, -3 ], [ 3 ], [ -2 ]>, 
      <bipartition: [ 1, 2, 3, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, -1, -3 ], [ 2 ], [ 3 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, -2 ], [ 3 ], [ -1, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, -2 ], [ 2 ], [ 3 ], [ -1, -3 ]>, 
      <bipartition: [ 1 ], [ 2 ], [ 3 ], [ -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 2, -2 ], [ 3 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 2 ], [ 3, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 3, -2 ], [ 2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 3 ], [ 2, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 2 ], [ 3 ], [ -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1 ], [ 2, 3, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1, -1, -3 ], [ 2, 3 ], [ -2 ]>, 
      <bipartition: [ 1, -2 ], [ 2, 3 ], [ -1, -3 ]>, 
      <bipartition: [ 1, 3 ], [ 2 ], [ -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 2, 3, -2 ], [ -1, -3 ]>, 
      <bipartition: [ 1 ], [ 2, 3 ], [ -1, -3 ], [ -2 ]>, 
      <bipartition: [ 1, 2, 3 ], [ -1, -3 ], [ -2 ]> ], 
  [ <bipartition: [ 1, 2, -1 ], [ 3, -2 ], [ -3 ]> ], 
  [ <block bijection: [ 1, 2, -3 ], [ 3, -1, -2 ]> ], 
  [ <bipartition: [ 1, 2, -2 ], [ 3, -1 ], [ -3 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2 ], [ 3, -2, -3 ]> ], 
  [ <block bijection: [ 1, -2 ], [ 2, 3, -1, -3 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, 3, -3 ], [ -2 ]> ], 
  [ <bipartition: [ 1, -3 ], [ 2, 3, -1 ], [ -2 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, -2, -3 ], [ 3 ]> ], 
  [ <bipartition: [ 1, -1, -3 ], [ 2, -2 ], [ 3 ]> ], 
  [ <bipartition: [ 1, 3, -3 ], [ 2, -2 ], [ -1 ]> ], 
  [ <bipartition: [ 1, 3, -1 ], [ 2, -2 ], [ -3 ]> ], 
  [ <block bijection: [ 1, 3, -3 ], [ 2, -1, -2 ]> ], 
  [ <bipartition: [ 1, 3, -2 ], [ 2, -1 ], [ -3 ]> ], 
  [ <bipartition: [ 1, -1, -3 ], [ 2 ], [ 3, -2 ]> ], 
  [ <bipartition: [ 1 ], [ 2, -2 ], [ 3, -1, -3 ]> ], 
  [ <block bijection: [ 1, 2, -1 ], [ 3, -2, -3 ]> ], 
  [ <bipartition: [ 1, -2 ], [ 2 ], [ 3, -1, -3 ]> ], 
  [ <bipartition: [ 1, -2 ], [ 2, 3, -3 ], [ -1 ]> ], 
  [ <bipartition: [ 1, -2 ], [ 2, 3, -1 ], [ -3 ]> ], 
  [ <block bijection: [ 1, -1, -2 ], [ 2, 3, -3 ]> ], 
  [ <bipartition: [ 1, -1 ], [ 2, 3, -2 ], [ -3 ]> ], 
  [ <bipartition: [ 1, -2 ], [ 2, -1, -3 ], [ 3 ]> ], 
  [ <block bijection: [ 1, 3, -1 ], [ 2, -2, -3 ]> ], 
  [ <block bijection: [ 1, 2, -2 ], [ 3, -1, -3 ]> ], 
  [ <block bijection: [ 1, -2, -3 ], [ 2, 3, -1 ]> ], 
  [ <block bijection: [ 1, 3, -2 ], [ 2, -1, -3 ]> ], 
  [ <block bijection: [ 1, -1, -3 ], [ 2, 3, -2 ]> ] ]

# TrivialCongruence
gap> S := PartitionMonoid(3);;
gap> TrivialCongruence(S);
<2-sided semigroup congruence over <regular bipartition *-monoid of size 203, 
 degree 3 with 4 generators> with 0 generating pairs>

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/congruences/cong.tst");
