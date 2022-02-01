#############################################################################
##
#W  standard/libsemigroups/cong.tst
#Y  Copyright (C) 2022                                     James Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/libsemigroups/cong.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# CppCongruenceConstructor for transf. semigroup
gap> S := Semigroup(Transformation([1, 1, 2]));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end
gap> S := Semigroup(ConstantTransformation(17, 2));
<commutative transformation semigroup of degree 17 with 1 generator>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end
gap> S := Semigroup(ConstantTransformation(65537, 2));
<commutative transformation semigroup of degree 65537 with 1 generator>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end

# CppCongruenceConstructor for pperm semigroup
gap> S := Semigroup(PartialPerm([1, 2, 5]));
<commutative partial perm semigroup of rank 3 with 1 generator>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end
gap> S := Semigroup(PartialPerm([1 .. 17]));
<trivial partial perm group of rank 17 with 1 generator>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end
gap> S := Semigroup(PartialPerm([1 .. 65537]));
<trivial partial perm group of rank 65537 with 1 generator>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end

# CppCongruenceConstructor for a bmat semigroup
gap> S := Semigroup(Matrix(IsBooleanMat, [[0, 0], [0, 0]]),
>                   Matrix(IsBooleanMat, [[1, 1], [0, 1]]));
<semigroup of 2x2 boolean matrices with 2 generators>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end
gap> S := Semigroup(
> Matrix(IsBooleanMat, [[0, 0, 0, 1, 1, 1, 0, 1, 0], [0, 0, 1, 1, 0, 0, 1, 1, 0],
>       [0, 1, 0, 1, 0, 1, 0, 1, 0], [1, 0, 0, 0, 1, 1, 0, 0, 0], [1, 0, 0, 0, 1, 1, 0, 1, 1],
>       [0, 0, 1, 1, 1, 1, 1, 1, 1], [1, 1, 1, 1, 0, 0, 0, 1, 0], [0, 1, 1, 0, 0, 0, 1, 0, 0],
>       [1, 0, 0, 1, 1, 1, 1, 0, 1]]));
<commutative semigroup of 9x9 boolean matrices with 1 generator>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end

# CppCongruenceConstructor for other matrix over semiring
gap> S := Semigroup(Matrix(IsMinPlusMatrix, [[-2, 2], [0, -1]]),
>                   Matrix(IsMinPlusMatrix, [[0, 0], [1, -3]]));
<semigroup of 2x2 min-plus matrices with 2 generators>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]),
>                   Matrix(IsMaxPlusMatrix, [[0, 0], [1, -3]]));
<semigroup of 2x2 max-plus matrices with 2 generators>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end

# CppCongruenceConstructor for bipartition semigroup
gap> S := PartitionMonoid(2);
<regular bipartition *-monoid of size 15, degree 2 with 3 generators>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end

# CppCongruenceConstructor for pbr semigroup
gap> S := FullPBRMonoid(1);
<pbr monoid of degree 1 with 4 generators>
gap> CppCongruenceConstructor(S);
function( arg1 ) ... end

# CppCongruence for a congruence on a semigroup with CanComputeCppFroidurePin
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> C := SemigroupCongruence(S, [[S.1, S.3]]);
<semigroup congruence over <regular monoid of size 16, 2x2 boolean matrices 
 with 3 generators> with 1 generating pairs>
gap> CppCongruence(C);; # Can't test output because it contains the memory address

# CppCongruence for a congruence on a fp semigroup
gap> S := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> C := SemigroupCongruence(S, [[S.1, S.2]]);
<semigroup congruence over <free semigroup on the generators [ s1, s2 ]> with 
1 generating pairs>
gap> CppCongruence(C);;

# CppCongruence for a congruence on a semigroup with CanComputeGapFroidurePin
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> C := SemigroupCongruence(S, [[S.1, S.2]]);
<semigroup congruence over <free band on the generators [ x1, x2 ]> with 
1 generating pairs>
gap> CppCongruence(C);;
gap> C := LeftSemigroupCongruence(S, [[S.1, S.2]]);
<left semigroup congruence over <free band on the generators [ x1, x2 ]> with 
1 generating pairs>
gap> CppCongruence(C);;

# CongruenceWordToClassIndex
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> C := LeftSemigroupCongruence(S, [[S.1, S.2]]);
<left semigroup congruence over <free band on the generators [ x1, x2 ]> with 
1 generating pairs>
gap> CongruenceWordToClassIndex(C, [1, 2, 1, 2, 1, 2, 1, 1, 1, 1]);
1
gap> CongruenceWordToClassIndex(C, EvaluateWord([S.1, S.2],
> [1, 2, 1, 2, 1, 2, 1, 1, 1, 1]));
1

# CongruenceLessNC
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> C := LeftSemigroupCongruence(S, [[S.1, S.2]]);
<left semigroup congruence over <free band on the generators [ x1, x2 ]> with 
1 generating pairs>
gap> CongruenceLessNC(C, S.1, S.2);
false
gap> EquivalenceRelationLookup(C);
[ 1, 1, 1, 1, 1, 1 ]
gap> CongruenceLessNC(C, S.2, S.1);
false
gap> NrEquivalenceClasses(C);
1
gap> C := RightSemigroupCongruence(S, [[S.1, S.2]]);
<right semigroup congruence over <free band on the generators 
[ x1, x2 ]> with 1 generating pairs>
gap> CongruenceLessNC(C, S.1, S.2);
false
gap> EquivalenceRelationLookup(C);
[ 1, 1, 1, 1, 1, 1 ]
gap> CongruenceLessNC(C, S.2, S.1);
false
gap> S := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> C := SemigroupCongruence(S, [[S.1, S.2]]);
<semigroup congruence over <free semigroup on the generators [ s1, s2 ]> with 
1 generating pairs>
gap> NrEquivalenceClasses(C);
infinity
gap> CongruenceLessNC(C, S.2, S.1);
false
gap> C := SemigroupCongruence(S, [[S.1, S.2], [S.1 * S.2, S.2 * S.1], [S.1 ^ 10, S.1]]);
<semigroup congruence over <free semigroup on the generators [ s1, s2 ]> with 
3 generating pairs>
gap> CongruenceLessNC(C, S.2, S.1);
false
gap> NrEquivalenceClasses(C);
9

# CongruenceTestMembershipNC
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> C := LeftSemigroupCongruence(S, [[S.1, S.2]]);
<left semigroup congruence over <free band on the generators [ x1, x2 ]> with 
1 generating pairs>
gap> CongruenceTestMembershipNC(C, S.1 ^ 4, S.1 * S.2 ^ 3 * S.1 ^ 3 * S.2 ^ 2 * S.1 ^ 2);
true
gap> EquivalenceRelationLookup(C);
[ 1, 1, 1, 1, 1, 1 ]
gap> CongruenceTestMembershipNC(C, S.1 ^ 4, S.1 * S.2 ^ 3 * S.1 ^ 3 * S.2 ^ 2 * S.1 ^ 2);
true
gap> S := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> C := SemigroupCongruence(S, [[S.1, S.2]]);
<semigroup congruence over <free semigroup on the generators [ s1, s2 ]> with 
1 generating pairs>
gap> CongruenceTestMembershipNC(C, S.1 ^ 4, S.1 * S.2 ^ 3 * S.1 ^ 3 * S.2 ^ 2 * S.1 ^ 2);
false
gap> CongruenceTestMembershipNC(C, S.1 ^ 4, S.1 * S.2 ^ 3);
true

# EquivalenceRelationPartition
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> C := LeftSemigroupCongruence(S, [[S.1, S.2]]);
<left semigroup congruence over <free band on the generators [ x1, x2 ]> with 
1 generating pairs>
gap> EquivalenceRelationPartition(C);
[ [ x1, x2, x1x2, x2x1, x1x2x1, x2x1x2 ] ]
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<semigroup of size 6, 7x7 boolean matrices with 2 generators>
gap> C := LeftSemigroupCongruence(T, [[T.1, T.2]]);
<left semigroup congruence over <semigroup of size 6, 7x7 boolean matrices 
 with 2 generators> with 1 generating pairs>
gap> EquivalenceRelationPartition(C);
[ [ Matrix(IsBooleanMat, [[1, 0, 0, 0, 0, 0, 0], [0, 0, 0, 1, 0, 0, 0], 
          [0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 0, 0], 
          [0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 0, 0], [1, 0, 0, 0, 0, 0, 0]]
          ), 
      Matrix(IsBooleanMat, [[0, 0, 1, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0], 
          [0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0], 
          [0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0], [0, 1, 0, 0, 0, 0, 0]]
          ), 
      Matrix(IsBooleanMat, [[0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0], 
          [0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0], 
          [0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0], [0, 0, 1, 0, 0, 0, 0]]
          ), 
      Matrix(IsBooleanMat, [[0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 0, 0], 
          [0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 0, 0], 
          [0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 0, 0], [0, 0, 0, 1, 0, 0, 0]]
          ), 
      Matrix(IsBooleanMat, [[0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 0, 0], 
          [0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 0, 0], 
          [0, 0, 0, 0, 1, 0, 0], [0, 0, 0, 1, 0, 0, 0], [0, 0, 0, 0, 1, 0, 0]]
          ), 
      Matrix(IsBooleanMat, [[0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0], 
          [0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0], 
          [0, 0, 1, 0, 0, 0, 0], [0, 0, 0, 0, 0, 1, 0], [0, 0, 0, 0, 0, 1, 0]]
          ) ] ]

# \< for congruence classes
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> C := LeftSemigroupCongruence(S, [[S.1, S.1 ^ 10]]);
<left semigroup congruence over <free band on the generators [ x1, x2 ]> with 
0 generating pairs>
gap> AsSSortedList(EquivalenceClasses(C));
[ <left congruence class of x1>, <left congruence class of x2>, 
  <left congruence class of x2x1>, <left congruence class of x1x2>, 
  <left congruence class of x1x2x1>, <left congruence class of x2x1x2> ]
gap> D := LeftSemigroupCongruence(S, [[S.1, S.2 ^ 10]]);
<left semigroup congruence over <free band on the generators [ x1, x2 ]> with 
1 generating pairs>
gap> EquivalenceClasses(D)[1] < EquivalenceClasses(C)[1];
false

# \< method doesn't apply
gap> I := SemigroupIdealByGenerators(FullTransformationSemigroup(4),
> [Transformation([1, 2, 2, 2])]);;
gap> cong := ReesCongruenceOfSemigroupIdeal(I);;
gap> hom := HomomorphismQuotientSemigroup(cong);;
gap> T := Range(hom);;
gap> IsSemigroup(T);
true
gap> Size(T);
169
gap> u := Image(hom, Transformation([1, 1, 1, 1]));
<congruence class of Transformation( [ 1, 2, 2, 2 ] )>
gap> t := Image(hom, Transformation([2, 1, 2, 3]));
<congruence class of Transformation( [ 2, 1, 2, 3 ] )>
gap> u < t;
true

# EquivalenceClasses for a congruence with infinitely many classes
gap> S := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> C := SemigroupCongruence(S, [[S.1, S.2]]);
<semigroup congruence over <free semigroup on the generators [ s1, s2 ]> with 
1 generating pairs>
gap> EquivalenceClasses(C);
Error, the argument (a congruence) must have a finite number of classes
gap> EquivalenceRelationLookup(C);
Error, the argument (a congruence) must have finite range

# ImagesElm
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> C := LeftSemigroupCongruence(S, [[S.1, S.1 ^ 10]]);
<left semigroup congruence over <free band on the generators [ x1, x2 ]> with 
0 generating pairs>
gap> ImagesElm(C, S.1);
[ x1 ]
gap> F := FreeSemigroup(2);
<free semigroup on the generators [ s1, s2 ]>
gap> R := [[F.1, F.2], [F.1 * F.2 * F.1, F.2 * F.1], [F.1 ^ 10, F.1]];
[ [ s1, s2 ], [ s1*s2*s1, s2*s1 ], [ s1^10, s1 ] ]
gap> S := F / R;
<fp semigroup with 2 generators and 3 relations>
gap> C := SemigroupCongruence(S, []);
<universal semigroup congruence over <trivial group with 2 generators>>
gap> ImagesElm(C, S.1 ^ 10);
[ s1 ]

#
gap> F := FreeSemigroup(3);
<free semigroup on the generators [ s1, s2, s3 ]>
gap> R := [[F.1 * F.2, F.2 * F.1], [F.1 * F.3, F.3 * F.1], [F.1 ^ 2, F.1],
>          [F.1 * F.3, F.1], [F.3 * F.1, F.1], [F.2 * F.3, F.3 * F.2], 
>          [F.2 ^ 3, F.2], [F.2 * F.3, F.2], [F.3 * F.2, F.2]];
[ [ s1*s2, s2*s1 ], [ s1*s3, s3*s1 ], [ s1^2, s1 ], [ s1*s3, s1 ], 
  [ s3*s1, s1 ], [ s2*s3, s3*s2 ], [ s2^3, s2 ], [ s2*s3, s2 ], [ s3*s2, s2 ] 
 ]
gap> S := F / R;
<fp semigroup with 3 generators and 9 relations>
gap> C := SemigroupCongruence(S, [[S.1, S.2]]);
<semigroup congruence over <fp semigroup with 3 generators and 9 relations> wi\
th 1 generating pairs>
gap> ImagesElm(C, S.1);
[ s1, s2, s1*s2, s2^2, s1*s2^2 ]
gap> ImagesElm(C, S.3);
[ s3 ]

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/libsemigroups/cong.tst");
