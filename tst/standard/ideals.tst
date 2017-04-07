#############################################################################
##
#W  standard/ideals.tst
#Y  Copyright (C) 2016                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/ideals.tst");
gap> LoadPackage("semigroups", false);;

# The tests in this file do not attempt to test every line in ideals.gi
# since that file needs to be completely rewritten.

#
gap> SEMIGROUPS.StartTest();

# Test SupersemigroupOfIdeal
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, 
>                        Matrix(IsBooleanMat, 
>                               [[1, 1, 1], [1, 0, 1], [1, 1, 1]]));
<semigroup ideal of 3x3 boolean matrices with 1 generator>
gap> J := MinimalIdeal(I);
<simple semigroup ideal of 3x3 boolean matrices with 1 generator>
gap> SupersemigroupOfIdeal(I) = S;
true
gap> SupersemigroupOfIdeal(J) = S;
true
gap> Parent(J) = I;
true

# Test PrintString
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, 
>                        Matrix(IsBooleanMat, 
>                               [[1, 1, 1], [1, 0, 1], [1, 1, 1]]));
<semigroup ideal of 3x3 boolean matrices with 1 generator>
gap> PrintString(I);
"\>\>SemigroupIdeal(\< \>Monoid( \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[0, 1\
, 0]\<, \<\>\>[1, 0, 0]\<, \<\>\>[0, 0, 1]\<\<]\<)\<\>\>\>Matrix(\<\>IsBoolean\
Mat\<, \>[\>\>[0, 1, 0]\<, \<\>\>[0, 0, 1]\<, \<\>\>[1, 0, 0]\<\<]\<)\<\<, \>\
\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\<, \<\>\>[0, 1, 0]\<, \<\>\>[1\
, 0, 1]\<\<]\<)\<\<, \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0]\<, \<\>\
\>[0, 1, 0]\<, \<\>\>[0, 0, 0]\<\<]\<)\<\<\<\> )\<,\< \>\>\>Matrix(\<\>IsBoole\
anMat\<, \>[\>\>[1, 1, 1]\<, \<\>\>[1, 0, 1]\<, \<\>\>[1, 1, 1]\<\<]\<\< )\<"

# Test ViewString
gap> S := RegularBooleanMatMonoid(1);;
gap> I := MinimalIdeal(S);
<group of 1x1 boolean matrices>
gap> IsTrivial(I);
true
gap> I;
<trivial group of 1x1 boolean matrices>
gap> S := SymmetricInverseMonoid(3);;
gap> MinimalIdeal(S);
<partial perm group of rank 3>
gap> S := RegularBooleanMatMonoid(3);;
gap> x := Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [1, 1, 1]]);;
gap> I := SemigroupIdeal(S, x);
<semigroup ideal of 3x3 boolean matrices with 1 generator>
gap> IsZeroSimpleSemigroup(I);
true
gap> I;
<0-simple regular semigroup ideal of 3x3 boolean matrices with 1 generator>
gap> x := Matrix(IsBooleanMat, [[0, 1, 0], [1, 0, 1], [1, 1, 0]]);;
gap> I := SemigroupIdeal(S, x);
<semigroup ideal of 3x3 boolean matrices with 1 generator>
gap> IsRegularSemigroup(I);
false
gap> I;
<non-regular semigroup ideal of 3x3 boolean matrices with 1 generator>
gap> S := SymmetricInverseMonoid(3);;
gap> x := PartialPerm([1, 2]);;
gap> I := SemigroupIdeal(S, x);
<inverse partial perm semigroup ideal of rank 3 with 1 generator>
gap> I := SemigroupIdeal(S, S);
<inverse partial perm semigroup ideal of rank 3 with 5 generators>
gap> Size(I);
34
gap> I;
<inverse partial perm semigroup ideal of size 34, rank 3 with 5 generators>
gap> IsMonoid(I);
false

# Test \. (for accessing generators)
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, S.1, S.2);;
gap> I.1 = S.1;
true
gap> I.2 = S.2;
true
gap> I.3;
Error, Semigroups: \.: usage,
the second argument <n> should be a positive integer
not greater than the number of generators of the semigroup <S> in
the first argument,

# Test \= for semigroup ideals
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, S.1, S.2);;
gap> J := SemigroupIdeal(S, S.1, S.2, S.3);;
gap> I = J;
true
gap> J = I;
true
gap> I := SemigroupIdeal(FullBooleanMatMonoid(3), S.1, S.2);;
gap> I = J;
false

# Test \= for semigroup and semigroup ideal
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, S.1, S.2);;
gap> S = I;
true
gap> I = S;
true
gap> I := SemigroupIdeal(FullBooleanMatMonoid(3), S.1, S.2);;
gap> I = S;
false
gap> S = I;
false
gap> I := SemigroupIdeal(FullBooleanMatMonoid(3), S.1, S.2);;
gap> GeneratorsOfSemigroup(I);;
gap> I = S;
false
gap> S = I;
false
gap> S = MinimalIdeal(S);
false

# Test SemigroupIdeal (the function)
gap> SemigroupIdeal("a");
Error, Semigroups: SemigroupIdeal: usage,
the first argument must be a semigroup,
gap> S := RegularBooleanMatMonoid(1);;
gap> SemigroupIdeal(S);
Error, Semigroups: SemigroupIdeal: usage,
there must be a second argument, which specifies
the generators of the ideal,
gap> S := Semigroup([[Z(2)]]);
<commutative semigroup with 1 generator>
gap> SemigroupIdeal(S, S.1);
<commutative semigroup ideal with 1 generator>
gap> S := RegularBooleanMatMonoid(2);;
gap> I := SemigroupIdeal(S, [S.1, S.2]);
<semigroup ideal of 2x2 boolean matrices with 2 generators>
gap> J := SemigroupIdeal(S, I, S.3);
<semigroup ideal of 2x2 boolean matrices with 3 generators>
gap> I := SemigroupIdeal(S, [S.1, S.2], rec());
<semigroup ideal of 2x2 boolean matrices with 2 generators>
gap> I := SemigroupIdeal(S, MaximalDClasses(S)[1]);
<semigroup ideal of 2x2 boolean matrices with 2 generators>
gap> I := SemigroupIdeal(S, []);
Error, Semigroups: SemigroupIdeal: usage,
the second argument must be a combination of generators,
 lists of generators, or semigroups,
gap> SemigroupIdeal();
Error, Semigroups: SemigroupIdeal: usage,
the second argument must be a combination of generators,
lists of generators, or semigroups,
gap> SemigroupIdeal(S, NullDigraph(2));
Error, Semigroups: SemigroupIdeal: usage,
the second argument must be a combination of generators,
lists of generators, or semigroups,

# Test SemigroupIdealByGenerators
gap> S := RegularBooleanMatMonoid(1);;
gap> T := RegularBooleanMatMonoid(2);;
gap> SemigroupIdeal(S, T.1);
Error, Semigroups: SemigroupIdealByGenerators: usage,
the second argument <gens> do not all belong to the semigroup,

# Test SemigroupIdealByGeneratorsNC
gap> S := FullTransformationMonoid(3);;
gap> I := SemigroupIdeal(S, S.1, rec(regular := true));
<regular transformation semigroup ideal of degree 3 with 1 generator>
gap> S := GLM(3, 2);;
gap> I := SemigroupIdeal(S, S.3);
<regular semigroup ideal of 3x3 matrices over GF(2) with 1 generator>
gap> IsMatrixOverFiniteFieldSemigroup(I);
true
gap> S := PartitionMonoid(3);;
gap> I := SemigroupIdeal(S, S.3);
<regular bipartition *-semigroup ideal of degree 3 with 1 generator>
gap> HasIsStarSemigroup(I) and IsStarSemigroup(I);
true
gap> S := RegularBooleanMatMonoid(3);;
gap> IsRegularSemigroup(S);
false
gap> I := SemigroupIdeal(S, S.1);
<semigroup ideal of 3x3 boolean matrices with 1 generator>
gap> S := Semigroup(FullTransformationMonoid(3));;
gap> I := SemigroupIdeal(S, S.1);;

# Test MinimalIdealGeneratingSet
gap> S := FullTransformationMonoid(3);;
gap> I := SemigroupIdeal(S, S);
<regular transformation semigroup ideal of degree 3 with 4 generators>
gap> MinimalIdealGeneratingSet(I);
[ IdentityTransformation ]
gap> I := SemigroupIdeal(S, S.1);
<regular transformation semigroup ideal of degree 3 with 1 generator>
gap> MinimalIdealGeneratingSet(I);
[ Transformation( [ 2, 3, 1 ] ) ]
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, 
>  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 0], [1, 0, 1]]),
>  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 0], [0, 0, 1]]),
>  Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 1], [1, 0, 1]]),
>  Matrix(IsBooleanMat, [[1, 1, 0], [1, 0, 0], [1, 0, 1]]),
>  Matrix(IsBooleanMat, [[0, 0, 1], [0, 0, 1], [0, 1, 0]]),
>  Matrix(IsBooleanMat, [[1, 0, 0], [0, 0, 0], [0, 0, 1]]),
>  Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 1], [1, 1, 1]]),
>  Matrix(IsBooleanMat, [[1, 1, 0], [0, 0, 1], [1, 1, 1]]),
>  Matrix(IsBooleanMat, [[1, 0, 1], [0, 0, 0], [0, 1, 0]]),
>  Matrix(IsBooleanMat, [[0, 1, 1], [0, 1, 1], [1, 0, 1]]));
<semigroup ideal of 3x3 boolean matrices with 10 generators>
gap> MinimalIdealGeneratingSet(I);
[ Matrix(IsBooleanMat, [[0, 1, 0], [1, 0, 1], [1, 1, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 1, 0], [1, 0, 1]]) ]

# Test InversesOfSemigroupElementNC
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, 
>  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 0], [1, 0, 1]]),
>  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 0], [0, 0, 1]]),
>  Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 1], [1, 0, 1]]),
>  Matrix(IsBooleanMat, [[1, 1, 0], [1, 0, 0], [1, 0, 1]]),
>  Matrix(IsBooleanMat, [[0, 0, 1], [0, 0, 1], [0, 1, 0]]),
>  Matrix(IsBooleanMat, [[1, 0, 0], [0, 0, 0], [0, 0, 1]]),
>  Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 1], [1, 1, 1]]),
>  Matrix(IsBooleanMat, [[1, 1, 0], [0, 0, 1], [1, 1, 1]]),
>  Matrix(IsBooleanMat, [[1, 0, 1], [0, 0, 0], [0, 1, 0]]),
>  Matrix(IsBooleanMat, [[0, 1, 1], [0, 1, 1], [1, 0, 1]]));;
gap> x := Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 0], [1, 0, 1]]);;
gap> InversesOfSemigroupElement(I, x);
[ Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 0], [1, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 0], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [0, 1, 0], [1, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 0], [1, 0, 1]]) ]

# Test IsCommutativeSemigroup
gap> x := Transformation([13, 4, 1, 2, 14, 14, 7, 12, 4, 9, 2, 14, 5, 14, 13,
> 18, 15, 8, 18, 9]);;
gap> y := Transformation([13, 15, 7, 18, 4, 2, 8, 12, 10, 7, 8, 11, 12, 12, 17,
> 6, 13, 9, 16, 13]);;
gap> T := DirectProduct(Semigroup(x), Semigroup(y));
<commutative transformation semigroup of size 45, degree 40 with 13 
 generators>
gap> z := Transformation([14, 2, 14, 4, 14, 14, 7, 14, 2, 4, 4, 14, 14, 14, 14,
> 14, 14, 14, 14, 4, 32, 31, 28, 28, 31, 32, 32, 31, 31, 28, 32, 28, 31, 31, 28,
> 28, 32, 32, 31, 32]);;
gap> I := SemigroupIdeal(T, z);;
gap> IsCommutativeSemigroup(I);
true
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, 
> [Matrix(IsBooleanMat, [[0, 1, 0], [1, 0, 1], [1, 1, 0]]), 
>  Matrix(IsBooleanMat, [[1, 0, 0], [1, 1, 0], [1, 0, 1]])]);;
gap> IsCommutativeSemigroup(I);
false
gap> T := Semigroup(T);
<transformation semigroup of degree 40 with 13 generators>
gap> I := SemigroupIdeal(T, z);;
gap> IsCommutativeSemigroup(I);
true
gap> T := Semigroup(T);
<transformation semigroup of degree 40 with 13 generators>
gap> I := SemigroupIdeal(T, z);;
gap> IsCommutativeSemigroup(T);
true
gap> IsCommutativeSemigroup(I);
true

#Test IsTrivial
gap> S := Semigroup(Matrix(IsBooleanMat, [[1, 1], [1, 1]]));
<commutative semigroup of 2x2 boolean matrices with 1 generator>
gap> I := SemigroupIdeal(S, S.1);
<commutative semigroup ideal of 2x2 boolean matrices with 1 generator>
gap> IsTrivial(S);
true
gap> IsTrivial(I);
true

# IsFactorisableInverseMonoid
gap> S := UniformBlockBijectionMonoid(4);
<inverse block bijection monoid of degree 4 with 3 generators>
gap> I := SemigroupIdeal(S, S.1);
<inverse bipartition semigroup ideal of degree 4 with 1 generator>
gap> IsFactorisableInverseMonoid(I);
true
gap> I := MinimalIdeal(I);
<bipartition group of degree 4>
gap> IsFactorisableInverseMonoid(I);
false

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/ideals.tst");
