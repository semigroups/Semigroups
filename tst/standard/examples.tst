#############################################################################
##
#W  standard/examples.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/examples.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# Catalan monoid
gap> S := CatalanMonoid(1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := CatalanMonoid(2);
<commutative transformation monoid of degree 2 with 1 generator>
gap> Size(S);
2
gap> S := CatalanMonoid(3);
<transformation monoid of degree 3 with 2 generators>
gap> Size(S);
5
gap> S := CatalanMonoid(4);
<transformation monoid of degree 4 with 3 generators>
gap> Size(S);
14

#T# Example from Semigroupe manual, Section 5.18: Knast's counterexample
gap> S := Semigroup(Transformation([4, 4, 8, 8, 8, 8, 4, 8]), 
>                   Transformation([8, 2, 8, 2, 5, 5, 8, 8]),
>                   Transformation([8, 8, 3, 7, 8, 3, 7, 8]),
>                   Transformation([8, 6, 6, 8, 6, 8, 8, 8]));;
gap> Size(S);
30
gap> NrDClasses(S);
6
gap> NrRClasses(S);
12
gap> NrLClasses(S);
12
gap> NrHClasses(S);
30
gap> NrIdempotents(S);
15
gap> S.1 ^ 2 = MultiplicativeZero(S);
true
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false

#T# Example from Semigroupe manual, Section 5.19
gap> S := Semigroup(Transformation([3, 5, 3, 3, 5]), 
>                   Transformation([6, 2, 4, 2, 2, 6]));;
gap> Size(S);
8
gap> IsomorphismFpSemigroup(S);;
gap> Length(RelationsOfFpSemigroup(Range(last)));
4
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
4
gap> NrDClasses(S);
2
gap> NrRClasses(S);
4
gap> NrLClasses(S);
4
gap> NrHClasses(S);
8
gap> NrIdempotents(S);
7
gap> MultiplicativeZero(S);
fail
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false

# Example from Semigroupe manual, Section 5.20
gap> S := Monoid(Matrix(IsBooleanMat, [[0, 1, 0], 
>                                      [1, 1, 0],
>                                      [0, 1, 0]]), 
>                Matrix(IsBooleanMat, [[1, 0, 0], 
>                                      [1, 0, 1],
>                                      [1, 0, 0]]));
<monoid of 3x3 boolean matrices with 2 generators>
gap> Size(S);
7
gap> Length(RelationsOfFpMonoid(Range(IsomorphismFpMonoid(S))));
8
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
2
gap> NrDClasses(S);
4
gap> NrRClasses(S);
4
gap> NrLClasses(S);
7
gap> NrHClasses(S);
7
gap> NrIdempotents(S);
5
gap> MultiplicativeZero(S);
fail
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsLTrivial(S);
true
gap> IsSemigroupWithCommutingIdempotents(S);
false

# Example from Semigroupe manual, Section 5.21
gap> S := Monoid(Matrix(IsNTPMatrix, [[0, 1, 0], 
>                                     [1, 1, 0],
>                                     [0, 1, 0]], 
>                       1, 2), 
>                Matrix(IsNTPMatrix, [[1, 0, 0], 
>                                     [1, 0, 1],
>                                     [1, 0, 0]],
>                       1, 2));
<monoid of 3x3 ntp matrices with 2 generators>
gap> Size(S);
37
gap> Length(RelationsOfFpMonoid(Range(IsomorphismFpMonoid(S))));
12
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
7
gap> NrDClasses(S);
8
gap> NrRClasses(S);
14
gap> NrLClasses(S);
17
gap> NrHClasses(S);
35
gap> NrIdempotents(S);
20
gap> MultiplicativeZero(S);
fail
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false

# Example from Semigroupe manual, Section 5.22
gap> S := Semigroup(Matrix(IsMaxPlusMatrix, [[0, -4],
>                                            [-4, -1]]),
>                   Matrix(IsMaxPlusMatrix, [[0, -3], 
>                                            [-3, -1]]));
<semigroup of 2x2 max-plus matrices with 2 generators>
gap> Size(S);
26
gap> Length(RelationsOfFpSemigroup(Range(IsomorphismFpSemigroup(S))));
9
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
8
gap> NrDClasses(S);
23
gap> NrRClasses(S);
24
gap> NrLClasses(S);
24
gap> NrHClasses(S);
26
gap> NrIdempotents(S);
4
gap> MultiplicativeZero(S);
fail
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
false

# Example from Semigroupe manual, Section 6
gap> S := Monoid(Transformation([2, 3, 4, 4]), 
>                Transformation([4, 1, 2, 4]), rec(generic := true));;
gap> Size(S);
15
gap> Length(RelationsOfFpMonoid(Range(IsomorphismFpMonoid(S))));
9
gap> Length(MinimalFactorization(S, Enumerator(S)[Size(S)]));
4
gap> NrDClasses(S);
4
gap> NrRClasses(S);
7
gap> NrLClasses(S);
7
gap> NrHClasses(S);
15
gap> NrIdempotents(S);
7
gap> MultiplicativeZero(S) = S.2 ^ 3; 
true
gap> IsCommutative(S);
false
gap> IsBand(S);
false
gap> IsNilpotentSemigroup(S);
false
gap> IsAperiodicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsLTrivial(S);
false
gap> IsRTrivial(S);
false
gap> IsSemigroupWithCommutingIdempotents(S);
true

#
gap> STOP_TEST("Semigroups package: standard/examples.tst");
