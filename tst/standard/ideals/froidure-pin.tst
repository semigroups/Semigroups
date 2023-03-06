#############################################################################
##
#W  standard/ideals/froidure-pin.tst
#Y  Copyright (C) 2016-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local I, S, acting, en
gap> START_TEST("Semigroups package: standard/ideals/froidure-pin.tst");
gap> LoadPackage("semigroups", false);;

# The tests in this file do not attempt to test every line in idealenum.gi
# since that file needs to be completely rewritten.

#
gap> SEMIGROUPS.StartTest();

# Test Idempotents
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, 
>                        Matrix(IsBooleanMat, 
>                               [[1, 1, 1], [1, 0, 1], [1, 1, 1]]));;
gap> Idempotents(I);
[ Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [0, 0, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 0], [0, 1, 0]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [0, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [0, 0, 0], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 0, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [0, 0, 0], [1, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 1], [0, 0, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 1, 1], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [0, 0, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 0, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 0], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 1], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 0, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [0, 0, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 1, 0], [0, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 1, 0], [1, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 1, 1], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 0, 0], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [0, 0, 0], [1, 1, 0]]), 
  Matrix(IsBooleanMat, [[0, 1, 0], [0, 1, 0], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 1, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [0, 1, 0], [1, 1, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 1, 0], [1, 1, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [0, 0, 0], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [0, 0, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 0], [1, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 1, 1], [0, 1, 0], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 1, 1], [1, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 1], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [0, 0, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 1, 1], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 1, 0], [1, 1, 0]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [0, 1, 0], [0, 1, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 0, 1], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 1, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [1, 1, 1], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 0], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [0, 0, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 0, 0], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 1, 1], [0, 0, 0], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [1, 1, 1], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 1, 0], [0, 1, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 1, 0], [0, 1, 0], [0, 1, 0]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 0, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [1, 0, 1], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 1, 1], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 0, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 0, 0], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 1], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 1, 1], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [0, 0, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 1, 0], [0, 1, 0]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [1, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 1, 1], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 0, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 0, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [1, 1, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [1, 0, 1], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 1], [0, 0, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [1, 1, 0], [1, 1, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 1], [1, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 1, 1], [0, 1, 1], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 1, 1], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 0, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 1, 1], [0, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [1, 0, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 1, 1], [0, 1, 1], [0, 1, 1]]) ]
gap> ForAll(Idempotents(I), x -> x ^ 2 = x and x in I);
true
gap> GeneratorsOfSemigroup(I);
[ Matrix(IsBooleanMat, [[1, 1, 1], [1, 0, 1], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 1, 1], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [0, 0, 1], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [0, 1, 0], [0, 1, 0]]), 
  Matrix(IsBooleanMat, [[1, 0, 1], [0, 0, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0, 0], [1, 1, 0], [1, 1, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 0, 0], [1, 1, 0]]), 
  Matrix(IsBooleanMat, [[1, 1, 0], [1, 1, 0], [1, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 1], [0, 0, 1], [0, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 0], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[0, 1, 0], [1, 1, 1], [1, 1, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [0, 0, 0], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [1, 0, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 1, 0], [1, 1, 0], [0, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 1], [0, 0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [1, 1, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 0], [1, 0, 1], [1, 0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0, 1], [0, 0, 1], [1, 0, 1]]), 
  Matrix(IsBooleanMat, [[0, 1, 1], [0, 1, 0], [0, 0, 0]]) ]
gap> S.1 in I;
false
gap> ForAll(GeneratorsOfSemigroup(I), x -> x in I);
true
gap> Size(I);
194
gap> en := Enumerator(I);;
gap> ForAll(en, x -> x in I);
true
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], x -> Position(en, en[x]) = x);
true
gap> IsBound(en[1000]);
false
gap> Position(en, S.1);
fail
gap> Position(en, Matrix(IsBooleanMat, [[1, 1, 0], [1, 0, 1], [0, 1, 1]]));
fail
gap> S := RegularBooleanMatMonoid(3);;
gap> I := SemigroupIdeal(S, 
>                        Matrix(IsBooleanMat, 
>                               [[1, 1, 1], [1, 0, 1], [1, 1, 1]]));;
gap> en := Enumerator(I);;
gap> ForAll(en, x -> x in I);
true
gap> ForAll(en, x -> en[Position(en, x)] = x);
true
gap> ForAll([1 .. Length(en)], x -> Position(en, en[x]) = x);
true
gap> IsBound(en[1000]);
false
gap> Position(en, S.1);
fail
gap> Position(en, Matrix(IsBooleanMat, [[1, 1, 0], [1, 0, 1], [0, 1, 1]]));
fail

# Test GeneratorsOfInverseSemigroup for an ideal
gap> S := InverseSemigroup(SymmetricInverseMonoid(4), rec(acting := false));
<inverse partial perm monoid of rank 4 with 3 generators>
gap> I := SemigroupIdeal(S, S.3);;
gap> I = InverseSemigroup(GeneratorsOfInverseSemigroup(I));
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/ideals/froidure-pin.tst");
