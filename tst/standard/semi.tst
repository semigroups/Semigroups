############################################################################
##
#W  standard/semi.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semi.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

# AsMonoid with 1 arg
gap> S := Semigroup(Transformation([2, 2, 3, 4]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> AsMonoid(S);
<trivial transformation group of degree 0 with 1 generator>
gap> T := AsSemigroup(IsBooleanMatSemigroup, S);
<commutative semigroup of 2x2 boolean matrices with 1 generator>
gap> IsMonoid(T);
false
gap> IsMonoidAsSemigroup(T);
true
gap> AsMonoid(T);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := AsSemigroup(IsFpSemigroup, S);
<fp semigroup on the generators [ s1 ]>
gap> AsMonoid(S);
<fp group on the generators [  ]>
gap> S := AsSemigroup(IsMaxPlusMatrixSemigroup, S);
<commutative semigroup of 2x2 max-plus matrices with 1 generator>
gap> AsMonoid(S);
<trivial group of 1x1 max-plus matrices with 1 generator>
gap> S := AsSemigroup(IsMinPlusMatrixSemigroup, S);
<trivial group of 1x1 min-plus matrices with 1 generator>
gap> S := Semigroup(Transformation([2,2,3,4]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> S := AsSemigroup(IsMinPlusMatrixSemigroup, S);
<commutative semigroup of 2x2 min-plus matrices with 1 generator>
gap> AsMonoid(S);
<trivial group of 1x1 min-plus matrices with 1 generator>
gap> S := Semigroup(Transformation([2,2,3,4]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> S := AsSemigroup(IsProjectiveMaxPlusMatrixSemigroup, S);
<commutative semigroup of 2x2 projective max-plus matrices with 1 generator>
gap> AsMonoid(S);
<trivial group of 1x1 projective max-plus matrices with 1 generator>
gap> S := AsSemigroup(IsIntegerMatrixSemigroup, S);
<trivial group of 1x1 integer matrices with 1 generator>
gap> S := Semigroup(Transformation([2,2,3,4]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> S := AsSemigroup(IsIntegerMatrixSemigroup, S);
<commutative semigroup of 2x2 integer matrices with 1 generator>
gap> AsMonoid(S);
<trivial group of 1x1 integer matrices with 1 generator>
gap> S := Semigroup(Transformation([2,2,3,4]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> S := AsSemigroup(IsTropicalMaxPlusMatrixSemigroup, 3, S);
<commutative semigroup of 2x2 tropical max-plus matrices with 1 generator>
gap> AsMonoid(S);
<trivial group of 1x1 tropical max-plus matrices with 1 generator>
gap> S := Semigroup(Transformation([2,2,3,4]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> S := AsSemigroup(IsTropicalMinPlusMatrixSemigroup, 3, S);
<commutative semigroup of 2x2 tropical min-plus matrices with 1 generator>
gap> AsMonoid(S);
<trivial group of 1x1 tropical min-plus matrices with 1 generator>
gap> S := Semigroup(Transformation([2, 2, 3, 4]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> S := AsSemigroup(IsNTPMatrixSemigroup, 3, 3, S);
<commutative semigroup of 2x2 ntp matrices with 1 generator>
gap> AsMonoid(S);
<trivial group of 1x1 ntp matrices with 1 generator>
gap> S := Semigroup(Transformation([2, 2, 3, 4]));
<commutative transformation semigroup of degree 2 with 1 generator>
gap> S := AsSemigroup(IsPBRSemigroup, S);
<commutative pbr semigroup of degree 2 with 1 generator>
gap> AsMonoid(S);
<trivial pbr group of degree 1 with 1 generator>

# AsMonoid returns fail
gap> S := InverseSemigroup(
> [Bipartition([[1, -4], [2, -1], [3, -2], [4], [-3]]),
>  Bipartition([[1, -3], [2], [3], [4, -1], [-2], [-4]])]);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsTransformationSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsPartialPermSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsBooleanMatSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsFpSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsMaxPlusMatrixSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsMinPlusMatrixSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsTropicalMaxPlusMatrixSemigroup, 3, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsTropicalMinPlusMatrixSemigroup, 3, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsProjectiveMaxPlusMatrixSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsIntegerMatrixSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsNTPMatrixSemigroup, 3, 3, S);;
gap> AsMonoid(S);
fail
gap> S := AsSemigroup(IsPBRSemigroup, S);;
gap> AsMonoid(S);
fail
gap> S := TrivialSemigroup(IsReesMatrixSemigroup); 
<Rees matrix semigroup 1x1 over Group(())>
gap> AsMonoid(S);
fail
gap> S := ReesZeroMatrixSemigroup(Group(()), [[()]]);;
gap> AsMonoid(S);
fail
gap> S := GraphInverseSemigroup(Digraph([[2],[]]));
<finite graph inverse semigroup with 2 vertices, 1 edge>
gap> AsMonoid(S);
fail

#E#
gap> STOP_TEST("Semigroups package: standard/semi.tst");
