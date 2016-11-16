#############################################################################
##
#W  standard/fropin.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/fropin.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test idempotents work with AddGenerators
gap> NrIdempotents(FullTransformationMonoid(6));
1057
gap> S := FullTransformationSemigroup(6);
<full transformation monoid of degree 6>
gap> T := Semigroup(S.1, rec(acting := false));
<commutative transformation semigroup of degree 6 with 1 generator>
gap> EN_SEMI_IDEMPOTENTS(T);
[ 6 ]
gap> T := SEMIGROUPS.AddGenerators(T, [S.2], T!.opts);
<transformation semigroup of degree 6 with 2 generators>
gap> EN_SEMI_IDEMPOTENTS(T);
[ 6 ]
gap> T := SEMIGROUPS.AddGenerators(T, [S.3], T!.opts);
<transformation semigroup of degree 6 with 3 generators>
gap> EN_SEMI_IDEMPOTENTS(T);;
gap> Length(last);
1057
gap> D := DClass(T, S.3);;
gap> ID := Idempotents(D);
[ Transformation( [ 1, 2, 3, 4, 5, 2 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 4 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 5 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 5, 3 ] ), Transformation( [ 3, 2, 3 ] ), 
  Transformation( [ 2, 2 ] ), Transformation( [ 4, 2, 3, 4 ] ), 
  Transformation( [ 6, 2, 3, 4, 5, 6 ] ), Transformation( [ 5, 2, 3, 4, 5 ] ),
  Transformation( [ 1, 4, 3, 4 ] ), Transformation( [ 1, 3, 3 ] ), 
  Transformation( [ 1, 5, 3, 4, 5 ] ), Transformation( [ 1, 1 ] ), 
  Transformation( [ 1, 6, 3, 4, 5, 6 ] ), Transformation( [ 1, 2, 5, 4, 5 ] ),
  Transformation( [ 1, 2, 4, 4 ] ), Transformation( [ 1, 2, 6, 4, 5, 6 ] ), 
  Transformation( [ 1, 2, 2 ] ), Transformation( [ 1, 2, 1 ] ), 
  Transformation( [ 1, 2, 3, 6, 5, 6 ] ), Transformation( [ 1, 2, 3, 5, 5 ] ),
  Transformation( [ 1, 2, 3, 1 ] ), Transformation( [ 1, 2, 3, 3 ] ), 
  Transformation( [ 1, 2, 3, 2 ] ), Transformation( [ 1, 2, 3, 4, 1 ] ), 
  Transformation( [ 1, 2, 3, 4, 6, 6 ] ), Transformation( [ 1, 2, 3, 4, 2 ] ),
  Transformation( [ 1, 2, 3, 4, 4 ] ), Transformation( [ 1, 2, 3, 4, 3 ] ) ]
gap> ForAll(ID, x -> IsIdempotent(x) and x in D);
true

#E#
gap> STOP_TEST("Semigroups package: standard/fropin.tst");
