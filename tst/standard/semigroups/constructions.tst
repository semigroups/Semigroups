#############################################################################
##
#W  standard/semigroups/constructions.tst
#Y  Copyright (C) 2015                                   Wilfred A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
gap> START_TEST("Semigroups package: standard/semigroups/constructions.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# constructions: ZeroSemigroup: errors
gap> S := ZeroSemigroup(0);
Error, Semigroups: ZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 0);
Error, Semigroups: ZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := ZeroSemigroup(0, 1);
Error, Semigroups: ZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := ZeroSemigroup(0, 0);
Error, Semigroups: ZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := ZeroSemigroup(IsPermGroup, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `ZeroSemigroupCons' on 2 arguments
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 2, true);
Error, Semigroups: ZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,

#T# constructions: ZeroSemigroup: known properties and attributes, n = 1
gap> S := ZeroSemigroup(1);;
gap> HasSize(S);
true
gap> Size(S);
1
gap> HasIsZeroSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> HasMultiplicativeZero(S);
true
gap> MultiplicativeZero(S);
IdentityTransformation
gap> HasAsList(S);
true
gap> AsList(S);
[ IdentityTransformation ]
gap> IsGroup(S);
true
gap> S := Semigroup(S);;
gap> HasSize(S);
false
gap> Size(S);
1
gap> HasIsZeroSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> HasMultiplicativeZero(S);
false
gap> MultiplicativeZero(S);
IdentityTransformation
gap> HasAsList(S);
false
gap> AsList(S);
[ IdentityTransformation ]
gap> IsGroup(S);
true

#T# constructions: ZeroSemigroup: known properties and attributes, n = 2
gap> S := ZeroSemigroup(2);;
gap> HasIsMonogenicSemigroup(S);
true
gap> IsMonogenicSemigroup(S);
true
gap> S := Semigroup(S);;
gap> HasIsMonogenicSemigroup(S);
true
gap> IsMonogenicSemigroup(S);
true

#T# constructions: ZeroSemigroup: known properties and attributes, n = 5
gap> S := ZeroSemigroup(5);;
gap> HasSize(S);
true
gap> Size(S);
5
gap> HasIsZeroSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> HasMultiplicativeZero(S);
true
gap> MultiplicativeZero(S);
Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )
gap> HasAsList(S);
true
gap> AsList(S);
[ Transformation( [ 1, 3, 1, 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 5, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 7, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 1, 9, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ) ]
gap> HasIsGroupAsSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> HasIsRegularSemigroup(S);
true
gap> IsRegularSemigroup(S);
false
gap> HasIsMonogenicSemigroup(S);
true
gap> IsMonogenicSemigroup(S);
false
gap> S := Semigroup(S);;
gap> HasSize(S);
false
gap> Size(S);
5
gap> HasIsZeroSemigroup(S);
false
gap> IsZeroSemigroup(S);
true
gap> HasMultiplicativeZero(S);
true
gap> MultiplicativeZero(S);
Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ] )
gap> HasAsList(S);
false
gap> AsList(S);
[ Transformation( [ 1, 3, 1, 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 5, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 7, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 1, 9, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1 ] ) ]
gap> HasIsGroupAsSemigroup(S);
false
gap> IsGroupAsSemigroup(S);
false
gap> HasIsRegularSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> HasIsMonogenicSemigroup(S);
false
gap> IsMonogenicSemigroup(S);
false

#T# constructions: ZeroSemigroup: default
gap> S := ZeroSemigroup(1);
<trivial transformation group of degree 0 with 0 generators>
gap> S := ZeroSemigroup(2);
<commutative non-regular transformation semigroup of size 2, degree 3 with 1 
 generator>
gap> S := ZeroSemigroup(3);
<non-regular transformation semigroup of size 3, degree 5 with 2 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: transformation semigroup
gap> S := ZeroSemigroup(IsTransformationSemigroup, 1);
<trivial transformation group of degree 0 with 0 generators>
gap> S := ZeroSemigroup(IsTransformationSemigroup, 5);
<non-regular transformation semigroup of size 5, degree 9 with 4 generators>
gap> S := ZeroSemigroup(IsTransformationSemigroup, 10);
<non-regular transformation semigroup of size 10, degree 19 with 9 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: partial perm semigroup
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 1);    
<trivial partial perm group of rank 0 with 1 generator>
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 5);
<non-regular partial perm semigroup of size 5, rank 4 with 4 generators>
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 10);
<non-regular partial perm semigroup of size 10, rank 9 with 9 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: bipartition semigroup
gap> S := ZeroSemigroup(IsBipartitionSemigroup, 1);
<trivial bipartition monoid of degree 1 with 0 generators>
gap> S := ZeroSemigroup(IsBipartitionSemigroup, 5);
<non-regular bipartition semigroup of size 5, degree 8 with 4 generators>
gap> S := ZeroSemigroup(IsBipartitionSemigroup, 10);
<non-regular bipartition semigroup of size 10, degree 18 with 9 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: block bijection semigroup
gap> S := ZeroSemigroup(IsBlockBijectionSemigroup, 1); 
<trivial bipartition monoid of degree 1 with 0 generators>
gap> S := ZeroSemigroup(IsBlockBijectionSemigroup, 5);
<non-regular bipartition semigroup of size 5, degree 8 with 4 generators>
gap> S := ZeroSemigroup(IsBlockBijectionSemigroup, 10);
<non-regular bipartition semigroup of size 10, degree 18 with 9 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: PBR semigroup
gap> S := ZeroSemigroup(IsPBRSemigroup, 1);
<trivial pbr group of degree 1 with 1 generator>
gap> S := ZeroSemigroup(IsPBRSemigroup, 5);
<non-regular pbr semigroup of size 5, degree 8 with 4 generators>
gap> S := ZeroSemigroup(IsPBRSemigroup, 10);
<non-regular pbr semigroup of size 10, degree 18 with 9 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: Boolean matrix semigroup
gap> S := ZeroSemigroup(IsBooleanMatSemigroup, 1);
<trivial monoid of 1x1 boolean matrices with 0 generators>
gap> S := ZeroSemigroup(IsBooleanMatSemigroup, 5);
<non-regular semigroup of size 5, 6x6 boolean matrices with 4 generators>
gap> S := ZeroSemigroup(IsBooleanMatSemigroup, 10);
<non-regular semigroup of size 10, 11x11 boolean matrices with 9 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: Rees 0-matrix semigroup, error
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 1);
Error, Semigroups: ZeroSemigroupCons: usage:
there is no Rees 0-matrix semigroup of order 1,

#T# constructions: ZeroSemigroup: Rees 0-matrix semigroup, 2
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 2); 
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 5);
<Rees 0-matrix semigroup 4x1 over Group(())>
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 10);
<Rees 0-matrix semigroup 9x1 over Group(())>
gap> IsZeroSemigroup(Semigroup(S));
true

# IsTransformationSemigroup
gap> S := Semigroup(ZeroSemigroup(IsTransformationSemigroup, 1));
<trivial transformation group of degree 0 with 0 generators>
gap> IsZeroSemigroup(S);
true
gap> Size(S);
1
gap> Elements(S);
[ IdentityTransformation ]
gap> S := Semigroup(ZeroSemigroup(IsTransformationSemigroup, 2));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> IsZeroSemigroup(S);
true
gap> Size(S);
2
gap> Elements(S);
[ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 3, 1 ] ) ]
gap> S := Semigroup(ZeroSemigroup(IsTransformationSemigroup, 10));
<transformation semigroup of degree 19 with 9 generators>
gap> IsZeroSemigroup(S);
true
gap> Size(S);
10
gap> Elements(S);
[ Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1, 19, 1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1, 17, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1, 15, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 13, 1, 1, 1, 1, 1, 1,
      1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 11, 1, 1, 1, 1, 1,
      1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 1, 1, 1, 1, 9, 1, 1, 1, 1, 1,
     1, 1, 1, 1, 1, 1 ] ), Transformation( [ 1, 1, 1, 1, 1, 7, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1 ] ), 
  Transformation( [ 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1 ] ), Transformation( [ 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1, 1, 1 ] ) ]

# IsReesZeroMatrixSemigroup
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 1);
Error, Semigroups: ZeroSemigroup: usage:
there is no Rees 0-matrix semigroup of order 1,
gap> S := ZeroSemigroupCons(IsReesZeroMatrixSemigroup, 1);
Error, Semigroups: ZeroSemigroupCons: usage:
there is no Rees 0-matrix semigroup of order 1,
gap> S := Semigroup(ZeroSemigroupCons(IsReesZeroMatrixSemigroup, 2));;
gap> IsReesZeroMatrixSemigroup(S);
true
gap> s;
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> IsZeroSemigroup(S);
true
gap> Size(S);
2
gap> S := Semigroup(ZeroSemigroupCons(IsReesZeroMatrixSemigroup, 20));;
gap> IsReesZeroMatrixSemigroup(S);
true
gap> s;
<Rees 0-matrix semigroup 19x1 over Group(())>
gap> IsZeroSemigroup(S);
true
gap> Size(S);
20

# IsBipartitionSemigroup and IsBlockBijectionSemigroup
gap> S := ZeroSemigroup(IsBipartitionSemigroup, 1);
<trivial bipartition monoid of degree 1 with 0 generators>
gap> S := ZeroSemigroup(IsBlockBijectionSemigroup, 1);
<trivial bipartition monoid of degree 1 with 0 generators>
gap> last = last2;
true
gap> S := Semigroup(ZeroSemigroupCons(IsBipartitionSemigroup, 2));
<commutative bipartition semigroup of degree 2 with 1 generator>
gap> IsBlockBijectionSemigroup(S);
false
gap> IsZeroSemigroup(S);
true
gap> Size(S);
2
gap> S := Semigroup(ZeroSemigroupCons(IsBlockBijectionSemigroup, 2));
<commutative bipartition semigroup of degree 3 with 1 generator>
gap> IsBlockBijectionSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> Size(S);
2
gap> S := Semigroup(ZeroSemigroupCons(IsBipartitionSemigroup, 20));
<bipartition semigroup of degree 38 with 19 generators>
gap> IsBlockBijectionSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> Size(S);
20
gap> S := Semigroup(ZeroSemigroupCons(IsBlockBijectionSemigroup, 20));
<bipartition semigroup of degree 38 with 19 generators>
gap> IsBlockBijectionSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> Size(S);
20

# Zero semigroup of order 1
gap> S := ZeroSemigroup(1);
<trivial partial perm group of rank 0 with 0 generators>
gap> GeneratorsOfSemigroup(S);
[ <empty partial perm> ]
gap> HasAsList(S);
true
gap> AsList(S) = GeneratorsOfSemigroup(S);
true
gap> HasMultiplicativeZero(S);
true
gap> MultiplicativeZero(S);
<empty partial perm>
gap> HasIsMonogenicSemigroup(S);
true
gap> IsMonogenicSemigroup(S);
true
gap> HasIsRegularSemigroup(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsGroup(S);
true
gap> HasIsGroupAsSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false

# Zero semigroup of order 2
gap> S := ZeroSemigroup(2);
<commutative non-regular partial perm semigroup of size 2, rank 1 with 1 
 generator>
gap> HasIsZeroSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> HasIsLeftZeroSemigroup(S);
false
gap> HasIsRightZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> HasAsList(S);
true
gap> AsList(S);
[ [1,2], <empty partial perm> ]
gap> Elements(S);
[ <empty partial perm>, [1,2] ]
gap> HasMultiplicativeZero(S);
true
gap> MultiplicativeZero(S);
<empty partial perm>
gap> HasIsMonogenicSemigroup(S);
true
gap> IsMonogenicSemigroup(S);
true
gap> HasIsRegularSemigroup(S);
true
gap> IsRegularSemigroup(S);
false
gap> HasIsGroupAsSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false

# Not created by ZeroSemigroup
gap> S := Semigroup(S);;
gap> HasIsZeroSemigroup(S);
false
gap> IsZeroSemigroup(S);
true
gap> HasIsLeftZeroSemigroup(S);
false
gap> HasIsRightZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> HasAsList(S);
false
gap> AsList(S);
[ [1,2], <empty partial perm> ]
gap> Elements(S);
[ <empty partial perm>, [1,2] ]
gap> S := Semigroup(S);;
gap> HasMultiplicativeZero(S);
false
gap> MultiplicativeZero(S);
<empty partial perm>
gap> HasIsMonogenicSemigroup(S);
true
gap> IsMonogenicSemigroup(S);
true
gap> HasIsRegularSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> HasIsGroupAsSemigroup(S);
false
gap> IsGroupAsSemigroup(S);
false

# Zero semigroup of order 50
gap> S := ZeroSemigroup(50);
<non-regular partial perm semigroup of size 50, rank 49 with 49 generators>
gap> HasIsZeroSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> HasIsLeftZeroSemigroup(S);
false
gap> HasIsRightZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> HasAsList(S);
true
gap> IsSubset(AsList(S), Elements(S)) and IsSubset(Elements(S), AsList(S));
true
gap> HasMultiplicativeZero(S);
true
gap> MultiplicativeZero(S);
<empty partial perm>
gap> HasIsMonogenicSemigroup(S);
true
gap> IsMonogenicSemigroup(S);
false
gap> HasIsRegularSemigroup(S);
true
gap> IsRegularSemigroup(S);
false
gap> HasIsGroupAsSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false

# Not created by ZeroSemigroup
gap> S := Semigroup(S);;
gap> HasIsZeroSemigroup(S);
false
gap> IsZeroSemigroup(S);
true
gap> HasIsLeftZeroSemigroup(S);
false
gap> HasIsRightZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> HasAsList(S);
false
gap> IsSubset(AsList(S), Elements(S)) and IsSubset(Elements(S), AsList(S));
true
gap> S := Semigroup(S);;
gap> HasMultiplicativeZero(S);
false
gap> MultiplicativeZero(S);
<empty partial perm>
gap> HasIsMonogenicSemigroup(S);
false
gap> IsMonogenicSemigroup(S);
false
gap> HasIsRegularSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> HasIsGroupAsSemigroup(S);
false
gap> IsGroupAsSemigroup(S);
false

#T# ConstructionsTest33: MonogenicSemigroup
gap> S := MonogenicSemigroup(0, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `MonogenicSemigroup' on 2 arguments
gap> S := MonogenicSemigroup(1, 0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `MonogenicSemigroup' on 2 arguments

# Trivial monogenic semigroup
gap> S := MonogenicSemigroup(1, 1);
<trivial transformation group of degree 0 with 0 generators>
gap> HasSize(S);
true
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
true
gap> HasIsRegularSemigroup(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> Size(S);
1
gap> IsMonogenicSemigroup(S);
true
gap> IsGroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> GeneratorsOfSemigroup(S);
[ IdentityTransformation ]

# Not created by MonogenicSemigroup
gap> S := Semigroup(S);;
gap> HasSize(S);
false
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
true
gap> HasIsRegularSemigroup(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> Size(S);
1
gap> IsMonogenicSemigroup(S);
true
gap> IsGroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsRegularSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> GeneratorsOfSemigroup(S);
[ IdentityTransformation ]
gap> IndexPeriodOfTransformation(last[1]);
[ 1, 1 ]

# MonogenicSemigroup(2, 1)
gap> S := MonogenicSemigroup(2, 1);
<commutative non-regular transformation semigroup of size 2, degree 3 with 1 
 generator>
gap> HasSize(S);
true
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
true
gap> HasIsRegularSemigroup(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> Size(S);
2
gap> IsMonogenicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsZeroSemigroup(S);
true
gap> GeneratorsOfSemigroup(S);
[ Transformation( [ 1, 1, 2 ] ) ]

# Not created by MonogenicSemigroup
gap> S := Semigroup(S);;
gap> HasSize(S);
false
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
false
gap> HasIsRegularSemigroup(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> Size(S);
2
gap> IsMonogenicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsZeroSemigroup(S);
true
gap> GeneratorsOfSemigroup(S);
[ Transformation( [ 1, 1, 2 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 2, 1 ]

# MonogenicSemigroup(3, 1)
gap> S := MonogenicSemigroup(3, 1);
<commutative non-regular transformation semigroup of size 3, degree 4 with 1 
 generator>
gap> HasSize(S);
true
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
true
gap> HasIsRegularSemigroup(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> Size(S);
3
gap> IsMonogenicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsZeroSemigroup(S);
false
gap> GeneratorsOfSemigroup(S);
[ Transformation( [ 1, 1, 2, 3 ] ) ]

# Not created by MonogenicSemigroup
gap> S := Semigroup(S);;
gap> HasSize(S);
false
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
false
gap> HasIsRegularSemigroup(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> Size(S);
3
gap> IsMonogenicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsZeroSemigroup(S);
false
gap> GeneratorsOfSemigroup(S);
[ Transformation( [ 1, 1, 2, 3 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 3, 1 ]

# MonogenicSemigroup(1, 2)
gap> S := MonogenicSemigroup(1, 2);
<transformation group of size 2, degree 2 with 1 generator>
gap> HasSize(S);
true
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
true
gap> HasIsRegularSemigroup(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> Size(S);
2
gap> IsMonogenicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsZeroSemigroup(S);
false
gap> GeneratorsOfSemigroup(S);
[ Transformation( [ 2, 1 ] ) ]

# Not created by MonogenicSemigroup
gap> S := Semigroup(S);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> HasSize(S);
false
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
false
gap> HasIsRegularSemigroup(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> Size(S);
2
gap> IsMonogenicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
true
gap> IsRegularSemigroup(S);
true
gap> IsZeroSemigroup(S);
false
gap> GeneratorsOfSemigroup(S);
[ Transformation( [ 2, 1 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 1, 2 ]

# MonogenicSemigroup(5, 10)
gap> S := MonogenicSemigroup(5, 10);
<commutative non-regular transformation semigroup of size 14, degree 15 with 
 1 generator>
gap> HasSize(S);
true
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
true
gap> HasIsRegularSemigroup(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> Size(S);
14
gap> IsMonogenicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsZeroSemigroup(S);
false
gap> GeneratorsOfSemigroup(S);
[ Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 10, 11, 12, 13, 14 ] ) ]

# Not created by MonogenicSemigroup
gap> S := Semigroup(S);
<commutative transformation semigroup of degree 15 with 1 generator>
gap> HasSize(S);
false
gap> HasIsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
false
gap> HasIsRegularSemigroup(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> Size(S);
14
gap> IsMonogenicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsZeroSemigroup(S);
false
gap> GeneratorsOfSemigroup(S);
[ Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 10, 11, 12, 13, 14 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 5, 10 ]

#T# ConstructionsTest34: RectangularBand
gap> S := RectangularBand(0, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBand' on 2 arguments
gap> S := RectangularBand(1, 0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBand' on 2 arguments

# Trivial rectangular band
gap> S := RectangularBand(1, 1);
<Rees matrix semigroup 1x1 over Group(())>
gap> HasIsRectangularBand(S);
true
gap> HasIsBand(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> HasSize(S);
true
gap> HasIsTrivial(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> HasIsLeftZeroSemigroup(S);
true
gap> HasIsRightZeroSemigroup(S);
true
gap> IsRectangularBand(S);
true
gap> IsBand(S);
true
gap> IsZeroSemigroup(S);
true
gap> Size(S);
1
gap> IsTrivial(S);
true
gap> IsZeroSemigroup(S);
true
gap> IsLeftZeroSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
true

# Not created by RectangularBand
gap> S := AsTransformationSemigroup(S);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> HasIsRectangularBand(S);
false
gap> HasIsBand(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> HasSize(S);
false
gap> HasIsTrivial(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> HasIsLeftZeroSemigroup(S);
false
gap> HasIsRightZeroSemigroup(S);
false
gap> IsRectangularBand(S);
true
gap> IsBand(S);
true
gap> IsZeroSemigroup(S);
true
gap> Size(S);
1
gap> IsTrivial(S);
true
gap> IsZeroSemigroup(S);
true
gap> IsLeftZeroSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
true

# 10 x 1 rectangular band
gap> S := RectangularBand(10, 1);
<Rees matrix semigroup 10x1 over Group(())>
gap> HasIsRectangularBand(S);
true
gap> HasIsBand(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> HasSize(S);
true
gap> HasIsTrivial(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> HasIsLeftZeroSemigroup(S);
true
gap> HasIsRightZeroSemigroup(S);
true
gap> IsRectangularBand(S);
true
gap> IsBand(S);
true
gap> IsZeroSemigroup(S);
false
gap> Size(S);
10
gap> IsTrivial(S);
false
gap> IsZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false

# Not created by RectangularBand
gap> S := AsTransformationSemigroup(S);
<transformation semigroup of degree 11 with 10 generators>
gap> HasIsRectangularBand(S);
false
gap> HasIsBand(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> HasSize(S);
false
gap> HasIsTrivial(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> HasIsLeftZeroSemigroup(S);
false
gap> HasIsRightZeroSemigroup(S);
false
gap> IsRectangularBand(S);
true
gap> IsBand(S);
true
gap> IsZeroSemigroup(S);
false
gap> Size(S);
10
gap> IsTrivial(S);
false
gap> IsZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false

# 1 x 8 rectangular band
gap> S := RectangularBand(1, 8);
<Rees matrix semigroup 1x8 over Group(())>
gap> HasIsRectangularBand(S);
true
gap> HasIsBand(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> HasSize(S);
true
gap> HasIsTrivial(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> HasIsLeftZeroSemigroup(S);
true
gap> HasIsRightZeroSemigroup(S);
true
gap> IsRectangularBand(S);
true
gap> IsBand(S);
true
gap> IsZeroSemigroup(S);
false
gap> Size(S);
8
gap> IsTrivial(S);
false
gap> IsZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
true

# Not created by RectangularBand
gap> S := AsTransformationSemigroup(S);
<transformation semigroup of degree 9 with 8 generators>
gap> HasIsRectangularBand(S);
false
gap> HasIsBand(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> HasSize(S);
false
gap> HasIsTrivial(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> HasIsLeftZeroSemigroup(S);
false
gap> HasIsRightZeroSemigroup(S);
false
gap> IsRectangularBand(S);
true
gap> IsBand(S);
true
gap> IsZeroSemigroup(S);
false
gap> Size(S);
8
gap> IsTrivial(S);
false
gap> IsZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
true

# 12 x 7 rectangular band
gap> S := RectangularBand(12, 7);
<Rees matrix semigroup 12x7 over Group(())>
gap> HasIsRectangularBand(S);
true
gap> HasIsBand(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> HasSize(S);
true
gap> HasIsTrivial(S);
true
gap> HasIsZeroSemigroup(S);
true
gap> HasIsLeftZeroSemigroup(S);
true
gap> HasIsRightZeroSemigroup(S);
true
gap> IsRectangularBand(S);
true
gap> IsBand(S);
true
gap> IsZeroSemigroup(S);
false
gap> Size(S);
84
gap> IsTrivial(S);
false
gap> IsZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false

# Not created by RectangularBand
gap> S := AsTransformationSemigroup(S);
<transformation semigroup of degree 85 with 84 generators>
gap> HasIsRectangularBand(S);
false
gap> HasIsBand(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> HasSize(S);
false
gap> HasIsTrivial(S);
false
gap> HasIsZeroSemigroup(S);
false
gap> HasIsLeftZeroSemigroup(S);
false
gap> HasIsRightZeroSemigroup(S);
false
gap> IsRectangularBand(S);
true
gap> IsBand(S);
true
gap> IsZeroSemigroup(S);
false
gap> Size(S);
84
gap> IsTrivial(S);
false
gap> IsZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> IsRightZeroSemigroup(S);
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E# 
gap> STOP_TEST("Semigroups package: standard/semigroups/constructions.tst");
