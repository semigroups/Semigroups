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

#T# ConstructionsTest32: ZeroSemigroup
gap> s := ZeroSemigroup(0);
Error, Semigroups: ZeroSemigroup: usage:
the argument <n> must be a positive integer,
gap> s := ZeroSemigroup(IsPartialPermSemigroup, 0);
Error, Semigroups: ZeroSemigroup: usage:
the argument <n> must be a positive integer,
gap> s := ZeroSemigroup(0, 1);
Error, Semigroups: ZeroSemigroup: usage:
the optional first argument <filter> must be a filter,
gap> s := ZeroSemigroup(0, 0);
Error, Semigroups: ZeroSemigroup: usage:
the optional first argument <filter> must be a filter,
gap> s := ZeroSemigroup(IsPermGroup, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `ZeroSemigroupCons' on 2 arguments
gap> s := ZeroSemigroup(IsPartialPermSemigroup, 2, true);
Error, Semigroups: ZeroSemigroup: usage:
this function takes at most two arguments,

# IsTransformationSemigroup
gap> s := Semigroup(ZeroSemigroup(IsTransformationSemigroup, 1));
<trivial transformation group of degree 0 with 0 generators>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
1
gap> Elements(s);
[ IdentityTransformation ]
gap> s := Semigroup(ZeroSemigroup(IsTransformationSemigroup, 2));
<commutative transformation semigroup of degree 3 with 1 generator>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
2
gap> Elements(s);
[ Transformation( [ 1, 1, 1 ] ), Transformation( [ 1, 3, 1 ] ) ]
gap> s := Semigroup(ZeroSemigroup(IsTransformationSemigroup, 10));
<transformation semigroup of degree 19 with 9 generators>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
10
gap> Elements(s);
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
gap> s := ZeroSemigroup(IsReesZeroMatrixSemigroup, 1);
Error, Semigroups: ZeroSemigroup: usage:
there is no Rees 0-matrix semigroup of order 1,
gap> s := ZeroSemigroupCons(IsReesZeroMatrixSemigroup, 1);
Error, Semigroups: ZeroSemigroupCons: usage:
there is no Rees 0-matrix semigroup of order 1,
gap> s := Semigroup(ZeroSemigroupCons(IsReesZeroMatrixSemigroup, 2));;
gap> IsReesZeroMatrixSemigroup(s);
true
gap> s;
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
2
gap> s := Semigroup(ZeroSemigroupCons(IsReesZeroMatrixSemigroup, 20));;
gap> IsReesZeroMatrixSemigroup(s);
true
gap> s;
<Rees 0-matrix semigroup 19x1 over Group(())>
gap> IsZeroSemigroup(s);
true
gap> Size(s);
20

# IsBipartitionSemigroup and IsBlockBijectionSemigroup
gap> s := ZeroSemigroup(IsBipartitionSemigroup, 1);
<trivial bipartition monoid of degree 1 with 0 generators>
gap> s := ZeroSemigroup(IsBlockBijectionSemigroup, 1);
<trivial bipartition monoid of degree 1 with 0 generators>
gap> last = last2;
true
gap> s := Semigroup(ZeroSemigroupCons(IsBipartitionSemigroup, 2));
<commutative bipartition semigroup of degree 2 with 1 generator>
gap> IsBlockBijectionSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> Size(s);
2
gap> s := Semigroup(ZeroSemigroupCons(IsBlockBijectionSemigroup, 2));
<commutative bipartition semigroup of degree 3 with 1 generator>
gap> IsBlockBijectionSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
2
gap> s := Semigroup(ZeroSemigroupCons(IsBipartitionSemigroup, 20));
<bipartition semigroup of degree 38 with 19 generators>
gap> IsBlockBijectionSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
20
gap> s := Semigroup(ZeroSemigroupCons(IsBlockBijectionSemigroup, 20));
<bipartition semigroup of degree 38 with 19 generators>
gap> IsBlockBijectionSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
20

# Zero semigroup of order 1
gap> s := ZeroSemigroup(1);
<trivial partial perm group of rank 0 with 0 generators>
gap> GeneratorsOfSemigroup(s);
[ <empty partial perm> ]
gap> HasAsList(s);
true
gap> AsList(s) = GeneratorsOfSemigroup(s);
true
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
true
gap> IsMonogenicSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> IsRegularSemigroup(s);
true
gap> IsGroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false

# Zero semigroup of order 2
gap> s := ZeroSemigroup(2);
<commutative non-regular partial perm semigroup of size 2, rank 1 with 1 
 generator>
gap> HasIsZeroSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> HasAsList(s);
true
gap> AsList(s);
[ [1,2], <empty partial perm> ]
gap> Elements(s);
[ <empty partial perm>, [1,2] ]
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
true
gap> IsMonogenicSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> IsRegularSemigroup(s);
false
gap> HasIsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false

# Not created by ZeroSemigroup
gap> s := Semigroup(s);;
gap> HasIsZeroSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> HasAsList(s);
false
gap> AsList(s);
[ [1,2], <empty partial perm> ]
gap> Elements(s);
[ <empty partial perm>, [1,2] ]
gap> s := Semigroup(s);;
gap> HasMultiplicativeZero(s);
false
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
true
gap> IsMonogenicSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> HasIsGroupAsSemigroup(s);
false
gap> IsGroupAsSemigroup(s);
false

# Zero semigroup of order 50
gap> s := ZeroSemigroup(50);
<non-regular partial perm semigroup of size 50, rank 49 with 49 generators>
gap> HasIsZeroSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> HasAsList(s);
true
gap> IsSubset(AsList(s), Elements(s)) and IsSubset(Elements(s), AsList(s));
true
gap> HasMultiplicativeZero(s);
true
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
true
gap> IsMonogenicSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
true
gap> IsRegularSemigroup(s);
false
gap> HasIsGroupAsSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false

# Not created by ZeroSemigroup
gap> s := Semigroup(s);;
gap> HasIsZeroSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false
gap> HasAsList(s);
false
gap> IsSubset(AsList(s), Elements(s)) and IsSubset(Elements(s), AsList(s));
true
gap> s := Semigroup(s);;
gap> HasMultiplicativeZero(s);
false
gap> MultiplicativeZero(s);
<empty partial perm>
gap> HasIsMonogenicSemigroup(s);
false
gap> IsMonogenicSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> HasIsGroupAsSemigroup(s);
false
gap> IsGroupAsSemigroup(s);
false

#T# ConstructionsTest33: MonogenicSemigroup
gap> s := MonogenicSemigroup(0, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `MonogenicSemigroup' on 2 arguments
gap> s := MonogenicSemigroup(1, 0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `MonogenicSemigroup' on 2 arguments

# Trivial monogenic semigroup
gap> s := MonogenicSemigroup(1, 1);
<trivial transformation group of degree 0 with 0 generators>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
1
gap> IsMonogenicSemigroup(s);
true
gap> IsGroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> GeneratorsOfSemigroup(s);
[ IdentityTransformation ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);;
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
1
gap> IsMonogenicSemigroup(s);
true
gap> IsGroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
true
gap> IsZeroSemigroup(s);
true
gap> GeneratorsOfSemigroup(s);
[ IdentityTransformation ]
gap> IndexPeriodOfTransformation(last[1]);
[ 1, 1 ]

# MonogenicSemigroup(2, 1)
gap> s := MonogenicSemigroup(2, 1);
<commutative non-regular transformation semigroup of size 2, degree 3 with 1 
 generator>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
2
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 1, 1, 2 ] ) ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);;
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> Size(s);
2
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
true
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 1, 1, 2 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 2, 1 ]

# MonogenicSemigroup(3, 1)
gap> s := MonogenicSemigroup(3, 1);
<commutative non-regular transformation semigroup of size 3, degree 4 with 1 
 generator>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
3
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 1, 1, 2, 3 ] ) ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);;
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> Size(s);
3
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 1, 1, 2, 3 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 3, 1 ]

# MonogenicSemigroup(1, 2)
gap> s := MonogenicSemigroup(1, 2);
<transformation group of size 2, degree 2 with 1 generator>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
2
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
true
gap> IsRegularSemigroup(s);
true
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 2, 1 ] ) ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> Size(s);
2
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
true
gap> IsRegularSemigroup(s);
true
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 2, 1 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 1, 2 ]

# MonogenicSemigroup(5, 10)
gap> s := MonogenicSemigroup(5, 10);
<commutative non-regular transformation semigroup of size 14, degree 15 with 
 1 generator>
gap> HasSize(s);
true
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
true
gap> HasIsRegularSemigroup(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> Size(s);
14
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 10, 11, 12, 13, 14 ] ) ]

# Not created by MonogenicSemigroup
gap> s := Semigroup(s);
<commutative transformation semigroup of degree 15 with 1 generator>
gap> HasSize(s);
false
gap> HasIsMonogenicSemigroup(s);
true
gap> HasIsGroupAsSemigroup(s);
false
gap> HasIsRegularSemigroup(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> Size(s);
14
gap> IsMonogenicSemigroup(s);
true
gap> IsGroupAsSemigroup(s);
false
gap> IsRegularSemigroup(s);
false
gap> IsZeroSemigroup(s);
false
gap> GeneratorsOfSemigroup(s);
[ Transformation( [ 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 10, 11, 12, 13, 14 ] ) ]
gap> IndexPeriodOfTransformation(last[1]);
[ 5, 10 ]

#T# ConstructionsTest34: RectangularBand
gap> s := RectangularBand(0, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBand' on 2 arguments
gap> s := RectangularBand(1, 0);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBand' on 2 arguments

# Trivial rectangular band
gap> s := RectangularBand(1, 1);
<Rees matrix semigroup 1x1 over Group(())>
gap> HasIsRectangularBand(s);
true
gap> HasIsBand(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasSize(s);
true
gap> HasIsTrivial(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
true
gap> HasIsRightZeroSemigroup(s);
true
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
1
gap> IsTrivial(s);
true
gap> IsZeroSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
true

# Not created by RectangularBand
gap> s := AsTransformationSemigroup(s);
<commutative transformation semigroup of degree 2 with 1 generator>
gap> HasIsRectangularBand(s);
false
gap> HasIsBand(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasSize(s);
false
gap> HasIsTrivial(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
true
gap> Size(s);
1
gap> IsTrivial(s);
true
gap> IsZeroSemigroup(s);
true
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
true

# 10 x 1 rectangular band
gap> s := RectangularBand(10, 1);
<Rees matrix semigroup 10x1 over Group(())>
gap> HasIsRectangularBand(s);
true
gap> HasIsBand(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasSize(s);
true
gap> HasIsTrivial(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
true
gap> HasIsRightZeroSemigroup(s);
true
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
10
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false

# Not created by RectangularBand
gap> s := AsTransformationSemigroup(s);
<transformation semigroup of degree 11 with 10 generators>
gap> HasIsRectangularBand(s);
false
gap> HasIsBand(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasSize(s);
false
gap> HasIsTrivial(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
10
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
true
gap> IsRightZeroSemigroup(s);
false

# 1 x 8 rectangular band
gap> s := RectangularBand(1, 8);
<Rees matrix semigroup 1x8 over Group(())>
gap> HasIsRectangularBand(s);
true
gap> HasIsBand(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasSize(s);
true
gap> HasIsTrivial(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
true
gap> HasIsRightZeroSemigroup(s);
true
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
8
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
true

# Not created by RectangularBand
gap> s := AsTransformationSemigroup(s);
<transformation semigroup of degree 9 with 8 generators>
gap> HasIsRectangularBand(s);
false
gap> HasIsBand(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasSize(s);
false
gap> HasIsTrivial(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
8
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
true

# 12 x 7 rectangular band
gap> s := RectangularBand(12, 7);
<Rees matrix semigroup 12x7 over Group(())>
gap> HasIsRectangularBand(s);
true
gap> HasIsBand(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasSize(s);
true
gap> HasIsTrivial(s);
true
gap> HasIsZeroSemigroup(s);
true
gap> HasIsLeftZeroSemigroup(s);
true
gap> HasIsRightZeroSemigroup(s);
true
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
84
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false

# Not created by RectangularBand
gap> s := AsTransformationSemigroup(s);
<transformation semigroup of degree 85 with 84 generators>
gap> HasIsRectangularBand(s);
false
gap> HasIsBand(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasSize(s);
false
gap> HasIsTrivial(s);
false
gap> HasIsZeroSemigroup(s);
false
gap> HasIsLeftZeroSemigroup(s);
false
gap> HasIsRightZeroSemigroup(s);
false
gap> IsRectangularBand(s);
true
gap> IsBand(s);
true
gap> IsZeroSemigroup(s);
false
gap> Size(s);
84
gap> IsTrivial(s);
false
gap> IsZeroSemigroup(s);
false
gap> IsLeftZeroSemigroup(s);
false
gap> IsRightZeroSemigroup(s);
false

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E# 
gap> STOP_TEST("Semigroups package: standard/semigroups/constructions.tst");
