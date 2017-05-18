#############################################################################
##
#W  standard/semicons.tst
#Y  Copyright (C) 2015                                      Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
gap> START_TEST("Semigroups package: standard/semicons.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# constructions: TrivialSemigroup: errors
gap> S := TrivialSemigroup(-1);
Error, Semigroups: TrivialSemigroup: usage,
the arguments must be a non-negative integer or a filter and a non-negative
integer,
gap> S := TrivialSemigroup(IsPartialPermSemigroup, -1);
Error, Semigroups: TrivialSemigroup: usage,
the arguments must be a non-negative integer or a filter and a non-negative
integer,
gap> S := TrivialSemigroup(0, 1);
Error, Semigroups: TrivialSemigroup: usage,
the arguments must be a non-negative integer or a filter and a non-negative
integer,
gap> S := TrivialSemigroup(IsPartialPermSemigroup, 1, 1);
Error, Semigroups: TrivialSemigroup: usage,
the arguments must be a non-negative integer or a filter and a non-negative
integer,
gap> S := TrivialSemigroup(IsPermGroup, 1, 1);
Error, Semigroups: TrivialSemigroup: usage,
the arguments must be a non-negative integer or a filter and a non-negative
integer,
gap> S := TrivialSemigroup(IsFreeBand);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `TrivialSemigroupCons' on 2 arguments

#T# constructions: TrivialSemigroup: known properties and attributes
gap> S := TrivialSemigroup(IsPartialPermSemigroup, 5);;
gap> HasIsTrivial(S);
true
gap> IsTrivial(S);
true
gap> S := Semigroup(S);;
gap> HasIsTrivial(S);
true
gap> IsTrivial(S);
true

#T# constructions: TrivialSemigroup: default
gap> S := TrivialSemigroup();
<trivial transformation group of degree 0 with 1 generator>
gap> S := TrivialSemigroup(0);
<trivial transformation group of degree 0 with 1 generator>
gap> S := TrivialSemigroup(1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := TrivialSemigroup(5);
<trivial transformation group of degree 5 with 1 generator>
gap> S := TrivialSemigroup(10);
<trivial transformation group of degree 10 with 1 generator>

#T# constructions: TrivialSemigroup: transformation semigroup
gap> S := TrivialSemigroup(IsTransformationSemigroup);
<trivial transformation group of degree 0 with 1 generator>
gap> S := TrivialSemigroup(IsTransformationSemigroup, 0);
<trivial transformation group of degree 0 with 1 generator>
gap> S := TrivialSemigroup(IsTransformationSemigroup, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := TrivialSemigroup(IsTransformationSemigroup, 5);
<trivial transformation group of degree 5 with 1 generator>
gap> S := TrivialSemigroup(IsTransformationSemigroup, 10);
<trivial transformation group of degree 10 with 1 generator>

#T# constructions: TrivialSemigroup: partial perm semigroup
gap> S := TrivialSemigroup(IsPartialPermSemigroup);
<trivial partial perm group of rank 0 with 1 generator>
gap> S := TrivialSemigroup(IsPartialPermSemigroup, 0);
<trivial partial perm group of rank 0 with 1 generator>
gap> S := TrivialSemigroup(IsPartialPermSemigroup, 1);
<trivial partial perm group of rank 1 with 1 generator>
gap> S := TrivialSemigroup(IsPartialPermSemigroup, 5);
<trivial partial perm group of rank 5 with 1 generator>
gap> S := TrivialSemigroup(IsPartialPermSemigroup, 10);
<trivial partial perm group of rank 10 with 1 generator>

#T# constructions: TrivialSemigroup: bipartition semigroup
gap> S := TrivialSemigroup(IsBipartitionSemigroup);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := TrivialSemigroup(IsBipartitionSemigroup, 0);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := TrivialSemigroup(IsBipartitionSemigroup, 1);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := TrivialSemigroup(IsBipartitionSemigroup, 5);
<trivial block bijection group of degree 5 with 1 generator>
gap> S := TrivialSemigroup(IsBipartitionSemigroup, 10);
<trivial block bijection group of degree 10 with 1 generator>

#T# constructions: TrivialSemigroup: block bijection semigroup
gap> S := TrivialSemigroup(IsBlockBijectionSemigroup);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := TrivialSemigroup(IsBlockBijectionSemigroup, 0);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := TrivialSemigroup(IsBlockBijectionSemigroup, 1);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := TrivialSemigroup(IsBlockBijectionSemigroup, 5);
<trivial block bijection group of degree 5 with 1 generator>
gap> S := TrivialSemigroup(IsBlockBijectionSemigroup, 10);
<trivial block bijection group of degree 10 with 1 generator>

#T# constructions: TrivialSemigroup: PBR semigroup
gap> S := TrivialSemigroup(IsPBRSemigroup);
<trivial pbr group of degree 1 with 1 generator>
gap> S := TrivialSemigroup(IsPBRSemigroup, 0);
<trivial pbr group of degree 1 with 1 generator>
gap> S := TrivialSemigroup(IsPBRSemigroup, 1);
<trivial pbr group of degree 1 with 1 generator>
gap> S := TrivialSemigroup(IsPBRSemigroup, 5);
<trivial pbr group of degree 5 with 1 generator>
gap> S := TrivialSemigroup(IsPBRSemigroup, 10);
<trivial pbr group of degree 10 with 1 generator>

#T# constructions: TrivialSemigroup: Boolean matrix semigroup
gap> S := TrivialSemigroup(IsBooleanMatSemigroup);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := TrivialSemigroup(IsBooleanMatSemigroup, 0);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := TrivialSemigroup(IsBooleanMatSemigroup, 1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := TrivialSemigroup(IsBooleanMatSemigroup, 5);
<trivial group of 5x5 boolean matrices with 1 generator>
gap> S := TrivialSemigroup(IsBooleanMatSemigroup, 10);
<trivial group of 10x10 boolean matrices with 1 generator>

#T# constructions: TrivialSemigroup: other constructors
gap> S := TrivialSemigroup(IsMaxPlusMatrixSemigroup);
<trivial group of 1x1 max-plus matrices with 1 generator>

#T# constructions: MonogenicSemigroup: errors
gap> S := MonogenicSemigroup(0);
Error, Semigroups: MonogenicSemigroup: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 0);
Error, Semigroups: MonogenicSemigroup: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := MonogenicSemigroup(0, 1);
Error, Semigroups: MonogenicSemigroup: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 0, 0);
Error, Semigroups: MonogenicSemigroup: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := MonogenicSemigroup(IsPermGroup, 1, 1, 1);
Error, Semigroups: MonogenicSemigroup: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 2, true);
Error, Semigroups: MonogenicSemigroup: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := MonogenicSemigroup(IsMaxPlusMatrixSemigroup, 100, 100);
<commutative non-regular semigroup of size 199, 200x200 max-plus matrices 
 with 1 generator>

#T# constructions: MonogenicSemigroup: known properties and attributes, [4, 7]
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 4, 7);;
gap> IndexPeriodOfPartialPerm(GeneratorsOfSemigroup(S)[1]);
[ 4, 7 ]
gap> HasSize(S);
true
gap> Size(S) = 4 + 7 - 1;
true
gap> HasIsMonogenicSemigroup(S);
true
gap> IsMonogenicSemigroup(S);
true
gap> HasIsGroupAsSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> HasIsRegularSemigroup(S);
true
gap> IsRegularSemigroup(S);
false
gap> HasIsZeroSemigroup(S);
true
gap> IsZeroSemigroup(S);
false
gap> S := Semigroup(S);;
gap> IndexPeriodOfPartialPerm(GeneratorsOfSemigroup(S)[1]);
[ 4, 7 ]
gap> Size(S) = 4 + 7 - 1;
true
gap> IsMonogenicSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsRegularSemigroup(S);
false
gap> IsZeroSemigroup(S);
false

#T# constructions: MonogenicSemigroup: known properties and attributes, [2, 1]
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 2, 1);;
gap> HasIsZeroSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> S := Semigroup(S);;
gap> IsZeroSemigroup(S);
true

#T# constructions: MonogenicSemigroup: known properties and attributes, [1, 2]
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 1, 2);;
gap> HasIsGroupAsSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
true
gap> S := Semigroup(S);;
gap> IsGroupAsSemigroup(S);
true

#T# constructions: MonogenicSemigroup: default
gap> S := MonogenicSemigroup(1, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := MonogenicSemigroup(2, 1);
<commutative non-regular transformation semigroup of size 2, degree 3 with 1 
 generator>
gap> S := MonogenicSemigroup(1, 2);
<transformation group of size 2, degree 2 with 1 generator>
gap> S := MonogenicSemigroup(5, 5);
<commutative non-regular transformation semigroup of size 9, degree 10 with 1 
 generator>
gap> S := MonogenicSemigroup(10, 11);
<commutative non-regular transformation semigroup of size 20, degree 21 with 
 1 generator>

#T# constructions: MonogenicSemigroup: transformation semigroup
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 1, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 2, 1);
<commutative non-regular transformation semigroup of size 2, degree 3 with 1 
 generator>
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 1, 2);
<transformation group of size 2, degree 2 with 1 generator>
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 5, 5);
<commutative non-regular transformation semigroup of size 9, degree 10 with 1 
 generator>
gap> S := MonogenicSemigroup(IsTransformationSemigroup, 10, 11);
<commutative non-regular transformation semigroup of size 20, degree 21 with 
 1 generator>

#T# constructions: MonogenicSemigroup: partial perm semigroup
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 1, 1);
<trivial partial perm group of rank 0 with 1 generator>
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 2, 1);
<commutative non-regular partial perm semigroup of size 2, rank 1 with 1 
 generator>
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 1, 2);
<partial perm group of size 2, rank 2 with 1 generator>
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 5, 5);
<commutative non-regular partial perm semigroup of size 9, rank 9 with 1 
 generator>
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 10, 11);
<commutative non-regular partial perm semigroup of size 20, rank 20 with 1 
 generator>

#T# constructions: MonogenicSemigroup: bipartition semigroup
gap> S := MonogenicSemigroup(IsBipartitionSemigroup, 1, 1);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := MonogenicSemigroup(IsBipartitionSemigroup, 2, 1);
<commutative non-regular block bijection semigroup of size 2, degree 3 with 1 
 generator>
gap> S := MonogenicSemigroup(IsBipartitionSemigroup, 1, 2);
<block bijection group of size 2, degree 2 with 1 generator>
gap> S := MonogenicSemigroup(IsBipartitionSemigroup, 5, 5);
<commutative non-regular block bijection semigroup of size 9, degree 11 with 
 1 generator>
gap> S := MonogenicSemigroup(IsBipartitionSemigroup, 10, 11);
<commutative non-regular block bijection semigroup of size 20, degree 22 with 
 1 generator>

#T# constructions: MonogenicSemigroup: block bijection semigroup
gap> S := MonogenicSemigroup(IsBlockBijectionSemigroup, 1, 1);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := MonogenicSemigroup(IsBlockBijectionSemigroup, 2, 1);
<commutative non-regular block bijection semigroup of size 2, degree 3 with 1 
 generator>
gap> S := MonogenicSemigroup(IsBlockBijectionSemigroup, 1, 2);
<block bijection group of size 2, degree 2 with 1 generator>
gap> S := MonogenicSemigroup(IsBlockBijectionSemigroup, 5, 5);
<commutative non-regular block bijection semigroup of size 9, degree 11 with 
 1 generator>
gap> S := MonogenicSemigroup(IsBlockBijectionSemigroup, 10, 11);
<commutative non-regular block bijection semigroup of size 20, degree 22 with 
 1 generator>

#T# constructions: MonogenicSemigroup: PBR semigroup
gap> S := MonogenicSemigroup(IsPBRSemigroup, 1, 1);
<trivial pbr group of degree 1 with 1 generator>
gap> S := MonogenicSemigroup(IsPBRSemigroup, 2, 1);
<commutative non-regular pbr semigroup of size 2, degree 3 with 1 generator>
gap> S := MonogenicSemigroup(IsPBRSemigroup, 1, 2);
<pbr group of size 2, degree 2 with 1 generator>
gap> S := MonogenicSemigroup(IsPBRSemigroup, 5, 5);
<commutative non-regular pbr semigroup of size 9, degree 10 with 1 generator>
gap> S := MonogenicSemigroup(IsPBRSemigroup, 10, 11);
<commutative non-regular pbr semigroup of size 20, degree 21 with 1 generator>

#T# constructions: MonogenicSemigroup: Boolean matrix semigroup
gap> S := MonogenicSemigroup(IsBooleanMatSemigroup, 1, 1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := MonogenicSemigroup(IsBooleanMatSemigroup, 2, 1);
<commutative non-regular semigroup of size 2, 3x3 boolean matrices with 1 
 generator>
gap> S := MonogenicSemigroup(IsBooleanMatSemigroup, 1, 2);
<group of size 2, 2x2 boolean matrices with 1 generator>
gap> S := MonogenicSemigroup(IsBooleanMatSemigroup, 5, 5);
<commutative non-regular semigroup of size 9, 10x10 boolean matrices with 1 
 generator>
gap> S := MonogenicSemigroup(IsBooleanMatSemigroup, 10, 11);
<commutative non-regular semigroup of size 20, 21x21 boolean matrices with 1 
 generator>

#T# constructions: RectangularBand: errors
gap> S := RectangularBand(0);
Error, Semigroups: RectangularBand: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := RectangularBand(IsPartialPermSemigroup, 0);
Error, Semigroups: RectangularBand: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := RectangularBand(0, 1);
Error, Semigroups: RectangularBand: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := RectangularBand(IsPartialPermSemigroup, 0, 0);
Error, Semigroups: RectangularBand: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := RectangularBand(IsPermGroup, 1, 1, 1);
Error, Semigroups: RectangularBand: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := RectangularBand(IsPartialPermSemigroup, 2, true);
Error, Semigroups: RectangularBand: usage,
the arguments must be two positive integers or a filter and a two positive
integers,
gap> S := RectangularBand(IsMaxPlusMatrixSemigroup, 100, 100);
<regular semigroup of size 10000, 21x21 max-plus matrices with 100 generators>

#T# constructions: RectangularBand: known properties and attributes, [3, 4]
gap> S := RectangularBand(3, 4);;
gap> HasSize(S);
true
gap> Size(S) = 3 * 4;
true
gap> HasIsRectangularBand(S);
true
gap> IsRectangularBand(S);
true
gap> HasIsGroupAsSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> HasIsTrivial(S);
true
gap> IsTrivial(S);
false
gap> HasIsRightZeroSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
false
gap> HasIsLeftZeroSemigroup(S);
true
gap> IsLeftZeroSemigroup(S);
false
gap> S := Semigroup(S);;
gap> Size(S) = 3 * 4;
true
gap> IsRectangularBand(S);
true
gap> IsGroupAsSemigroup(S);
false
gap> IsTrivial(S);
false
gap> IsRightZeroSemigroup(S);
false
gap> IsLeftZeroSemigroup(S);
false
gap> S := RectangularBand(5, 2);
<regular transformation semigroup of size 10, degree 7 with 5 generators>

#T# constructions: RectangularBand: known properties and attributes, [1, 1]
gap> S := RectangularBand(IsBooleanMatSemigroup, 1, 1);;
gap> HasIsTrivial(S);
true
gap> IsTrivial(S);
true
gap> S := Semigroup(S);;
gap> IsTrivial(S);
true

#T# constructions: RectangularBand: known properties and attributes, [2, 1]
gap> S := RectangularBand(IsBooleanMatSemigroup, 2, 1);;
gap> HasIsLeftZeroSemigroup(S);
true
gap> IsLeftZeroSemigroup(S);
true
gap> S := Semigroup(S);;
gap> IsLeftZeroSemigroup(S);
true

#T# constructions: RectangularBand: known properties and attributes, [1, 2]
gap> S := RectangularBand(IsBooleanMatSemigroup, 1, 2);;
gap> HasIsRightZeroSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
true
gap> S := Semigroup(S);;
gap> IsRightZeroSemigroup(S);
true

#T# constructions: RectangularBand: default
gap> S := RectangularBand(1, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := RectangularBand(2, 1);
<regular transformation semigroup of size 2, degree 3 with 2 generators>
gap> S := RectangularBand(1, 2);
<regular transformation semigroup of size 2, degree 2 with 2 generators>
gap> S := RectangularBand(5, 5);
<regular transformation semigroup of size 25, degree 10 with 5 generators>
gap> S := RectangularBand(10, 11);
<regular transformation semigroup of size 110, degree 13 with 11 generators>

#T# constructions: RectangularBand: transformation semigroup
gap> S := RectangularBand(IsTransformationSemigroup, 1, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := RectangularBand(IsTransformationSemigroup, 2, 1);
<regular transformation semigroup of size 2, degree 3 with 2 generators>
gap> S := RectangularBand(IsTransformationSemigroup, 1, 2);
<regular transformation semigroup of size 2, degree 2 with 2 generators>
gap> S := RectangularBand(IsTransformationSemigroup, 5, 5);
<regular transformation semigroup of size 25, degree 10 with 5 generators>
gap> S := RectangularBand(IsTransformationSemigroup, 10, 11);
<regular transformation semigroup of size 110, degree 13 with 11 generators>

#T# constructions: RectangularBand: partial perm semigroup
gap> S := RectangularBand(IsPartialPermSemigroup, 1, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments
gap> S := RectangularBand(IsPartialPermSemigroup, 2, 2);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments

#T# constructions: RectangularBand: bipartition semigroup
gap> S := RectangularBand(IsBipartitionSemigroup, 1, 1);
<trivial bipartition group of degree 1 with 1 generator>
gap> S := RectangularBand(IsBipartitionSemigroup, 2, 1);
<regular bipartition semigroup of size 2, degree 2 with 2 generators>
gap> S := RectangularBand(IsBipartitionSemigroup, 1, 2);
<regular bipartition semigroup of size 2, degree 2 with 2 generators>
gap> S := RectangularBand(IsBipartitionSemigroup, 5, 5);
<regular bipartition semigroup of size 25, degree 3 with 5 generators>
gap> S := RectangularBand(IsBipartitionSemigroup, 10, 11);
<regular bipartition semigroup of size 110, degree 4 with 11 generators>

#T# constructions: RectangularBand: block bijection semigroup
gap> S := RectangularBand(IsBlockBijectionSemigroup, 1, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments
gap> S := RectangularBand(IsBlockBijectionSemigroup, 2, 2);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments

#T# constructions: RectangularBand: PBR semigroup
gap> S := RectangularBand(IsPBRSemigroup, 1, 1);
<trivial pbr group of degree 1 with 1 generator>
gap> S := RectangularBand(IsPBRSemigroup, 2, 1);
<regular pbr semigroup of size 2, degree 2 with 2 generators>
gap> S := RectangularBand(IsPBRSemigroup, 1, 2);
<regular pbr semigroup of size 2, degree 2 with 2 generators>
gap> S := RectangularBand(IsPBRSemigroup, 5, 5);
<regular pbr semigroup of size 25, degree 3 with 5 generators>
gap> S := RectangularBand(IsPBRSemigroup, 10, 11);
<regular pbr semigroup of size 110, degree 4 with 11 generators>

#T# constructions: RectangularBand: Boolean matrix semigroup
gap> S := RectangularBand(IsBooleanMatSemigroup, 1, 1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := RectangularBand(IsBooleanMatSemigroup, 2, 1);
<regular semigroup of size 2, 3x3 boolean matrices with 2 generators>
gap> S := RectangularBand(IsBooleanMatSemigroup, 1, 2);
<regular semigroup of size 2, 2x2 boolean matrices with 2 generators>
gap> S := RectangularBand(IsBooleanMatSemigroup, 5, 5);
<regular semigroup of size 25, 10x10 boolean matrices with 5 generators>

#T# constructions: RectangularBand: Rees matrix semigroup
gap> S := RectangularBand(IsReesMatrixSemigroup, 1, 1);
<Rees matrix semigroup 1x1 over Group(())>
gap> S := RectangularBand(IsReesMatrixSemigroup, 2, 1);
<Rees matrix semigroup 2x1 over Group(())>
gap> S := RectangularBand(IsReesMatrixSemigroup, 1, 2);
<Rees matrix semigroup 1x2 over Group(())>
gap> S := RectangularBand(IsReesMatrixSemigroup, 5, 5);
<Rees matrix semigroup 5x5 over Group(())>
gap> S := RectangularBand(IsReesMatrixSemigroup, 10, 11);
<Rees matrix semigroup 10x11 over Group(())>

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
gap> S := ZeroSemigroup(IsMaxPlusMatrixSemigroup, 10);
<commutative non-regular semigroup of size 10, 6x6 max-plus matrices with 9 
 generators>

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
false
gap> AsList(S);
[ IdentityTransformation ]
gap> IsGroup(S);
false
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
false

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
Transformation( [ 1, 1, 1, 1, 1 ] )
gap> HasAsList(S);
false
gap> AsList(S);
[ Transformation( [ 1, 1, 1, 1, 2 ] ), Transformation( [ 1, 1, 1, 1, 3 ] ), 
  Transformation( [ 1, 1, 1, 2, 1 ] ), Transformation( [ 1, 1, 1, 2, 2 ] ), 
  Transformation( [ 1, 1, 1, 1, 1 ] ) ]
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
Transformation( [ 1, 1, 1, 1, 1 ] )
gap> HasAsList(S);
false
gap> AsList(S);
[ Transformation( [ 1, 1, 1, 1, 2 ] ), Transformation( [ 1, 1, 1, 1, 3 ] ), 
  Transformation( [ 1, 1, 1, 2, 1 ] ), Transformation( [ 1, 1, 1, 2, 2 ] ), 
  Transformation( [ 1, 1, 1, 1, 1 ] ) ]
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
<trivial transformation group of degree 0 with 1 generator>
gap> S := ZeroSemigroup(2);
<commutative non-regular transformation semigroup of size 2, degree 3 with 1 
 generator>
gap> S := ZeroSemigroup(3);
<commutative non-regular transformation semigroup of size 3, degree 4 with 2 
 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: transformation semigroup
gap> S := ZeroSemigroup(IsTransformationSemigroup, 1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := ZeroSemigroup(IsTransformationSemigroup, 5);
<commutative non-regular transformation semigroup of size 5, degree 5 with 4 
 generators>
gap> S := ZeroSemigroup(IsTransformationSemigroup, 10);
<commutative non-regular transformation semigroup of size 10, degree 6 with 9 
 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: partial perm semigroup
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 1);
<trivial partial perm group of rank 0 with 1 generator>
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 5);
<commutative non-regular partial perm semigroup of size 5, rank 4 with 4 
 generators>
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 10);
<commutative non-regular partial perm semigroup of size 10, rank 9 with 9 
 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: bipartition semigroup
gap> S := ZeroSemigroup(IsBipartitionSemigroup, 1);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := ZeroSemigroup(IsBipartitionSemigroup, 2);
<commutative non-regular bipartition semigroup of size 2, degree 2 with 1 
 generator>
gap> S := ZeroSemigroup(IsBipartitionSemigroup, 5);
<commutative non-regular bipartition semigroup of size 5, degree 5 with 4 
 generators>
gap> S := ZeroSemigroup(IsBipartitionSemigroup, 10);
<commutative non-regular bipartition semigroup of size 10, degree 6 with 9 
 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: block bijection semigroup
gap> S := ZeroSemigroup(IsBlockBijectionSemigroup, 1);
<trivial block bijection group of degree 1 with 1 generator>
gap> S := ZeroSemigroup(IsBlockBijectionSemigroup, 2);
<commutative non-regular block bijection semigroup of size 2, degree 3 with 1 
 generator>
gap> S := ZeroSemigroup(IsBlockBijectionSemigroup, 5);
<commutative non-regular block bijection semigroup of size 5, degree 8 with 4 
 generators>
gap> S := ZeroSemigroup(IsBlockBijectionSemigroup, 10);
<commutative non-regular block bijection semigroup of size 10, degree 18 with 
 9 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: PBR semigroup
gap> S := ZeroSemigroup(IsPBRSemigroup, 1);
<trivial pbr group of degree 1 with 1 generator>
gap> S := ZeroSemigroup(IsPBRSemigroup, 5);
<commutative non-regular pbr semigroup of size 5, degree 5 with 4 generators>
gap> S := ZeroSemigroup(IsPBRSemigroup, 10);
<commutative non-regular pbr semigroup of size 10, degree 6 with 9 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

#T# constructions: ZeroSemigroup: Boolean matrix semigroup
gap> S := ZeroSemigroup(IsBooleanMatSemigroup, 1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := ZeroSemigroup(IsBooleanMatSemigroup, 5);
<commutative non-regular semigroup of size 5, 5x5 boolean matrices with 4 
 generators>
gap> S := ZeroSemigroup(IsBooleanMatSemigroup, 10);
<commutative non-regular semigroup of size 10, 6x6 boolean matrices with 9 
 generators>
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

#T# constructions: LeftZeroSemigroup, error
gap> S := LeftZeroSemigroup();
Error, Semigroups: LeftZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := LeftZeroSemigroup(0);
Error, Semigroups: LeftZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := LeftZeroSemigroup(0, 1);
Error, Semigroups: LeftZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := LeftZeroSemigroup(IsTransformationSemigroup, 0);
Error, Semigroups: LeftZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := LeftZeroSemigroup(1, 2, 3);
Error, Semigroups: LeftZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := LeftZeroSemigroup(IsMaxPlusMatrixSemigroup, 4);
<regular semigroup of size 4, 4x4 max-plus matrices with 4 generators>
gap> S := LeftZeroSemigroup(IsGroup, 4);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments

#T# constructions: LeftZeroSemigroup 
gap> S := LeftZeroSemigroup(1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := LeftZeroSemigroup(IsBipartitionSemigroup, 10);
<regular bipartition semigroup of size 10, degree 4 with 10 generators>
gap> IsLeftZeroSemigroup(Semigroup(S));
true
gap> Size(S);
10

#T# constructions: RightZeroSemigroup, error
gap> S := RightZeroSemigroup();
Error, Semigroups: RightZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := RightZeroSemigroup(0);
Error, Semigroups: RightZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := RightZeroSemigroup(0, 1);
Error, Semigroups: RightZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := RightZeroSemigroup(IsTransformationSemigroup, 0);
Error, Semigroups: RightZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := RightZeroSemigroup(1, 2, 3);
Error, Semigroups: RightZeroSemigroup: usage,
the arguments must be a positive integer or a filter and a positive integer,
gap> S := RightZeroSemigroup(IsMaxPlusMatrixSemigroup, 4);
<regular semigroup of size 4, 4x4 max-plus matrices with 4 generators>
gap> S := RightZeroSemigroup(IsGroup, 4);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments

#T# constructions: RightZeroSemigroup
gap> S := RightZeroSemigroup(1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := RightZeroSemigroup(IsBipartitionSemigroup, 10);
<regular bipartition semigroup of size 10, degree 4 with 10 generators>
gap> IsRightZeroSemigroup(Semigroup(S));
true
gap> Size(S);
10

#T# constructions: RightZeroSemigroup, deg = 0 mod 3
gap> S := RightZeroSemigroup(9);
<transformation semigroup of degree 6 with 9 generators>

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);

#E# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semicons.tst");
