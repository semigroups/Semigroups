#############################################################################
##
#W  standard/semigroups/semicons.tst
#Y  Copyright (C) 2015-2022                                 Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

#@local D, H, L, S, S1, S2, c, c1, c2, c3, id, m1, m2, out, p, temp
gap> START_TEST("Semigroups package: standard/semigroups/semicons.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# constructions: TrivialSemigroup: errors
gap> S := TrivialSemigroup(-1);
Error, the arguments must be a non-negative integer or a filter and a non-nega\
tive integer
gap> S := TrivialSemigroup(IsPartialPermSemigroup, -1);
Error, the arguments must be a non-negative integer or a filter and a non-nega\
tive integer
gap> S := TrivialSemigroup(0, 1);
Error, the arguments must be a non-negative integer or a filter and a non-nega\
tive integer
gap> S := TrivialSemigroup(IsPartialPermSemigroup, 1, 1);
Error, the arguments must be a non-negative integer or a filter and a non-nega\
tive integer
gap> S := TrivialSemigroup(IsPermGroup, 1, 1);
Error, the arguments must be a non-negative integer or a filter and a non-nega\
tive integer
gap> S := TrivialSemigroup(IsFreeBand);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `TrivialSemigroupCons' on 2 arguments

# constructions: TrivialSemigroup: known properties and attributes
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

# constructions: TrivialSemigroup: default
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

# constructions: TrivialSemigroup: transformation semigroup
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

# constructions: TrivialSemigroup: partial perm semigroup
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

# constructions: TrivialSemigroup: bipartition semigroup
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

# constructions: TrivialSemigroup: block bijection semigroup
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

# constructions: TrivialSemigroup: PBR semigroup
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

# constructions: TrivialSemigroup: Boolean matrix semigroup
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

# constructions: TrivialSemigroup: other constructors
gap> S := TrivialSemigroup(IsMaxPlusMatrixSemigroup);
<trivial group of 1x1 max-plus matrices with 1 generator>

# constructions: MonogenicSemigroup: errors
gap> S := MonogenicSemigroup(0);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 0);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := MonogenicSemigroup(0, 1);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 0, 0);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := MonogenicSemigroup(IsPermGroup, 1, 1, 1);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 2, true);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := MonogenicSemigroup(IsMaxPlusMatrixSemigroup, 100, 100);
<commutative non-regular semigroup of size 199, 200x200 max-plus matrices 
 with 1 generator>

# constructions: MonogenicSemigroup: known properties and attributes, [4, 7]
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

# constructions: MonogenicSemigroup: known properties and attributes, [2, 1]
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 2, 1);;
gap> HasIsZeroSemigroup(S);
true
gap> IsZeroSemigroup(S);
true
gap> S := Semigroup(S);;
gap> IsZeroSemigroup(S);
true

# constructions: MonogenicSemigroup: known properties and attributes, [1, 2]
gap> S := MonogenicSemigroup(IsPartialPermSemigroup, 1, 2);;
gap> HasIsGroupAsSemigroup(S);
true
gap> IsGroupAsSemigroup(S);
true
gap> S := Semigroup(S);;
gap> IsGroupAsSemigroup(S);
true

# constructions: MonogenicSemigroup: default
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

# constructions: MonogenicSemigroup: transformation semigroup
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

# constructions: MonogenicSemigroup: partial perm semigroup
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

# constructions: MonogenicSemigroup: bipartition semigroup
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

# constructions: MonogenicSemigroup: block bijection semigroup
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

# constructions: MonogenicSemigroup: PBR semigroup
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

# constructions: MonogenicSemigroup: Boolean matrix semigroup
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

# constructions: RectangularBand: errors
gap> S := RectangularBand(0);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := RectangularBand(IsPartialPermSemigroup, 0);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := RectangularBand(0, 1);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := RectangularBand(IsPartialPermSemigroup, 0, 0);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := RectangularBand(IsPermGroup, 1, 1, 1);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := RectangularBand(IsPartialPermSemigroup, 2, true);
Error, the arguments must be 2 positive integers or a filter and a 2 positive \
integers
gap> S := RectangularBand(IsMaxPlusMatrixSemigroup, 100, 100);
<regular semigroup of size 10000, 21x21 max-plus matrices with 100 generators>

# constructions: RectangularBand: known properties and attributes, [3, 4]
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

# constructions: RectangularBand: known properties and attributes, [1, 1]
gap> S := RectangularBand(IsBooleanMatSemigroup, 1, 1);;
gap> HasIsTrivial(S);
true
gap> IsTrivial(S);
true
gap> S := Semigroup(S);;
gap> IsTrivial(S);
true

# constructions: RectangularBand: known properties and attributes, [2, 1]
gap> S := RectangularBand(IsBooleanMatSemigroup, 2, 1);;
gap> HasIsLeftZeroSemigroup(S);
true
gap> IsLeftZeroSemigroup(S);
true
gap> S := Semigroup(S);;
gap> IsLeftZeroSemigroup(S);
true

# constructions: RectangularBand: known properties and attributes, [1, 2]
gap> S := RectangularBand(IsBooleanMatSemigroup, 1, 2);;
gap> HasIsRightZeroSemigroup(S);
true
gap> IsRightZeroSemigroup(S);
true
gap> S := Semigroup(S);;
gap> IsRightZeroSemigroup(S);
true

# constructions: RectangularBand: default
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

# constructions: RectangularBand: transformation semigroup
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

# constructions: RectangularBand: partial perm semigroup
gap> S := RectangularBand(IsPartialPermSemigroup, 1, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments
gap> S := RectangularBand(IsPartialPermSemigroup, 2, 2);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments

# constructions: RectangularBand: bipartition semigroup
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

# constructions: RectangularBand: block bijection semigroup
gap> S := RectangularBand(IsBlockBijectionSemigroup, 1, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments
gap> S := RectangularBand(IsBlockBijectionSemigroup, 2, 2);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments

# constructions: RectangularBand: PBR semigroup
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

# constructions: RectangularBand: Boolean matrix semigroup
gap> S := RectangularBand(IsBooleanMatSemigroup, 1, 1);
<trivial group of 1x1 boolean matrices with 1 generator>
gap> S := RectangularBand(IsBooleanMatSemigroup, 2, 1);
<regular semigroup of size 2, 3x3 boolean matrices with 2 generators>
gap> S := RectangularBand(IsBooleanMatSemigroup, 1, 2);
<regular semigroup of size 2, 2x2 boolean matrices with 2 generators>
gap> S := RectangularBand(IsBooleanMatSemigroup, 5, 5);
<regular semigroup of size 25, 10x10 boolean matrices with 5 generators>

# constructions: RectangularBand: Rees matrix semigroup
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

# constructions: FreeSemilattice: errors
gap> S := FreeSemilattice(0);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := FreeSemilattice(IsPartialPermSemigroup, 0);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := FreeSemilattice(IsPermGroup, 1, 1);
Error, expected 2 arguments found 3
gap> S := FreeSemilattice(IsPartialPermSemigroup, true);
Error, the arguments must be a positive integer or a filter and a positive int\
eger

# constructions: FreeSemilattice: known properties and attributes, 17
gap> S := FreeSemilattice(17);;
gap> HasSize(S);
true
gap> Size(S) = 2 ^ 17 - 1;
true

# constructions: FreeSemilattice: default
gap> S := FreeSemilattice(1);
<trivial transformation group of degree 2 with 1 generator>
gap> S := FreeSemilattice(2);
<inverse transformation semigroup of size 3, degree 3 with 2 generators>
gap> S := FreeSemilattice(5);
<inverse transformation semigroup of size 31, degree 6 with 5 generators>
gap> S := FreeSemilattice(21);
<inverse transformation semigroup of size 2097151, degree 22 with 21 
 generators>

# constructions: FreeSemilattice: transformation semigroup
gap> S := FreeSemilattice(IsTransformationSemigroup, 1);
<trivial transformation group of degree 2 with 1 generator>
gap> S := FreeSemilattice(IsTransformationSemigroup, 2);
<inverse transformation semigroup of size 3, degree 3 with 2 generators>
gap> S := FreeSemilattice(IsTransformationSemigroup, 5);
<inverse transformation semigroup of size 31, degree 6 with 5 generators>
gap> S := FreeSemilattice(IsTransformationSemigroup, 11);
<inverse transformation semigroup of size 2047, degree 12 with 11 generators>

# constructions: ZeroSemigroup: errors
gap> S := ZeroSemigroup(0);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 0);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := ZeroSemigroup(0, 1);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := ZeroSemigroup(0, 0);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := ZeroSemigroup(IsPermGroup, 1);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `ZeroSemigroupCons' on 2 arguments
gap> S := ZeroSemigroup(IsPartialPermSemigroup, 2, true);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := ZeroSemigroup(IsMaxPlusMatrixSemigroup, 10);
<commutative non-regular semigroup of size 10, 6x6 max-plus matrices with 9 
 generators>

# constructions: ZeroSemigroup: known properties and attributes, n = 1
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

# constructions: ZeroSemigroup: known properties and attributes, n = 2
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

# constructions: ZeroSemigroup: known properties and attributes, n = 5
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

# constructions: ZeroSemigroup: default
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

# constructions: ZeroSemigroup: transformation semigroup
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

# constructions: ZeroSemigroup: partial perm semigroup
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

# constructions: ZeroSemigroup: bipartition semigroup
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

# constructions: ZeroSemigroup: block bijection semigroup
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

# constructions: ZeroSemigroup: PBR semigroup
gap> S := ZeroSemigroup(IsPBRSemigroup, 1);
<trivial pbr group of degree 1 with 1 generator>
gap> S := ZeroSemigroup(IsPBRSemigroup, 5);
<commutative non-regular pbr semigroup of size 5, degree 5 with 4 generators>
gap> S := ZeroSemigroup(IsPBRSemigroup, 10);
<commutative non-regular pbr semigroup of size 10, degree 6 with 9 generators>
gap> IsZeroSemigroup(Semigroup(S));
true

# constructions: ZeroSemigroup: Boolean matrix semigroup
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

# constructions: ZeroSemigroup: Rees 0-matrix semigroup, error
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 1);
Error, there is no Rees 0-matrix semigroup of order 1

# constructions: ZeroSemigroup: Rees 0-matrix semigroup, 2
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 2);
<Rees 0-matrix semigroup 1x1 over Group(())>
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 5);
<Rees 0-matrix semigroup 4x1 over Group(())>
gap> S := ZeroSemigroup(IsReesZeroMatrixSemigroup, 10);
<Rees 0-matrix semigroup 9x1 over Group(())>
gap> IsZeroSemigroup(Semigroup(S));
true

# constructions: LeftZeroSemigroup, error
gap> S := LeftZeroSemigroup();
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := LeftZeroSemigroup(0);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := LeftZeroSemigroup(0, 1);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := LeftZeroSemigroup(IsTransformationSemigroup, 0);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := LeftZeroSemigroup(1, 2, 3);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := LeftZeroSemigroup(IsMaxPlusMatrixSemigroup, 4);
<regular semigroup of size 4, 4x4 max-plus matrices with 4 generators>
gap> S := LeftZeroSemigroup(IsGroup, 4);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments

# constructions: LeftZeroSemigroup 
gap> S := LeftZeroSemigroup(1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := LeftZeroSemigroup(IsBipartitionSemigroup, 10);
<regular bipartition semigroup of size 10, degree 4 with 10 generators>
gap> IsLeftZeroSemigroup(Semigroup(S));
true
gap> Size(S);
10

# constructions: RightZeroSemigroup, error
gap> S := RightZeroSemigroup();
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := RightZeroSemigroup(0);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := RightZeroSemigroup(0, 1);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := RightZeroSemigroup(IsTransformationSemigroup, 0);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := RightZeroSemigroup(1, 2, 3);
Error, the arguments must be a positive integer or a filter and a positive int\
eger
gap> S := RightZeroSemigroup(IsMaxPlusMatrixSemigroup, 4);
<regular semigroup of size 4, 4x4 max-plus matrices with 4 generators>
gap> S := RightZeroSemigroup(IsGroup, 4);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RectangularBandCons' on 3 arguments

# constructions: RightZeroSemigroup
gap> S := RightZeroSemigroup(1);
<trivial transformation group of degree 0 with 1 generator>
gap> S := RightZeroSemigroup(IsBipartitionSemigroup, 10);
<regular bipartition semigroup of size 10, degree 4 with 10 generators>
gap> IsRightZeroSemigroup(Semigroup(S));
true
gap> Size(S);
10

# constructions: RightZeroSemigroup, deg = 0 mod 3
gap> S := RightZeroSemigroup(9);
<transformation semigroup of degree 6 with 9 generators>

# constructions: Brandt semigroups, partial perm semigroups, default
gap> S := BrandtSemigroup(Group((1, 2)), 1);
<0-simple inverse partial perm semigroup of rank 2 with 2 generators>
gap> MultiplicativeZero(S);
<empty partial perm>
gap> Size(S);
3
gap> IsBrandtSemigroup(S);
true
gap> S := BrandtSemigroup(Group((1, 2)), 2);
<0-simple inverse partial perm semigroup of rank 4 with 2 generators>
gap> MultiplicativeZero(S);
<empty partial perm>
gap> Size(S);
9
gap> IsBrandtSemigroup(S);
true
gap> S := BrandtSemigroup(IsPartialPermSemigroup, Group((1, 2)), 5);
<0-simple inverse partial perm semigroup of rank 10 with 5 generators>
gap> Size(S);
51
gap> S := BrandtSemigroup(10);
<0-simple inverse partial perm semigroup of rank 10 with 9 generators>
gap> Size(S);
101
gap> S := BrandtSemigroup(1);
<0-simple inverse partial perm monoid of rank 1 with 2 generators>
gap> Size(S);
2
gap> S := BrandtSemigroup(TrivialGroup(IsPermGroup), 1);
<0-simple inverse partial perm monoid of rank 1 with 2 generators>
gap> Size(S);
2

# constructions: Brandt semigroups, Rees 0-matrix semigroup
gap> S := BrandtSemigroup(IsReesZeroMatrixSemigroup, 4);
<Rees 0-matrix semigroup 4x4 over Group(())>
gap> S := BrandtSemigroup(IsReesZeroMatrixSemigroup, DihedralGroup(4), 4);
<Rees 0-matrix semigroup 4x4 over <pc group of size 4 with 2 generators>>
gap> IsInverseSemigroup(last);
true
gap> S := BrandtSemigroup(IsReesZeroMatrixSemigroup, DihedralGroup(4));
Error, the arguments must be a positive integer or a filter and a positive int\
eger, or  a perm group and positive integer, or a filter, perm group, and posi\
tive integer

# constructions: Brandt semigroups, other semigroups
gap> S := BrandtSemigroup(IsTransformationSemigroup, 3);
<0-simple transformation semigroup of degree 4 with 4 generators>
gap> S := BrandtSemigroup(IsTransformationSemigroup, Group((1, 2)), 3);
<0-simple transformation semigroup of degree 7 with 5 generators>
gap> S := BrandtSemigroup(IsTransformationSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsBipartitionSemigroup, 3);
<0-simple inverse bipartition semigroup of degree 3 with 2 generators>
gap> S := BrandtSemigroup(IsBipartitionSemigroup, Group((1, 2)), 3);
<0-simple inverse bipartition semigroup of degree 6 with 3 generators>
gap> S := BrandtSemigroup(IsBipartitionSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsPBRSemigroup, 3);
<0-simple pbr semigroup of degree 4 with 4 generators>
gap> S := BrandtSemigroup(IsPBRSemigroup, Group((1, 2)), 3);
<0-simple pbr semigroup of degree 7 with 5 generators>
gap> S := BrandtSemigroup(IsPBRSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsBooleanMatSemigroup, 3);
<0-simple semigroup of 4x4 boolean matrices with 4 generators>
gap> S := BrandtSemigroup(IsBooleanMatSemigroup, Group((1, 2)), 3);
<0-simple semigroup of 7x7 boolean matrices with 5 generators>
gap> S := BrandtSemigroup(IsBooleanMatSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsNTPMatrixSemigroup, 3);
<0-simple semigroup of 4x4 ntp matrices with 4 generators>
gap> S := BrandtSemigroup(IsNTPMatrixSemigroup, Group((1, 2)), 3);
<0-simple semigroup of 7x7 ntp matrices with 5 generators>
gap> S := BrandtSemigroup(IsNTPMatrixSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsMaxPlusMatrixSemigroup, 3);
<0-simple semigroup of 4x4 max-plus matrices with 4 generators>
gap> S := BrandtSemigroup(IsMaxPlusMatrixSemigroup, Group((1, 2)), 3);
<0-simple semigroup of 7x7 max-plus matrices with 5 generators>
gap> S := BrandtSemigroup(IsMaxPlusMatrixSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsMinPlusMatrixSemigroup, 3);
<0-simple semigroup of 4x4 min-plus matrices with 4 generators>
gap> S := BrandtSemigroup(IsMinPlusMatrixSemigroup, Group((1, 2)), 3);
<0-simple semigroup of 7x7 min-plus matrices with 5 generators>
gap> S := BrandtSemigroup(IsMinPlusMatrixSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsTropicalMaxPlusMatrixSemigroup, 3);
<0-simple semigroup of 4x4 tropical max-plus matrices with 4 generators>
gap> S := BrandtSemigroup(IsTropicalMaxPlusMatrixSemigroup, Group((1, 2)), 3);
<0-simple semigroup of 7x7 tropical max-plus matrices with 5 generators>
gap> S := BrandtSemigroup(IsTropicalMaxPlusMatrixSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsTropicalMinPlusMatrixSemigroup, 3);
<0-simple semigroup of 4x4 tropical min-plus matrices with 4 generators>
gap> S := BrandtSemigroup(IsTropicalMinPlusMatrixSemigroup, Group((1, 2)), 3);
<0-simple semigroup of 7x7 tropical min-plus matrices with 5 generators>
gap> S := BrandtSemigroup(IsTropicalMinPlusMatrixSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsProjectiveMaxPlusMatrixSemigroup, 3);
<0-simple semigroup of 4x4 projective max-plus matrices with 4 generators>
gap> S := BrandtSemigroup(IsProjectiveMaxPlusMatrixSemigroup, Group((1, 2)), 3);
<0-simple semigroup of 7x7 projective max-plus matrices with 5 generators>
gap> S := BrandtSemigroup(IsProjectiveMaxPlusMatrixSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments
gap> S := BrandtSemigroup(IsIntegerMatrixSemigroup, 3);
<0-simple semigroup of 4x4 integer matrices with 4 generators>
gap> S := BrandtSemigroup(IsIntegerMatrixSemigroup, Group((1, 2)), 3);
<0-simple semigroup of 7x7 integer matrices with 5 generators>
gap> S := BrandtSemigroup(IsIntegerMatrixSemigroup, DihedralGroup(4), 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BrandtSemigroupCons' on 3 arguments

# constructions: strong semilattices of semigroups: trivial argument checks
gap> D := CompleteDigraph(2);;
gap> S1 := TrivialSemigroup();;
gap> id := IdentityMapping(S1);;
gap> StrongSemilatticeOfSemigroups(D, [S1, S1], [[id, id], [id, id]]);
Error, the reflexive transitive closure of the 1st argument (a digraph) must b\
e a meet semilattice
gap> D := Digraph([[2], []]);;
gap> StrongSemilatticeOfSemigroups(D, [S1, 1], [[id], []]);
Error, the 2nd argument (a list) must consist of semigroups, but found integer\
 in position 2
gap> StrongSemilatticeOfSemigroups(D, [S1, S1, S1], [[id], []]);
Error, the 2nd argument (a list) must have length 2, the number of vertices of\
 the 1st argument (a digraph), but found length 3
gap> StrongSemilatticeOfSemigroups(D, [S1, S1], [[id], [], []]);
Error, the 3rd argument (a list) must have length 2, the number of vertices of\
 the 1st argument (a digraph), but found length 3
gap> StrongSemilatticeOfSemigroups(D, [S1, S1], [1, []]);
Error, the 3rd argument (a list) must consist of lists, but found integer in p\
osition 1
gap> StrongSemilatticeOfSemigroups(D, [S1, S1], [[id, id], []]);
Error, the 3rd argument (a list) must have the same shape as the out-neighbour\
s of the 1st argument (a digraph), expected shape [ 1, 0 ] but found [ 2, 0 ]
gap> S2 := FullTransformationMonoid(2);;
gap> m1 := SemigroupHomomorphismByFunction(S2, S2, x -> Transformation([2, 1]));;
gap> StrongSemilatticeOfSemigroups(D, [S2, S2], [[m1], []]);
Error, the 3rd argument (a list) must consist of lists of homomorphisms, but p\
osition [1, 1] is not a homomorphism
gap> StrongSemilatticeOfSemigroups(D, [S1, S2], [[id], []]);
Error, expected the homomorphism in position [ 1, 1 ] of the 3rd argument to h\
ave source equal to position 2 in the 2nd argument
gap> id := SemigroupHomomorphismByFunction(S1, S2, IdFunc);;
gap> StrongSemilatticeOfSemigroups(D, [S2, S1], [[id], []]);
<strong semilattice of 2 semigroups>

# constructions: strong semilattices of semigroups: homomorphism checks
gap> D := Digraph([[2, 3], [4], [4], []]);;
gap> S1 := FullTransformationMonoid(2);;
gap> id := IdentityMapping(S1);;
gap> m1 := SemigroupHomomorphismByFunction(S1, S1, x -> Transformation([1, 1]));;
gap> m2 := SemigroupHomomorphismByFunction(S1, S1, x -> Transformation([2, 2]));;
gap> L := [S1, S1, S1, S1];;
gap> H := [[m1, m2], [id], [id], []];;
gap> StrongSemilatticeOfSemigroups(D, L, H);
Error, Composing homomorphisms along different paths from 1 to 
4 does not produce the same result. The homomorphisms must commute
gap> D := Digraph([[2, 3], [4], [4], [4]]);;
gap> H := [[id, id], [id], [id], [m1]];;
gap> StrongSemilatticeOfSemigroups(D, L, H);
Error, Expected homomorphism from 4 to 4 to be the identity

# constructions: strong semilattices of semigroups: valid example
gap> D := Digraph([[2, 3], [2], []]);;
gap> S1 := FullTransformationMonoid(2);;
gap> id := IdentityMapping(S1);;
gap> m1 := SemigroupHomomorphismByFunction(S1, S1, x -> Transformation([1, 1]));
<full transformation monoid of degree 2> -> 
<full transformation monoid of degree 2>
gap> m2 := SemigroupHomomorphismByFunction(S1, S1, x -> Transformation([2, 2]));
<full transformation monoid of degree 2> -> 
<full transformation monoid of degree 2>
gap> L := [S1, S1, S1];;
gap> H := [[m1, m2], [id], []];;
gap> StrongSemilatticeOfSemigroups(D, L, H);
<strong semilattice of 3 semigroups>
gap> Size(last);
12

# constructions: strong semilattices of semigroups: SSSEs
gap> D := Digraph([[2, 3], [2], []]);;
gap> S1 := FullTransformationMonoid(2);;
gap> id := IdentityMapping(S1);;
gap> m1 := SemigroupHomomorphismByFunction(S1, S1, x -> Transformation([1, 1]));
<full transformation monoid of degree 2> -> 
<full transformation monoid of degree 2>
gap> m2 := SemigroupHomomorphismByFunction(S1, S1, x -> Transformation([2, 2]));
<full transformation monoid of degree 2> -> 
<full transformation monoid of degree 2>
gap> L := [S1, S1, S1];;
gap> H := [[m1, m2], [id], []];;
gap> S := StrongSemilatticeOfSemigroups(D, L, H);;
gap> SSSE(S, 10, IdentityTransformation);
Error, expected 2nd argument to be an integer between 1 and the size of the se\
milattice, i.e. 3
gap> SSSE(S, 1, Transformation([3, 2, 1]));
Error, where S, n and x are the 1st, 2nd and 3rd arguments respectively, expec\
ted x to be an element of SemigroupsOfStrongSemilatticeOfSemigroups(S)[n]
gap> SSSE(S, 2, Transformation([2, 1])) < SSSE(S, 3, Transformation([1, 1]));
true
gap> SSSE(S, 2, Transformation([2, 1])) = SSSE(S, 3, Transformation([1, 1]));
false
gap> SSSE(S, 2, Transformation([2, 1])) * SSSE(S, 3, Transformation([1, 1]))
>    = SSSE(S, 1, Transformation([2, 2]));
true
gap> S = UnderlyingSemilatticeOfSemigroups(SSSE(S, 2, Transformation([2, 1])));
true
gap> ViewString(SSSE(S, 1, Transformation([2, 2])));
"SSSE(1, \>Transformation( [ \>2\<,\> 2\< ] )\<)"

# constructions: strong semilattices of semigroups: full worked example (SLOW!)
#
#           5     4
#          / \   /
#         /   \ /
#        2     3
#         \   /
#          \ /
#           1
#
gap> L := [];;
gap> H := [[], [], [], [], []];;
gap> Add(L, Semigroup(Transformation([1, 1, 1]),
>                     Transformation([1, 2, 3]),
>                     Transformation([1, 3, 2])));
gap> Add(L, MagmaWithZeroAdjoined(SymmetricGroup(3)));
gap> Add(L, FullTransformationMonoid(4));
gap> Add(L, RightZeroSemigroup(4));
gap> Add(L, FullTransformationMonoid(3));
gap> m1 := function(trans)
>      local temp, out;
>      # A clever way of embedding T3 in T4, fixing the point 1 rather than 4.
>      temp := ShallowCopy(ListTransformation(trans)) + 1;
>      out  := [1];
>      Append(out, temp);
>      return Transformation(out);
>    end;;
gap> H[3][2] := SemigroupHomomorphismByFunction(L[5], L[3], m1);;
gap> c := SemigroupCongruence(L[3], [[Transformation([1, 2, 4, 3]),
>                                     Transformation([1, 3, 2, 4])]]);;
gap> # this congruence separates T_4 into three sets:
gap> # A_4;
gap> # S_4 \ A_4;
gap> # all transformations with size of image < 4.
gap> c1 := EquivalenceClassOfElement(c,
>                                   Transformation([1, 2, 3, 1]));;
gap> c2 := EquivalenceClassOfElement(c,
>                                   IdentityTransformation);;
gap> c3 := EquivalenceClassOfElement(c,
>                                   Transformation([2, 3, 4, 1]));;
gap> m1 := function(trans)
>      if trans in c1 then
>        return Transformation([1, 1, 1]);
>      elif trans in c2 then
>        return Transformation([1, 2, 3]);
>      elif trans in c3 then
>        return Transformation([1, 3, 2]);
>      else
>        return fail;
>      fi;
>    end;;
gap> H[1][2] := SemigroupHomomorphismByFunction(L[3], L[1], m1);;
gap> m1 := function(trans)
>      if Length(ImageSetOfTransformation(trans, 3)) < 3 then
>        return MultiplicativeZero(L[2]);
>      else
>        return PermutationOfImage(trans) ^ UnderlyingInjectionZeroMagma(L[2]);
>      fi;
>    end;;
gap> H[2][1] := SemigroupHomomorphismByFunction(L[5], L[2], m1);;
gap> m1 := function(magelem)
>      local p;
>      p := magelem ^ InverseGeneralMapping(UnderlyingInjectionZeroMagma(L[2]));
>      if p = fail then
>        return Transformation([1, 1, 1]);
>      elif p in AlternatingGroup(3) then
>        return Transformation([1, 2, 3]);
>      else
>        return Transformation([1, 3, 2]);
>      fi;
>    end;;
gap> H[1][1] := SemigroupHomomorphismByFunction(L[2], L[1], m1);;
gap> H[3][1] := SemigroupHomomorphismByFunction(L[4], L[3], IdFunc);
<transformation semigroup of size 4, degree 4 with 4 generators> -> 
<full transformation monoid of degree 4>
gap> D := Digraph([[2, 3], [5], [4, 5], [], []]);;
gap> S := StrongSemilatticeOfSemigroups(D, L, H);
<strong semilattice of 5 semigroups>
gap> IsStrongSemilatticeOfSemigroups(S);
true
gap> Size(S);
297
gap> Length(GreensDClasses(S));
12
gap> SSSE(S, 2, (1, 2) ^ UnderlyingInjectionZeroMagma(L[2]))
>    * SSSE(S, 3, Transformation([1, 4, 3, 2]))
>    = SSSE(S, 1, IdentityTransformation);
true
gap> SSSE(S, 4, Transformation([4, 4, 4, 4]))
>    * SSSE(S, 5, Transformation([3, 3, 2]))
>    = SSSE(S, 3, Transformation([3, 3, 3, 3]));
true
gap> MultiplicativeZero(S) = SSSE(S, 1, Transformation([1, 1, 1]));
true
gap> MultiplicativeNeutralElement(S);
fail

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semigroups/semicons.tst");
