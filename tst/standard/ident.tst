############################################################################
##
#W  standard/ident.tst
#Y  Copyright (C) 2017                                 Fernando Flores Brito
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/ident.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();;

#T# Test the VerifyIdentity function
#T# Symmetric Group
gap> S3 := SymmetricGroup(3);;
gap> VerifyIdentity(S3, [[1, 1, 2, 1], [1, 1, 2, 1, 1, 1, 1, 1, 1, 1]]);
true

#T# Full Transformation Monoid
gap> T3 := FullTransformationMonoid(3);;
gap> VerifyIdentity(T3, [[1, 1, 1, 1], [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]]);
true
gap> VerifyIdentity(T3, [[1, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2],
>              [1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2]]);
true

#T# Set of Idempotents
gap> S := Semigroup(Transformation([2, 2, 4, 1]),
>                   Transformation([3, 3, 1, 1]));;
gap> I := AsSet(Idempotents(S, 2));;
gap> VerifyIdentity(I, [[1, 1], [1]]);
true

#T# Rectangular Band
gap> RB := RectangularBand(5, 6);;
gap> VerifyIdentity(RB, [[1, 2, 3], [1, 3]]);
true
gap> VerifyIdentity(RB, [[1, 1], [1]]);
true

#T# Left Zero Semigroup
gap> LZ := LeftZeroSemigroup(5);;
gap> VerifyIdentity(LZ, [[1, 2], [1]]);
true

#T# Right Zero Semigroup
gap> RZ := RightZeroSemigroup(5);;
gap> VerifyIdentity(RZ, [[1, 2], [2]]);
true

#T# Test NrLettersIdentity
gap> NrLettersIdentity([[1, 2], [2,1]]);
2
gap> NrLettersIdentity([[1, 2], [2, 1, 3]]);
3

#T# Test ReverseIdentity
gap> ReverseIdentity([[1, 2], [2, 1, 3]]);
[ [ 2, 1 ], [ 3, 1, 2 ] ]

#T# Test GroupAlgebraProduct
gap> out := EmptyPlist(3 ^ 9);;
gap> nr := 0;;
gap> iter1 := IteratorOfTuples([1..3], 3);;
gap> for tup1 in iter1 do
>   iter2 := IteratorOfTuples([1..3], 3);
>   for tup2 in iter2 do
>     iter3 := IteratorOfTuples([1..3], 3);
>     for tup3 in iter3 do
>       nr := nr + 1;
>       out[nr] := [tup1, tup2, tup3];
>     od;
>   od;
> od;
gap> x := [1, 0, 1];;
gap> y := [1, 1, 0];;
gap> GroupAlgebraProduct(out[1729], x, y);
[ 0, 0, 0 ]
gap> GroupAlgebraProduct(out[19000], x, y);
[ 1, 0, 1 ]

#T# Test RandomAssociativityTest
gap> f := Collected(List(out, x -> RandomAssociativityTest(x)));;
gap> f[2][2] < 19570;
false

#T# Test RandomTable
gap> g := RandomTable(3);;
gap> IsHomogeneousList(g);
true
gap> Size(g) = 3;
true
gap> IsPosInt(g[1][1]);
true

#T# Test RandomTuple
gap> t := RandomTuple(5);;
gap> IsHomogeneousList(t);
true
gap> Length(t) = 5;
true
gap> IsPosInt(t[1]);
true

#
gap> STOP_TEST("Semigroups package: standard/ident.tst");
