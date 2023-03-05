#############################################################################
##
#W  standard/elements/elements.tst
#Y  Copyright (C) 2016-2022                                 Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local S, x
gap> START_TEST("Semigroups package: standard/elements/elements.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# elements: SEMIGROUPS.IndexPeriodByRank
gap> x := Transformation(
> [71, 14, 60, 68, 67, 74, 61, 19, 81, 10, 17, 21, 9, 49, 78,
>  73, 81, 22, 59, 50, 29, 31, 6, 27, 40, 21, 75, 44, 71, 62, 22, 56, 90, 53,
>  64, 53, 78, 73, 18, 96, 58, 46, 11, 19, 72, 12, 33, 83, 21, 34, 94, 94, 51,
>  6, 52, 24, 66, 46, 61, 95, 40, 81, 41, 38, 26, 39, 90, 93, 30, 37, 55, 51,
>  99, 38, 38, 79, 33, 58, 90, 89, 98, 54, 18, 58, 28, 27, 61, 82, 96, 26, 89,
>  93, 44, 4, 96, 11, 39, 14, 57, 34]);
<transformation on 100 pts with rank 64>
gap> SEMIGROUPS.IndexPeriodByRank(x, RankOfTransformation);
[ 12, 42 ]
gap> x := Transformation([1, 1, 2, 3, 4, 5, 6, 7, 8]);;
gap> SEMIGROUPS.IndexPeriodByRank(x, RankOfTransformation);
[ 8, 1 ]
gap> x := Bipartition([[1], [2, -5], [3, -4], [4, -3], [5, -2], [-1]]);
<bipartition: [ 1 ], [ 2, -5 ], [ 3, -4 ], [ 4, -3 ], [ 5, -2 ], [ -1 ]>
gap> SEMIGROUPS.IndexPeriodByRank(x, RankOfBipartition);
[ 1, 2 ]
gap> x := Bipartition([[1, 2, -1], [3, -2], [4, -3], [5, -4], [6, -5],
> [7, -6], [8, -7], [9, -8], [-9]]);;
gap> SEMIGROUPS.IndexPeriodByRank(x, RankOfBipartition);
[ 8, 1 ]
gap> x := Bipartition([[1, -2], [2, -1]]);;
gap> SEMIGROUPS.IndexPeriodByRank(x, RankOfBipartition);
[ 1, 2 ]

# elements: IndexPeriodOfSemigroupElement
gap> x := PBR([[-3, -2, -1, 1, 2, 3], [-3, -2, -1, 1, 3], [-3, -2, -1, 1, 2]],
> [[-3, -2, -1, 1, 2], [-3, 2, 3], [-2, -1, 2, 3]]);;
gap> IndexPeriodOfSemigroupElement(x);
[ 2, 1 ]
gap> IsIdempotent(x);
false
gap> IndexPeriodOfSemigroupElement(GeneratorsOfMagma(FreeMagma(1))[1]);
Error, the argument (a mult. elt.) is not the generator of a semigroup
gap> x := Matrix(IsBooleanMat, [[0, 1], [1, 0]]);;
gap> IndexPeriodOfSemigroupElement(x);
[ 1, 2 ]
gap> x;;
gap> IndexPeriodOfSemigroupElement(x);
[ 1, 2 ]
gap> x := Matrix(IsBooleanMat,
> [[0, 0, 0, 1, 0, 0, 0],
>  [0, 0, 0, 1, 0, 0, 0],
>  [0, 0, 0, 1, 0, 0, 0],
>  [0, 0, 0, 0, 0, 1, 0],
>  [0, 0, 0, 0, 1, 0, 0],
>  [0, 1, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 1, 0]]);;
gap> IndexPeriodOfSemigroupElement(x);
[ 1, 3 ]

# elements: SmallestIdempotentPower
gap> x := PBR([[-3, -2, -1, 1, 2, 3], [-3, -2, -1, 1, 3], [-3, -2, -1, 1, 2]],
> [[-3, -2, -1, 1, 2], [-3, 2, 3], [-2, -1, 2, 3]]);;
gap> SmallestIdempotentPower(x);
2
gap> First([1 .. 100], i -> IsIdempotent(x ^ i)) = last;
true
gap> x := PBR([[-2, 2], [-1]], [[1], [1, 2]]);;
gap> SmallestIdempotentPower(x);
4
gap> First([1 .. 100], i -> IsIdempotent(x ^ i)) = last;
true
gap> x := PBR([[-2], [-1]], [[1], [1, 2]]);;
gap> SmallestIdempotentPower(x);
2
gap> First([1 .. 100], i -> IsIdempotent(x ^ i)) = last;
true
gap> x := Transformation(
> [71, 14, 60, 68, 67, 74, 61, 19, 81, 10, 17, 21, 9, 49, 78,
>  73, 81, 22, 59, 50, 29, 31, 6, 27, 40, 21, 75, 44, 71, 62, 22, 56, 90, 53,
>  64, 53, 78, 73, 18, 96, 58, 46, 11, 19, 72, 12, 33, 83, 21, 34, 94, 94, 51,
>  6, 52, 24, 66, 46, 61, 95, 40, 81, 41, 38, 26, 39, 90, 93, 30, 37, 55, 51,
>  99, 38, 38, 79, 33, 58, 90, 89, 98, 54, 18, 58, 28, 27, 61, 82, 96, 26, 89,
>  93, 44, 4, 96, 11, 39, 14, 57, 34]);;
gap> SmallestIdempotentPower(x);
42
gap> First([1 .. 100], i -> IsIdempotent(x ^ i)) = last;
true
gap> x := Transformation([2, 6, 4, 5, 3, 7, 8, 8]);;
gap> SmallestIdempotentPower(x);
6
gap> First([1 .. 100], i -> IsIdempotent(x ^ i)) = last;
true
gap> SmallestIdempotentPower(GeneratorsOfMagma(FreeMagma(1))[1]);
Error, the argument (a mult. elt.) is not the generator of a semigroup

# elements: IsMultiplicativeZero
gap> S := SymmetricInverseMonoid(3);;
gap> x := MultiplicativeZero(S);
<empty partial perm>
gap> IsMultiplicativeZero(S, x);
true
gap> IsMultiplicativeZero(S, PartialPerm([1]));
false

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/elements/elements.tst");
