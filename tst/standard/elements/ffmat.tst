#############################################################################
##
#W  standard/elements/ffmat.tst
#Y  Copyright (C) 2015-2022                                Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local M, S, coll, ht, mat, mat2, r1, r2, rs, x, y, z
gap> START_TEST("Semigroups package: standard/elements/ffmat.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# MatrixTest1: Create
gap> M := Matrix(GF(2),
> Z(2) * [[1, 0, 1, 0], [0, 1, 0, 1], [0, 1, 0, 0], [0, 0, 0, 1]]);
<a 4x4 matrix over GF2>

# MatrixTest2: EvalPrintString
gap> EvalString(String(M)) = M;
true

# MatrixTest3: IndexPeriodOfSemigroupElement
gap> M := Matrix(GF(3),
> [[Z(3) ^ 0, 0 * Z(3), 0 * Z(3), 0 * Z(3), 0 * Z(3)],
>  [Z(3) ^ 0, 0 * Z(3), 0 * Z(3), 0 * Z(3), 0 * Z(3)],
>  [0 * Z(3), Z(3), 0 * Z(3), 0 * Z(3), 0 * Z(3)],
>  [0 * Z(3), 0 * Z(3), Z(3), 0 * Z(3), 0 * Z(3)],
>  [0 * Z(3), 0 * Z(3), 0 * Z(3), 0 * Z(3), 0 * Z(3)]]);;
gap> IndexPeriodOfSemigroupElement(M);
[ 3, 1 ]
gap> M := Matrix(GF(2 ^ 2),
> [[Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [Z(2 ^ 2), 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), Z(2) ^ 0, 0 * Z(2)]]);;
gap> IndexPeriodOfSemigroupElement(M);
[ 3, 3 ]

# MatrixTest4: SmallestIdempotentPower
gap> M := Matrix(GF(5),
> [[Z(5) ^ 3, 0 * Z(5), Z(5) ^ 0],
>  [Z(5) ^ 3, 0 * Z(5), 0 * Z(5)],
>  [0 * Z(5), 0 * Z(5), 0 * Z(5)]]);;
gap> SmallestIdempotentPower(M);
4
gap> M := Matrix(GF(2 ^ 2),
> [[Z(2 ^ 2), 0 * Z(2), Z(2 ^ 2) ^ 2, Z(2 ^ 2) ^ 2],
>  [Z(2 ^ 2) ^ 2, 0 * Z(2), 0 * Z(2), Z(2) ^ 0],
>  [Z(2 ^ 2) ^ 2, 0 * Z(2), 0 * Z(2), 0 * Z(2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2), 0 * Z(2)]]);;
gap> SmallestIdempotentPower(M);
15

# Test ELM_LIST and IsBound
gap> mat := Matrix(GF(3), Z(3) * [[1, 0, 0], [0, 1, 0], [0, 0, 1]]);;
gap> mat[1];
[ Z(3), 0*Z(3), 0*Z(3) ]
gap> mat[4];
Error, row index 4 exceeds 3, the number of rows

# Test \*
gap> x := Matrix(GF(3), Z(3) * [[1, 2], [1, 2]]);;
gap> y := Matrix(GF(3), Z(3) * [[0, 0], [0, 0]]);;
gap> x * y;
[ [ 0*Z(3), 0*Z(3) ], [ 0*Z(3), 0*Z(3) ] ]
gap> y * x;
[ [ 0*Z(3), 0*Z(3) ], [ 0*Z(3), 0*Z(3) ] ]

# Test OneImmutable
gap> x := Matrix(GF(3), Z(3) * [[1, 2], [1, 2]]);;
gap> y := OneImmutable(x);
[ [ Z(3)^0, 0*Z(3) ], [ 0*Z(3), Z(3)^0 ] ]
gap> x * y = x;
true
gap> y * x = x;
true

# Test RandomMatrix
gap> x := RandomMatrix(GF(8), 1);;
gap> NrRows(x);
1
gap> x := RandomMatrix(GF(8), 10);;
gap> NrRows(x);
10
gap> x := RandomMatrix(GF(3), 3, [3]);;
gap> Rank(x);
3
gap> x := RandomMatrix(GF(3), 10, [3 .. 8]);;
gap> Rank(x) in [3 .. 8];
true
gap> x := RandomMatrix(GF(3), 5, [0]);
[ [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ] ]
gap> x := RandomMatrix(GF(3), 5, [1, -2, 1314]);
Error, the list of ranks has to consist of numbers > 0 and < n

# Test \ = , \< for row basis
gap> mat := Matrix(GF(3 ^ 2),
> [[Z(3 ^ 2) ^ 7, Z(3 ^ 2) ^ 3, Z(3 ^ 2) ^ 5], [Z(3 ^ 2) ^ 3, Z(3),
>   Z(3 ^ 2) ^ 2], [Z(3 ^ 2) ^ 7, Z(3 ^ 2) ^ 7, Z(3 ^ 2)]]);;
gap> r1 := RowSpaceBasis(mat);
<rowbasis of rank 3 over GF(3^2)>
gap> r2 := RowSpaceBasis(TransposedMat(mat));
<rowbasis of rank 3 over GF(3^2)>
gap> r1 = r2;
true
gap> r2 < r1;
false
gap> mat := Matrix(GF(3 ^ 2),
> [[Z(3 ^ 2) ^ 7, Z(3 ^ 2) ^ 7, Z(3 ^ 2) ^ 6], [Z(3 ^ 2), Z(3 ^ 2) ^ 2,
>   Z(3) ^ 0], [Z(3 ^ 2) ^ 6, Z(3 ^ 2) ^ 7, Z(3 ^ 2) ^ 5]]);;
gap> RowSpaceBasis(TransposedMat(mat)) = RowSpaceBasis(mat);
false
gap> RowSpaceBasis(TransposedMat(mat)) < RowSpaceBasis(mat);
true
gap> RowSpaceBasis(TransposedMat(mat)) > RowSpaceBasis(mat);
false

# Test printing viewing etc for row basis
gap> mat := Matrix(GF(3 ^ 2),
> [[Z(3 ^ 2) ^ 7, Z(3 ^ 2) ^ 3, Z(3 ^ 2) ^ 5], [Z(3 ^ 2) ^ 3, Z(3),
>   Z(3 ^ 2) ^ 2], [Z(3 ^ 2) ^ 7, Z(3 ^ 2) ^ 7, Z(3 ^ 2)]]);;
gap> rs := RowSpaceBasis(mat);;
gap> EvalString(String(rs)) = rs;
true

# Test RightInverse and LeftInverse
gap> mat := Matrix(GF(2 ^ 2),
> [[Z(2 ^ 2), Z(2) ^ 0, Z(2 ^ 2) ^ 2, Z(2) ^ 0, Z(2 ^ 2) ^ 2],
>  [Z(2 ^ 2) ^ 2, Z(2 ^ 2), Z(2 ^ 2) ^ 2, Z(2 ^ 2) ^ 2, Z(2) ^ 0],
>  [Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0, 0 * Z(2), 0 * Z(2)],
>  [Z(2 ^ 2) ^ 2, Z(2 ^ 2) ^ 2, Z(2) ^ 0, Z(2) ^ 0, 0 * Z(2)],
>  [Z(2) ^ 0, 0 * Z(2), Z(2 ^ 2), 0 * Z(2), Z(2 ^ 2) ^ 2]]);;
gap> RightInverse(mat);
[ [ 0*Z(2), 0*Z(2), Z(2^2)^2, 0*Z(2), 0*Z(2) ], 
  [ Z(2^2)^2, Z(2^2), Z(2)^0, Z(2^2), 0*Z(2) ], 
  [ 0*Z(2), 0*Z(2), Z(2^2), 0*Z(2), 0*Z(2) ], 
  [ Z(2^2), Z(2)^0, Z(2^2)^2, 0*Z(2), 0*Z(2) ], 
  [ 0*Z(2), Z(2)^0, Z(2^2)^2, Z(2^2)^2, 0*Z(2) ] ]
gap> LeftInverse(mat);
[ [ 0*Z(2), Z(2^2), Z(2^2), Z(2)^0, Z(2)^0 ], 
  [ Z(2^2)^2, Z(2^2), Z(2)^0, Z(2^2), 0*Z(2) ], 
  [ 0*Z(2), Z(2^2), Z(2^2)^2, Z(2)^0, Z(2)^0 ], 
  [ Z(2^2), Z(2^2), Z(2)^0, Z(2^2), Z(2^2) ], 
  [ 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2) ] ]
gap> Inverse(mat);
fail

# Test Inverse
gap> mat := Matrix(GF(2 ^ 2), [[Z(2 ^ 2) ^ 2]]);;
gap> Inverse(mat);
[ [ Z(2^2) ] ]
gap> mat := Matrix(GF(2 ^ 2),
> [[Z(2 ^ 2), Z(2 ^ 2) ^ 2, Z(2 ^ 2)], [Z(2) ^ 0, Z(2 ^ 2), Z(2 ^ 2)],
>  [Z(2) ^ 0, Z(2 ^ 2) ^ 2, Z(2 ^ 2)]]);;
gap> Inverse(mat);
[ [ Z(2^2), 0*Z(2), Z(2^2) ], [ 0*Z(2), Z(2)^0, Z(2)^0 ], 
  [ Z(2)^0, Z(2^2), 0*Z(2) ] ]

# Test ComputeRowSpaceAndTransformation
gap> ComputeRowSpaceAndTransformation(2);
Error, Assertion failure
gap> ComputeRowSpaceAndTransformation(Matrix(GF(9), [[Z(9)]]));
[ <rowbasis of rank 1 over GF(3^2)>, [ [ Z(3^2)^7 ] ], [ [ Z(3^2) ] ] ]

# Test Matrix, checker, 1/1
gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5) ^ 2, Z(5) ^ 0]]);
[ [ 0*Z(5), Z(5)^3 ], [ Z(5)^2, Z(5)^0 ] ]

# Test One, 1/1
gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5) ^ 2, Z(5) ^ 0]]);
[ [ 0*Z(5), Z(5)^3 ], [ Z(5)^2, Z(5)^0 ] ]
gap> One(mat);
[ [ Z(5)^0, 0*Z(5) ], [ 0*Z(5), Z(5)^0 ] ]

# Test RandomMatrix, 1/1
gap> mat := RandomMatrix(GF(5), 10);
< mutable compressed matrix 10x10 over GF(5) >

# Test \*, works, 1/3
gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5), 0 * Z(5)]]);
[ [ 0*Z(5), Z(5)^3 ], [ Z(5), 0*Z(5) ] ]
gap> mat ^ 2;
[ [ Z(5)^0, 0*Z(5) ], [ 0*Z(5), Z(5)^0 ] ]

# Test \*, wrong base field, 3/3
gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5), 0 * Z(5)]]);
[ [ 0*Z(5), Z(5)^3 ], [ Z(5), 0*Z(5) ] ]
gap> mat2 := Matrix(GF(7), [[0 * Z(7), Z(7) ^ 3], [Z(7), 0 * Z(7)]]);
[ [ 0*Z(7), Z(7)^3 ], [ Z(7), 0*Z(7) ] ]
gap> mat * mat2;
Error, <x> and <y> have different characteristic

# Test Display, 1/1
gap> mat := Matrix(GF(11), [[Z(11) ^ 9, 0 * Z(11), Z(11), Z(11) ^ 9, 0 * Z(11)],
>   [Z(11) ^ 3, Z(11) ^ 4, 0 * Z(11), Z(11) ^ 2, Z(11) ^ 7],
>   [Z(11) ^ 9, Z(11) ^ 3, Z(11) ^ 5, Z(11) ^ 4, Z(11) ^ 4],
>   [Z(11) ^ 6, Z(11), Z(11) ^ 7, Z(11) ^ 3, Z(11) ^ 5],
>   [0 * Z(11), Z(11) ^ 8, Z(11) ^ 3, Z(11) ^ 6, Z(11) ^ 0]]);
[ [ Z(11)^9, 0*Z(11), Z(11), Z(11)^9, 0*Z(11) ], 
  [ Z(11)^3, Z(11)^4, 0*Z(11), Z(11)^2, Z(11)^7 ], 
  [ Z(11)^9, Z(11)^3, Z(11)^5, Z(11)^4, Z(11)^4 ], 
  [ Z(11)^6, Z(11), Z(11)^7, Z(11)^3, Z(11)^5 ], 
  [ 0*Z(11), Z(11)^8, Z(11)^3, Z(11)^6, Z(11)^0 ] ]
gap> Display(mat);
  6  .  2  6  .
  8  5  .  4  7
  6  8 10  5  5
  9  2  7  8 10
  .  3  8  9  1

# Test RandomMatrix for finite field, dim, rank
gap> x := RandomMatrix(GF(3), 3, 1);;
gap> Rank(x);
1
gap> Rank(TransposedMat(x));
1

# Test MatrixNC
gap> x := Matrix(GF(3), [[Z(3) ^ 0, 0 * Z(3), Z(3)], [Z(3), 0 * Z(3), Z(3)],
> [Z(3) ^ 0, Z(3) ^ 0, Z(3)]]);;
gap> y := Matrix(GF(3), [[0 * Z(3), Z(3), Z(3) ^ 0], [0 * Z(3), Z(3) ^ 0, 0 * Z(3)],
>  [Z(3), 0 * Z(3), Z(3)]]);;
gap> z := Matrix(Unpack(x), y);
[ [ Z(3)^0, 0*Z(3), Z(3) ], [ Z(3), 0*Z(3), Z(3) ], [ Z(3)^0, Z(3)^0, Z(3) ] ]
gap> z = x;
true

# Test RandomMatrix
gap> RandomMatrix(GF(11), 0, []);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RandomMatrixOp' on 3 arguments
gap> RandomMatrix(GF(11), 0, [0]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RandomMatrixOp' on 3 arguments
gap> RandomMatrix(GF(11), 0, [1]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RandomMatrixOp' on 3 arguments

# Test AsList for IsPlistMatrixRep 
gap> S := Semigroup(Transformation([2, 3, 1]));;
gap> IsGroupAsSemigroup(S);
true
gap> IsGroupAsSemigroup(AsSemigroup(IsMatrixOverFiniteFieldSemigroup, S));
true

# Test BaseDomain (error)
gap> coll := [Matrix(GF(3), [[Z(3), Z(3), 0 * Z(3)],
>                            [Z(3), Z(3) ^ 0, Z(3)],
>                            [0 * Z(3), 0 * Z(3), Z(3)]]),
>             Matrix(GF(2 ^ 2), [[Z(2 ^ 2) ^ 2, Z(2 ^ 2) ^ 2, Z(2) ^ 0],
>                              [Z(2) ^ 0, 0 * Z(2), Z(2) ^ 0],
>                              [0 * Z(2), 0 * Z(2), Z(2 ^ 2) ^ 2]])];;
gap> BaseDomain(coll);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `BaseDomain' on 1 arguments

# Test OneMutable
gap> x := Matrix(GF(3), [[Z(3) ^ 0, 0 * Z(3), Z(3)], [Z(3), 0 * Z(3), Z(3)],
> [Z(3) ^ 0, Z(3) ^ 0, Z(3)]]);;
gap> OneMutable(x);
[ [ Z(3)^0, 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0, 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3)^0 ] ]

# Test \< 
gap> x := Matrix(GF(3), [[Z(3) ^ 0, 0 * Z(3), Z(3)], [Z(3), 0 * Z(3), Z(3)],
> [Z(3) ^ 0, Z(3) ^ 0, Z(3)]]);;
gap> y := Matrix(GF(3), [[0 * Z(3), Z(3), Z(3) ^ 0], [0 * Z(3), Z(3) ^ 0, 0 * Z(3)],
>  [Z(3), 0 * Z(3), Z(3)]]);;
gap> x < y;
false
gap> y < x;
true
gap> x < x;
false

# Test TransposedMat
gap> x := Matrix(GF(2 ^ 2),
> [[0 * Z(2), Z(2) ^ 0, 0 * Z(2)], [Z(2) ^ 0, Z(2 ^ 2) ^ 2, Z(2 ^ 2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2)]]);;
gap> TransposedMat(x);
[ [ 0*Z(2), Z(2)^0, 0*Z(2) ], [ Z(2)^0, Z(2^2)^2, 0*Z(2) ], 
  [ 0*Z(2), Z(2^2), 0*Z(2) ] ]
gap> TransposedMat(TransposedMat(x)) = x;
true

# Test hashing (for finite field row basis)
gap> x := Matrix(GF(2 ^ 2),
> [[0 * Z(2), Z(2) ^ 0, 0 * Z(2)], [Z(2) ^ 0, Z(2 ^ 2) ^ 2, Z(2 ^ 2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2)]]);;
gap> ht := HTCreate(RowSpaceBasis(x));;
gap> HTAdd(ht, RowSpaceBasis(x), true);;
gap> HTAdd(ht, RowSpaceBasis(Matrix(GF(11), [[Z(11)]])), true);;
gap> ht := HTCreate(RowSpaceBasis(Matrix(GF(11), [[Z(11)]])));;
gap> HTAdd(ht, RowSpaceBasis(Matrix(GF(11), [[Z(11)]])), true);;
gap> HTAdd(ht, RowSpaceBasis(Matrix(GF(7), [[Z(7)]])), true);;
gap> HTAdd(ht, RowSpaceBasis(x), true);;
gap> HTAdd(ht, RowSpaceBasis(ZeroMatrix(GF(4), 2, 2)), true);;
gap> ht := HTCreate(RowSpaceBasis(ZeroMatrix(GF(4), 2, 2)));;
gap> HTAdd(ht, RowSpaceBasis(ZeroMatrix(GF(4), 2, 2)), true);;

# TraceMat
gap> x := Matrix(GF(2 ^ 2),
> [[0 * Z(2), Z(2) ^ 0, 0 * Z(2)], 
>  [Z(2) ^ 0, Z(2 ^ 2) ^ 2, Z(2 ^ 2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2)]]);;
gap> TraceMat(x);
Z(2^2)^2

# Finite field matrix attributes TryNextMethod
gap> x := Matrix(Integers, [[-1, -2, 0], [0, 3, -1], [1, 0, -3]]);
<3x3-matrix over Integers>
gap> RowSpaceBasis(x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `RowSpaceBasis' on 1 arguments
gap> RowSpaceTransformation(x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `RowSpaceTransformation' on 1 arguments
gap> RightInverse(x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `RightInverse' on 1 arguments
gap> LeftInverse(x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `LeftInverse' on 1 arguments
gap> RowSpaceTransformationInv(x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `RowSpaceTransformationInv' on 1 argumen\
ts
gap> x := Matrix(GF(2 ^ 2),
> [[0 * Z(2), Z(2) ^ 0, 0 * Z(2)], [Z(2) ^ 0, Z(2 ^ 2) ^ 2, Z(2 ^ 2)],
>  [0 * Z(2), 0 * Z(2), 0 * Z(2)]]);;
gap> RowSpaceTransformation(x);
[ [ Z(2^2)^2, Z(2)^0, 0*Z(2) ], [ Z(2)^0, 0*Z(2), 0*Z(2) ], 
  [ 0*Z(2), 0*Z(2), Z(2)^0 ] ]
gap> RowSpaceTransformationInv(x);
[ [ 0*Z(2), Z(2)^0, 0*Z(2) ], [ Z(2)^0, Z(2^2)^2, 0*Z(2) ], 
  [ 0*Z(2), 0*Z(2), Z(2)^0 ] ]

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/elements/ffmat.tst");
