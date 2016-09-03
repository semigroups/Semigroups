#############################################################################
##
#W  standard/ffmat.tst
#Y  Copyright (C) 2015                                     Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/ffmat.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# MatrixTest1: Create
gap> M := NewMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep, GF(2),
> Z(2) * [[1, 0, 1, 0], [0, 1, 0, 1], [0, 1, 0, 0], [0, 0, 0, 1]]);
Matrix(GF(2), [[Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2)], 
  [0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0], [0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2)], 
  [0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0]])

#T# MatrixTest2: EvalPrintString
gap> EvalString(String(M)) = M;
true

#T# MatrixTest3: IndexPeriodOfSemigroupElement
gap> M := Matrix(GF(3),
> [[Z(3)^0, 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3)],
>  [Z(3)^0, 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3)],
>  [0*Z(3), Z(3), 0*Z(3), 0*Z(3), 0*Z(3)],
>  [0*Z(3), 0*Z(3), Z(3), 0*Z(3), 0*Z(3)],
>  [0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3)]]);;
gap> IndexPeriodOfSemigroupElement(M);
[ 3, 1 ]
gap> M := Matrix(GF(2^2),
> [[Z(2^2), 0*Z(2), 0*Z(2), 0*Z(2)],
>  [Z(2^2), 0*Z(2), 0*Z(2), 0*Z(2)],
>  [0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2)],
>  [0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2)]]);;
gap> IndexPeriodOfSemigroupElement(M);
[ 3, 3 ]

#T# MatrixTest4: SmallestIdempotentPower
gap> M := Matrix(GF(5),
> [[Z(5)^3, 0*Z(5), Z(5)^0],
>  [Z(5)^3, 0*Z(5), 0*Z(5)],
>  [0*Z(5), 0*Z(5), 0*Z(5)]]);;
gap> SmallestIdempotentPower(M);
4
gap> M := Matrix(GF(2^2),
> [[Z(2^2), 0*Z(2), Z(2^2)^2, Z(2^2)^2],
>  [Z(2^2)^2, 0*Z(2), 0*Z(2), Z(2)^0],
>  [Z(2^2)^2, 0*Z(2), 0*Z(2), 0*Z(2)],
>  [0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2)]]);;
gap> SmallestIdempotentPower(M);
15

# Test SEMIGROUPS_TypeViewStringOfMatrixOverSemiring
gap> mat := Matrix(GF(3), []);;
gap> SEMIGROUPS_TypeViewStringOfMatrixOverSemiring(mat);
"finite field"
gap> mat := NewIdentityMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
> GF(2), 10);
<10x10 finite field matrix>

# Test SEMIGROUPS_FilterOfMatrixOverSemiring
gap> mat := Matrix(GF(3), []);;
gap> SEMIGROUPS_FilterOfMatrixOverSemiring(mat);
<Representation "IsPlistMatrixOverFiniteFieldRep">

# Test SEMIGROUPS_TypeOfMatrixOverSemiringCons
gap> SEMIGROUPS_TypeOfMatrixOverSemiringCons(IsPlistMatrixOverFiniteFieldRep);;

# Test ELM_LIST and IsBound
gap> mat := Matrix(GF(3), []);;
gap> mat[1];
Error, Semigroups: ELM_LIST (for a plist matrix over finitefield):
the position is greater than the dimension of the matrix,
gap> mat := Matrix(GF(3), Z(3) * [[1, 0, 0], [0, 1, 0], [0, 0, 1]]);;
gap> mat[1];
[ Z(3), 0*Z(3), 0*Z(3) ]
gap> mat[4];
Error, Semigroups: ELM_LIST (for a plist matrix over finitefield):
the position is greater than the dimension of the matrix,

# Test MatrixNC for a sample
gap> mat := Matrix(GF(3), Z(3) * [ [1, 2], [1, 2]]);;
gap> MatrixNC(mat, Z(3) * [ [0, 0], [0, 0]]);
Matrix(GF(3), [[0*Z(3), 0*Z(3)], [0*Z(3), 0*Z(3)]])
gap> MatrixNC(mat, Z(3) * [ [0, 0, 0], [0, 0, 0], [0, 0, 0]]);
Matrix(GF(3), [[0*Z(3), 0*Z(3), 0*Z(3)], [0*Z(3), 0*Z(3), 0*Z(3)], 
  [0*Z(3), 0*Z(3), 0*Z(3)]])

# Test \* 
gap> x := Matrix(GF(3), Z(3) * [ [1, 2], [1, 2]]);;
gap> y := Matrix(GF(3), Z(3) * [ [0, 0], [0, 0]]);;
gap> x * y;
Matrix(GF(3), [[0*Z(3), 0*Z(3)], [0*Z(3), 0*Z(3)]])
gap> y * x;
Matrix(GF(3), [[0*Z(3), 0*Z(3)], [0*Z(3), 0*Z(3)]])

# Test OneImmutable
gap> x := Matrix(GF(3), Z(3) * [ [1, 2], [1, 2]]);;
gap> y := OneImmutable(x);
Matrix(GF(3), [[Z(3)^0, 0*Z(3)], [0*Z(3), Z(3)^0]])
gap> x * y = x;
true
gap> y * x = x;
true
gap> x := Matrix(GF(3), []);;
gap> y := OneImmutable(x);
Matrix(GF(3), [])
gap> x * y = x;
true
gap> y * x = x;
true

# Test RandomMatrix
gap> x := RandomMatrix(GF(8), 1);;
gap> IsMatrixOverFiniteField(x);
true
gap> DimensionOfMatrixOverSemiring(x);
1
gap> x := RandomMatrix(GF(8), 10);;
gap> DimensionOfMatrixOverSemiring(x);
10
gap> x := RandomMatrix(GF(3), 3, [3]);;
gap> RowRank(x);
3
gap> x := RandomMatrix(GF(3), 10, [3 .. 8]);;
gap> RowRank(x) in [3 .. 8];
true
gap> x := RandomMatrix(GF(3), 5, [0]);
Matrix(GF(3), [[0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3)], 
  [0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3)], 
  [0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3)], 
  [0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3)], 
  [0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3), 0*Z(3)]])
gap> x := RandomMatrix(GF(3), 5, [1, -2, 1314]);
Error, Semigroups: RandomMatrixOp: usage,
the list of ranks has to consist of numbers > 0 and < n,

# Test \=, \< for row basis
gap> mat := Matrix(GF(3^2), [[Z(3^2)^7, Z(3^2)^3, Z(3^2)^5], [Z(3^2)^3, Z(3),
> Z(3^2)^2], [Z(3^2)^7, Z(3^2)^7, Z(3^2)]]);;
gap> r1 := RowSpaceBasis(mat);
<rowbasis of rank 3 over GF(3^2)>
gap> r2 := RowSpaceBasis(TransposedMat(mat));
<rowbasis of rank 3 over GF(3^2)>
gap> r1 = r2;
true
gap> r2 < r1;
false
gap> mat := Matrix(GF(3^2), [[Z(3^2)^7, Z(3^2)^7, Z(3^2)^6], [Z(3^2), Z(3^2)^2,
> Z(3)^0], [Z(3^2)^6, Z(3^2)^7, Z(3^2)^5]]);;
gap> RowSpaceBasis(TransposedMat(mat)) = RowSpaceBasis(mat);
false
gap> RowSpaceBasis(TransposedMat(mat)) < RowSpaceBasis(mat);
true
gap> RowSpaceBasis(TransposedMat(mat)) > RowSpaceBasis(mat);
false

# Test printing viewing etc for row basis
gap> mat := Matrix(GF(3^2), [[Z(3^2)^7, Z(3^2)^3, Z(3^2)^5], [Z(3^2)^3, Z(3),
> Z(3^2)^2], [Z(3^2)^7, Z(3^2)^7, Z(3^2)]]);;
gap> rs := RowSpaceBasis(mat);;
gap> EvalString(String(rs)) = rs;
true

# Test NewIdentity/ZeroMatrixOverFiniteField
gap> x := NewIdentityMatrixOverFiniteField(
>                   IsPlistMatrixOverFiniteFieldRep, GF(4), 2);
Matrix(GF(2^2), [[Z(2)^0, 0*Z(2)], [0*Z(2), Z(2)^0]])
gap> y := NewZeroMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
>                                      GF(4), 2);
Matrix(GF(2^2), [[0*Z(2), 0*Z(2)], [0*Z(2), 0*Z(2)]])
gap> x := NewIdentityMatrixOverFiniteField(
>                   IsPlistMatrixOverFiniteFieldRep, GF(4), 0);
Matrix(GF(2^2), [])
gap> y := NewZeroMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
>                                      GF(4), 0);
Matrix(GF(2^2), [])

# Test RightInverse and LeftInverse
gap> mat := Matrix(GF(2^2), [[Z(2^2), Z(2)^0, Z(2^2)^2, Z(2)^0, Z(2^2)^2],
>  [Z(2^2)^2, Z(2^2), Z(2^2)^2, Z(2^2)^2, Z(2)^0],
>  [Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2)],
>  [Z(2^2)^2, Z(2^2)^2, Z(2)^0, Z(2)^0, 0*Z(2)],
>  [Z(2)^0, 0*Z(2), Z(2^2), 0*Z(2), Z(2^2)^2]]);;
gap> RightInverse(mat);
Matrix(GF(2^2), [[0*Z(2), 0*Z(2), Z(2^2)^2, 0*Z(2), 0*Z(2)], 
  [Z(2^2)^2, Z(2^2), Z(2)^0, Z(2^2), 0*Z(2)], 
  [0*Z(2), 0*Z(2), Z(2^2), 0*Z(2), 0*Z(2)], 
  [Z(2^2), Z(2)^0, Z(2^2)^2, 0*Z(2), 0*Z(2)], 
  [0*Z(2), Z(2)^0, Z(2^2)^2, Z(2^2)^2, 0*Z(2)]])
gap> LeftInverse(mat);
Matrix(GF(2^2), [[0*Z(2), Z(2^2), Z(2^2), Z(2)^0, Z(2)^0], 
  [Z(2^2)^2, Z(2^2), Z(2)^0, Z(2^2), 0*Z(2)], 
  [0*Z(2), Z(2^2), Z(2^2)^2, Z(2)^0, Z(2)^0], 
  [Z(2^2), Z(2^2), Z(2)^0, Z(2^2), Z(2^2)], 
  [0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2)]])
gap> Inverse(mat);
fail

# Test Inverse
gap> Inverse(Matrix(GF(9), []));
Matrix(GF(3^2), [])
gap> mat := Matrix(GF(2^2), [[Z(2^2)^2]]);;
gap> Inverse(mat);
Matrix(GF(2^2), [[Z(2^2)]])
gap> mat := Matrix(GF(2^2), [[Z(2^2), Z(2^2)^2, Z(2^2)], [Z(2)^0, Z(2^2), Z(2^2)],
>  [Z(2)^0, Z(2^2)^2, Z(2^2)]]);;
gap> Inverse(mat);
Matrix(GF(2^2), [[Z(2^2), 0*Z(2), Z(2^2)], [0*Z(2), Z(2)^0, Z(2)^0], 
  [Z(2)^0, Z(2^2), 0*Z(2)]])

# Test ComputeRowSpaceAndTransformation
gap> ComputeRowSpaceAndTransformation(2);
Error, Semigroups: ComputeRowSpaceAndTransformation: usage,
the argument must belong to`IsPlistMatrixOverFiniteFieldRep`,
gap> y := NewZeroMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep,
>                                      GF(4), 2);;
gap> ComputeRowSpaceAndTransformation(y);
gap> ComputeRowSpaceAndTransformation(Matrix(GF(9), []));

# Test RandomListOfMatricesWithRanks
gap> RandomListOfMatricesWithRanks("1", 2, 3, [1,3]);
Error, Semigroups: RandomListOfMatricesWithRanks: usage,
the 1st argument must be a ring,
gap> RandomListOfMatricesWithRanks(GF(2), 0, 3, [1,3]);
Error, Semigroups: RandomListOfMatricesWithRanks: usage,
the number of matrices (2nd argument) must be a positive integer,
gap> RandomListOfMatricesWithRanks(GF(2), 3, -1, [1,3]);
Error, Semigroups: RandomListOfMatricesWithRanks: usage,
the dimension of the matrices (3rd argument) must be a non-negative integer,
gap> RandomListOfMatricesWithRanks(GF(2), 3, 0, [1,3]);
Error, Semigroups: RandomListOfMatricesWithRanks: usage,
the list of ranks (4th argument) must consist of numbers > 0 and < n,
gap> RandomListOfMatricesWithRanks(GF(2), 3, 0, [0]);
[ Matrix(GF(2), []), Matrix(GF(2), []), Matrix(GF(2), []) ]
gap> ForAll(RandomListOfMatricesWithRanks(GF(2), 3, 3, [2]), 
>           x -> RowRank(x) = 2);
true

# Test IdentityMatrixOverFiniteField
gap> IdentityMatrixOverFiniteField(GF(16), 0);
Matrix(GF(2^4), [])
gap> IdentityMatrixOverFiniteField(GF(16), 3);
Matrix(GF(2^4), [[Z(2)^0, 0*Z(2), 0*Z(2)], [0*Z(2), Z(2)^0, 0*Z(2)], 
  [0*Z(2), 0*Z(2), Z(2)^0]])
gap> IdentityMatrixOverFiniteField(Matrix(GF(17), []), 0);
Matrix(GF(17), [])
gap> IdentityMatrixOverFiniteField(Matrix(GF(17), []), 3);
Matrix(GF(17), [[Z(17)^0, 0*Z(17), 0*Z(17)], [0*Z(17), Z(17)^0, 0*Z(17)], 
  [0*Z(17), 0*Z(17), Z(17)^0]])

#T# Test Matrix, checker, 1/1
gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5) ^ 2, Z(5) ^ 0]]);
Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5)^2, Z(5)^0]])

#T# Test One, 1/1
gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5) ^ 2, Z(5) ^ 0]]);
Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5)^2, Z(5)^0]])
gap> One(mat);
Matrix(GF(5), [[Z(5)^0, 0*Z(5)], [0*Z(5), Z(5)^0]])

#T# Test RandomMatrix, 1/1
gap> mat := RandomMatrix(GF(5), 10);
<10x10 finite field matrix>

#T# Test \*, works, 1/3
gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5), 0 * Z(5)]]);
Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5), 0*Z(5)]])
gap> mat ^ 2;
Matrix(GF(5), [[Z(5)^0, 0*Z(5)], [0*Z(5), Z(5)^0]])

#T# Test \*, wrong dims, 2/3
gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5), 0 * Z(5)]]);
Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5), 0*Z(5)]])
gap> mat2 := Matrix(GF(5), [[Z(5)]]);
Matrix(GF(5), [[Z(5)]])
gap> mat * mat2;
Error, Semigroups: * (for matrices over a finite field): usage,
the degree or domain of the arguments do not match,

#T# Test \*, wrong base field, 3/3
gap> mat := Matrix(GF(5), [[0 * Z(5), Z(5) ^ 3], [Z(5), 0 * Z(5)]]);
Matrix(GF(5), [[0*Z(5), Z(5)^3], [Z(5), 0*Z(5)]])
gap> mat2 := Matrix(GF(7), [[0 * Z(7), Z(7) ^ 3], [Z(7), 0 * Z(7)]]);
Matrix(GF(7), [[0*Z(7), Z(7)^3], [Z(7), 0*Z(7)]])
gap> mat * mat2;
Error, Semigroups: * (for matrices over a finite field): usage,
the degree or domain of the arguments do not match,

#T# Test Display, 1/1
gap> mat := Matrix(GF(11), [[Z(11) ^ 9, 0 * Z(11), Z(11), Z(11) ^ 9, 0 * Z(11)],
>   [Z(11) ^ 3, Z(11) ^ 4, 0 * Z(11), Z(11) ^ 2, Z(11) ^ 7],
>   [Z(11) ^ 9, Z(11) ^ 3, Z(11) ^ 5, Z(11) ^ 4, Z(11) ^ 4],
>   [Z(11) ^ 6, Z(11), Z(11) ^ 7, Z(11) ^ 3, Z(11) ^ 5],
>   [0 * Z(11), Z(11) ^ 8, Z(11) ^ 3, Z(11) ^ 6, Z(11) ^ 0]]);
Matrix(GF(11), [[Z(11)^9, 0*Z(11), Z(11), Z(11)^9, 0*Z(11)], 
  [Z(11)^3, Z(11)^4, 0*Z(11), Z(11)^2, Z(11)^7], 
  [Z(11)^9, Z(11)^3, Z(11)^5, Z(11)^4, Z(11)^4], 
  [Z(11)^6, Z(11), Z(11)^7, Z(11)^3, Z(11)^5], 
  [0*Z(11), Z(11)^8, Z(11)^3, Z(11)^6, Z(11)^0]])
gap> Display(mat);
Z(11)^9 0*Z(11)   Z(11) Z(11)^9 0*Z(11)
Z(11)^3 Z(11)^4 0*Z(11) Z(11)^2 Z(11)^7
Z(11)^9 Z(11)^3 Z(11)^5 Z(11)^4 Z(11)^4
Z(11)^6   Z(11) Z(11)^7 Z(11)^3 Z(11)^5
0*Z(11) Z(11)^8 Z(11)^3 Z(11)^6 Z(11)^0

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(M);

#E#
gap> STOP_TEST("Semigroups package: standard/ffmat.tst");
