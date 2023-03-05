#############################################################################
##
#W  standard/elements/maxplusmat.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local A, S, mat, mat2, mat3, x
gap> START_TEST("Semigroups package: standard/elements/maxplusmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();
gap> Matrix(IsNTPMatrix, [[1]], -1, 2);
Error, the 2nd argument (a pos. int.) is not >= 0

# maxplusmat: test max-plus matrix code, 1/1
gap> mat := Matrix(IsMaxPlusMatrix, [[4, 0, -2],
>                                    [1, -3, 0],
>                                    [5, -1, -4]]);
Matrix(IsMaxPlusMatrix, [[4, 0, -2], [1, -3, 0], [5, -1, -4]])
gap> mat ^ 2;
Matrix(IsMaxPlusMatrix, [[8, 4, 2], [5, 1, -1], [9, 5, 3]])
gap> One(mat);
Matrix(IsMaxPlusMatrix, [[0, -infinity, -infinity], 
  [-infinity, 0, -infinity], [-infinity, -infinity, 0]])
gap> One(mat) * mat = mat;
true
gap> mat * One(mat) = mat;
true
gap> mat = One(mat);
false
gap> mat2 := Matrix(IsMaxPlusMatrix,
>                   [[0, 1, 1, 0, -infinity], 
>                    [0, -3, -2, -2, -infinity],
>                    [-4, 0, -2, 2, -infinity], 
>                    [1, 1, -6, 3, 1], 
>                    [-1, 0, -1, 0, -1]]);;
gap> mat * mat2;
Matrix(IsMaxPlusMatrix, [[4, 5, 5], [1, 2, 2], [5, 6, 6]])

# maxplusmat: test min-plus matrix code, 1/1
gap> mat := Matrix(IsMinPlusMatrix, [[-1, infinity],
>                                    [1, -1]]);
Matrix(IsMinPlusMatrix, [[-1, infinity], [1, -1]])
gap> mat ^ 2;
Matrix(IsMinPlusMatrix, [[-2, infinity], [0, -2]])
gap> One(mat);
Matrix(IsMinPlusMatrix, [[0, infinity], [infinity, 0]])
gap> One(mat) * mat = mat;
true
gap> mat * One(mat) = mat;
true
gap> mat = One(mat);
false
gap> mat2 := Matrix(IsMinPlusMatrix,
>                   [[0, 1, 1, 0, infinity], 
>                    [0, -3, -2, -2, infinity],
>                    [-4, 0, -2, 2, infinity], 
>                    [1, 1, -6, 3, 1], 
>                    [-1, 0, -1, 0, -1]]);;
gap> mat * mat2;
Matrix(IsMinPlusMatrix, [[-1, 0], [-1, -4]])

# maxplusmat: test tropical max-plus matrix code, 1/1
gap> mat := Matrix(IsTropicalMaxPlusMatrix, [[3, 2, 4],
>                                            [3, 1, 1],
>                                            [-infinity, 1, 1]],
>                  9);
Matrix(IsTropicalMaxPlusMatrix, [[3, 2, 4], [3, 1, 1], [-infinity, 1, 1]], 9)
gap> mat ^ 2;
Matrix(IsTropicalMaxPlusMatrix, [[6, 5, 7], [6, 5, 7], [4, 2, 2]], 9)
gap> mat ^ 2 * mat;
Matrix(IsTropicalMaxPlusMatrix, [[9, 8, 9], [9, 8, 9], [7, 6, 8]], 9)
gap> One(mat);
Matrix(IsTropicalMaxPlusMatrix, [[0, -infinity, -infinity], 
  [-infinity, 0, -infinity], [-infinity, -infinity, 0]], 9)
gap> One(mat) * mat = mat;
true
gap> mat * One(mat) = mat;
true
gap> mat = One(mat);
false
gap> RandomMatrix(IsTropicalMaxPlusMatrix, 20);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RandomMatrixCons' on 2 arguments
gap> mat2 := Matrix(IsTropicalMaxPlusMatrix, [[-infinity, 3, 4, -infinity, 1],
>  [2, 1, 1, 6, 0], [3, 1, 3, 2, 1], [1, 3, 1, -infinity, -infinity],
>  [3, 4, -infinity, 3, 1]], 9);;
gap> mat * mat2;
Matrix(IsTropicalMaxPlusMatrix, [[7, 6, 7], [4, 6, 7], [4, 2, 4]], 9)
gap> mat3 := RandomMatrix(IsTropicalMaxPlusMatrix, 20, 5);
<20x20 tropical max-plus matrix>
gap> mat2 * mat3;
Error, the arguments (tropical max-plus matrices)do not have the same threshol\
d

# maxplusmat: test tropical min-plus matrix code, 1/1
gap> mat := Matrix(IsTropicalMinPlusMatrix, [[1, 1, 1],
>                                     [0, 3, 0],
>                                     [1, 1, 3]],
>           9);
Matrix(IsTropicalMinPlusMatrix, [[1, 1, 1], [0, 3, 0], [1, 1, 3]], 9)
gap> mat ^ 2;
Matrix(IsTropicalMinPlusMatrix, [[1, 2, 1], [1, 1, 1], [1, 2, 1]], 9)
gap> mat := Matrix(IsTropicalMinPlusMatrix, [[2, 2], [2, 2]], 2);
Matrix(IsTropicalMinPlusMatrix, [[2, 2], [2, 2]], 2)
gap> mat ^ 2;
Matrix(IsTropicalMinPlusMatrix, [[2, 2], [2, 2]], 2)
gap> One(mat);
Matrix(IsTropicalMinPlusMatrix, [[0, infinity], [infinity, 0]], 2)
gap> One(mat) * mat = mat;
true
gap> mat * One(mat) = mat;
true
gap> mat = One(mat);
false
gap> RandomMatrix(IsTropicalMinPlusMatrix, 20);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RandomMatrixCons' on 2 arguments
gap> mat2 := Matrix(IsTropicalMinPlusMatrix, [[infinity, 2, 1, 1, infinity],
>  [2, infinity, 2, infinity, 1], [1, infinity, 2, 1, 2],
>  [0, 1, 1, 1, infinity], [infinity, 0, 0, 0, 0]], 2);;
gap> mat * mat2;
Matrix(IsTropicalMinPlusMatrix, [[2, 2], [2, 2]], 2)
gap> mat3 := RandomMatrix(IsTropicalMinPlusMatrix, 20, 5);
<20x20 tropical min-plus matrix>
gap> mat2 * mat3;
Error, the arguments (tropical min-plus matrices) do not have the same thresho\
ld

# maxplusmat: test projective max-plus matrix code, 1/1
gap> mat := Matrix(IsProjectiveMaxPlusMatrix,
> [[0, -infinity, -1, 0],
>  [0, -1, -infinity, -infinity],
>  [4, 4, 2, -1],
>  [1, 1, 0, 3]]);
Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -1, 0], 
  [0, -1, -infinity, -infinity], [4, 4, 2, -1], [1, 1, 0, 3]])
gap> mat ^ 2;
Matrix(IsProjectiveMaxPlusMatrix, [[-3, -3, -5, -3], [-6, -8, -7, -6], 
  [0, 0, -2, -2], [-2, -2, -3, 0]])
gap> One(mat);
Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -infinity, -infinity], 
  [-infinity, 0, -infinity, -infinity], [-infinity, -infinity, 0, -infinity], 
  [-infinity, -infinity, -infinity, 0]])

# maxplusmat: FIXME(later) this doesn't work, I'm not sure that this is
# well-defined.
# gap> One(mat) * mat = mat;
# true
# gap> mat * One(mat) = mat;
# true
gap> mat = One(mat);
false
gap> mat2 := Matrix(IsProjectiveMaxPlusMatrix,
> [[-3, 0, 0, -infinity, -1],
>  [1, -1, 2, -3, -2],
>  [-2, 2, 2, 0, 0],
>  [-1, 0, 2, 0, 0],
>  [3, -infinity, 4, -2, 1]]);;
gap> mat * mat2;
Matrix(IsProjectiveMaxPlusMatrix, [[-7, -5, -4, -6], [-6, -6, -5, -10], 
  [-1, -2, 0, -4], [-4, -3, -1, -3]])

# maxplusmat: test ntp matrix code, 1/1
gap> mat := Matrix(IsNTPMatrix, [[0, 0, 0],
>                         [2, 0, 1],
>                         [2, 2, 2]],
>           2, 1);
Matrix(IsNTPMatrix, [[0, 0, 0], [2, 0, 1], [2, 2, 2]], 2, 1)
gap> mat ^ 2;
Matrix(IsNTPMatrix, [[0, 0, 0], [2, 2, 2], [2, 2, 2]], 2, 1)
gap> One(mat);
Matrix(IsNTPMatrix, [[1, 0, 0], [0, 1, 0], [0, 0, 1]], 2, 1)
gap> One(mat) * mat = mat;
true
gap> mat * One(mat) = mat;
true
gap> mat = One(mat);
false
gap> RandomMatrix(IsNTPMatrix, 20);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RandomMatrixCons' on 2 arguments
gap> RandomMatrix(IsNTPMatrix, 20, 2);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RandomMatrixCons' on 3 arguments
gap> mat2 := RandomMatrix(IsNTPMatrix, 20, 4, 2);
<20x20 ntp matrix>
gap> mat * mat2;
Error, the arguments (ntp matrices) are not over the same semiring
gap> mat3 := RandomMatrix(IsNTPMatrix, 21, 4, 2);
<21x21 ntp matrix>
gap> mat3 * mat2;
<20x20 ntp matrix>

# Test period can't be 0
gap> A := Matrix(IsNTPMatrix, [[1, 0], [1, 1]], 59, 0);
Error, the 3rd argument (a pos. int.) is not > 0

# maxplusmat: test integer matrix code, 1/1
gap> mat := Matrix(Integers, [[-1, -2, 0],
>                             [0, 3, -1],
>                             [1, 0, -3]]);
<3x3-matrix over Integers>
gap> mat2 := Matrix(Integers, [[-1, -2, 0],
>                      [0, 3, -1],
>                      [1, 0, -3]]);
<3x3-matrix over Integers>
gap> mat2 * mat;
<3x3-matrix over Integers>
gap> One(mat);
<immutable 3x3-matrix over Integers>
gap> One(mat) * mat = mat;
true
gap> mat * One(mat) = mat;
true
gap> mat = One(mat);
false
gap> mat3 := Matrix(Integers, [[2, 2, 0, 1, 0], [2, 3, 0, 1, -2],
> [-2, -2, -2, 0, 3], [0, 2, -1, 0, 0], [0, 1, 0, -1, -1]]);;
gap> mat * mat3;
Error, \*: Matrices do not fit together
gap> RandomMatrix(Integers, 20);
<20x20-matrix over Integers>

# maxplusmat: AsMatrix, trop. min-plus <-> min-plus, 1/3
gap> mat := Matrix(IsTropicalMinPlusMatrix, [[1, infinity, 1], [0, infinity, 2],
>  [infinity, 4, 0]], 10);;
gap> AsMatrix(IsMinPlusMatrix, mat);
Matrix(IsMinPlusMatrix, [[1, infinity, 1], [0, infinity, 2], [infinity, 4, 0]]
  )
gap> AsMatrix(IsTropicalMinPlusMatrix, last, 10);
Matrix(IsTropicalMinPlusMatrix, [[1, infinity, 1], [0, infinity, 2], 
  [infinity, 4, 0]], 10)
gap> last = mat;
true

# maxplusmat: AsMatrix, trop. min-plus <-> trop. min-plus, 2/3
gap> mat := Matrix(IsTropicalMinPlusMatrix, [[1, infinity, 1], [0, infinity, 2],
>  [infinity, 4, 0]], 10);;
gap> AsMatrix(IsTropicalMinPlusMatrix, mat, 2);
Matrix(IsTropicalMinPlusMatrix, [[1, infinity, 1], [0, infinity, 2], 
  [infinity, 2, 0]], 2)

# maxplusmat: AsMatrix, everything, 3/3
gap> mat := Matrix(IsTropicalMinPlusMatrix, [[0, 1, 3],
>                                            [1, 1, 6],
>                                            [0, 4, 2]], 10);;
gap> AsMatrix(IsMinPlusMatrix, mat);
Matrix(IsMinPlusMatrix, [[0, 1, 3], [1, 1, 6], [0, 4, 2]])
gap> mat := Matrix(IsTropicalMaxPlusMatrix, [[-infinity, -infinity, 3],
>                                            [0, 1, 3],
>                                            [4, 1, 0]], 10);;
gap> AsMatrix(IsMaxPlusMatrix, mat);
Matrix(IsMaxPlusMatrix, [[-infinity, -infinity, 3], [0, 1, 3], [4, 1, 0]])
gap> mat := Matrix(IsProjectiveMaxPlusMatrix, [[-1, 2, 1],
>                                              [-2, -1, 1],
>                                              [1, 1, 2]]);;
gap> AsMatrix(IsMaxPlusMatrix, mat);
Matrix(IsMaxPlusMatrix, [[-1, 2, 1], [-2, -1, 1], [1, 1, 2]])
gap> mat := Matrix(IsTropicalMaxPlusMatrix, [[-infinity, -infinity, 3],
>                                            [0, 1, 3],
>                                            [4, 1, 0]], 10);;
gap> AsMatrix(IsProjectiveMaxPlusMatrix, mat);
Matrix(IsProjectiveMaxPlusMatrix, [[-infinity, -infinity, 3], [0, 1, 3], 
  [4, 1, 0]])
gap> mat := Matrix(IsNTPMatrix, [[1, 2, 2],
>                                [0, 2, 0],
>                                [1, 3, 0]], 4, 5);;
gap> Matrix(Integers, mat);
<3x3-matrix over Integers>
gap> mat := Matrix(IsMinPlusMatrix, [[0, 1, 3], [1, 1, 6], [0, 4, 2]]);;
gap> mat := AsMatrix(IsTropicalMinPlusMatrix, mat, 2);
Matrix(IsTropicalMinPlusMatrix, [[0, 1, 2], [1, 1, 2], [0, 2, 2]], 2)
gap> mat := AsMatrix(IsTropicalMinPlusMatrix, mat, 1);
Matrix(IsTropicalMinPlusMatrix, [[0, 1, 1], [1, 1, 1], [0, 1, 1]], 1)
gap> mat := Matrix(IsTropicalMaxPlusMatrix, [[-infinity, -infinity, 3],
>                                            [0, 1, 3],
>                                            [4, 1, 0]], 10);;
gap> AsMatrix(IsTropicalMaxPlusMatrix, mat, 4);
Matrix(IsTropicalMaxPlusMatrix, [[-infinity, -infinity, 3], [0, 1, 3], 
  [4, 1, 0]], 4)
gap> mat := Matrix(IsProjectiveMaxPlusMatrix,
>                  [[-infinity, -infinity, 3],
>                   [0, 1, 3],
>                   [4, 1, 0]]);;
gap> AsMatrix(IsTropicalMaxPlusMatrix, mat, 1);
Matrix(IsTropicalMaxPlusMatrix, [[-infinity, -infinity, 1], [0, 1, 1], 
  [1, 1, 0]], 1)
gap> mat := Matrix(IsMaxPlusMatrix, [[-infinity, -infinity, 3],
>                                    [0, 1, 3],
>                                    [4, 1, 0]]);;
gap> AsMatrix(IsTropicalMaxPlusMatrix, mat, 10);
Matrix(IsTropicalMaxPlusMatrix, [[-infinity, -infinity, 3], [0, 1, 3], 
  [4, 1, 0]], 10)
gap> AsMatrix(IsProjectiveMaxPlusMatrix, mat);
Matrix(IsProjectiveMaxPlusMatrix, [[-infinity, -infinity, 3], [0, 1, 3], 
  [4, 1, 0]])
gap> mat := Matrix(IsNTPMatrix, [[0, 1, 0],
>                                [1, 3, 1],
>                                [1, 0, 1]], 10, 10);;
gap> mat := AsMatrix(IsNTPMatrix, mat, 5, 6);
Matrix(IsNTPMatrix, [[0, 1, 0], [1, 3, 1], [1, 0, 1]], 5, 6)
gap> mat := AsMatrix(IsNTPMatrix, mat, 2, 6);
Matrix(IsNTPMatrix, [[0, 1, 0], [1, 3, 1], [1, 0, 1]], 2, 6)
gap> mat := AsMatrix(IsNTPMatrix, mat, 2, 1);
Matrix(IsNTPMatrix, [[0, 1, 0], [1, 2, 1], [1, 0, 1]], 2, 1)
gap> mat := Matrix(Integers, mat);
<3x3-matrix over Integers>
gap> AsMatrix(IsNTPMatrix, mat, 1, 2);
Matrix(IsNTPMatrix, [[0, 1, 0], [1, 2, 1], [1, 0, 1]], 1, 2)

# maxplusmat: IsFinite, for a semigroup of integer matrices, 1/3
gap> [Matrix(Integers, [[0, 2, 1], [1, 0, 1], [0, 2, 1]]),
>     Matrix(Integers, [[5, 1, 1], [0, 0, 1], [2, 4, 3]])];;
gap> S := Semigroup(last);
<semigroup of 3x3 integer matrices with 2 generators>
gap> IsFinite(S);
false
gap> Size(S);
infinity

# maxplusmat: IsFinite, for a semigroup of integer matrices, 2/3
gap> S := Semigroup(Matrix(Integers,
>                   [[-1, 0, 0], [0, -1, 0], [0, 0, -1]]));
<commutative semigroup of 3x3 integer matrices with 1 generator>
gap> IsFinite(S);
true

# maxplusmat: IsFinite, for a semigroup of integer matrices, 3/3
gap> S := Semigroup(Matrix(Integers,
>                   [[1, 0, 0], [0, 1, 0], [0, 0, 0]]));
<commutative semigroup of 3x3 integer matrices with 1 generator>
gap> IsFinite(S);
true

# maxplusmat: InverseOp, for integer matrices
gap> mat := Matrix(Integers, [[0, 0, -1], [0, 1, 0], [1, 0, 0]]);;
gap> InverseOp(mat);
<3x3-matrix over Integers>
gap> mat * InverseOp(mat) = One(mat);
true
gap> InverseOp(mat) * mat = One(mat);
true
gap> mat := Matrix(Integers, [[0, -3, 0, -2], [-1, 1, -1, 0],
> [0, 1, 0, 1], [0, 0, 2, 0]]);;

# TODO(MatrixObj-later): with MatrixObj's this returns an invalid answer see
# https://github.com/gap-system/gap/issues/4884
# gap> InverseOp(mat); 
# fail
gap> mat := Matrix(Integers,
> [[0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
>  [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]);;
gap> InverseOp(mat) * mat = One(mat);
true
gap> mat * InverseOp(mat) = One(mat);
true

# maxplusmat: Order, for integer matrices
gap> mat := Matrix(Integers, [[0, 0, -1], [0, 1, 0], [1, 0, 0]]);;
gap> Order(mat);
4
gap> mat := Matrix(Integers, [[0, -3, 0, -2], [-1, 1, -1, 0],
> [0, 1, 0, 1], [0, 0, 2, 0]]);;
gap> Order(mat);
infinity
gap> mat := Matrix(Integers,
> [[0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
>  [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]);;
gap> Order(mat);
10
gap> mat := Matrix(Integers, [[0, 0, -1, 0], [0, -1, 0, 0],
> [4, 4, 2, -1], [1, 1, 0, 3]]);;
gap> Order(mat);
infinity

# maxplusmat: IsTorsion, for integer matrices
gap> mat := Matrix(Integers, [[0, 0, -1], [0, 1, 0], [1, 0, 0]]);;
gap> IsTorsion(mat);
true
gap> mat := Matrix(Integers, [[0, -3, 0, -2], [-1, 1, -1, 0],
> [0, 1, 0, 1], [0, 0, 2, 0]]);;
gap> IsTorsion(mat);
false
gap> mat := Matrix(Integers,
> [[0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
>  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
>  [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
>  [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]);;
gap> IsTorsion(mat);
true
gap> mat := Matrix(Integers, [[0, 0, -1, 0], [0, -1, 0, 0],
> [4, 4, 2, -1], [1, 1, 0, 3]]);;
gap> IsTorsion(mat);
false

# maxplusmat: RadialEigenvector for a max-plus matrix with SpectralRadius = 0
gap> RadialEigenvector(Matrix(IsMaxPlusMatrix, [[0, -3], [-2, -10]]));
[ 0, -2 ]
gap> RadialEigenvector(Matrix(IsMaxPlusMatrix, [[3, -3], [-2, -10]]));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `RadialEigenvector' on 1 arguments
gap> SpectralRadius(Matrix(IsMaxPlusMatrix, [[0, -3], [-2, -10]]));
0

# maxplusmat: UnweightedPrecedenceDigraph for a max-plus matrix
gap> UnweightedPrecedenceDigraph(Matrix(IsMaxPlusMatrix, [[2, -2, 0],
> [-infinity, 10, -2], [-infinity, 2, 1]]));
<immutable digraph with 3 vertices, 7 edges>

# maxplusmat: InverseOp for a max-plus matrix
gap> mat := Matrix(IsMaxPlusMatrix, [[-1, -infinity, -infinity],
>  [-infinity, -infinity, 1], [-infinity, 2, -infinity]]);;
gap> InverseOp(mat) * mat;
Matrix(IsMaxPlusMatrix, [[0, -infinity, -infinity], 
  [-infinity, 0, -infinity], [-infinity, -infinity, 0]])
gap> mat * InverseOp(mat);
Matrix(IsMaxPlusMatrix, [[0, -infinity, -infinity], 
  [-infinity, 0, -infinity], [-infinity, -infinity, 0]])
gap> mat * InverseOp(mat) = One(mat);
true
gap> InverseOp(mat) * mat = One(mat);
true
gap> mat := ListWithIdenticalEntries(10, -infinity);;
gap> mat := List([1 .. 10], x -> ShallowCopy(mat));;
gap> mat[1][10] := -1;;
gap> mat[2][7] := 5;;
gap> mat[3][9] := -2;;
gap> mat[4][4] := -5;;
gap> mat[5][6] := -2;;
gap> mat[6][2] := 0;;
gap> mat[7][5] := 1;;
gap> mat[8][3] := -2;;
gap> mat[9][1] := -1;;
gap> mat[10][8] := -3;;
gap> mat := Matrix(IsMaxPlusMatrix, mat);
<10x10 max-plus matrix>
gap> InverseOp(mat) * mat = One(mat);
true
gap> mat * InverseOp(mat) = One(mat);
true
gap> mat := Matrix(IsMaxPlusMatrix, [[-infinity, -1, 3, -infinity],
> [4, 0, -4, 1], [-2, 1, 1, 1], [0, -infinity, 5, 0]]);;
gap> InverseOp(mat);
fail
gap> InverseOp(Matrix(IsMaxPlusMatrix,
> [[-infinity, -infinity, 0],
>  [0, -infinity, -infinity],
>  [-infinity, 0, -infinity]]));
Matrix(IsMaxPlusMatrix, [[-infinity, 0, -infinity], 
  [-infinity, -infinity, 0], [0, -infinity, -infinity]])
gap> InverseOp(Matrix(IsMaxPlusMatrix, [[-infinity, 2], [2, 1]]));
fail

# maxplusmat: UnweightedPrecedenceDigraph for a max-plus matrix
gap> mat := Matrix(IsMaxPlusMatrix, [[0, -3, 0, -2], [-1, 1, -1, 0],
>  [-infinity, 1, -infinity, 1], [0, -infinity, 2, -infinity]]);;
gap> UnweightedPrecedenceDigraph(mat);
<immutable digraph with 4 vertices, 12 edges>
gap> mat := Matrix(IsMaxPlusMatrix, [[-infinity, -3, 3, -1, -1],
> [1, 1, 1, -2, 0], [-infinity, 0, 0, 1, -1], [2, 2, 1, 0, -infinity],
> [1, -1, 5, -infinity, -infinity]]);;

# maxplusmat: SpectralRadius for a max-plus matrix
gap> mat := Matrix(IsMaxPlusMatrix, [[0, -3, 0, -2, -1], [1, -1, 0, -infinity, 1],
>  [-infinity, 1, 0, -infinity, 2], [-infinity, 4, 0, -2, 1],
>  [-3, 0, 5, -1, -4]]);;
gap> SpectralRadius(mat);
7/2
gap> mat := Matrix(IsMaxPlusMatrix, [[-1, -infinity, 1, -1, 0],
>  [-2, -1, -infinity, -infinity, -1], [-1, 1, 4, 5, -1],
>  [1, -1, -1, -infinity, 0], [-infinity, 1, -1, -infinity, 3]]);;
gap> SpectralRadius(mat);
4
gap> mat := Matrix(IsMaxPlusMatrix, [[2, 4, 3, -1, 1],
> [-infinity, -1, 1, 1, -1], [-1, 0, 3, 0, -1], [1, 3, 0, -infinity, -1],
> [0, 0, -1, -infinity, -infinity]]);;
gap> SpectralRadius(mat);
3
gap> mat := Matrix(IsMaxPlusMatrix, [[4, 4, 2, -1, 1],
> [1, 0, 3, 0, -3], [0, 0, 4, 0, 2],
> [-3, 1, 0, 0, -infinity], [4, -infinity, 1, 2, -3]]);;
gap> SpectralRadius(mat);
4
gap> mat := Matrix(IsMaxPlusMatrix, [[3, -1, -2, -infinity, -infinity],
>  [3, -1, 1, -infinity, -3], [3, -1, -1, 1, 1], [1, -2, 0, -infinity, 0],
>  [0, 1, -1, 2, 2]]);;
gap> SpectralRadius(mat);
3
gap> SpectralRadius(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
4
gap> SpectralRadius(Matrix(IsMaxPlusMatrix, [[-infinity, 1, -infinity], 
> [-infinity, -infinity, -infinity], [-infinity, 1, -infinity]]));
-infinity

# Test AsMatrix (for a transformation)
gap> AsMatrix(IsMaxPlusMatrix, IdentityTransformation);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `AsMatrix' on 3 arguments
gap> AsMatrix(IsMaxPlusMatrix, Transformation([2, 1, 1]));
Matrix(IsMaxPlusMatrix, [[-infinity, 0, -infinity], 
  [0, -infinity, -infinity], [0, -infinity, -infinity]])
gap> AsMatrix(IsMinPlusMatrix, IdentityTransformation);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `AsMatrix' on 3 arguments
gap> AsMatrix(IsMinPlusMatrix, Transformation([2, 1, 1]));
Matrix(IsMinPlusMatrix, [[infinity, 0, infinity], [0, infinity, infinity], 
  [0, infinity, infinity]])
gap> AsMatrix(IsTropicalMinPlusMatrix, IdentityTransformation, 5);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `AsMatrix' on 4 arguments
gap> AsMatrix(IsTropicalMinPlusMatrix, Transformation([2, 1, 1]), 5);
Matrix(IsTropicalMinPlusMatrix, [[infinity, 0, infinity], 
  [0, infinity, infinity], [0, infinity, infinity]], 5)
gap> AsMatrix(IsTropicalMaxPlusMatrix, IdentityTransformation, 5);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `AsMatrix' on 4 arguments
gap> AsMatrix(IsTropicalMaxPlusMatrix, Transformation([2, 1, 1]), 5);
Matrix(IsTropicalMaxPlusMatrix, [[-infinity, 0, -infinity], 
  [0, -infinity, -infinity], [0, -infinity, -infinity]], 5)
gap> AsMatrix(IsProjectiveMaxPlusMatrix, IdentityTransformation);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `AsMatrix' on 3 arguments
gap> AsMatrix(IsProjectiveMaxPlusMatrix, Transformation([2, 1, 1]));
Matrix(IsProjectiveMaxPlusMatrix, [[-infinity, 0, -infinity], 
  [0, -infinity, -infinity], [0, -infinity, -infinity]])
gap> AsMatrix(IsNTPMatrix, Transformation([2, 1, 1]), 3, 3);
Matrix(IsNTPMatrix, [[0, 1, 0], [1, 0, 0], [1, 0, 0]], 3, 3)
gap> AsMatrix(IsNTPMatrix, IdentityTransformation, 3, 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `AsMatrix' on 5 arguments
gap> Matrix(Integers, Transformation([2, 1, 1]));
<3x3-matrix over Integers>
gap> Matrix(Integers, IdentityTransformation);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `Matrix' on 3 arguments

# Test SEMIGROUPS_TypeViewStringOfMatrixOverSemiring and RandomMatrix
gap> RandomMatrix(IsMaxPlusMatrix, 10);
<10x10 max-plus matrix>
gap> RandomMatrix(IsMinPlusMatrix, 10);
<10x10 min-plus matrix>
gap> RandomMatrix(IsProjectiveMaxPlusMatrix, 10);
<10x10 projective max-plus matrix>

# Test NTPMatrix entry checker
gap> x := Matrix(IsNTPMatrix, [[100, 1], [0, 0]], 5, 10);
Error, the entries in the 2nd argument do not define a matrix of type IsNTPMat\
rix
gap> x := Matrix(IsNTPMatrix, [[1, 1], [0, 0]], 5, -10);
Error, the 3rd argument (a pos. int.) is not > 0

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/elements/maxplusmat.tst");
