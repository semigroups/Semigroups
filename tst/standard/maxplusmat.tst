#############################################################################
##
#W  standard/maxplusmat.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/maxplusmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

#T# maxplusmat: test max-plus matrix code, 1/1
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

#T# maxplusmat: test min-plus matrix code, 1/1
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

#T# maxplusmat: test tropical max-plus matrix code, 1/1
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
Error, Semigroups: * (for tropical max-plus matrices): usage,
the arguments do not have the same threshold,

#T# maxplusmat: test tropical min-plus matrix code, 1/1
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
Error, Semigroups: * (for tropical min-plus matrices): usage,
the arguments do not have the same threshold,

#T# maxplusmat: test projective max-plus matrix code, 1/1
gap> mat := Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -1, 0],
>                                       [0, -1, -infinity, -infinity],
>                                       [4, 4, 2, -1],
>                                       [1, 1, 0, 3]]);
Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -1, 0], 
  [0, -1, -infinity, -infinity], [4, 4, 2, -1], [1, 1, 0, 3]])
gap> mat ^ 2;
Matrix(IsProjectiveMaxPlusMatrix, [[-3, -3, -5, -3], [-6, -8, -7, -6], 
  [0, 0, -2, -2], [-2, -2, -3, 0]])
gap> One(mat);
Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -infinity, -infinity], 
  [-infinity, 0, -infinity, -infinity], [-infinity, -infinity, 0, -infinity], 
  [-infinity, -infinity, -infinity, 0]])

#T# maxplusmat: FIXME this doesn't work, I'm not sure that this is
#well-defined.
#gap> One(mat) * mat = mat;
#true
#gap> mat * One(mat) = mat;
#true
gap> mat = One(mat);
false
gap> mat2 := Matrix(IsProjectiveMaxPlusMatrix, [[-3, 0, 0, -infinity, -1], [1,
> -1, 2, -3, -2], [-2, 2, 2, 0, 0], [-1, 0, 2, 0, 0], [3, -infinity, 4, -2,
> 1]]);;
gap> mat * mat2;
Matrix(IsProjectiveMaxPlusMatrix, [[-7, -5, -4, -6], [-6, -6, -5, -10], 
  [-1, -2, 0, -4], [-4, -3, -1, -3]])

#T# maxplusmat: test projective ntp matrix code, 1/1
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
Error, Semigroups: * (for ntp matrices): usage,
the arguments must be matrices over the same semiring,
gap> mat3 := RandomMatrix(IsNTPMatrix, 21, 4, 2);
<21x21 ntp matrix>
gap> mat3 * mat2;
<20x20 ntp matrix>

#T# maxplusmat: test integer matrix code, 1/1
gap> mat := Matrix(IsIntegerMatrix, [[-1, -2, 0],
>                             [0, 3, -1],
>                             [1, 0, -3]]);
Matrix(IsIntegerMatrix, [[-1, -2, 0], [0, 3, -1], [1, 0, -3]])
gap> mat2 := Matrix(Integers, [[-1, -2, 0],
>                      [0, 3, -1],
>                      [1, 0, -3]]);
Matrix(IsIntegerMatrix, [[-1, -2, 0], [0, 3, -1], [1, 0, -3]])
gap> mat2 * mat;
Matrix(IsIntegerMatrix, [[1, -4, 2], [-1, 9, 0], [-4, -2, 9]])
gap> One(mat);
Matrix(IsIntegerMatrix, [[1, 0, 0], [0, 1, 0], [0, 0, 1]])
gap> One(mat) * mat = mat;
true
gap> mat * One(mat) = mat;
true
gap> mat = One(mat);
false
gap> mat3 := Matrix(IsIntegerMatrix, [[2, 2, 0, 1, 0], [2, 3, 0, 1, -2],
> [-2, -2, -2, 0, 3], [0, 2, -1, 0, 0], [0, 1, 0, -1, -1]]);;
gap> mat * mat3;
Matrix(IsIntegerMatrix, [[-6, -8, 0], [8, 11, 2], [8, 8, 6]])
gap> RandomMatrix(Integers, 20);
<20x20 integer matrix>

#T# maxplusmat: AsMatrix, trop. min-plus <-> min-plus, 1/3
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

#T# maxplusmat: AsMatrix, trop. min-plus <-> trop. min-plus, 2/3
gap> mat := Matrix(IsTropicalMinPlusMatrix, [[1, infinity, 1], [0, infinity, 2],
>  [infinity, 4, 0]], 10);;
gap> AsMatrix(IsTropicalMinPlusMatrix, mat, 2);
Matrix(IsTropicalMinPlusMatrix, [[1, infinity, 1], [0, infinity, 2], 
  [infinity, 2, 0]], 2)

#T# maxplusmat: AsMatrix, everything, 3/3
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
gap> AsMatrix(IsIntegerMatrix, mat);
Matrix(IsIntegerMatrix, [[1, 2, 2], [0, 2, 0], [1, 3, 0]])
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
gap> mat := AsMatrix(IsIntegerMatrix, mat);
Matrix(IsIntegerMatrix, [[0, 1, 0], [1, 2, 1], [1, 0, 1]])
gap> AsMatrix(IsNTPMatrix, mat, 1, 2);
Matrix(IsNTPMatrix, [[0, 1, 0], [1, 2, 1], [1, 0, 1]], 1, 2)

# maxplusmat: IsFinite, for a semigroup of integer matrices, 1/3
gap> [Matrix(IsIntegerMatrix, [[0, 2, 1], [1, 0, 1], [0, 2, 1]]),
>     Matrix(IsIntegerMatrix, [[5, 1, 1], [0, 0, 1], [2, 4, 3]])];;
gap> S := Semigroup(last);
<semigroup of 3x3 integer matrices with 2 generators>
gap> IsFinite(S);
false
gap> Size(S);
infinity

# maxplusmat: IsFinite, for a semigroup of integer matrices, 2/3
gap> S := Semigroup(Matrix(IsIntegerMatrix,
>                   [[-1, 0, 0], [0, -1, 0], [0, 0, -1]]));
<commutative semigroup of 3x3 integer matrices with 1 generator>
gap> IsFinite(S);
true

# maxplusmat: IsFinite, for a semigroup of integer matrices, 3/3
gap> S := Semigroup(Matrix(IsIntegerMatrix,
>                   [[1, 0, 0], [0, 1, 0], [0, 0, 0]]));
<commutative semigroup of 3x3 integer matrices with 1 generator>
gap> IsFinite(S);
true

#T# maxplutmat: InverseOp for a maxplus matrix
gap> InverseOp(Matrix(IsMaxPlusMatrix, [[-infinity, -infinity, 0],  
> [0, -infinity, -infinity], [-infinity, 0,  -infinity]]));
Matrix(IsMaxPlusMatrix, [[-infinity, 0, -infinity], 
  [-infinity, -infinity, 0], [0, -infinity, -infinity]])
gap> InverseOp(Matrix(IsMaxPlusMatrix, [[-infinity, 2], [2, 1]]));
fail
gap> RadialEigenvector(Matrix(IsMaxPlusMatrix,[[0,-3],[-2,-10]]));
[ 0, -2 ]
gap> RadialEigenvector(Matrix(IsMaxPlusMatrix,[[3,-3],[-2,-10]]));
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 2nd choice method found for `RadialEigenvector' on 1 arguments
gap> SpectralRadius(Matrix(IsMaxPlusMatrix,[[0,-3],[-2,-10]]));
0
gap> SpectralRadius(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2],[-2, 4, -infinity], [1, 0, 3]]));
4
gap> SpectralRadius(Matrix(IsMaxPlusMatrix, [[-infinity, 1, -infinity],  
> [-infinity, -infinity, -infinity], [-infinity, 1, -infinity]]));
-infinity
gap> UnweightedPrecedenceDigraph(Matrix(IsMaxPlusMatrix, [[2, -2, 0],
> [-infinity, 10,-2], [-infinity, 2, 1]]));
<digraph with 3 vertices, 7 edges>

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(mat);
gap> Unbind(mat2);
gap> Unbind(mat3);

#E#
gap> STOP_TEST("Semigroups package: standard/maxplusmat.tst");
