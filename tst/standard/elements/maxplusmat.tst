#############################################################################
##
#W  standard/elements/maxplusmat.tst
#Y  Copyright (C) 2015                                <NAMES>
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/elements/maxplusmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS_StartTest();

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
gap> mat2 := RandomMatrix(IsMaxPlusMatrix, 20);
<20x20 max-plus matrix>
gap> mat * mat2;
Error, Semigroups: * (for max-plus matrices): usage,
the arguments must be matrices of the same dimensions,

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
gap> mat2 := RandomMatrix(IsMinPlusMatrix, 20);
<20x20 min-plus matrix>
gap> mat * mat2;
Error, Semigroups: * (for min-plus matrices): usage,
the arguments must be matrices of the same dimensions,

#T# maxplusmat: test tropical max-plus matrix code, 1/1
gap> mat :=  Matrix(IsTropicalMaxPlusMatrix, [[3, 2, 4], 
>                                             [3, 1, 1], 
>                                             [-infinity, 1, 1]], 
>           9);
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
gap> mat2 := RandomMatrix(IsTropicalMaxPlusMatrix, 20, 9);
<20x20 tropical max-plus matrix>
gap> mat * mat2;
Error, Semigroups: * (for tropical max-plus matrices): usage,
the arguments must be matrices of the same dimensions,
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
gap> mat2 := RandomMatrix(IsTropicalMinPlusMatrix, 20, 2);
<20x20 tropical min-plus matrix>
gap> mat * mat2;
Error, Semigroups: * (for tropical min-plus matrices): usage,
the arguments must be matrices of the same dimensions,
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
gap> mat2 := RandomMatrix(IsProjectiveMaxPlusMatrix, 20);
<20x20 projective max-plus matrix>
gap> mat * mat2;
Error, Semigroups: * (for projective max-plus matrices): usage,
the arguments must be matrices of the same dimensions,

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
Error, Semigroups: * (for ntp matrices): usage,
the arguments must be matrices of the same dimensions,

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
gap> mat3 := RandomMatrix(IsIntegerMatrix, 20);
<20x20 integer matrix>
gap> mat * mat3;
Error, Semigroups: * (for integer matrices): usage,
the arguments must be matrices of the same dimensions,
gap> RandomMatrix(Integers, 20);
<20x20 integer matrix>

#E#
gap> STOP_TEST("Semigroups package: standard/elements/maxplusmat.tst");
