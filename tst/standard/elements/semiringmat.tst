#############################################################################
##
#W  standard/elements/semiringmat.tst
#Y  Copyright (C) 2015                                <NAMES>
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/elements/semiringmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS_StartTest();

#T# Matrix: return an answer, all possibilities
gap> Matrix(IsBooleanMat, [[1, 0, 0, 0], 
>                          [0, 0, 0, 0], 
>                          [1, 1, 1, 1], 
>                          [1, 0, 1, 1]]);
Matrix(IsBooleanMat, [[1, 0, 0, 0], [0, 0, 0, 0], [1, 1, 1, 1], [1, 0, 1, 1]])
gap> Matrix(IsMaxPlusMatrix, [[4, 0, -2], 
>                             [1, -3, 0], 
>                             [5, -1, -4]]);
Matrix(IsMaxPlusMatrix, [[4, 0, -2], [1, -3, 0], [5, -1, -4]])
gap> Matrix(IsMinPlusMatrix, [[-1, infinity], 
>                             [1, -1]]);
Matrix(IsMinPlusMatrix, [[-1, infinity], [1, -1]])
gap> Matrix(IsTropicalMaxPlusMatrix, [[3, 2, 4], 
>                                     [3, 1, 1], 
>                                     [-infinity, 1, 1]], 
>           9);
Matrix(IsTropicalMaxPlusMatrix, [[3, 2, 4], [3, 1, 1], [-infinity, 1, 1]], 9)
gap> Matrix(IsTropicalMinPlusMatrix, [[1, 1, 1], 
>                                     [0, 3, 0], 
>                                     [1, 1, 3]], 
>           9);
Matrix(IsTropicalMinPlusMatrix, [[1, 1, 1], [0, 3, 0], [1, 1, 3]], 9)
gap> Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -1, 0], 
>                                       [0, -1, -infinity, -infinity],
>                                       [4, 4, 2, -1],
>                                       [1, 1, 0, 3]]);
Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -1, 0], 
  [0, -1, -infinity, -infinity], [4, 4, 2, -1], [1, 1, 0, 3]])
gap> Matrix(IsNTPMatrix, [[0, 0, 0], 
>                         [2, 0, 1],
>                         [2, 2, 2]], 
>           2, 1);
Matrix(IsNTPMatrix, [[0, 0, 0], [2, 0, 1], [2, 2, 2]], 2, 1)
gap> Matrix(IsIntegerMatrix, [[-1, -2, 0], 
>                             [0, 3, -1], 
>                             [1, 0, -3]]);
Matrix(IsIntegerMatrix, [[-1, -2, 0], [0, 3, -1], [1, 0, -3]])
gap> Matrix(Integers, [[-1, -2, 0], 
>                      [0, 3, -1], 
>                      [1, 0, -3]]);
Matrix(IsIntegerMatrix, [[-1, -2, 0], [0, 3, -1], [1, 0, -3]])
gap> Matrix(GF(3), [[Z(3), Z(3)^0, Z(3)], 
>                   [Z(3), Z(3)^0, Z(3)^0], 
>                   [Z(3), 0*Z(3), 0*Z(3)]]);
Matrix(GF(3), [[Z(3), Z(3)^0, Z(3)], [Z(3), Z(3)^0, Z(3)^0], 
  [Z(3), 0*Z(3), 0*Z(3)]])

#E#
gap> STOP_TEST("Semigroups package: standard/elements/semiringmat.tst");
