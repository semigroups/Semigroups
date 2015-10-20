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

#T# Matrix: return an answer, all possibilities, and TransposedMat
gap> Matrix(IsBooleanMat, [[1, 0, 0, 0], 
>                          [0, 0, 0, 0], 
>                          [1, 1, 1, 1], 
>                          [1, 0, 1, 1]]);
Matrix(IsBooleanMat, [[1, 0, 0, 0], [0, 0, 0, 0], [1, 1, 1, 1], [1, 0, 1, 1]])
gap> TransposedMat(TransposedMat(last)) = last;
true
gap> Matrix(IsMaxPlusMatrix, [[4, 0, -2], 
>                             [1, -3, 0], 
>                             [5, -1, -4]]);
Matrix(IsMaxPlusMatrix, [[4, 0, -2], [1, -3, 0], [5, -1, -4]])
gap> TransposedMat(TransposedMat(last)) = last;
true
gap> Matrix(IsMinPlusMatrix, [[-1, infinity], 
>                             [1, -1]]);
Matrix(IsMinPlusMatrix, [[-1, infinity], [1, -1]])
gap> TransposedMat(TransposedMat(last)) = last;
true
gap> Matrix(IsTropicalMaxPlusMatrix, [[3, 2, 4], 
>                                     [3, 1, 1], 
>                                     [-infinity, 1, 1]], 
>           9);
Matrix(IsTropicalMaxPlusMatrix, [[3, 2, 4], [3, 1, 1], [-infinity, 1, 1]], 9)
gap> TransposedMat(TransposedMat(last)) = last;
true
gap> Matrix(IsTropicalMinPlusMatrix, [[1, 1, 1], 
>                                     [0, 3, 0], 
>                                     [1, 1, 3]], 
>           9);
Matrix(IsTropicalMinPlusMatrix, [[1, 1, 1], [0, 3, 0], [1, 1, 3]], 9)
gap> TransposedMat(TransposedMat(last)) = last;
true
gap> Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -1, 0], 
>                                       [0, -1, -infinity, -infinity],
>                                       [4, 4, 2, -1],
>                                       [1, 1, 0, 3]]);
Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -1, 0], 
  [0, -1, -infinity, -infinity], [4, 4, 2, -1], [1, 1, 0, 3]])
gap> TransposedMat(TransposedMat(last)) = last;
true
gap> Matrix(IsNTPMatrix, [[0, 0, 0], 
>                         [2, 0, 1],
>                         [2, 2, 2]], 
>           2, 1);
Matrix(IsNTPMatrix, [[0, 0, 0], [2, 0, 1], [2, 2, 2]], 2, 1)
gap> TransposedMat(TransposedMat(last)) = last;
true
gap> Matrix(IsIntegerMatrix, [[-1, -2, 0], 
>                             [0, 3, -1], 
>                             [1, 0, -3]]);
Matrix(IsIntegerMatrix, [[-1, -2, 0], [0, 3, -1], [1, 0, -3]])
gap> TransposedMat(TransposedMat(last)) = last;
true
gap> Matrix(Integers, [[-1, -2, 0], 
>                      [0, 3, -1], 
>                      [1, 0, -3]]);
Matrix(IsIntegerMatrix, [[-1, -2, 0], [0, 3, -1], [1, 0, -3]])
gap> TransposedMat(TransposedMat(last)) = last;
true
gap> Matrix(GF(3), [[Z(3), Z(3)^0, Z(3)], 
>                   [Z(3), Z(3)^0, Z(3)^0], 
>                   [Z(3), 0*Z(3), 0*Z(3)]]);
Matrix(GF(3), [[Z(3), Z(3)^0, Z(3)], [Z(3), Z(3)^0, Z(3)^0], 
  [Z(3), 0*Z(3), 0*Z(3)]])
gap> TransposedMat(TransposedMat(last)) = last;
true

# semiringmat: MatrixNC for a matrix over semiring and homogeneous list
gap> mat := Matrix(IsTropicalMaxPlusMatrix, [[0, 3], [0, 2]], 5);
Matrix(IsTropicalMaxPlusMatrix, [[0, 3], [0, 2]], 5)
gap> One(mat);
Matrix(IsTropicalMaxPlusMatrix, [[0, -infinity], [-infinity, 0]], 5)

# semiringmat: Matrix, for a filter, homogeneous list, pos int, and pos int,
# 1/3
gap> Matrix(IsNTPMatrix, [[1, 1], [2]], 3, 3);
Error, Semigroups: Matrix: usage,
the 1st argument must be a square table,
gap> Matrix(IsNTPMatrix, [[1, 1, 3], [1, 2, 3]], 3, 3);
Error, Semigroups: Matrix: usage,
the 1st argument must be a square table,

# semiringmat: Matrix, for a filter, homogeneous list, pos int, and pos int,
# 2/3
gap> Matrix(IsIntegerMatrix, [[1, 1], [2, 2]], 3, 3);
Error, Semigroups: Matrix:
cannot create a matrix from the given arguments,

# semiringmat: Matrix, for a filter, homogeneous list, pos int, and pos int,
# 3/3
gap> Matrix(IsNTPMatrix, [[1, 1], [2, 10]], 3, 3);
Error, Semigroups: Matrix: usage,
the entries in the 2nd argument do not define a matrix of type IsNTPMatrix,

# semiringmat: Matrix, for a filter, homogeneous list, and pos int, 1/3
gap> Matrix(IsTropicalMaxPlusMatrix, [[1, 1], [2]], 3);
Error, Semigroups: Matrix: usage,
the 1st argument must be a square table,
gap> Matrix(IsTropicalMaxPlusMatrix, [[1, 1, 3], [1, 2, 3]], 3);
Error, Semigroups: Matrix: usage,
the 1st argument must be a square table,

# semiringmat: Matrix, for a filter, homogeneous list, and pos int, 2/3
gap> Matrix(IsIntegerMatrix, [[1, 1], [2, 2]], 3);
Error, Semigroups: Matrix:
cannot create a matrix from the given arguments,

# semiringmat: Matrix, for a filter, homogeneous list, and pos int, 3/3
gap> Matrix(IsTropicalMinPlusMatrix, [[1, 1], [2, 10]], 3);
Error, Semigroups: Matrix: usage,
the entries in the 2nd argument do not define a matrix of type IsTropicalMinPl\
usMatrix,

# semiringmat: Matrix, for a filter and homogeneous list, 1/3
gap> Matrix(IsIntegerMatrix, [[1, 1], [2]]);
Error, Semigroups: Matrix: usage,
the 1st argument must be a square table,
gap> Matrix(IsIntegerMatrix, [[1, 1, 3], [1, 2, 3]]);
Error, Semigroups: Matrix: usage,
the 1st argument must be a square table,

# semiringmat: Matrix, for a filter and homogeneous list, 2/3
gap> Matrix(IsNTPMatrix, [[1, 1], [1, 2]]);
Error, Semigroups: Matrix:
cannot create a matrix from the given arguments,

# semiringmat: Matrix, for a filter and homogeneous list, 3/3
gap> Matrix(IsIntegerMatrix, [[1, 1], [2, E(8)]]);
Error, Semigroups: Matrix: usage,
the entries in the 2nd argument do not define a matrix of type IsIntegerMatrix\
,

# semiringmat: Matrix, for a semiring and homogeneous list, 1/3
gap> Matrix(Integers, [[1, 1], [2]]);
Error, Semigroups: Matrix: usage,
the 1st argument must be a square table,
gap> Matrix(Integers, [[1, 1, 3], [1, 2, 3]]);
Error, Semigroups: Matrix: usage,
the 1st argument must be a square table,

# semiringmat: Matrix, for a semiring and homogeneous list, 2/3
gap> Matrix(Rationals, [[1, 1], [2, 2]]);
Error, Semigroups: Matrix:
cannot create a matrix from the given arguments,

# semiringmat: Matrix, for a semiring and homogeneous list, 3/3
gap> Matrix(Integers, [[1, 1], [2, E(8)]]);
Error, Semigroups: Matrix: usage,
the entries in the 2nd argument do not define a matrix of type IsIntegerMatrix\
,

# semiringmat: RandomMatrix, 1/6
gap> RandomMatrix(IsIntegerMatrix, 2);;

# semiringmat: RandomMatrix, 2/6
gap> RandomMatrix(IsTropicalMaxPlusMatrix, 2, 2);;

# semiringmat: RandomMatrix, 3/6
gap> RandomMatrix(IsNTPMatrix, 2, 2, 2);;

# semiringmat: RandomMatrix, 4/6
gap> RandomMatrix(GF(7), 2);;

# semiringmat: RandomMatrix, 5/6
gap> RandomMatrix(7, 2);;

# semiringmat: RandomMatrix, 6/6
gap> RandomMatrix(7, 2, 3);
Error, Semigroups: RandomMatrix: usage,
the arguments must be: filter, pos int[, pos int[, pos int]],

# semiringmat: PrintString, DisplayString for a collection
gap> mat := Matrix(IsBooleanMat, [[1, 0, 0, 0], 
>                                 [0, 0, 0, 0], 
>                                 [1, 1, 1, 1], 
>                                 [1, 0, 1, 1]]);
Matrix(IsBooleanMat, [[1, 0, 0, 0], [0, 0, 0, 0], [1, 1, 1, 1], [1, 0, 1, 1]])
gap> Display([mat, mat ^ 2]);
1 0 0 0
0 0 0 0
1 1 1 1
1 0 1 1

1 0 0 0
0 0 0 0
1 1 1 1
1 1 1 1
gap> PrintString([mat, mat ^ 2]);
"\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0, 0]\<, \<\>\>[0, 0, 0, 0]\<, \
\<\>\>[1, 1, 1, 1]\<, \<\>\>[1, 0, 1, 1]\<\<]\<)\<\>\>\>Matrix(\<\>IsBooleanMa\
t\<, \>[\>\>[1, 0, 0, 0]\<, \<\>\>[0, 0, 0, 0]\<, \<\>\>[1, 1, 1, 1]\<, \<\>\>\
[1, 1, 1, 1]\<\<]\<)\<\<"
gap> PrintString(MinimalDClass(Semigroup(mat)));
"\>\>\>GreensDClassOfElement\<(\>\>Semigroup(\>\n\>\>\>Matrix(\<\>IsBooleanMat\
\<, \>[\>\>[1, 0, 0, 0]\<, \<\>\>[0, 0, 0, 0]\<, \<\>\>[1, 1, 1, 1]\<, \<\>\>[\
1, 0, 1, 1]\<\<]\<\<\> \<)\<\<,\< \>\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, \
0, 0, 0]\<, \<\>\>[0, 0, 0, 0]\<, \<\>\>[1, 1, 1, 1]\<, \<\>\>[1, 1, 1, 1]\<\<\
]\<)\<\<)\<\<"

# semiringmat: Display and DisplayString for a matrix
gap> Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -1, 0], 
>                                       [0, -1, -infinity, -infinity],
>                                       [4, 4, 2, -1],
>                                       [1, 1, 0, 3]]);
Matrix(IsProjectiveMaxPlusMatrix, [[0, -infinity, -1, 0], 
  [0, -1, -infinity, -infinity], [4, 4, 2, -1], [1, 1, 0, 3]])
gap> Display(last);
 0 -∞ -1  0
 0 -1 -∞ -∞
 4  4  2 -1
 1  1  0  3
gap> mat :=One(RandomMatrix(IsMinPlusMatrix, 2));
Matrix(IsMinPlusMatrix, [[0, infinity], [infinity, 0]])
gap> Display(mat);
0 ∞
∞ 0
gap> mat := Matrix(IsMaxPlusMatrix, [[0, -infinity, -1, 0], 
>                                    [100, -1, -infinity, -infinity],
>                                    [4, 40, 2, -1],
>                                    [1, 1, 1000, 33313]]);;
gap> Display(mat);
    0    -∞    -1     0
  100    -1    -∞    -∞
    4    40     2    -1
    1     1  1000 33313

# semiringmat: ViewString for a matrix of dim >= 9
gap> mat := RandomMatrix(IsTropicalMinPlusMatrix, 9, 3);
<9x9 tropical min-plus matrix>

# semiringmat: \=, different types, 1/4
gap> Matrix(IsTropicalMinPlusMatrix, [[2, infinity], [infinity, 3]], 3)
> = Matrix(IsTropicalMaxPlusMatrix, [[1, 1], [-infinity, 3]], 3);
false

# semiringmat: \=, different dims, 2/4
gap> Matrix(IsTropicalMinPlusMatrix, [[2, infinity], [infinity, 3]], 3) 
> = Matrix(IsTropicalMinPlusMatrix, [[2]], 3);
false

# semiringmat: \=, different threshold, 3/4
gap> Matrix(IsTropicalMinPlusMatrix, [[2, infinity], [infinity, 3]], 3)
> = Matrix(IsTropicalMinPlusMatrix, [[2, infinity], [infinity, 3]], 4);
false

# semiringmat: \=, different period, 4/4
gap> Matrix(IsNTPMatrix, [[2, 1], [1, 3]], 3, 3)
> = Matrix(IsNTPMatrix, [[2, 1], [1, 3]], 3, 4);
false

# semiringmat: \<, 1/?
gap> mat := Matrix(IsTropicalMinPlusMatrix, 
> [[2, infinity, infinity, 3, 1, 1, infinity, 3, 3],
>  [1, 1, 1, 1, 1, 2, 0, infinity, 0], 
>  [0, 1, 1, 2, 2, 1, 0, infinity, 1],
>  [1, 3, infinity, infinity, 3, 2, 2, 3, 2], 
>  [1, infinity, infinity, 1, 2, 1, 1, 3, 2],
>  [infinity, infinity, 0, 0, 2, 1, 3, 3, infinity], 
>  [infinity, 2, 1, 0, 1, 2, 2, 0, 2],
>  [infinity, 0, 3, infinity, 1, 3, infinity, 2, 3],
>  [0, 2, infinity, 2, 1, 0, infinity, 1, 1]], 3);
<9x9 tropical min-plus matrix>
gap> S := Semigroup(mat);
<commutative semigroup of 9x9 tropical min-plus matrices with 1 generator>
gap> Size(S);
9
gap> AsSet(S);
[ <9x9 tropical min-plus matrix>, <9x9 tropical min-plus matrix>, 
  <9x9 tropical min-plus matrix>, <9x9 tropical min-plus matrix>, 
  <9x9 tropical min-plus matrix>, <9x9 tropical min-plus matrix>, 
  <9x9 tropical min-plus matrix>, <9x9 tropical min-plus matrix>, 
  <9x9 tropical min-plus matrix> ]

# semiringmat: \<, equal matrices, 2/?
gap> mat := Matrix(IsIntegerMatrix, [[-2, 0], [0, 3]]);;
gap> mat < mat;
false

# semiringmat: \<, different types, 3/?
gap> Matrix(IsTropicalMinPlusMatrix, [[2, infinity], [infinity, 3]], 3)
> < Matrix(IsTropicalMaxPlusMatrix, [[1, 1], [-infinity, 3]], 3);
Error, Semigroups:  (for matrices over a semiring):
the matrices are not of the same type,

# semiringmat: \<, different dims, 4/?
gap> Matrix(IsTropicalMinPlusMatrix, [[2, infinity], [infinity, 3]], 3) 
> < Matrix(IsTropicalMinPlusMatrix, [[2]], 3);
false
gap> Matrix(IsTropicalMinPlusMatrix, [[2, infinity], [infinity, 3]], 3) 
> > Matrix(IsTropicalMinPlusMatrix, [[2]], 3);
true

# semiringmat: ChooseHashFunction and use that hash function, 1/1
gap> mat := Matrix(IsTropicalMinPlusMatrix,
> [[2, infinity, infinity, 3, 1, 1, infinity, 3, 3],
>  [1, 1, 1, 1, 1, 2, 0, infinity, 0],
>  [0, 1, 1, 2, 2, 1, 0, infinity, 1],
>  [1, 3, infinity, infinity, 3, 2, 2, 3, 2],
>  [1, infinity, infinity, 1, 2, 1, 1, 3, 2],
>  [infinity, infinity, 0, 0, 2, 1, 3, 3, infinity],
>  [infinity, 2, 1, 0, 1, 2, 2, 0, 2],
>  [infinity, 0, 3, infinity, 1, 3, infinity, 2, 3],
>  [0, 2, infinity, 2, 1, 0, infinity, 1, 1]], 3);
<9x9 tropical min-plus matrix>
gap> ht := HTCreate(mat);
<tree hash table len=100003 used=0 colls=0 accs=0>
gap> for x in Semigroup(mat) do
> HTAdd(ht, x, true);
> od;
gap> ht;
<tree hash table len=100003 used=9 colls=0 accs=9>

#E#
gap> STOP_TEST("Semigroups package: standard/elements/semiringmat.tst");
