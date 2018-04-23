#############################################################################
##
#W  standard/semiringmat.tst
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/semiringmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Matrix: return an answer, all possibilities, and TransposedMat
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

# gap> Matrix(GF(3), [[Z(3), Z(3) ^ 0, Z(3)],
# >                   [Z(3), Z(3) ^ 0, Z(3) ^ 0],
# >                   [Z(3), 0 * Z(3), 0 * Z(3)]]);
# Matrix(GF(3), [[Z(3), Z(3)^0, Z(3)], [Z(3), Z(3)^0, Z(3)^0], 
#   [Z(3), 0*Z(3), 0*Z(3)]])
# gap> TransposedMat(TransposedMat(last)) = last;
# true

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

# semiringmat: RandomMatrix, 1
gap> RandomMatrix(IsIntegerMatrix, 2);;

# semiringmat: RandomMatrix, 2
gap> RandomMatrix(IsTropicalMaxPlusMatrix, 2, 2);;

# semiringmat: RandomMatrix, 3
gap> RandomMatrix(IsNTPMatrix, 2, 2, 2);;

# # semiringmat: RandomMatrix, 4
# gap> RandomMatrix(GF(7), 2);;
#
# # semiringmat: RandomMatrix, 5
# gap> RandomMatrix(7, 2);;

# semiringmat: RandomMatrix, 6
gap> RandomMatrix(7, 2, 3);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `RandomMatrix' on 3 arguments

# semiringmat: RandomMatrix, for a semiring, 7
gap> RandomMatrix(Integers, 20);;

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
gap> mat := One(RandomMatrix(IsMinPlusMatrix, 2));
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

# semiringmat: \<, 1/4
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
gap> AsSet(AsList(S));
[ <9x9 tropical min-plus matrix>, <9x9 tropical min-plus matrix>, 
  <9x9 tropical min-plus matrix>, <9x9 tropical min-plus matrix>, 
  <9x9 tropical min-plus matrix>, <9x9 tropical min-plus matrix>, 
  <9x9 tropical min-plus matrix>, <9x9 tropical min-plus matrix>, 
  <9x9 tropical min-plus matrix> ]

# semiringmat: \<, equal matrices, 2/4
gap> mat := Matrix(IsIntegerMatrix, [[-2, 0], [0, 3]]);;
gap> mat < mat;
false

# semiringmat: \<, different types, 3/4
gap> Matrix(IsTropicalMinPlusMatrix, [[2, infinity], [infinity, 3]], 3)
> < Matrix(IsTropicalMaxPlusMatrix, [[1, 1], [-infinity, 3]], 3);
Error, Semigroups: \< (for matrices over a semiring):
the matrices are not of the same type,

# semiringmat: \<, different dims, 4/4
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

# semiringmat: Iterator, for a matrix over semiring 1/1
gap> mat := Matrix(IsIntegerMatrix, [[0, 1, 0], [1, 2, 1], [1, 0, 1]]);;
gap> for row in mat do od;
gap> iter := Iterator(mat);
<iterator>
gap> NextIterator(iter);
[ 0, 1, 0 ]
gap> for x in iter do od;
gap> NextIterator(iter);
fail
gap> iter := ShallowCopy(iter);
<iterator>
gap> IsDoneIterator(iter);
false
gap> for x in iter do od;

# semiringmat: ELM_LIST, 1/1
gap> mat := Matrix(IsIntegerMatrix, [[0, 1, 0], [1, 2, 1], [1, 0, 1]]);;
gap> mat[1];
[ 0, 1, 0 ]
gap> mat[4];
Error, Semigroups: ELM_LIST (for a matrix over semiring):
the position is greater than the dimension of the matrix,

# semiringmat, pickling
gap> S := FullTropicalMaxPlusMonoid(2, 10);
<monoid of 2x2 tropical max-plus matrices with 69 generators>
gap> filename := Concatenation(SEMIGROUPS.PackageDir, "/tst/standard/fulltrop.gz");;
gap> WriteGenerators(filename, [S]);
IO_OK
gap> S = Semigroup(ReadGenerators(filename)[1]);
true

# semiringmat, unpickling boolean mats
gap> S := FullBooleanMatMonoid(4);
<monoid of 4x4 boolean matrices with 7 generators>
gap> filename := Concatenation(SEMIGROUPS.PackageDir, "/tst/standard/fulltrop.gz");;
gap> WriteGenerators(filename, [S]);
IO_OK
gap> S = Semigroup(ReadGenerators(filename)[1]);
true
gap> Exec("rm ", filename);

# semiringmat, IsGeneratorsOfSemigroup
gap> coll := [Matrix(IsIntegerMatrix, [[-1, -1, 0, 3, 0], [-1, 1, 3, 0, 0],
>      [-1, 0, 0, -1, 0], [0, 4, 4, 2, -1], [1, 1, 0, 3, 0]]),
>  Matrix(IsIntegerMatrix, [[-3, 0, 0, 4, 0, 2], [-3, 1, 0, 0, 0, 4],
>      [0, 1, 2, -3, 3, -1], [-2, 0, 0, 3, -1, 1], [0, -3, 3, -1, -1, 1],
>      [1, 1, -2, 0, 0, 0]])];;
gap> IsGeneratorsOfSemigroup(coll);
false
gap> IsGeneratorsOfSemigroup([coll[1]]);
true

# Test AsTransformation
gap> mat := Matrix(IsMaxPlusMatrix, [[-2, 2], [0, -1]]);;
gap> AsTransformation(mat);
fail
gap> mat := Matrix(IsMaxPlusMatrix, [[-infinity, 0], [-infinity, 0]]);;
gap> AsTransformation(mat);
Transformation( [ 2, 2 ] )

# Test AsMatrix
gap> mat := AsMatrix(IsMaxPlusMatrix, Transformation([2, 2]));
Matrix(IsMaxPlusMatrix, [[-infinity, 0], [-infinity, 0]])

# Test IsGeneratorsOfSemigroup
gap> S := Semigroup(Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10));
<commutative semigroup of 2x2 tropical max-plus matrices with 1 generator>
gap> IsGeneratorsOfSemigroup(DClass(S, S.1));
true
gap> coll := [Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10),
>             Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 11)];;
gap> IsGeneratorsOfSemigroup(coll);
false
gap> coll := [Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10),
>             Matrix(IsTropicalMinPlusMatrix, [[2, 2], [0, 1]], 10)];;
gap> IsGeneratorsOfSemigroup(coll);
false
gap> coll := [Matrix(IsTropicalMaxPlusMatrix, [[2]], 10),
>             Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10)];;
gap> IsGeneratorsOfSemigroup(coll);
false
gap> coll := [Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 10),
>             Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 9, 10)];;
gap> IsGeneratorsOfSemigroup(coll);
false
gap> coll := [Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 10),
>             Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 9)];;
gap> IsGeneratorsOfSemigroup(coll);
false

# Test Matrix for a finite field
gap> mat := Matrix(GF(3), [[Z(3)]]);
Matrix(GF(3), [[Z(3)]])
gap> Matrix(GF(3), mat);
Matrix(GF(3), [[Z(3)]])

# Test RandomMatrix for a finite field
gap> mat := RandomMatrix(GF(3), 3, 2);;
gap> RowRank(mat);
2
gap> mat := RandomMatrix(GF(3), 3, [2, 3]);;
gap> RowRank(mat) in [2, 3];
true

# Test OneImmutable, fails
gap> coll := [Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 10),
>             Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 9)];;
gap> OneImmutable(coll);
fail

# Test InverseMutable, fails
gap> InverseMutable(Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 10));
fail

# Test InverseImmutable, fails
gap> InverseImmutable(Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 10));
fail

# Test DimensionOfMatrixOverSemiring for zero dim
gap> mat := Matrix(GF(3), []);;
gap> DimensionOfMatrixOverSemiring(mat);
0

# Test DimensionOfMatrixOverSemiringCollection, for a list
gap> coll := [Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 10),
>             Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 9)];;
gap> DimensionOfMatrixOverSemiringCollection(coll);
2
gap> coll := [Matrix(IsTropicalMaxPlusMatrix, [[2]], 10),
>             Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10)];;
gap> DimensionOfMatrixOverSemiringCollection(coll);
Error, Semigroups: DimensionOfMatrixOverSemiringCollection: usage,
the argument <coll> must be a collection of matrices of equal dimension,

# Test DimensionOfMatrixOverSemiringCollection, for a semigroup
gap> S := Semigroup(Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10));;
gap> DimensionOfMatrixOverSemiringCollection(S);
2

# Test PrintString for 0 dim
gap> mat := Matrix(GF(3), []);;
gap> PrintString(mat);
"\>\>Matrix(\<\>GF(3)\<, \>[]\<)\<"

# Test String
gap> mat := Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 10);;
gap> String(mat);
"Matrix(IsNTPMatrix, [ [ 2, 2 ], [ 0, 1 ] ], 10, 10)"
gap> mat = EvalString(String(mat));
true
gap> mat := Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10);;
gap> String(mat);
"Matrix(IsTropicalMaxPlusMatrix, [ [ 2, 2 ], [ 0, 1 ] ], 10)"
gap> mat = EvalString(String(mat));
true
gap> mat := Matrix(GF(3), []);;
gap> String(mat);
"Matrix(GF(3), [ ])"
gap> mat = EvalString(String(mat));
true

# Test IsBound
gap> mat := Matrix(IsNTPMatrix, [[2, 2], [0, 1]], 10, 10);;
gap> IsBound(mat[10]);
false
gap> IsBound(mat[1]);
true
gap> IsBound(mat[3]);
false

# Test \<
gap> Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10) <
> Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 11);
true
gap> Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10) <
> Matrix(IsTropicalMaxPlusMatrix, [[2, 2], [0, 1]], 10);
false

# Test Matrix for a finite field and list consisting of an empty list
gap> Matrix(GF(3), [[]]);
Error, Semigroups: Matrix: usage,
the 1st argument must be a square table,

# SEMIGROUPS_UnbindVariables
gap> Unbind(S);
gap> Unbind(ht);
gap> Unbind(iter);
gap> Unbind(mat);
gap> Unbind(row);
gap> Unbind(x);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/semiringmat.tst");
