#############################################################################
##
#W  standard/elements/boolmat.tst
#Y  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local BMats, S, a, b, blist, blists, c, d, e, ht, mat, x, y, z
gap> START_TEST("Semigroups package: standard/elements/boolmat.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# boolmat: BooleanMat, error 1/7
gap> BooleanMat([[true, false, 1], [0, 1, 0], [false, true, false]]);
Error, the argument is not a non-empty list of homogeneous lists

# boolmat: BooleanMat, for 0s and 1s, 2/7
gap> BooleanMat([[1, 0, 1], [0, 1, 0], [0, 1, 0]]);
Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 0], [0, 1, 0]])

# boolmat: BooleanMat, for blists in blist_rep, 3/7
gap> BooleanMat([[true, false, true], [false, true, false],
> [false, true, false]]);
Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 0], [0, 1, 0]])

# boolmat: BooleanMat, for blists not in blist_rep, 4/7
gap> BooleanMat(List([1 .. 3], y -> List([1 .. 3], x -> x = x)));
Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [1, 1, 1]])

# boolmat: BooleanMat, for successors, 5/7
gap> BooleanMat([[1], [3, 4], [2], [1]]);
Matrix(IsBooleanMat, [[1, 0, 0, 0], [0, 0, 1, 1], [0, 1, 0, 0], [1, 0, 0, 0]])

# boolmat: BooleanMat, for successors, 6/7
gap> BooleanMat([[1], []]);
Matrix(IsBooleanMat, [[1, 0], [0, 0]])

# boolmat: BooleanMat, for successors, error, 7/7
gap> BooleanMat([[1], [3, 4], [2], [5]]);
Error, the entries of each list must not exceed 4

# boolmat: AsBooleanMat, for transformation and pos int, 1/2
gap> x := Transformation([1, 3, 4, 1, 3]);;
gap> Display(AsBooleanMat(x, 4));
1 0 0 0
0 0 1 0
0 0 0 1
1 0 0 0
gap> AsBooleanMat(x, 3);
Error, the transformation in the 1st argument does not map [1 .. 3] to itself

# boolmat: AsBooleanMat, for partial perm and pos int, 2/2
gap> x := PartialPerm([1, 2, 3, 6, 8, 10],
>                      [2, 6, 7, 9, 1, 5]);
[3,7][8,1,2,6,9][10,5]
gap> Display(AsBooleanMat(x));
0 1 0 0 0 0 0 0 0 0
0 0 0 0 0 1 0 0 0 0
0 0 0 0 0 0 1 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 1 0
0 0 0 0 0 0 0 0 0 0
1 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 1 0 0 0 0 0
gap> Display(AsBooleanMat(x, 3));
Error, the partial perm in the 1st argument does not map [1 .. 3] into itself

# boolmat: SEMIGROUPS_TypePrintStringOfMatrixOverSemiring, 1/1
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> PrintString(x);
"\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0, 1]\<, \<\>\>[1, 0, 1, 0]\<, \
\<\>\>[0, 0, 0, 0]\<, \<\>\>[0, 1, 1, 0]\<\<]\<)\<"

# boolmat: \* for boolean mats, 1/2
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> Display(x ^ 2);
1 1 1 1
1 0 0 1
0 0 0 0
1 0 1 0

# boolmat: \* for boolean mats, fail, 2/2
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> y := BooleanMat([[1, 0, 0],
>                     [1, 0, 1],
>                     [0, 0, 0]]);;
gap> x * y;
Matrix(IsBooleanMat, [[1, 0, 0], [1, 0, 0], [0, 0, 0]])

# boolmat: \< for boolean mats, 1
gap> a := BooleanMat([[0]]);;
gap> b := BooleanMat([[0, 1],
>                     [0, 0]]);;
gap> c := BooleanMat([[1, 0, 1],
>                     [1, 1, 0],
>                     [1, 0, 0]]);;
gap> d := BooleanMat([[1, 0, 1],
>                     [1, 1, 0],
>                     [1, 0, 1]]);;
gap> e := BooleanMat([[1, 0, 1],
>                     [1, 1, 0],
>                     [1, 0, 1]]);;

# test different dimensions
gap> a < b;
true
gap> a < c;
true
gap> b < a;
false
gap> b < c;
true
gap> c < a;
false
gap> c < b;
false

# test identical obj
gap> a < a;
false
gap> b < b;
false
gap> c < c;
false
gap> d < d;
false
gap> e < e;
false

# test same dimension
gap> c < d;
true
gap> c < e;
true
gap> d < c;
false
gap> d < e;
false
gap> e < c;
false
gap> e < d;
false

# boolmat: OneImmutable, for boolean mats, 1/1
gap> x := BooleanMat([[1, 0, 0],
>                     [1, 0, 1],
>                     [0, 0, 0]]);;
gap> Display(One(x));
1 0 0
0 1 0
0 0 1

# boolmat: RandomMatrix, for boolean mats, 1/1
gap> x := RandomMatrix(IsBooleanMat, 1);;

# boolmat: \in, for boolean mats, 1/2
gap> x := BooleanMat([[1, 0, 0],
>                     [1, 0, 1],
>                     [0, 0, 0]]);;
gap> y := BooleanMat([[1, 0, 1],
>                     [1, 0, 1],
>                     [0, 1, 0]]);;
gap> x in y;
true
gap> y in x;
false

# boolmat: \in, for boolean mats, 2/2
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> y := BooleanMat([[1, 0, 1],
>                     [1, 0, 1],
>                     [0, 1, 0]]);;
gap> x in y;
Error, the arguments (boolean matrices) do not have equal dimensions
gap> y in x;
Error, the arguments (boolean matrices) do not have equal dimensions

# boolmat: OnBlist, for boolean mat and blist, 1/1
gap> mat := BooleanMat([[1, 0, 0, 1],
>                       [0, 0, 0, 0],
>                       [1, 0, 1, 1],
>                       [0, 1, 1, 1]]);;
gap> blist := BlistList([1 .. 4], [1, 2]);
[ true, true, false, false ]
gap> OnBlist(blist, mat);
[ true, false, false, true ]

# boolmat: Successors, for a boolean mat, 1/1
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> Successors(x);
[ [ 1, 4 ], [ 1, 3 ], [  ], [ 2, 3 ] ]

# boolmat: Is(Row/Column)TrimBooleanMat, for a boolean mat, 1/4
gap> mat := BooleanMat([[1, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 1, 1]]);;
gap> IsRowTrimBooleanMat(mat);
true
gap> IsColTrimBooleanMat(mat);
true
gap> IsTrimBooleanMat(mat);
true

# boolmat: Is(Row/Column)TrimBooleanMat, for a boolean mat, 2/4
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [0, 0, 1, 0],
>                       [1, 0, 0, 1],
>                       [1, 0, 1, 0]]);;
gap> IsRowTrimBooleanMat(mat);
false
gap> IsColTrimBooleanMat(mat);
false

# boolmat: Is(Row/Column)TrimBooleanMat, for a boolean mat, 3/4
gap> mat := BooleanMat([[1, 0, 1, 0],
>                       [0, 0, 1, 0],
>                       [1, 0, 0, 1],
>                       [1, 1, 1, 0]]);;
gap> IsRowTrimBooleanMat(mat);
false
gap> IsColTrimBooleanMat(mat);
false
gap> IsTrimBooleanMat(mat);
false

# boolmat: Is(Row/Column)TrimBooleanMat, for a boolean mat, 4/4
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [0, 1, 0, 1],
>                       [1, 0, 0, 1],
>                       [0, 0, 1, 1]]);;
gap> IsRowTrimBooleanMat(mat);
true
gap> IsColTrimBooleanMat(mat);
false
gap> IsTrimBooleanMat(mat);
false

# boolmat: NumberBooleanMat/BooleanMatNumber, for a boolean mat, 1/2
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 0, 1]]);;
gap> NumberBooleanMat(mat);
27606
gap> BooleanMatNumber(27606, 4) = mat;
true

# boolmat: NumberBooleanMat/BooleanMatNumber, for a boolean mat, 2/2
gap> Set(FullBooleanMatMonoid(3), NumberBooleanMat) = [1 .. 2 ^ 9];
true

# boolmat: NumberBlist/BlistNumber, for a boolean mat, 1/1
gap> blist := BlistList([1 .. 10], []);
[ false, false, false, false, false, false, false, false, false, false ]
gap> NumberBlist(blist);
1
gap> blist := BlistList([1 .. 10], [10]);
[ false, false, false, false, false, false, false, false, false, true ]
gap> NumberBlist(blist);
2
gap> BlistNumber(1, 10);
[ false, false, false, false, false, false, false, false, false, false ]
gap> BlistNumber(2, 10);
[ false, false, false, false, false, false, false, false, false, true ]

# boolmat: AsBooleanMat, for everything, 1/1
gap> Display(AsBooleanMat((1, 2), 5));
0 1 0 0 0
1 0 0 0 0
0 0 1 0 0
0 0 0 1 0
0 0 0 0 1
gap> Display(AsBooleanMat((1, 2)));
0 1
1 0
gap> x := Transformation([1, 3, 4, 1, 3]);;
gap> Display(AsBooleanMat(x));
1 0 0 0 0
0 0 1 0 0
0 0 0 1 0
1 0 0 0 0
0 0 1 0 0
gap> Display(AsBooleanMat(x, 4));
1 0 0 0
0 0 1 0
0 0 0 1
1 0 0 0
gap> x := PartialPerm([1, 2, 3, 6, 8, 10],
>                      [2, 6, 7, 9, 1, 5]);
[3,7][8,1,2,6,9][10,5]
gap> Display(AsBooleanMat(x));
0 1 0 0 0 0 0 0 0 0
0 0 0 0 0 1 0 0 0 0
0 0 0 0 0 0 1 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 1 0
0 0 0 0 0 0 0 0 0 0
1 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0
0 0 0 0 1 0 0 0 0 0
gap> x := Bipartition([[1, 4, -2, -3],
>                        [2, 3, 5, -5], [-1, -4]]);
<bipartition: [ 1, 4, -2, -3 ], [ 2, 3, 5, -5 ], [ -1, -4 ]>
gap> y := AsBooleanMat(x);
<10x10 boolean matrix>
gap> Display(y);
1 0 0 1 0 0 1 1 0 0
0 1 1 0 1 0 0 0 0 1
0 1 1 0 1 0 0 0 0 1
1 0 0 1 0 0 1 1 0 0
0 1 1 0 1 0 0 0 0 1
0 0 0 0 0 1 0 0 1 0
1 0 0 1 0 0 1 1 0 0
1 0 0 1 0 0 1 1 0 0
0 0 0 0 0 1 0 0 1 0
0 1 1 0 1 0 0 0 0 1
gap> IsEquivalenceBooleanMat(y);
true
gap> AsBooleanMat(x, 1);
Matrix(IsBooleanMat, [[1]])
gap> Display(AsBooleanMat(x, 1));
1
gap> Display(AsBooleanMat(x, 2));
1 0
0 1
gap> Display(AsBooleanMat(x, 3));
1 0 0
0 1 1
0 1 1
gap> Display(AsBooleanMat(x, 11));
1 0 0 1 0 0 1 1 0 0 0
0 1 1 0 1 0 0 0 0 1 0
0 1 1 0 1 0 0 0 0 1 0
1 0 0 1 0 0 1 1 0 0 0
0 1 1 0 1 0 0 0 0 1 0
0 0 0 0 0 1 0 0 1 0 0
1 0 0 1 0 0 1 1 0 0 0
1 0 0 1 0 0 1 1 0 0 0
0 0 0 0 0 1 0 0 1 0 0
0 1 1 0 1 0 0 0 0 1 0
0 0 0 0 0 0 0 0 0 0 0
gap> x := PBR([[-1, 1], [2, 3], [-3, 2, 3]],
>             [[-1, 1, 2], [-3, -1, 1, 3],
>               [-3, -1, 1, 2, 3]]);;
gap> AsBooleanMat(x);
Matrix(IsBooleanMat, [[1, 0, 0, 1, 0, 0], [0, 1, 1, 0, 0, 0], 
  [0, 1, 1, 0, 0, 1], [1, 1, 0, 1, 0, 0], [1, 0, 1, 1, 0, 1], 
  [1, 1, 1, 1, 0, 1]])
gap> Display(AsBooleanMat(x));
1 0 0 1 0 0
0 1 1 0 0 0
0 1 1 0 0 1
1 1 0 1 0 0
1 0 1 1 0 1
1 1 1 1 0 1
gap> x := PBR([[-1, 1], [2, 3], [-3, 2, 3]],
>             [[-1, 1, 2], [-3, -1, 1, 3],
>               [-3, -1, 1, 2, 3]]);;
gap> Display(AsBooleanMat(x, 8));
1 0 0 1 0 0 0 0
0 1 1 0 0 0 0 0
0 1 1 0 0 1 0 0
1 1 0 1 0 0 0 0
1 0 1 1 0 1 0 0
1 1 1 1 0 1 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0

# boolmat: ChooseHashFunction and SEMIGROUPS_HashFunctionBooleanMat, for
# boolean mats, 1/1
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> ht := HTCreate(S.1);
<tree hash table len=100003 used=0 colls=0 accs=0>
gap> for x in S do
> HTAdd(ht, x, true);
> od;
gap> ht;
<tree hash table len=100003 used=16 colls=0 accs=16>

# boolmat: CanonicalBooleanMat, 1/3
gap> mat := BooleanMat([[1, 1, 1, 0, 0, 0],
>                       [0, 0, 0, 1, 0, 1],
>                       [1, 0, 0, 1, 0, 1],
>                       [0, 0, 0, 0, 0, 0],
>                       [0, 1, 1, 1, 1, 1],
>                       [0, 1, 1, 0, 1, 0]]);
Matrix(IsBooleanMat, [[1, 1, 1, 0, 0, 0], [0, 0, 0, 1, 0, 1], 
  [1, 0, 0, 1, 0, 1], [0, 0, 0, 0, 0, 0], [0, 1, 1, 1, 1, 1], 
  [0, 1, 1, 0, 1, 0]])
gap> CanonicalBooleanMat(mat);
Matrix(IsBooleanMat, [[0, 0, 0, 0, 0, 0], [1, 1, 0, 0, 0, 0], 
  [0, 0, 1, 1, 1, 0], [1, 1, 0, 0, 1, 0], [0, 0, 1, 1, 0, 1], 
  [1, 1, 1, 1, 0, 1]])
gap> Display(CanonicalBooleanMat(mat));
0 0 0 0 0 0
1 1 0 0 0 0
0 0 1 1 1 0
1 1 0 0 1 0
0 0 1 1 0 1
1 1 1 1 0 1
gap> Display(CanonicalBooleanMat(Group((1, 3)), mat));
0 1 1 0 0 1
0 0 1 0 0 1
1 1 0 1 0 0
0 0 0 0 0 0
1 0 1 1 1 1
1 0 0 1 1 0
gap> Display(CanonicalBooleanMat(Group((1, 3)), Group(()), mat));
1 1 1 0 0 0
0 0 0 1 0 1
0 1 0 1 0 1
0 0 0 0 0 0
1 0 1 1 1 1
1 0 1 0 1 0

# boolmat: CanonicalBooleanMat, 2/3
gap> mat := BooleanMat([[1, 1, 1, 0, 0, 0],
>                       [0, 0, 0, 1, 0, 1],
>                       [1, 0, 0, 1, 0, 1],
>                       [0, 0, 0, 0, 0, 0],
>                       [0, 1, 1, 1, 1, 1],
>                       [0, 1, 1, 0, 1, 0]]);;
gap> CanonicalBooleanMat(SymmetricGroup(7), mat);
Error, the largest moved point of the 1st argument (a perm group) exceeds the \
dimension of the 3rd argument (a boolean matrix)

# boolmat: CanonicalBooleanMat (check that bliss is used in all 3 versions), 3/3
gap> mat := Matrix(IsBooleanMat, [[1, 0, 1, 1, 0, 0, 0, 1],
>                                 [0, 1, 0, 0, 1, 1, 0, 1],
>                                 [0, 1, 0, 0, 0, 1, 0, 1],
>                                 [1, 0, 0, 1, 1, 1, 1, 1],
>                                 [1, 1, 1, 0, 0, 1, 1, 0],
>                                 [1, 0, 1, 0, 1, 0, 0, 1],
>                                 [0, 0, 0, 0, 0, 0, 1, 1],
>                                 [1, 0, 1, 0, 0, 1, 1, 1]]);;
gap> CanonicalBooleanMat(mat);
Matrix(IsBooleanMat, [[0, 1, 0, 0, 0, 0, 0, 1], [1, 0, 1, 0, 0, 0, 0, 1], 
  [1, 0, 1, 0, 1, 0, 0, 1], [0, 0, 0, 1, 1, 1, 0, 1], 
  [0, 0, 0, 1, 0, 1, 1, 1], [1, 1, 1, 1, 0, 1, 0, 0], 
  [1, 1, 0, 1, 0, 1, 0, 1], [1, 1, 0, 0, 1, 1, 1, 1]])
gap> CanonicalBooleanMat(SymmetricGroup(8), mat);
Matrix(IsBooleanMat, [[0, 1, 0, 0, 0, 0, 0, 1], [1, 0, 1, 0, 0, 0, 0, 1], 
  [1, 0, 1, 0, 1, 0, 0, 1], [0, 0, 0, 1, 1, 1, 0, 1], 
  [0, 0, 0, 1, 0, 1, 1, 1], [1, 1, 1, 1, 0, 1, 0, 0], 
  [1, 1, 0, 1, 0, 1, 0, 1], [1, 1, 0, 0, 1, 1, 1, 1]])
gap> CanonicalBooleanMat(SymmetricGroup(8),
>                        Group((1, 2), (1, 2, 3, 4, 5, 6, 7, 8)),
>                        mat);
Matrix(IsBooleanMat, [[0, 1, 0, 0, 0, 0, 0, 1], [1, 0, 1, 0, 0, 0, 0, 1], 
  [1, 0, 1, 0, 1, 0, 0, 1], [0, 0, 0, 1, 1, 1, 0, 1], 
  [0, 0, 0, 1, 0, 1, 1, 1], [1, 1, 1, 1, 0, 1, 0, 0], 
  [1, 1, 0, 1, 0, 1, 0, 1], [1, 1, 0, 0, 1, 1, 1, 1]])

# boolmat: IsSymmetricBooleanMat, 1/1
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 0, 1]]);
Matrix(IsBooleanMat, [[0, 1, 1, 0], [1, 0, 1, 1], [1, 1, 0, 1], [0, 1, 0, 1]])
gap> IsSymmetricBooleanMat(mat);
false
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 1, 1]]);
Matrix(IsBooleanMat, [[0, 1, 1, 0], [1, 0, 1, 1], [1, 1, 0, 1], [0, 1, 1, 1]])
gap> IsSymmetricBooleanMat(mat);
true

# boolmat: IsReflexiveBooleanMat, 1/1
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 0, 1]]);
Matrix(IsBooleanMat, [[0, 1, 1, 0], [1, 0, 1, 1], [1, 1, 0, 1], [0, 1, 0, 1]])
gap> IsReflexiveBooleanMat(mat);
false

# boolmat: IsTransitiveBooleanMat, 1/1
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 0, 1]]);
Matrix(IsBooleanMat, [[0, 1, 1, 0], [1, 0, 1, 1], [1, 1, 0, 1], [0, 1, 0, 1]])
gap> IsTransitiveBooleanMat(mat);
false

# boolmat: IsAntiSymmetricBooleanMat, 1/1
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 1],
>                     [1, 1, 1, 0],
>                     [0, 1, 1, 0]]);
Matrix(IsBooleanMat, [[1, 0, 0, 1], [1, 0, 1, 1], [1, 1, 1, 0], [0, 1, 1, 0]])
gap> IsAntiSymmetricBooleanMat(x);
false
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [1, 0, 1, 0],
>                     [0, 1, 1, 0]]);
Matrix(IsBooleanMat, [[1, 0, 0, 1], [1, 0, 1, 0], [1, 0, 1, 0], [0, 1, 1, 0]])
gap> IsAntiSymmetricBooleanMat(x);
true

# boolmat: IsTotalBooleanMat, IsOntoBooleanMat, 1/1
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 1],
>                     [1, 1, 1, 0],
>                     [0, 1, 1, 0]]);
Matrix(IsBooleanMat, [[1, 0, 0, 1], [1, 0, 1, 1], [1, 1, 1, 0], [0, 1, 1, 0]])
gap> IsTotalBooleanMat(x);
true
gap> IsOntoBooleanMat(x);
true
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);
Matrix(IsBooleanMat, [[1, 0, 0, 1], [1, 0, 1, 0], [0, 0, 0, 0], [0, 1, 1, 0]])
gap> IsTotalBooleanMat(x);
false
gap> IsOntoBooleanMat(x);
true

# boolmat: AsBooleanMat, for a boolean mat, 1/2
gap> mat := Matrix(IsBooleanMat, [[1, 0, 0, 1],
>                                 [0, 1, 1, 0],
>                                 [1, 0, 1, 1],
>                                 [0, 0, 0, 1]]);;
gap> AsBooleanMat(mat, 2);
Matrix(IsBooleanMat, [[1, 0], [0, 1]])
gap> AsBooleanMat(mat, 6);
Matrix(IsBooleanMat, [[1, 0, 0, 1, 0, 0], [0, 1, 1, 0, 0, 0], 
  [1, 0, 1, 1, 0, 0], [0, 0, 0, 1, 0, 0], [0, 0, 0, 0, 0, 0], 
  [0, 0, 0, 0, 0, 0]])

# boolmat: AsBooleanMat, AsDigraph for a digraph, boolean mat 2/2
gap> x := Digraph([[], [8, 10], [1, 10], [5], [8], [], [10], [], [9], []]);
<immutable digraph with 10 vertices, 8 edges>
gap> AsDigraph(AsBooleanMat(x)) = x;
true
gap> x := Matrix(IsBooleanMat, [[1, 0, 0, 1, 0],
>                               [1, 1, 0, 1, 0],
>                               [1, 1, 0, 0, 0],
>                               [1, 1, 1, 1, 0],
>                               [0, 0, 0, 0, 0]]);;
gap> AsBooleanMat(AsDigraph(mat)) = mat;
true

# Fix converter for BMats in the kernel, prior to PR #647 the size of returned
# below was 4, which is nonsense.
gap> S := Semigroup([
> Matrix(IsBooleanMat,
>        [[false, true], [false, true]]),
> Matrix(IsBooleanMat,
>        [[true, false], [true, false]])]);
<semigroup of 2x2 boolean matrices with 2 generators>
gap> Union2(AsList(S.2)[1], AsList(S.2)[2]);;
gap> Size(S);
2

# IsTransformationBooleanMat
gap> x := Transformation([10, 8, 4, 6, 4, 5, 3, 8, 8, 2]);
Transformation( [ 10, 8, 4, 6, 4, 5, 3, 8, 8, 2 ] )
gap> y := AsBooleanMat(x);
<10x10 boolean matrix>
gap> IsTransformationBooleanMat(y);
true
gap> AsTransformation(y) = x;
true
gap> z := Matrix(IsBooleanMat,
>           [[1, 1, 1, 1], 
>            [1, 1, 1, 1], 
>            [0, 1, 0, 1], 
>            [1, 1, 0, 1]]);
Matrix(IsBooleanMat, [[1, 1, 1, 1], [1, 1, 1, 1], [0, 1, 0, 1], [1, 1, 0, 1]])
gap> IsTransformationBooleanMat(z);
false
gap> AsTransformation(z);
fail

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/elements/oolmat.tst");
