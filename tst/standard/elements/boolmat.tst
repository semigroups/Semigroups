#############################################################################
##
#W  boolmat.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##                                                       
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/elements/boolmat.tst");
gap> LoadPackage("semigroups", false);;

#T#
gap> SEMIGROUPS.StartTest();

#T# boolmat: BooleanMat, error 1/6
gap> BooleanMat([[true, false, 1], [0, 1, 0], [false, true, false]]);
Error, Semigroups: BooleanMat: usage,
the argmuent must be a non-empty homogeneous list of homogeneous lists,

#T# boolmat: BooleanMat, for 0s and 1s, 2/6
gap> BooleanMat([[1, 0, 1], [0, 1, 0], [0, 1, 0]]);
Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 0], [0, 1, 0]])

#T# boolmat: BooleanMat, for blists in blist_rep, 3/6
gap> BooleanMat([[true, false, true], [false, true, false],
> [false, true, false]]);
Matrix(IsBooleanMat, [[1, 0, 1], [0, 1, 0], [0, 1, 0]])

#T# boolmat: BooleanMat, for blists not in blist_rep, 4/6
gap> BooleanMat(List([1 .. 3], y -> List([1 .. 3], x -> x = x)));
Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 1], [1, 1, 1]])

#T# boolmat: BooleanMat, for successors, 5/6
gap> BooleanMat([[1], [3, 4], [2], [1]]);
Matrix(IsBooleanMat, [[1, 0, 0, 0], [0, 0, 1, 1], [0, 1, 0, 0], [1, 0, 0, 0]])

#T# boolmat: BooleanMat, for successors, error, 6/6
gap> BooleanMat([[1], [3, 4], [2], [5]]);
Error, Semigroups: BooleanMat:
the entries of each list must not exceed 4,

#T# boolmat: AsBooleanMat, for transformation and pos int, 1/2
gap> x := Transformation([1, 3, 4, 1, 3]);;
gap> Display(AsBooleanMat(x, 4));
1 0 0 0
0 0 1 0
0 0 0 1
1 0 0 0
gap> AsBooleanMat(x, 3);
Error, Semigroups: AsBooleanMat: usage,
the transformation in the first argument must map [1 .. 3] to itself,

#T# boolmat: AsBooleanMat, for partial perm and pos int, 2/2
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
Error, Semigroups: AsBooleanMat: usage,
the partial perm in the first argument must map [1 .. 3] into itself,

#T# boolmat: SEMIGROUPS_TypePrintStringOfMatrixOverSemiring, 1/1
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> PrintString(x);
"\>\>Matrix(\<\>IsBooleanMat\<, \>[\>\>[1, 0, 0, 1]\<, \<\>\>[1, 0, 1, 0]\<, \
\<\>\>[0, 0, 0, 0]\<, \<\>\>[0, 1, 1, 0]\<\<]\<)\<"

#T# boolmat: \* for boolean mats, 1/2
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> Display(x ^ 2);
1 1 1 1
1 0 0 1
0 0 0 0
1 0 1 0

#T# boolmat: \* for boolean mats, fail, 2/2
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> y := BooleanMat([[1, 0, 0],
>                     [1, 0, 1],
>                     [0, 0, 0]]);;
gap> x * y;
Error, Semigroups: * (for boolean matrices):
the matrices must have equal dimension,

#T# boolmat: OneImmutable, for boolean mats, 1/1
gap> x := BooleanMat([[1, 0, 0],
>                     [1, 0, 1],
>                     [0, 0, 0]]);;
gap> Display(One(x));
1 0 0
0 1 0
0 0 1

#T# boolmat: RandomMatrix, for boolean mats, 1/1
gap> x := RandomMatrix(IsBooleanMat, 1);;

#T# boolmat: \in, for boolean mats, 1/2
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

#T# boolmat: \in, for boolean mats, 2/2
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> y := BooleanMat([[1, 0, 1],
>                     [1, 0, 1],
>                     [0, 1, 0]]);;
gap> x in y;
Error, Semigroups: in: usage,
the arguments <x> and <y> must be boolean matrix of equal size,
gap> y in x;
Error, Semigroups: in: usage,
the arguments <x> and <y> must be boolean matrix of equal size,

#T# boolmat: OnBlist, for boolean mat and blist, 1/1
gap> mat := BooleanMat([[1, 0, 0, 1],
>                       [0, 0, 0, 0],
>                       [1, 0, 1, 1],
>                       [0, 1, 1, 1]]);;
gap> blist := BlistList([1 .. 4], [1, 2]);
[ true, true, false, false ]
gap> OnBlist(blist, mat);
[ true, false, false, true ]

#T# boolmat: Successors, for a boolean mat, 1/1
gap> x := BooleanMat([[1, 0, 0, 1],
>                     [1, 0, 1, 0],
>                     [0, 0, 0, 0],
>                     [0, 1, 1, 0]]);;
gap> Successors(x);
[ [ 1, 4 ], [ 1, 3 ], [  ], [ 2, 3 ] ]

#T# boolmat: Is(Row/Column)TrimBooleanMat, for a boolean mat, 1/2
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 1, 1]]);;
gap> IsTrimBooleanMat(mat);
true

#T# boolmat: Is(Row/Column)TrimBooleanMat, for a boolean mat, 2/2
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [0, 0, 1, 0],
>                       [1, 0, 0, 1],
>                       [1, 0, 1, 0]]);;
gap> IsRowTrimBooleanMat(mat);
false
gap> IsColTrimBooleanMat(mat);
false

#T# boolmat: NumberBooleanMat/BooleanMatNumber, for a boolean mat, 1/2
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 0, 1]]);;
gap> NumberBooleanMat(mat);
27606
gap> BooleanMatNumber(27606, 4) = mat;
true

#T# boolmat: NumberBooleanMat/BooleanMatNumber, for a boolean mat, 2/2
gap> Set(FullBooleanMatMonoid(3), NumberBooleanMat) = [1 .. 2 ^ 9];
true

#T# boolmat: NumberBlist/BlistNumber, for a boolean mat, 1/1
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

#T# boolmat: AsBooleanMat, for everything, 1/1
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

#T# boolmat: ChooseHashFunction and SEMIGROUPS_HashFunctionBooleanMat, for
# boolean mats, 1/1
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 4 generators>
gap> ht := HTCreate(S.1);
<tree hash table len=100003 used=0 colls=0 accs=0>
gap> for x in S do
> HTAdd(ht, x, true);
> od;
gap> ht;
<tree hash table len=100003 used=16 colls=0 accs=16>

#T# boolmat: CanonicalBooleanMat, 1/2
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
Matrix(IsBooleanMat, [[0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 1, 1], 
  [0, 0, 0, 1, 1, 1], [0, 1, 1, 1, 0, 0], [1, 1, 1, 0, 0, 0], 
  [1, 1, 1, 0, 1, 1]])
gap> Display(CanonicalBooleanMat(mat));
0 0 0 0 0 0
0 0 0 0 1 1
0 0 0 1 1 1
0 1 1 1 0 0
1 1 1 0 0 0
1 1 1 0 1 1
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

#T# boolmat: CanonicalBooleanMat, 2/2
gap> mat := BooleanMat([[1, 1, 1, 0, 0, 0],
>                       [0, 0, 0, 1, 0, 1],
>                       [1, 0, 0, 1, 0, 1],
>                       [0, 0, 0, 0, 0, 0],
>                       [0, 1, 1, 1, 1, 1],
>                       [0, 1, 1, 0, 1, 0]]);;
gap> CanonicalBooleanMat(SymmetricGroup(7), mat);
Error, Semigroups: CanonicalBooleanMat: usage,
the largest moved point of the first argument must not exceed the dimension
of the Boolean matrix,

#T# boolmat: IsSymmetricBooleanMat, 1/1
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

#T# boolmat: IsReflexiveBooleanMat, 1/1
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 0, 1]]);
Matrix(IsBooleanMat, [[0, 1, 1, 0], [1, 0, 1, 1], [1, 1, 0, 1], [0, 1, 0, 1]])
gap> IsReflexiveBooleanMat(mat);
false

#T# boolmat: IsTransitiveBooleanMat, 1/1
gap> mat := BooleanMat([[0, 1, 1, 0],
>                       [1, 0, 1, 1],
>                       [1, 1, 0, 1],
>                       [0, 1, 0, 1]]);
Matrix(IsBooleanMat, [[0, 1, 1, 0], [1, 0, 1, 1], [1, 1, 0, 1], [0, 1, 0, 1]])
gap> IsTransitiveBooleanMat(mat);
false

#T# boolmat: IsAntiSymmetricBooleanMat, 1/1
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

#T# boolmat: IsTotalBooleanMat, IsOntoBooleanMat, 1/1
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

#T# boolmat: AsBooleanMat, for a boolean mat, 1/1
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

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(x);
gap> Unbind(y);

#E# 
gap> STOP_TEST("Semigroups package: standard/elements/boolmat.tst");
