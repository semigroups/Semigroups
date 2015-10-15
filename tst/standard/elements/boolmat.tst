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
gap> SEMIGROUPS_StartTest();

#T# boolmat: BooleanMat, error 1/6
gap> BooleanMat([[true, false, 1], [0, 1, 0], [false, true, false]]);
Error, Semigroups: BooleanMat: usage,
the argmuent must be a non-empty homogeneous list of homogeneous lists,

#T# boolmat: BooleanMat, for 0s and 1s, 2/6
gap> BooleanMat([[1, 0, 1], [0, 1, 0], [0, 1, 0]]);
<3x3 boolean matrix>

#T# boolmat: BooleanMat, for blists in blist_rep, 3/6
gap> BooleanMat([[true, false, true], [false, true, false],
> [false, true, false]]);
<3x3 boolean matrix>

#T# boolmat: BooleanMat, for blists not in blist_rep, 4/6
gap> BooleanMat(List([1 .. 3], y -> List([1 .. 3], x -> x = x)));
<3x3 boolean matrix>

#T# boolmat: BooleanMat, for successors, 5/6
gap> BooleanMat([[1], [3, 4], [2], [1]]);
<4x4 boolean matrix>

#T# boolmat: BooleanMat, for successors, error, 6/6
gap> BooleanMat([[1], [3, 4], [2], [5]]);
Error, Semigroups: BooleanMat:
the entries of each list must not exceed 4,

#T# boolmat: AsBooleanMat, for transformation and pos int
gap> x := Transformation( [ 1, 3, 4, 1, 3 ] );;
gap> Display(AsBooleanMat(x, 4));
1 0 0 0
0 0 1 0
0 0 0 1
1 0 0 0
gap> AsBooleanMat(x, 3);
Error, Semigroups: AsBooleanMat: usage,
the transformation in the first argument must map [1 .. 3] to itself,

#T# boolmat: AsBooleanMat, for partial perm and pos int
gap> x := PartialPerm( [ 1, 2, 3, 6, 8, 10 ], 
>                      [ 2, 6, 7, 9, 1, 5 ] );
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

#E# 
gap> STOP_TEST("Semigroups package: standard/elements/boolmat.tst");
