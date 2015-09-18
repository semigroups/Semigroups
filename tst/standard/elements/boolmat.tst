#############################################################################
##
#W  matrix-boolean.tst
#Y  Copyright (C) 2015                                  James D. Mitchell
##                                                       
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/elements/matrix-boolean.tst");
gap> LoadPackage("semigroups", false);;

#T#
gap> SEMIGROUPS_StartTest();

#T# matrix-boolean: BooleanMat, error 1/6
gap> BooleanMat([[true, false, 1], [0, 1, 0], [false, true, false]]);
Error, Semigroups: BooleanMat: usage,
the argmuent must be a non-empty homogeneous list of homogeneous lists,


#T# matrix-boolean: BooleanMat, for 0s and 1s, 2/6
gap> BooleanMat([[1, 0, 1], [0, 1, 0], [0, 1, 0]]);
<3x3 boolean matrix>

#T# matrix-boolean: BooleanMat, for blists in blist_rep, 3/6
gap> BooleanMat([[true, false, true], [false, true, false],
> [false, true, false]]);
<3x3 boolean matrix>

#T# matrix-boolean: BooleanMat, for blists not in blist_rep, 4/6
gap> BooleanMat(List([1 .. 3], y -> List([1 .. 3], x -> x = x)));
<3x3 boolean matrix>

#T# matrix-boolean: BooleanMat, for successors, 5/6
gap> BooleanMat([[1], [3, 4], [2], [1]]);
<4x4 boolean matrix>

#T# matrix-boolean: BooleanMat, for successors, error, 6/6
gap> BooleanMat([[1], [3, 4], [2], [5]]);
Error, Semigroups: BooleanMat:
the entries of each list must not exceed 4,

#E# 
gap> STOP_TEST("Semigroups package: standard/elements/matrix-boolean.tst");
