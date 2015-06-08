#############################################################################
##
#W  matrix.tst
#Y  Copyright (C) 2015                                       Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

gap> START_TEST("Semigroups package: matrix.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SemigroupsStartTest();;

#T# MatrixTest1: Create
gap> M := NewSMatrix(IsPlistSMatrixRep, GF(2), 4,
> Z(2) * [[1,0,1,0],[0,1,0,1],[0,1,0,0],[0,0,0,1]]);
<s-matrix of degree 4 over GF(2)>

#T# MatrixTest2: EvalPrintString
gap> EvalString(PrintString(M));
<s-matrix of degree 4 over GF(2)>

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(M);

#E#
gap> STOP_TEST("Semigroups package: matrix.tst");
