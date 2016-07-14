#############################################################################
##
#W  standard/ffmat.tst
#Y  Copyright (C) 2015                                     Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/ffmat.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

#T# MatrixTest1: Create
gap> M := NewMatrixOverFiniteField(IsPlistMatrixOverFiniteFieldRep, GF(2),
> Z(2) * [[1, 0, 1, 0], [0, 1, 0, 1], [0, 1, 0, 0], [0, 0, 0, 1]]);
Matrix(GF(2), [[Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2)], 
  [0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0], [0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2)], 
  [0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0]])

#T# MatrixTest2: EvalPrintString
gap> EvalString(String(M)) = M;
true

#T# SEMIGROUPS_UnbindVariables
gap> Unbind(M);

#E#
gap> STOP_TEST("Semigroups package: standard/ffmat.tst");
