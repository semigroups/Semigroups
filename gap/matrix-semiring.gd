############################################################################
##
#W  semiring-matrix.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for matrices over semirings.

DeclareCategory("IsMatrixOverSemiring",
                    IsMultiplicativeElementWithOne
                and IsAssociativeElement
                and IsPositionalObjectRep);

DeclareCategoryCollections("IsMatrixOverSemiring");
DeclareCategoryCollections("IsMatrixOverSemiringCollection");

DeclareAttribute("DimensionOfMatrixOverSemiring", IsMatrixOverSemiring);

InstallTrueMethod(IsGeneratorsOfSemigroup, IsMatrixOverSemiringCollection);

DeclareOperation("SEMIGROUPS_RandomMatrixOverSemiring",
                 [IsPosInt, IsObject, IsObject]);
DeclareGlobalFunction("SEMIGROUPS_HashFunctionMatrixOverSemiring");

DeclareAttribute("TypeViewStringOfMatrixOverSemiring", IsMatrixOverSemiring);
DeclareAttribute("TypePrintStringOfMatrixOverSemiring", IsMatrixOverSemiring);
