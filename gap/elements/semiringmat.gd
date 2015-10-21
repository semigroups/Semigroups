############################################################################
##
#W  semiringmat.gd
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

InstallTrueMethod(IsGeneratorsOfSemigroup, IsMatrixOverSemiringCollection);

# IsList rather than IsHomogeneousList to allow us to
# include the threshold and/or period.
DeclareOperation("MatrixNC", [IsType, IsList]);
DeclareOperation("MatrixNC", [IsOperation and IsFunction,
                              IsList]);
DeclareOperation("MatrixNC", [IsOperation and IsFunction,
                              IsList,
                              IsFunction]);
# create another matrix of the same type as the first arg using the second
# arg, and with no checks.
DeclareOperation("MatrixNC", [IsMatrixOverSemiring,
                              IsList]);

DeclareOperation("Matrix", [IsFunction and IsOperation, IsHomogeneousList]);
DeclareOperation("Matrix", [IsFunction and IsOperation, IsHomogeneousList,
                            IsPosInt]);
DeclareOperation("Matrix", [IsFunction and IsOperation, IsHomogeneousList,
                            IsPosInt, IsPosInt]);
DeclareOperation("Matrix", [IsSemiring, IsHomogeneousList]);

DeclareGlobalFunction("RandomMatrix");
DeclareConstructor("RandomMatrixCons", [IsMatrixOverSemiring,
                                        IsPosInt]);
DeclareConstructor("RandomMatrixCons", [IsMatrixOverSemiring,
                                        IsPosInt,
                                        IsPosInt]);
DeclareConstructor("RandomMatrixCons", [IsMatrixOverSemiring,
                                        IsPosInt,
                                        IsPosInt,
                                        IsPosInt]);
DeclareOperation("RandomMatrixOp", [IsSemiring, IsPosInt]);

DeclareOperation("ELM_LIST", [IsMatrixOverSemiring, IsPosInt]);
DeclareAttribute("DimensionOfMatrixOverSemiring", IsMatrixOverSemiring);
DeclareAttribute("TransposedMat", IsMatrixOverSemiring);

DeclareGlobalFunction("SEMIGROUPS_HashFunctionMatrixOverSemiring");

# Cannot use TypeObj since it can contain information about
# properties satisfied (or not) by the object.
DeclareAttribute("SEMIGROUPS_TypeViewStringOfMatrixOverSemiring",
                 IsMatrixOverSemiring);
DeclareAttribute("SEMIGROUPS_FilterOfMatrixOverSemiring",
                 IsMatrixOverSemiring);

# Go from IsWhateverKindOfMatrix to the type.
DeclareConstructor("SEMIGROUPS_TypeOfMatrixOverSemiringCons",
                   [IsMatrixOverSemiring]);
DeclareConstructor("SEMIGROUPS_MatrixOverSemiringEntryCheckerCons",
                   [IsMatrixOverSemiring]);
DeclareConstructor("SEMIGROUPS_MatrixOverSemiringEntryCheckerCons",
                   [IsMatrixOverSemiring, IsPosInt]);
DeclareConstructor("SEMIGROUPS_MatrixOverSemiringEntryCheckerCons",
                   [IsMatrixOverSemiring, IsPosInt, IsPosInt]);
