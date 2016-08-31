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
                IsMultiplicativeElementWithInverse);

DeclareCategoryCollections("IsMatrixOverSemiring");
DeclareCategoryCollections("IsMatrixOverSemiringCollection");

DeclareRepresentation("IsPlistMatrixOverSemiringPositionalRep",
                      IsMatrixOverSemiring and IsPositionalObjectRep, 1);

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
                            IsInt, IsInt]);
DeclareOperation("Matrix", [IsSemiring, IsHomogeneousList]);

DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsMatrixOverSemiring]);
DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsMatrix]);
DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsMatrixOverSemiring,
                                IsPosInt]);
DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsMatrixOverSemiring,
                                IsMatrix]);
DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsMatrixOverSemiring,
                                IsPosInt,
                                IsPosInt]);
DeclareConstructor("AsMatrix", [IsSemiring, IsMatrixOverSemiring]);

DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsTransformation,
                                IsPosInt]);
DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsTransformation,
                                IsPosInt,
                                IsPosInt]);
DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsTransformation,
                                IsPosInt,
                                IsPosInt,
                                IsPosInt]);
DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsTransformation]);

DeclareAttribute("AsTransformation", IsMatrixOverSemiring);

DeclareGlobalFunction("RandomMatrix");
DeclareConstructor("RandomMatrixCons", [IsMatrixOverSemiring,
                                        IsPosInt]);
DeclareConstructor("RandomMatrixCons", [IsMatrixOverSemiring,
                                        IsPosInt,
                                        IsPosInt]);
DeclareConstructor("RandomMatrixCons", [IsMatrixOverSemiring,
                                        IsPosInt,
                                        IsInt,
                                        IsInt]);
DeclareOperation("RandomMatrixOp", [IsSemiring, IsPosInt]);
DeclareOperation("RandomMatrixOp", [IsField and IsFinite, IsPosInt, IsList]);

DeclareAttribute("AsList", IsMatrixOverSemiring);
DeclareOperation("AsMutableList", [IsMatrixOverSemiring]);
DeclareOperation("ELM_LIST", [IsMatrixOverSemiring, IsPosInt]);
DeclareOperation("IsBound[]", [IsMatrixOverSemiring, IsPosInt]);
DeclareOperation("Iterator", [IsMatrixOverSemiring]);
DeclareAttribute("DimensionOfMatrixOverSemiring", IsMatrixOverSemiring);
DeclareAttribute("DimensionOfMatrixOverSemiringCollection",
                 IsMatrixOverSemiringCollection);
DeclareAttribute("TransposedMat", IsMatrixOverSemiring);
DeclareAttribute("IsTorsion", IsMatrixOverSemiring);

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
                   [IsMatrixOverSemiring, IsInt, IsInt]);
