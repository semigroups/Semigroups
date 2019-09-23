############################################################################
##
##  elements/semiringmat.gd
##  Copyright (C) 2015-2022                              James D. Mitchell
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

DeclareOperation("OneMutable", [IsMatrixOverSemiringCollection]);
DeclareAttribute("OneImmutable", IsMatrixOverSemiringCollection);

# IsList rather than IsHomogeneousList to allow us to
# include the threshold and/or period.
DeclareOperation("MatrixNC", [IsType, IsList]);
DeclareOperation("MatrixNC", [IsOperation,
                              IsList]);
DeclareOperation("MatrixNC", [IsOperation,
                              IsList,
                              IsFunction]);
# create another matrix of the same type as the first arg using the second
# arg, and with no checks.
DeclareOperation("MatrixNC", [IsMatrixOverSemiring,
                              IsList]);

DeclareOperation("Matrix", [IsOperation, IsHomogeneousList]);
DeclareOperation("Matrix", [IsOperation, IsHomogeneousList, IsPosInt]);
DeclareOperation("Matrix", [IsOperation, IsHomogeneousList, IsInt, IsInt]);
DeclareOperation("Matrix", [IsSemiring, IsMatrixOverSemiring]);

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

DeclareConstructor("AsMatrix", [IsMatrixOverSemiring,
                                IsMatrixObj,
                                IsPosInt,
                                IsPosInt]);

DeclareAttribute("AsTransformation", IsMatrixOverSemiring);

DeclareOperation("RandomMatrix", [IsOperation, IsPosInt]);
DeclareOperation("RandomMatrix",
                 [IsOperation, IsPosInt, IsInt]);
DeclareOperation("RandomMatrix",
                 [IsOperation, IsPosInt, IsInt, IsInt]);
DeclareOperation("RandomMatrix", [IsSemiring, IsInt]);
DeclareOperation("RandomMatrix", [IsSemiring, IsInt, IsPosInt]);
DeclareOperation("RandomMatrix", [IsSemiring, IsInt, IsList]);

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
DeclareOperation("RandomMatrixOp", [IsField and IsFinite, IsZeroCyc, IsList]);
DeclareOperation("RandomMatrixOp", [IsField and IsFinite, IsPosInt, IsPosInt]);

DeclareAttribute("AsList", IsMatrixOverSemiring);
DeclareOperation("AsMutableList", [IsMatrixOverSemiring]);
DeclareOperation("ELM_LIST", [IsMatrixOverSemiring, IsPosInt]);
DeclareOperation("IsBound[]", [IsMatrixOverSemiring, IsPosInt]);
DeclareOperation("Iterator", [IsMatrixOverSemiring]);
DeclareAttribute("DimensionOfMatrixOverSemiring", IsMatrixOverSemiring);
DeclareAttribute("DimensionOfMatrixOverSemiringCollection",
                 IsMatrixOverSemiringCollection);
DeclareAttribute("TransposedMatImmutable", IsMatrixOverSemiring);
DeclareProperty("IsTorsion", IsMatrixOverSemiring);

# Cannot use TypeObj since it can contain information about
# properties satisfied (or not) by the object.
DeclareAttribute("SEMIGROUPS_TypeViewStringOfMatrixOverSemiring",
                 IsMatrixOverSemiring);
DeclareAttribute("SEMIGROUPS_TypeViewStringOfMatrixOverSemiring",
                 IsMatrixObj);
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

DeclareOperation("MatElm", [IsMatrixOverSemiring, IsPosInt, IsPosInt]);
DeclareAttribute("TransposedMatImmutable", IsMatrixOverSemiring);
DeclareAttribute("NumberRows", IsMatrixOverSemiring);
DeclareAttribute("NumberColumns", IsMatrixOverSemiring);
DeclareProperty("IsZero", IsMatrixOverSemiring);
