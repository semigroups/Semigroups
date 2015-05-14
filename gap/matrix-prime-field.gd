############################################################################
##
#W  matrix-prime-field.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsMatrixOverPrimeField", IsMatrixOverSemiring);

DeclareCategoryCollections("IsMatrixOverPrimeField");
DeclareCategoryCollections("IsMatrixOverPrimeFieldCollection");

BindGlobal("MatrixOverPrimeFieldFamily",
           NewFamily("MatrixOverPrimeFieldFamily",
                     IsMatrixOverPrimeField, CanEasilySortElements,
                     CanEasilySortElements));
BindGlobal("MatrixOverPrimeFieldType",
           NewType(MatrixOverPrimeFieldFamily,
                   IsMatrixOverPrimeField));

DeclareGlobalFunction("MatrixOverPrimeField");
DeclareGlobalFunction("MatrixOverPrimeFieldNC");

DeclareOperation("RandomMatrixOverPrimeField", 
                 [IsPosInt, IsField and IsFinite]);
DeclareOperation("RandomMatrixOverPrimeField", 
                 [IsPosInt, IsPosInt, IsPosInt]);

DeclareOperation("AsMatrix", [IsMatrixOverPrimeField]);

DeclareOperation("AsMatrixOverPrimeField", 
                 [IsFinite and IsField, IsMatrix and IsFFECollColl]);
DeclareOperation("AsMatrixOverPrimeField", 
                 [IsPosInt, IsMatrix and IsFFECollColl]);
DeclareOperation("AsMatrixOverPrimeField", 
                 [IsPosInt, IsMatrix and IsCyclotomicCollColl]);
DeclareOperation("AsMatrixOverPrimeFieldNC", 
                 [IsPosInt, IsObject]);

DeclareGlobalFunction("SEMIGROUPS_HashFunctionForMatrixOverNonPrimeField");
