############################################################################
##
#W  matrix-finite-field.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsMatrixOverFiniteField", IsMatrixOverSemiring);

DeclareCategoryCollections("IsMatrixOverFiniteField");
DeclareCategoryCollections("IsMatrixOverFiniteFieldCollection");

BindGlobal("MatrixOverFiniteFieldFamily",
           NewFamily("MatrixOverFiniteFieldFamily",
                     IsMatrixOverFiniteField, CanEasilySortElements,
                     CanEasilySortElements));
BindGlobal("MatrixOverFiniteFieldType",
           NewType(MatrixOverFiniteFieldFamily,
                   IsMatrixOverFiniteField));

DeclareGlobalFunction("MatrixOverFiniteField");
DeclareGlobalFunction("MatrixOverFiniteFieldNC");

DeclareOperation("RandomMatrixOverFiniteField", 
                 [IsPosInt, IsField and IsFinite]);
DeclareOperation("RandomMatrixOverFiniteField", 
                 [IsPosInt, IsPosInt, IsPosInt]);

DeclareOperation("AsMatrix", [IsMatrixOverFiniteField]);

DeclareOperation("AsMatrixOverFiniteField", 
                 [IsFinite and IsField, IsMatrix and IsFFECollColl]);
DeclareOperation("AsMatrixOverFiniteField", 
                 [IsPosInt, IsMatrix and IsFFECollColl]);
DeclareOperation("AsMatrixOverFiniteField", 
                 [IsPosInt, IsMatrix and IsCyclotomicCollColl]);
DeclareOperation("AsMatrixOverFiniteFieldNC", 
                 [IsPosInt, IsObject]);
