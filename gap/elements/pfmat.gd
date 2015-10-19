############################################################################
##
#W  pfmat.gd
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

DeclareOperation("AsMatrix", [IsMatrixOverPrimeField]);
