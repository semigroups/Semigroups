############################################################################
##
#W  min-plus.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for min-plus matrices, i.e. matrices with
# entries in the integer union {infinity} where addition is min and
# multiplication is plus.

DeclareCategory("IsMinPlusMatrix", IsMatrixOverSemiring);
DeclareCategoryCollections("IsMinPlusMatrix");
DeclareCategoryCollections("IsMinPlusMatrixCollection");

BindGlobal("MinPlusMatrixFamily",
           NewFamily("MinPlusMatrixFamily",
                     IsMinPlusMatrix, CanEasilySortElements,
                     CanEasilySortElements));
BindGlobal("MinPlusMatrixType",
           NewType(MinPlusMatrixFamily,
                   IsMinPlusMatrix));

DeclareGlobalFunction("MinPlusMatrix");
DeclareGlobalFunction("MinPlusMatrixNC");

DeclareOperation("RandomMinPlusMatrix", [IsPosInt]);
