############################################################################
##
#W  max-plus.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for max-plus matrices, i.e. matrices with
# entries in the integer union {-infinity} where addition is max and
# multiplication is plus.

DeclareCategory("IsMaxPlusMatrix", IsMatrixOverSemiring);

DeclareCategoryCollections("IsMaxPlusMatrix");
DeclareCategoryCollections("IsMaxPlusMatrixCollection");

BindGlobal("MaxPlusMatrixFamily",
           NewFamily("MaxPlusMatrixFamily",
                     IsMaxPlusMatrix, CanEasilySortElements,
                     CanEasilySortElements));
BindGlobal("MaxPlusMatrixType",
           NewType(MaxPlusMatrixFamily,
                   IsMaxPlusMatrix));

DeclareGlobalFunction("MaxPlusMatrix");
DeclareGlobalFunction("MaxPlusMatrixNC");

DeclareOperation("RandomMaxPlusMatrix", [IsPosInt]);

DeclareGlobalFunction("SEMIGROUPS_HashFunctionMaxPlusMatrix");
