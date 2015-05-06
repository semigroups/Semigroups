############################################################################
##
#W  max-plus.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for max-plus, min-plus, tropical max-plus,
# and tropical min-plus matrices.


#############################################################################
## 1. Max-plus matrices
#############################################################################

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

#############################################################################
## 2. Min-plus matrices
#############################################################################

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

#############################################################################
## 3. Tropical max-plus matrices
#############################################################################

DeclareAttribute("ThresholdTropicalMatrix", IsMatrixOverSemiring);

DeclareCategory("IsTropicalMaxPlusMatrix", IsMatrixOverSemiring);
DeclareCategoryCollections("IsTropicalMaxPlusMatrix");
DeclareCategoryCollections("IsTropicalMaxPlusMatrixCollection");

BindGlobal("TropicalMaxPlusMatrixFamily",
           NewFamily("TropicalMaxPlusMatrixFamily",
                     IsTropicalMaxPlusMatrix, CanEasilySortElements,
                     CanEasilySortElements));
BindGlobal("TropicalMaxPlusMatrixType",
           NewType(TropicalMaxPlusMatrixFamily,
                   IsTropicalMaxPlusMatrix));

DeclareGlobalFunction("TropicalMaxPlusMatrix");
DeclareGlobalFunction("TropicalMaxPlusMatrixNC");

DeclareOperation("RandomTropicalMaxPlusMatrix", [IsPosInt, IsPosInt]);
