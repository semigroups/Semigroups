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
# tropical min-plus matrices, and natural number matrices.

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
## 3. Tropical matrices
#############################################################################

DeclareCategory("IsTropicalMatrix", IsMatrixOverSemiring);
DeclareCategoryCollections("IsTropicalMatrix");
DeclareAttribute("ThresholdTropicalMatrix", IsTropicalMatrix);

#############################################################################
## 4. Tropical max-plus matrices
#############################################################################

DeclareCategory("IsTropicalMaxPlusMatrix", IsTropicalMatrix);
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

#############################################################################
## 5. Tropical min-plus matrices
#############################################################################

DeclareCategory("IsTropicalMinPlusMatrix", IsTropicalMatrix);
DeclareCategoryCollections("IsTropicalMinPlusMatrix");
DeclareCategoryCollections("IsTropicalMinPlusMatrixCollection");

BindGlobal("TropicalMinPlusMatrixFamily",
           NewFamily("TropicalMinPlusMatrixFamily",
                     IsTropicalMinPlusMatrix, CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("TropicalMinPlusMatrixType",
           NewType(TropicalMinPlusMatrixFamily,
                   IsTropicalMinPlusMatrix));

DeclareGlobalFunction("TropicalMinPlusMatrix");
DeclareGlobalFunction("TropicalMinPlusMatrixNC");

DeclareOperation("RandomTropicalMinPlusMatrix", [IsPosInt, IsPosInt]);

#############################################################################
## 5. Projective max-plus matrices
#############################################################################

DeclareCategory("IsProjectiveMaxPlusMatrix", IsMatrixOverSemiring);
DeclareCategoryCollections("IsProjectiveMaxPlusMatrix");
DeclareCategoryCollections("IsProjectiveMaxPlusMatrixCollection");

BindGlobal("ProjectiveMaxPlusMatrixFamily",
           NewFamily("ProjectiveMaxPlusMatrixFamily",
                     IsProjectiveMaxPlusMatrix, CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("ProjectiveMaxPlusMatrixType",
           NewType(ProjectiveMaxPlusMatrixFamily,
                   IsProjectiveMaxPlusMatrix));

DeclareGlobalFunction("ProjectiveMaxPlusMatrix");
DeclareGlobalFunction("ProjectiveMaxPlusMatrixNC");

DeclareOperation("RandomProjectiveMaxPlusMatrix", [IsPosInt]);

#############################################################################
## 6. Natural number matrices
#############################################################################

DeclareCategory("IsNaturalMatrix", IsTropicalMatrix);
DeclareCategoryCollections("IsNaturalMatrix");
DeclareCategoryCollections("IsNaturalMatrixCollection");

BindGlobal("NaturalMatrixFamily",
           NewFamily("NaturalMatrixFamily",
                     IsNaturalMatrix, CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("NaturalMatrixType",
           NewType(NaturalMatrixFamily,
                   IsNaturalMatrix));

DeclareGlobalFunction("NaturalMatrix");
DeclareGlobalFunction("NaturalMatrixNC");

DeclareOperation("RandomNaturalMatrix", [IsPosInt, IsPosInt, IsPosInt]);

DeclareAttribute("ThresholdNaturalMatrix", IsNaturalMatrix);
DeclareAttribute("PeriodNaturalMatrix", IsNaturalMatrix);
