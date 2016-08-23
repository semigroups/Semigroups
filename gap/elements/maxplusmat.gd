############################################################################
##
#W  maxplusmat.gd
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

DeclareCategory("IsMaxPlusMatrix", IsMatrixOverSemiring and IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsMaxPlusMatrix");
DeclareCategoryCollections("IsMaxPlusMatrixCollection");

BindGlobal("MaxPlusMatrixFamily",
           NewFamily("MaxPlusMatrixFamily",
                     IsMaxPlusMatrix, CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("MaxPlusMatrixType",
           NewType(MaxPlusMatrixFamily,
                   IsMaxPlusMatrix));

DeclareOperation("SpectralRadius", [IsMaxPlusMatrix]);
DeclareOperation("UnweightedPrecedenceDigraph", [IsMaxPlusMatrix]);
DeclareOperation("RadialEigenvector", [IsMaxPlusMatrix]);

#############################################################################
## 2. Min-plus matrices
#############################################################################

DeclareCategory("IsMinPlusMatrix", IsMatrixOverSemiring and IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsMinPlusMatrix");
DeclareCategoryCollections("IsMinPlusMatrixCollection");

BindGlobal("MinPlusMatrixFamily",
           NewFamily("MinPlusMatrixFamily",
                     IsMinPlusMatrix, CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("MinPlusMatrixType",
           NewType(MinPlusMatrixFamily,
                   IsMinPlusMatrix));

#############################################################################
## 3. Tropical matrices
#############################################################################

DeclareCategory("IsTropicalMatrix", IsMatrixOverSemiring and IsPlistMatrixOverSemiringPositionalRep);
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

#############################################################################
## 6. Projective max-plus matrices
#############################################################################

DeclareCategory("IsProjectiveMaxPlusMatrix", IsMatrixOverSemiring and IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsProjectiveMaxPlusMatrix");
DeclareCategoryCollections("IsProjectiveMaxPlusMatrixCollection");

BindGlobal("ProjectiveMaxPlusMatrixFamily",
           NewFamily("ProjectiveMaxPlusMatrixFamily",
                     IsProjectiveMaxPlusMatrix, CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("ProjectiveMaxPlusMatrixType",
           NewType(ProjectiveMaxPlusMatrixFamily,
                   IsProjectiveMaxPlusMatrix));

#############################################################################
## 7. NTP (Natural Threshold Period) matrices
#############################################################################

DeclareCategory("IsNTPMatrix", IsMatrixOverSemiring and IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsNTPMatrix");
DeclareCategoryCollections("IsNTPMatrixCollection");

BindGlobal("NTPMatrixFamily",
           NewFamily("NTPMatrixFamily",
                     IsNTPMatrix, CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("NTPMatrixType",
           NewType(NTPMatrixFamily,
                   IsNTPMatrix));

DeclareAttribute("ThresholdNTPMatrix", IsNTPMatrix);
DeclareAttribute("PeriodNTPMatrix", IsNTPMatrix);

#############################################################################
## 8. Integer matrices
#############################################################################

DeclareCategory("IsIntegerMatrix", IsMatrixOverSemiring and IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsIntegerMatrix");
DeclareCategoryCollections("IsIntegerMatrixCollection");

BindGlobal("IntegerMatrixFamily",
           NewFamily("IntegerMatrixFamily",
                     IsIntegerMatrix, CanEasilySortElements,
                     CanEasilySortElements));

BindGlobal("IntegerMatrixType",
           NewType(IntegerMatrixFamily,
                   IsIntegerMatrix));
