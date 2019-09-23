############################################################################
##
##  elements/maxplusmat.gd
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains declarations for max-plus, min-plus, tropical max-plus,
# tropical min-plus matrices, and natural number matrices.

DeclareProperty("IsTorsion", IsMatrixObj);
DeclareOperation("Matrix", [IsIntegers, IsTransformation]);
DeclareOperation("Matrix", [IsIntegers, IsTransformation, IsPosInt]);

#############################################################################
## 1. Max-plus matrices
#############################################################################

DeclareCategory("IsMaxPlusMatrix", IsMatrixOverSemiring and
                                   IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsMaxPlusMatrix");
DeclareCategoryCollections("IsMaxPlusMatrixCollection");

BindGlobal("MaxPlusMatrixType",
           NewType(NewFamily("MaxPlusMatrixFamily",
                             IsMaxPlusMatrix,
                             CanEasilySortElements,
                             CanEasilySortElements),
                   IsMaxPlusMatrix));

DeclareOperation("SpectralRadius", [IsMaxPlusMatrix]);
DeclareOperation("UnweightedPrecedenceDigraph", [IsMaxPlusMatrix]);
DeclareOperation("RadialEigenvector", [IsMaxPlusMatrix]);

#############################################################################
## 2. Min-plus matrices
#############################################################################

DeclareCategory("IsMinPlusMatrix",
                IsMatrixOverSemiring
                and IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsMinPlusMatrix");
DeclareCategoryCollections("IsMinPlusMatrixCollection");

BindGlobal("MinPlusMatrixType",
           NewType(NewFamily("MinPlusMatrixFamily",
                             IsMinPlusMatrix,
                             CanEasilySortElements,
                             CanEasilySortElements),
                   IsMinPlusMatrix));

#############################################################################
## 3. Tropical matrices
#############################################################################

DeclareCategory("IsTropicalMatrix",
                IsMatrixOverSemiring
                and IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsTropicalMatrix");

DeclareAttribute("ThresholdTropicalMatrix", IsTropicalMatrix);

#############################################################################
## 4. Tropical max-plus matrices
#############################################################################

DeclareCategory("IsTropicalMaxPlusMatrix", IsTropicalMatrix);
DeclareCategoryCollections("IsTropicalMaxPlusMatrix");
DeclareCategoryCollections("IsTropicalMaxPlusMatrixCollection");

BindGlobal("TropicalMaxPlusMatrixType",
           NewType(NewFamily("TropicalMaxPlusMatrixFamily",
                             IsTropicalMaxPlusMatrix,
                             CanEasilySortElements,
                             CanEasilySortElements),
                   IsTropicalMaxPlusMatrix));

#############################################################################
## 5. Tropical min-plus matrices
#############################################################################

DeclareCategory("IsTropicalMinPlusMatrix", IsTropicalMatrix);
DeclareCategoryCollections("IsTropicalMinPlusMatrix");
DeclareCategoryCollections("IsTropicalMinPlusMatrixCollection");

BindGlobal("TropicalMinPlusMatrixType",
           NewType(NewFamily("TropicalMinPlusMatrixFamily",
                             IsTropicalMinPlusMatrix,
                             CanEasilySortElements,
                             CanEasilySortElements),
                   IsTropicalMinPlusMatrix));

#############################################################################
## 6. Projective max-plus matrices
#############################################################################

DeclareCategory("IsProjectiveMaxPlusMatrix",
                IsMatrixOverSemiring
                and IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsProjectiveMaxPlusMatrix");
DeclareCategoryCollections("IsProjectiveMaxPlusMatrixCollection");

BindGlobal("ProjectiveMaxPlusMatrixType",
           NewType(NewFamily("ProjectiveMaxPlusMatrixFamily",
                             IsProjectiveMaxPlusMatrix,
                             CanEasilySortElements,
                             CanEasilySortElements),
                   IsProjectiveMaxPlusMatrix));

#############################################################################
## 7. NTP (Natural Threshold Period) matrices
#############################################################################

DeclareCategory("IsNTPMatrix",
                IsMatrixOverSemiring
                and IsPlistMatrixOverSemiringPositionalRep);
DeclareCategoryCollections("IsNTPMatrix");
DeclareCategoryCollections("IsNTPMatrixCollection");

BindGlobal("NTPMatrixType",
           NewType(NewFamily("NTPMatrixFamily",
                             IsNTPMatrix,
                             CanEasilySortElements,
                             CanEasilySortElements),
                   IsNTPMatrix));

DeclareAttribute("ThresholdNTPMatrix", IsNTPMatrix);
DeclareAttribute("PeriodNTPMatrix", IsNTPMatrix);
