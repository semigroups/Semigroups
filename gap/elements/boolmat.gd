############################################################################
##
##  elements/boolmat.gd
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an implementation of boolean matrices.

#############################################################################
## Specializations of declarations for MatrixOverSemiring
#############################################################################

DeclareCategory("IsBooleanMat", IsMatrixOverSemiring and
                                IsPlistMatrixOverSemiringPositionalRep);

DeclareCategoryCollections("IsBooleanMat");
DeclareCategoryCollections("IsBooleanMatCollection");

BindGlobal("BooleanMatType",
           NewType(NewFamily("BooleanMatFamily",
                             IsBooleanMat,
                             CanEasilySortElements,
                             CanEasilySortElements),
                   IsBooleanMat));

DeclareGlobalFunction("BooleanMat");

#############################################################################
## Declarations specifically for Boolean mats
#############################################################################

DeclareOperation("AsBooleanMat", [IsMultiplicativeElement, IsPosInt]);
DeclareOperation("AsBooleanMat", [IsMultiplicativeElement]);
DeclareOperation("AsBooleanMat", [IsDigraph]);
DeclareOperation("AsDigraph", [IsBooleanMat]);

DeclareOperation("NumberBooleanMat", [IsBooleanMat]);
DeclareOperation("BooleanMatNumber", [IsPosInt, IsPosInt]);
DeclareGlobalFunction("NumberBlist");
DeclareGlobalFunction("BlistNumber");

DeclareAttribute("Successors", IsBooleanMat);

DeclareProperty("IsRowTrimBooleanMat", IsBooleanMat);
DeclareProperty("IsColTrimBooleanMat", IsBooleanMat);
DeclareProperty("IsTrimBooleanMat", IsBooleanMat);

DeclareGlobalFunction("OnBlist");

DeclareAttribute("CanonicalBooleanMat", IsBooleanMat);
DeclareOperation("CanonicalBooleanMat", [IsPermGroup, IsBooleanMat]);
DeclareOperation("CanonicalBooleanMat",
                 [IsPermGroup, IsPermGroup, IsBooleanMat]);
DeclareOperation("CanonicalBooleanMatNC",
                 [IsPermGroup, IsPermGroup, IsBooleanMat]);

DeclareProperty("IsSymmetricBooleanMat", IsBooleanMat);
DeclareProperty("IsAntiSymmetricBooleanMat", IsBooleanMat);
DeclareProperty("IsTransitiveBooleanMat", IsBooleanMat);
DeclareProperty("IsReflexiveBooleanMat", IsBooleanMat);

DeclareProperty("IsTotalBooleanMat", IsBooleanMat);
DeclareProperty("IsOntoBooleanMat", IsBooleanMat);

DeclareSynonymAttr("IsPartialOrderBooleanMat", IsReflexiveBooleanMat and
                   IsAntiSymmetricBooleanMat and IsTransitiveBooleanMat);
DeclareSynonymAttr("IsEquivalenceBooleanMat", IsReflexiveBooleanMat and
                   IsSymmetricBooleanMat and IsTransitiveBooleanMat);

DeclareProperty("IsTransformationBooleanMat", IsBooleanMat);
