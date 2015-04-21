############################################################################
##
#W  boolean.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains implementation of boolean matrices.

DeclareCategory("IsBooleanMat",
                    IsMultiplicativeElementWithOne 
                and IsAssociativeElement 
                and IsPositionalObjectRep);

DeclareCategoryCollections("IsBooleanMat");
DeclareCategoryCollections("IsBooleanMatCollection");
DeclareSynonym("IsBooleanMatSemigroup", IsBooleanMatCollection and IsSemigroup);

InstallTrueMethod(IsGeneratorsOfSemigroup, IsBooleanMatCollection);

BindGlobal("BooleanMatFamily",
           NewFamily("BooleanMatFamily",
                     IsBooleanMat, CanEasilySortElements,
                     CanEasilySortElements));
BindGlobal("BooleanMatType",
           NewType(BooleanMatFamily,
                   IsBooleanMat));

DeclareGlobalFunction("BooleanMatNC");
DeclareGlobalFunction("BooleanMat");
DeclareGlobalFunction("BooleanMatByIntRep");

DeclareOperation("AsBooleanMat", [IsPerm, IsPosInt]);

DeclareAttribute("DimensionOfBooleanMat", IsBooleanMat);
DeclareOperation("RandomBooleanMat", [IsPosInt]);

DeclareOperation("RandomBooleanMatSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomBooleanMatMonoid", [IsPosInt, IsPosInt]);
DeclareOperation("RegularBooleanMatSemigroup", [IsPosInt]);

DeclareGlobalFunction("SEMIGROUPS_HashFunctionBooleanMat");
