############################################################################
##
#W  boolean.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains an implementation of boolean matrices.

DeclareCategory("IsBooleanMat",
                    IsMultiplicativeElementWithOne
                and IsAssociativeElement
                and IsPositionalObjectRep);

DeclareCategoryCollections("IsBooleanMat");
DeclareCategoryCollections("IsBooleanMatCollection");

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

DeclareOperation("AsBooleanMat", [IsPerm, IsPosInt]);
DeclareOperation("AsBooleanMat", [IsTransformation, IsPosInt]);

DeclareAttribute("DimensionOfBooleanMat", IsBooleanMat);
DeclareOperation("RandomBooleanMat", [IsPosInt]);

DeclareOperation("NumberBooleanMat", [IsBooleanMat]);
DeclareOperation("BooleanMatNumber", [IsPosInt, IsPosInt]);

DeclareGlobalFunction("SEMIGROUPS_HashFunctionBooleanMat");
