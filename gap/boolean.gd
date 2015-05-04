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

DeclareCategory("IsBooleanMat", IsMatrixOverSemiring);

DeclareCategoryCollections("IsBooleanMat");
DeclareCategoryCollections("IsBooleanMatCollection");


BindGlobal("BooleanMatFamily",
           NewFamily("BooleanMatFamily",
                     IsBooleanMat, CanEasilySortElements,
                     CanEasilySortElements));
BindGlobal("BooleanMatType",
           NewType(BooleanMatFamily,
                   IsBooleanMat));

DeclareGlobalFunction("BooleanMat");
DeclareGlobalFunction("BooleanMatNC");

DeclareOperation("RandomBooleanMat", [IsPosInt]);

DeclareOperation("AsBooleanMat", [IsPerm, IsPosInt]);
DeclareOperation("AsBooleanMat", [IsTransformation, IsPosInt]);

DeclareOperation("NumberBooleanMat", [IsBooleanMat]);
DeclareOperation("BooleanMatNumber", [IsPosInt, IsPosInt]);

DeclareGlobalFunction("SEMIGROUPS_HashFunctionBooleanMat");
