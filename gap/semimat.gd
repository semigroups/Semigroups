############################################################################
##
#W  semimat.gd
#Y  Copyright (C) 2013                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## Some of this code is based on code taken from grpmat.gd in the GAP
## library

#T Is there a better way of saying what a matrix semigroup is?

DeclareCategory("IsMatrixSemigroup", IsSemigroup and IsFFECollCollColl);

DeclareOperation("OneMutable", [IsRingElementCollCollColl]);

#DeclareCategory("IsMatrixSemigroupElement", IsMatrixObj);
#DeclareCategoryCollections("IsMatrixSemigroupElement");

#############################################################################
##
#T This should really be IsSemiringCollCollColl or alternatively
## IsAssociativeElementList.
## The MatrixObj interface requires the base domain to be a ring,
## which can be  non-associative, but declares matrices to be a 
## CollColl, which makes it impossible to tell whether a list
## of Matrices has IsAssociativeElementCollection

DeclareAttribute("DegreeOfMatrixSemigroup", IsMatrixSemigroup);

#############################################################################
##
#M  CanComputeSize( <mat-semigrp> )
##
InstallTrueMethod(CanComputeSize, IsMatrixSemigroup and IsFinite);

DeclareAttribute( "TransposedMatrixSemigroup", IsMatrixSemigroup );

# (mp) This is defined for groups, and already difficult there, so I
# guess close to impossible to do in matrix semigroups
DeclareProperty( "IsFullMatrixSemigroup", IsSemigroup );
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);

DeclareProperty( "IsGeneralLinearGroupAsMatrixSemigroup", IsMatrixSemigroup);

DeclareOperation("CanonicalRowSpace", [ IsMatrixObj and IsFFECollColl ]);

# Right action of a matrix over a field on a row space
DeclareGlobalFunction("MatrixObjRowSpaceRightAction");
# Given a row space rsp and a matrix mat, computes
# a matrix mat' such that rsp.(mat * mat') = id_rsp
DeclareGlobalFunction("MatrixObjLocalRightInverse");
# Given two H-related matrices x and y, computes the element
# of the schutzenberger group of the matrices' H-class
# that maps im x to im y. Returns an invertible matrix
DeclareGlobalFunction("MatrixObjSchutzGrpElement");



