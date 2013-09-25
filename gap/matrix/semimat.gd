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

DeclareSynonym("IsMatrixSemigroup", IsSemigroup and IsRingElementCollCollColl);
DeclareOperation("OneMutable", [IsRingElementCollCollColl]);


#############################################################################
##
#M  CanComputeSize( <mat-grp> )
##
InstallTrueMethod(CanComputeSize, IsMatrixSemigroup and IsFinite);

DeclareAttribute( "TransposedMatrixSemigroup", IsMatrixSemigroup );


# (mp) This is defined for groups, and already difficult there, so I
# guess close to impossible to do in matrix semigroups
DeclareProperty( "IsFullMatrixSemigroup", IsSemigroup );
DeclareSynonymAttr("IsGeneralLinearSemigroup", IsFullMatrixSemigroup);

DeclareProperty( "IsGeneralLinearGroupAsMatrixSemigroup", IsMatrixSemigroup);


