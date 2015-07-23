############################################################################
##
#W  semigroup-matrix-prime-field.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareSynonym("IsMatrixOverPrimeFieldSemigroup", 
               IsSemigroup and IsMatrixOverPrimeFieldCollection);
InstallTrueMethod(IsFinite, IsMatrixOverPrimeFieldSemigroup);
DeclareOperation("RandomMatrixOverPrimeFieldSemigroup", [IsPosInt, IsPosInt]);
DeclareOperation("RandomMatrixOverPrimeFieldMonoid", [IsPosInt, IsPosInt]);
