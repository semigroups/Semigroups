############################################################################
##
#W  constructors.gd
#Y  Copyright (C) 2015                                      Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##

DeclareGlobalFunction("TrivialSemigroup");
DeclareConstructor("TrivialSemigroupCons", [IsSemigroup, IsInt]);
DeclareGlobalFunction("RectangularBand");
DeclareConstructor("RectangularBandCons", [IsSemigroup, IsPosInt, IsPosInt]);
DeclareGlobalFunction("MonogenicSemigroup");
DeclareConstructor("MonogenicSemigroupCons", [IsSemigroup, IsPosInt, IsPosInt]);
DeclareGlobalFunction("ZeroSemigroup");
DeclareConstructor("ZeroSemigroupCons", [IsSemigroup, IsPosInt]);
DeclareGlobalFunction("LeftZeroSemigroup");
DeclareGlobalFunction("RightZeroSemigroup");
