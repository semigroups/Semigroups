############################################################################
##
#W  semibipart.gd
#Y  Copyright (C) 2011-13                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareSynonym("IsBipartitionSemigroup", IsSemigroup and
IsBipartitionCollection);
DeclareSynonym("IsBipartitionMonoid", IsMonoid and
IsBipartitionCollection);
DeclareProperty("IsBipartitionSemigroupGreensClass", IsGreensClass);
DeclareAttribute("DegreeOfBipartitionSemigroup", IsBipartitionSemigroup);
DeclareAttribute("IsomorphismBipartitionSemigroup", IsSemigroup);
DeclareAttribute("IsomorphismBlockBijectionSemigroup", IsSemigroup);

