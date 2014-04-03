############################################################################
##
#W  semibipart.gd
#Y  Copyright (C) 2013-14                                James D. Mitchell
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

DeclareProperty("IsBlockBijectionSemigroup", IsSemigroup);
DeclareProperty("IsPartialPermBipartitionSemigroup", IsSemigroup);
DeclareProperty("IsPermBipartitionGroup", IsSemigroup);
DeclareProperty("IsBlockBijectionSemigroup", IsSemigroupIdeal);
DeclareProperty("IsPartialPermBipartitionSemigroup", IsSemigroupIdeal);
DeclareProperty("IsPermBipartitionGroup", IsSemigroupIdeal);

DeclareSynonymAttr("IsBlockBijectionMonoid", IsBlockBijectionSemigroup and IsMonoid);
DeclareSynonymAttr("IsPartialPermBipartitionMonoid",
IsPartialPermBipartitionSemigroup and IsMonoid);

