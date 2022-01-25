############################################################################
##
##  semigroups/semibipart.gd
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareSynonym("IsBipartitionSemigroup",
               IsSemigroup and IsBipartitionCollection);
DeclareSynonym("IsBipartitionMonoid",
               IsMonoid and IsBipartitionCollection);

InstallTrueMethod(IsFinite, IsBipartitionSemigroup);

DeclareAttribute("DegreeOfBipartitionSemigroup", IsBipartitionSemigroup);

DeclareProperty("IsBlockBijectionSemigroup", IsBipartitionSemigroup);
DeclareProperty("IsPartialPermBipartitionSemigroup", IsBipartitionSemigroup);
DeclareProperty("IsPermBipartitionGroup", IsBipartitionSemigroup);
InstallTrueMethod(IsGroupAsSemigroup, IsPermBipartitionGroup);

DeclareSynonymAttr("IsBlockBijectionMonoid",
                   IsBlockBijectionSemigroup and IsMonoid);
DeclareSynonymAttr("IsPartialPermBipartitionMonoid",
                   IsPartialPermBipartitionSemigroup and IsMonoid);
