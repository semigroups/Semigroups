############################################################################
##
##  elements/twisted-bipart.gd
##  Copyright (C) 2025                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

DeclareCategory("IsTwistedBipartition", IsMultiplicativeElementWithOne
and IsMultiplicativeElementWithZero and
IsAssociativeElement);
DeclareCategoryCollections("IsTwistedBipartition");
DeclareCategoryCollections("IsTwistedBipartitionCollection");

DeclareOperation("TwistedBipartition", [IsInt, IsBipartition, IsInt]);
DeclareOperation("ZeroTwistedBipartition", [IsInt, IsInt]);
DeclareAttribute("DegreeOfTwistedBipartition",
IsTwistedBipartition);
DeclareAttribute("MaxFloatingBlocks",
IsTwistedBipartition);
DeclareAttribute("NrFloatingBlocks",
IsTwistedBipartition);
DeclareAttribute("UnderlyingBipartition",
IsTwistedBipartition);

DeclareOperation("RandomNonZeroTwistedBipartition", [IsInt, IsInt]);
DeclareProperty("IsZero", IsTwistedBipartition);
DeclareAttribute("ZeroImmutable", IsTwistedBipartition);

# TODO move the following into a separate file named semitwistedbipart.gd

DeclareSynonym("IsTwistedBipartitionSemigroup",
IsTwistedBipartitionCollection and IsSemigroup);
DeclareSynonym("IsTwistedBipartitionMonoid",
IsTwistedBipartitionCollection and IsMonoid);

InstallTrueMethod(IsFinite, IsTwistedBipartitionSemigroup);
