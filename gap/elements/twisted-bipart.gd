############################################################################
##
##  elements/twisted-bipart.gd
##  Copyright (C) 2025                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## In collaboration with
##
##############################################
##                                          ##
##               Code created               ##
##                    by                    ##
##          *--------------------*          ##
##          | Matthias Fresacher |          ##
##          *--------------------*          ##
##                                          ##
##############################################

#    *------------------------------*
#    |``````````````````````````````|
#    |`````____````____`````````````|
#    |````|MFMF\  /MFMF|````````````|
#    |````|MF|MF\/MF|MF|````````````|
#    |````|MF|\MFMF/|MF|_______`````|
#    |````|MF|``````|MFMFMFMFMF|````|
#    |````|MF|``````|MF|````````````|
#    |````|MF|``````|MF|___`````````|
#    |``````````````|MFMFMF|````````|
#    |``````````````|MF|````````````|
#    |``````````````|MF|````````````|
#    |``````````````|MF|````````````|
#    |``````````````````````````````|
#    *------------------------------*

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
DeclareOperation("RandomNonZeroTwistedBipartition", [IsRandomSource, IsInt, IsInt]);
DeclareOperation("RandomTwistedBipartition", [IsInt, IsInt]);
DeclareOperation("RandomTwistedBipartition", [IsRandomSource, IsInt, IsInt]);
DeclareProperty("IsZero", IsTwistedBipartition);
DeclareProperty("IsOne", IsTwistedBipartition);
DeclareOperation("IdentityTwistedBipartition", [IsPosInt, IsInt]);
DeclareAttribute("ZeroImmutable", IsTwistedBipartition);

DeclareAttribute("DegreeOfBipartition", IsTwistedBipartition);
DeclareAttribute("NrBlocks", IsTwistedBipartition);
DeclareAttribute("NrLeftBlocks", IsTwistedBipartition);
DeclareAttribute("NrRightBlocks", IsTwistedBipartition);
DeclareAttribute("RankOfBipartition", IsTwistedBipartition);

DeclareAttribute("DomainOfBipartition", IsTwistedBipartition);
DeclareAttribute("CodomainOfBipartition", IsTwistedBipartition);