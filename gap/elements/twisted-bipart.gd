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
DeclareAttribute("DegreeOfTwistedBipartitionCollection",
IsTwistedBipartitionCollection);
DeclareAttribute("MaxFloatingBlocks",
IsTwistedBipartition);
DeclareAttribute("NrFloatingBlocks",
IsTwistedBipartition);
DeclareAttribute("UnderlyingBipartition",
IsTwistedBipartition);
DeclareAttribute("UnderlyingBipartitionCollection",
IsTwistedBipartitionCollection);

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

DeclareAttribute("NrTransverseBlocks", IsTwistedBipartition);
DeclareOperation("OneMutable", [IsTwistedBipartition, IsInt]);
DeclareOperation("OneMutable", [IsTwistedBipartitionCollection, IsInt]);

DeclareProperty("IsBlockBijection", IsTwistedBipartition);
DeclareProperty("IsUniformBlockBijection", IsTwistedBipartition);
DeclareProperty("IsPartialPermBipartition", IsTwistedBipartition);
DeclareProperty("IsTransBipartition", IsTwistedBipartition);
DeclareProperty("IsDualTransBipartition", IsTwistedBipartition);
DeclareProperty("IsPermBipartition", IsTwistedBipartition);

DeclareOperation("AsTwistedBipartition", [IsInt, IsPerm, IsZeroCyc, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsPerm, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsPerm, IsPosInt, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsPartialPerm, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsPartialPerm, IsZeroCyc, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsPartialPerm, IsPosInt, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsTransformation, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsTransformation, IsPosInt, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsTransformation, IsZeroCyc, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsBipartition, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsBipartition, IsPosInt, IsInt]);
DeclareOperation("AsTwistedBipartition", [IsTwistedBipartition]);
DeclareOperation("AsTwistedBipartition", [IsTwistedBipartition, IsPosInt]);
DeclareOperation("AsTwistedBipartition", [IsInt, IsBipartition, IsZeroCyc, IsInt]);
# DeclareOperation("AsTwistedBipartition", [IsInt, IsPBR, IsZeroCyc, IsInt]);
# DeclareOperation("AsTwistedBipartition", [IsInt, IsPBR, IsPosInt, IsInt]);
# DeclareOperation("AsTwistedBipartition", [IsInt, IsPBR, IsInt]);

DeclareOperation("AsBlockBijection", [IsTwistedBipartition, IsPosInt]);
DeclareOperation("AsBlockBijection", [IsTwistedBipartition]);