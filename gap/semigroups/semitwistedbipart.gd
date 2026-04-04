############################################################################
##
##  elements/semitwistedbipart.gd
##  Copyright (C) 2025                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
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

DeclareSynonym("IsTwistedBipartitionSemigroup",
IsTwistedBipartitionCollection and IsSemigroup);
DeclareSynonym("IsTwistedBipartitionMonoid",
IsTwistedBipartitionCollection and IsMonoid);

InstallTrueMethod(IsFinite, IsTwistedBipartitionSemigroup);

DeclareAttribute("DegreeOfTwistedBipartitionSemigroup",
IsTwistedBipartitionSemigroup);
DeclareAttribute("MaxFloatingBlocksOfTwistedBipartitionSemigroup",
IsTwistedBipartitionSemigroup);
