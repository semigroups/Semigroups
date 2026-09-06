#############################################################################
##
##  semigroups/semibipart.gi
##  Copyright (C) 2025                               James D. Mitchell
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


# this file contains methods for every operation/attribute/property that is
# specific to twisted bipartition semigroups.

InstallTrueMethod(CanUseGapFroidurePin, IsTwistedBipartitionSemigroup);

#############################################################################
## Printing and viewing
#############################################################################

InstallMethod(SemigroupViewStringPrefix, "for a twisted bipartition semigroup",
[IsTwistedBipartitionSemigroup], S -> Concatenation("\>", ViewString(MaxFloatingBlocksOfTwistedBipartitionSemigroup(S)), "-twisted bipartition\< "));

InstallMethod(SemigroupViewStringPrefix, "for a twisted bipartition *-semigroup",
[IsTwistedBipartitionSemigroup and IsStarSemigroup], S -> Concatenation("\>", ViewString(MaxFloatingBlocksOfTwistedBipartitionSemigroup(S)), "-twisted bipartition *-\< "));

InstallMethod(SemigroupViewStringSuffix, "for a twisted bipartition semigroup",
[IsTwistedBipartitionSemigroup],
function(S)
  return Concatenation("\>degree \>",
                       ViewString(DegreeOfTwistedBipartitionSemigroup(S)),
                       "\<\< ");
end);

InstallMethod(DegreeOfTwistedBipartitionSemigroup, "for a twisted bipartition semigroup",
[IsTwistedBipartitionSemigroup], S -> DegreeOfBipartition(Representative(S)));
InstallMethod(MaxFloatingBlocksOfTwistedBipartitionSemigroup, "for a twisted bipartition semigroup",
[IsTwistedBipartitionSemigroup], S -> MaxFloatingBlocks(Representative(S)));