############################################################################
##
#W  partbinrel.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# This file contains methods for semigroups of PBRs.

InstallMethod(DegreeOfPartitionedBinaryRelationSemigroup, 
"for a PBR semigroup",
[IsPartitionedBinaryRelationSemigroup],
function(S)
  return DegreeOfPartitionedBinaryRelation(Representative(S));
end);
