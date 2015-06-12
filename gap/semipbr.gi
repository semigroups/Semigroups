############################################################################
##
#W  semipbr.gi
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

InstallMethod(AsPBRSemigroup, "for a semigroup", [IsSemigroup],
function(S)
  return Range(IsomorphismPBRSemigroup(S));
end);

# fall back method

InstallMethod(IsomorphismPBRSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  local map;
  map := IsomorphismTransformationSemigroup(S);
  return CompositionMapping(IsomorphismPBRSemigroup(Range(map)), map);
end); 

InstallMethod(IsomorphismPBRSemigroup, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  local deg, gens;
  deg := DegreeOfTransformationSemigroup(S);
  gens := List(GeneratorsOfSemigroup(S), x -> AsPBR(x, deg));
  return MagmaIsomorphismByFunctionsNC(S, Semigroup(gens),
                                       AsPBR, AsTransformation);
end);

InstallMethod(IsomorphismPBRSemigroup, "for a bipartition semigroup",
[IsBipartitionSemigroup],
function(S)
  local gens;
  gens := List(GeneratorsOfSemigroup(S), AsPBR);
  return MagmaIsomorphismByFunctionsNC(S, Semigroup(gens),
                                       AsPBR, AsBipartition);
end);
