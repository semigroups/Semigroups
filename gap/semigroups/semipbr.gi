############################################################################
##
#W  semipbr.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# This file contains methods for semigroups of PBRs.

InstallMethod(FullPBRMonoid, "for a positive integer",
[IsPosInt],
function(n)
  local gens;

  gens := [[PBR([[]], [[1]]), PBR([[-1, 1]], [[1]]),
            PBR([[-1]], [[]]), PBR([[-1]], [[1]]),
            PBR([[-1]], [[-1, 1]])],

           [PBR([[], [-1]], [[2], [-2, 1]]),
            PBR([[-2, 1], [-1]], [[2], []]),
            PBR([[-1, 2], [-2]], [[1], [2]]),
            PBR([[-1], [-2]], [[1], [-2, 2]]),
            PBR([[-2], [2]], [[1], [2]]),
            PBR([[-2], [-1]], [[1], [1, 2]]),
            PBR([[-2], [-1]], [[1], [2]]),
            PBR([[-2], [-1]], [[1], [-2]]),
            PBR([[-2], [-1]], [[2], [1]]),
            PBR([[-2], [-2, -1]], [[1], [2]])]];

  if n > 2 then
    ErrorNoReturn("Semigroups: FullPBRMonoid: usage,\n",
                  "the argument <n> must be at most 2,");
  fi;
  return Monoid(gens[n]);
end);

InstallMethod(SemigroupViewStringPrefix, "for a pbr semigroup",
[IsPBRSemigroup], S -> "\>pbr\< ");

InstallMethod(SemigroupViewStringSuffix, "for a pbr semigroup",
[IsPBRSemigroup],
function(S)
  return Concatenation("\>degree \>",
                       ViewString(DegreeOfPBRSemigroup(S)),
                       "\<\< ");
end);

InstallMethod(DegreeOfPBRSemigroup,
"for a PBR semigroup",
[IsPBRSemigroup],
function(S)
  return DegreeOfPBR(Representative(S));
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
  deg := Maximum(1, DegreeOfTransformationSemigroup(S));
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
