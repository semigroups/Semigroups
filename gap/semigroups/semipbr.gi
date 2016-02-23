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

# fall back method via a transformation semigroup

InstallMethod(IsomorphismSemigroup, "for IsPBRSemigroup and a semigroup",
[IsPBRSemigroup, IsSemigroup], SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsPBRSemigroup and a transformation semigroup",
[IsPBRSemigroup, IsTransformationSemigroup],
function(filt, S)
  local deg, gens;
  deg := Maximum(1, DegreeOfTransformationSemigroup(S));
  gens := List(GeneratorsOfSemigroup(S), x -> AsPBR(x, deg));
  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(gens),
                                       AsPBR,
                                       AsTransformation);
end);

InstallMethod(IsomorphismSemigroup,
"for IsPBRSemigroup and a bipartition semigroup",
[IsPBRSemigroup, IsBipartitionSemigroup],
function(filt, S)
  local gens;
  gens := List(GeneratorsOfSemigroup(S), AsPBR);
  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(gens),
                                       AsPBR,
                                       AsBipartition);
end);
