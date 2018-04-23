############################################################################
##
##  semipbr.gi
##  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

# TODO IsomorphismSemigroup for IsBooleanMatSemigroup
#
# This file contains methods for semigroups of PBRs.

#############################################################################
## ?. Random
#############################################################################

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons,
[IsPBRSemigroup, IsList],
function(filt, params)
  return SEMIGROUPS_ProcessRandomArgsCons(IsSemigroup, params);
end);

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons,
[IsPBRMonoid, IsList],
function(filt, params)
  return SEMIGROUPS_ProcessRandomArgsCons(IsSemigroup, params);
end);

InstallMethod(RandomSemigroupCons, "for IsPBRSemigroup and a list",
[IsPBRSemigroup, IsList],
function(filt, params)
  return Semigroup(List([1 .. params[1]], i -> RandomPBR(params[2])));
end);

InstallMethod(RandomMonoidCons, "for IsPBRMonoid and a list",
[IsPBRMonoid, IsList],
function(filt, params)
  return Monoid(List([1 .. params[1]], i -> RandomPBR(params[2])));
end);

InstallMethod(RandomInverseSemigroupCons, "for IsPBRSemigroup and a list",
[IsPBRSemigroup, IsList], SEMIGROUPS.DefaultRandomInverseSemigroup);

InstallMethod(RandomInverseMonoidCons, "for IsPBRMonoid and a list",
[IsPBRMonoid, IsList], SEMIGROUPS.DefaultRandomInverseMonoid);

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
  local deg, T;

  deg := Maximum(1, DegreeOfTransformationSemigroup(S));
  T := Semigroup(List(GeneratorsOfSemigroup(S), x -> AsPBR(x, deg)));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       x -> AsPBR(x, deg),
                                       AsTransformation);
end);

# The following is not a monoid isomorphism

InstallMethod(IsomorphismSemigroup,
"for IsPBRSemigroup and a bipartition semigroup",
[IsPBRSemigroup, IsBipartitionSemigroup],
function(filt, S)
  local T;

  T := Semigroup(List(GeneratorsOfSemigroup(S), AsPBR));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       AsPBR,
                                       AsBipartition);
end);

InstallMethod(IsomorphismSemigroup,
"for IsPBRSemigroup and a pbr semigroup",
[IsPBRSemigroup, IsPBRSemigroup],
function(filter, S)
  return MagmaIsomorphismByFunctionsNC(S, S, IdFunc, IdFunc);
end);

InstallMethod(AsMonoid, "for a PBR semigroup",
[IsPBRSemigroup],
function(S)
  if MultiplicativeNeutralElement(S) = fail then
    return fail;  # so that we do the same as the GAP / ref manual says
  fi;
  return Range(IsomorphismMonoid(IsPBRMonoid, S));
end);

InstallMethod(IsomorphismMonoid, "for IsPBRMonoid and a semigroup",
[IsPBRMonoid, IsSemigroup], SEMIGROUPS.DefaultIsomorphismMonoid);

InstallMethod(IsomorphismMonoid, "for IsPBRMonoid and a monoid",
[IsPBRMonoid, IsMonoid], SEMIGROUPS.DefaultIsomorphismMonoid);

InstallMethod(IsomorphismMonoid, "for IsPBRMonoid and a transformation monoid",
[IsPBRMonoid, IsTransformationMonoid],
function(filt, S)
  local deg, T;

  deg := Maximum(1, DegreeOfTransformationSemigroup(S));
  T := Monoid(List(GeneratorsOfSemigroup(S), x -> AsPBR(x, deg)));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       x -> AsPBR(x, deg),
                                       AsTransformation);
end);

InstallMethod(IsomorphismMonoid,
"for IsPBRMonoid and a pbr monoid",
[IsPBRMonoid, IsPBRMonoid],
function(filter, S)
  return MagmaIsomorphismByFunctionsNC(S, S, IdFunc, IdFunc);
end);
