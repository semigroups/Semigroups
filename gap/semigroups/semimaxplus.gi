############################################################################
##
#W  semimaxplus.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains implementations for semigroups of max-plus, min-plus,
# tropical max-plus, and tropical min-plus matrices.

#############################################################################
## Random for matrices with 0 additional parameters
#############################################################################

_InstallRandom0 := function(filter)
  local IsXSemigroup, IsXMonoid;

  IsXSemigroup := Concatenation(filter, "Semigroup");
  IsXMonoid := Concatenation(filter, "Monoid");

  InstallMethod(RandomSemigroupCons,
  Concatenation("for ", IsXSemigroup, ", pos int, int, int, int"),
  [IsXSemigroup, IsPosInt, IsInt, IsInt, IsInt],
  function(filt, nrgens, dim, dummy1, dummy2)
    return Semigroup(List([1 .. nrgens],
                          i -> RandomMatrix(EvalString(filter), dim)));
  end);

  InstallMethod(RandomMonoidCons,
  Concatenation("for ", IsXMonoid, ", pos int, int, int, int"),
  [IsXMonoid, IsPosInt, IsInt, IsInt, IsInt],
  function(filt, nrgens, dim, dummy1, dummy2)
    return Monoid(List([1 .. nrgens],
                       i -> RandomMatrix(EvalString(filter), dim)));
  end);

  InstallMethod(RandomInverseSemigroupCons,
  Concatenation("for ", IsXSemigroup, ", pos int, int, int, int"),
  [IsXSemigroup, IsPosInt, IsInt, IsInt, IsInt],
  function(filt, nrgens, deg, dummy1, dummy2)
    return SEMIGROUPS.DefaultRandomInverseSemigroup(filt, nrgens, deg);
  end);

  InstallMethod(RandomInverseMonoidCons,
  Concatenation("for ", IsXMonoid, ", pos int, int, int, int"),
  [IsXMonoid, IsPosInt, IsInt, IsInt, IsInt],
  function(filt, nrgens, deg, dummy1, dummy2)
    return SEMIGROUPS.DefaultRandomInverseMonoid(filt, nrgens, deg);
  end);

end;

for _IsXMatrix in ["IsMaxPlusMatrix",
                   "IsMinPlusMatrix",
                   "IsProjectiveMaxPlusMatrix",
                   "IsIntegerMatrix"] do
  _InstallRandom0(_IsXMatrix);
od;

Unbind(_IsXMatrix);
Unbind(_InstallRandom0);

#############################################################################
## Random for matrices with 1 additional parameters
#############################################################################

_InstallRandom1 := function(filter)
  local IsXSemigroup, IsXMonoid;

  IsXSemigroup := Concatenation(filter, "Semigroup");
  IsXMonoid := Concatenation(filter, "Monoid");

  InstallMethod(RandomSemigroupCons,
  Concatenation("for ", IsXSemigroup, ", pos int, int, int, int"),
  [IsXSemigroup, IsPosInt, IsInt, IsInt, IsInt],
  function(filt, nrgens, dim, threshold, dummy)
    return Semigroup(List([1 .. nrgens],
                          i -> RandomMatrix(EvalString(filter),
                                            dim,
                                            threshold)));
  end);

  InstallMethod(RandomMonoidCons,
  Concatenation("for ", IsXMonoid, ", pos int, int, int, int"),
  [IsXMonoid, IsPosInt, IsInt, IsInt, IsInt],
  function(filt, nrgens, dim, threshold, dummy)
    return Monoid(List([1 .. nrgens],
                        i -> RandomMatrix(EvalString(filter),
                                          dim,
                                          threshold)));
  end);

  InstallMethod(RandomInverseSemigroupCons,
  Concatenation("for ", IsXSemigroup, ", pos int, int, int, int"),
  [IsXSemigroup, IsPosInt, IsInt, IsInt, IsInt],
  function(filt, nrgens, dim, threshold, dummy)
    return SEMIGROUPS.DefaultRandomInverseSemigroup(filt,
                                                    nrgens,
                                                    dim,
                                                    threshold);
  end);

  InstallMethod(RandomInverseMonoidCons,
  Concatenation("for ", IsXMonoid, ", pos int, int, int, int"),
  [IsXMonoid, IsPosInt, IsInt, IsInt, IsInt],
  function(filt, nrgens, dim, threshold, dummy)
    return SEMIGROUPS.DefaultRandomInverseMonoid(filt,
                                                 nrgens,
                                                 dim,
                                                 threshold);
  end);

end;

for _IsXMatrix in ["IsTropicalMaxPlusMatrix",
                   "IsTropicalMinPlusMatrix"] do
  _InstallRandom1(_IsXMatrix);
od;

Unbind(_IsXMatrix);
Unbind(_InstallRandom1);

#############################################################################
## Random for matrices with 1 additional parameters
#############################################################################

InstallMethod(RandomSemigroupCons,
"for IsNTPMatrixSemigroup, pos int, int, int, int",
[IsNTPMatrixSemigroup, IsPosInt, IsInt, IsInt, IsInt],
function(filt, nrgens, dim, threshold, period)
  return Semigroup(List([1 .. nrgens],
                        i -> RandomMatrix(IsNTPMatrix,
                                          dim,
                                          threshold,
                                          period)));
end);

InstallMethod(RandomMonoidCons,
"for IsNTPMatrixMonoid, pos int, int, int, int",
[IsNTPMatrixMonoid, IsPosInt, IsInt, IsInt, IsInt],
function(filt, nrgens, dim, threshold, period)
  return Monoid(List([1 .. nrgens],
                        i -> RandomMatrix(IsNTPMatrix,
                                          dim,
                                          threshold,
                                          period)));
end);

InstallMethod(RandomInverseSemigroupCons,
"for IsNTPMatrixSemigroup, pos int, int, int, int",
[IsNTPMatrixSemigroup, IsPosInt, IsInt, IsInt, IsInt],
function(filt, nrgens, dim, threshold, period)
  return SEMIGROUPS.DefaultRandomInverseSemigroup(filt,
                                                  nrgens,
                                                  dim,
                                                  threshold,
                                                  period);
end);

InstallMethod(RandomInverseMonoidCons,
"for IsNTPMatrixMonoid, pos int, int, int, int",
[IsNTPMatrixMonoid, IsPosInt, IsInt, IsInt, IsInt],
function(filt, nrgens, dim, threshold, period)
  return SEMIGROUPS.DefaultRandomInverseMonoid(filt,
                                               nrgens,
                                               dim,
                                               threshold,
                                               period);
end);

#############################################################################
## 1. Isomorphisms
#############################################################################

#############################################################################
## Isomorphism from an arbitrary semigroup to a matrix semigroup, via a
## transformation semigroup
#############################################################################

# 0 additional parameters

for _IsXMatrix in ["IsMaxPlusMatrix",
                   "IsMinPlusMatrix",
                   "IsProjectiveMaxPlusMatrix",
                   "IsIntegerMatrix"] do

  _IsXSemigroup := Concatenation(_IsXMatrix, "Semigroup");

  InstallMethod(IsomorphismSemigroup,
  Concatenation("for ", _IsXSemigroup, " and a semigroup"),
  [EvalString(_IsXSemigroup), IsSemigroup],
  SEMIGROUPS.DefaultIsomorphismSemigroup);

od;

Unbind(_IsXMatrix);
Unbind(_IsXSemigroup);

# 1 additional parameters

for _IsXMatrix in ["IsTropicalMaxPlusMatrix",
                   "IsTropicalMinPlusMatrix"] do

  _IsXSemigroup := Concatenation(_IsXMatrix, "Semigroup");

  InstallMethod(IsomorphismSemigroup,
  Concatenation("for ", _IsXSemigroup, ", pos int, and a semigroup"),
  [EvalString(_IsXSemigroup), IsPosInt, IsSemigroup],
  function(filter, threshold, S)
    local iso1, inv1, iso2, inv2;

    iso1 := IsomorphismTransformationSemigroup(S);
    inv1 := InverseGeneralMapping(iso1);
    iso2 := IsomorphismSemigroup(filter, threshold, Range(iso1));
    inv2 := InverseGeneralMapping(iso2);

    return MagmaIsomorphismByFunctionsNC(S,
                                         Range(iso2),
                                         x -> (x ^ iso1) ^ iso2,
                                         x -> (x ^ inv2) ^ inv1);
  end);
od;

Unbind(_IsXMatrix);
Unbind(_IsXSemigroup);

# 2 additional parameters

InstallMethod(IsomorphismSemigroup,
"for IsNTPMatrixSemigroup, pos int, pos int, and a semigroup",
[IsNTPMatrixSemigroup, IsPosInt, IsPosInt, IsSemigroup],
function(filter, threshold, period, S)
  local iso1, inv1, iso2, inv2;

  iso1 := IsomorphismTransformationSemigroup(S);
  inv1 := InverseGeneralMapping(iso1);
  iso2 := IsomorphismSemigroup(filter, threshold, period, Range(iso1));
  inv2 := InverseGeneralMapping(iso2);

  return MagmaIsomorphismByFunctionsNC(S,
                                       Range(iso2),
                                       x -> (x ^ iso1) ^ iso2,
                                       x -> (x ^ inv2) ^ inv1);
end);

#############################################################################
## Isomorphism from a transformation semigroup to a matrix semigroup
## 0 additional parameters!!!
#############################################################################

## These are installed inside a function so that the value of IsXMatrix and
## IsXSemigroup are retained as a local variables.

_InstallIsomorphism0 := function(filter)
  local IsXMatrix, IsXSemigroup, IsXMonoid;

  IsXMatrix := filter;
  IsXSemigroup := Concatenation(filter, "Semigroup");
  IsXMonoid := Concatenation(filter, "Monoid");

  InstallMethod(IsomorphismMonoid,
  Concatenation("for ", IsXMonoid, " and a semigroup"),
  [EvalString(IsXMonoid), IsSemigroup],
  SEMIGROUPS.DefaultIsomorphismMonoid);

  InstallMethod(IsomorphismMonoid,
  Concatenation("for ", IsXMonoid, " and a monoid"),
  [EvalString(IsXMonoid), IsMonoid],
  function(filter, S)
    return IsomorphismSemigroup(EvalString(IsXSemigroup), S);
  end);

  InstallMethod(IsomorphismSemigroup,
  Concatenation("for ", IsXSemigroup,
                " and a transformation semigroup with generators"),
  [EvalString(IsXSemigroup),
   IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  function(filt, S)
    local n, map, T;

    n    := Maximum(DegreeOfTransformationSemigroup(S), 1);
    map  := x -> AsMatrix(EvalString(IsXMatrix), x, n);
    T := Semigroup(List(GeneratorsOfSemigroup(S), map));
    UseIsomorphismRelation(S, T);

    return MagmaIsomorphismByFunctionsNC(S,
                                         T,
                                         map,
                                         AsTransformation);
  end);
end;

for _IsXMatrix in ["IsMaxPlusMatrix",
                   "IsMinPlusMatrix",
                   "IsProjectiveMaxPlusMatrix",
                   "IsIntegerMatrix"] do
  _InstallIsomorphism0(_IsXMatrix);
od;

Unbind(_IsXMatrix);
Unbind(_InstallIsomorphism0);

#############################################################################
## Isomorphism from a transformation semigroup to a matrix semigroup
## 1 additional parameters!!!
#############################################################################

## These are installed inside a function so that the value of IsXMatrix and #
## IsXSemigroup are retained as a local variables.

_InstallIsomorphism1 := function(filter)
  local IsXMatrix, IsXSemigroup, IsXMonoid;

  IsXMatrix := filter;
  IsXSemigroup := Concatenation(filter, "Semigroup");
  IsXMonoid := Concatenation(filter, "Monoid");

  InstallMethod(IsomorphismMonoid,
  Concatenation("for ", IsXMonoid, ", pos int, and a semigroup"),
  [EvalString(IsXMonoid), IsPosInt, IsSemigroup],
  function(filter, threshold, S)
    local iso1, inv1, iso2, inv2;

    iso1 := IsomorphismTransformationMonoid(S);
    inv1 := InverseGeneralMapping(iso1);
    iso2 := IsomorphismMonoid(filter, threshold, Range(iso1));
    inv2 := InverseGeneralMapping(iso2);

    return MagmaIsomorphismByFunctionsNC(S,
                                         Range(iso2),
                                         x -> (x ^ iso1) ^ iso2,
                                         x -> (x ^ inv2) ^ inv1);
  end);

  InstallMethod(IsomorphismMonoid,
  Concatenation("for ", IsXMonoid, ", pos int, and a monoid"),
  [EvalString(IsXMonoid), IsPosInt, IsMonoid],
  function(filter, threshold, S)
    return IsomorphismSemigroup(EvalString(IsXSemigroup), threshold, S);
  end);

  InstallMethod(IsomorphismSemigroup,
  Concatenation("for ", IsXSemigroup,
                ", pos int, and a transformation semigroup with generators"),
  [EvalString(IsXSemigroup), IsPosInt,
   IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  function(filt, threshold, S)
    local n, map, T;

    n    := Maximum(DegreeOfTransformationSemigroup(S), 1);
    map  := x -> AsMatrix(EvalString(IsXMatrix), x, n, threshold);
    T := Semigroup(List(GeneratorsOfSemigroup(S), map));
    UseIsomorphismRelation(S, T);

    return MagmaIsomorphismByFunctionsNC(S,
                                         T,
                                         map,
                                         AsTransformation);
  end);
end;

for _IsXMatrix in ["IsTropicalMaxPlusMatrix",
                   "IsTropicalMinPlusMatrix"] do
  _InstallIsomorphism1(_IsXMatrix);
od;

Unbind(_IsXMatrix);
Unbind(_InstallIsomorphism1);

#############################################################################
## Isomorphism from a transformation semigroup to a matrix semigroup
## 2 additional parameters!!!
#############################################################################

InstallMethod(IsomorphismMonoid,
"for IsNTPMatrixMonoid, pos int, pos int, and a semigroup",
[IsNTPMatrixMonoid, IsPosInt, IsPosInt, IsSemigroup],
function(filter, threshold, period, S)
  local iso1, inv1, iso2, inv2;

  iso1 := IsomorphismTransformationMonoid(S);
  inv1 := InverseGeneralMapping(iso1);
  iso2 := IsomorphismMonoid(filter, threshold, period, Range(iso1));
  inv2 := InverseGeneralMapping(iso2);

  return MagmaIsomorphismByFunctionsNC(S,
                                       Range(iso2),
                                       x -> (x ^ iso1) ^ iso2,
                                       x -> (x ^ inv2) ^ inv1);
end);

InstallMethod(IsomorphismMonoid,
"for IsNTPMatrixMonoid, pos int, pos int, and a semigroup",
[IsNTPMatrixMonoid, IsPosInt, IsPosInt, IsMonoid],
function(filter, threshold, period, S)
  return IsomorphismSemigroup(IsNTPMatrixSemigroup, threshold, period, S);
end);

InstallMethod(IsomorphismSemigroup,
"for IsNTPMatrixSemigroup, pos int, pos int, trans semigroup with gens",
[IsNTPMatrixSemigroup, IsPosInt, IsPosInt,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filt, threshold, period, S)
  local n, map, T;

  n := Maximum(DegreeOfTransformationSemigroup(S), 1);
  map := x -> AsMatrix(IsNTPMatrix, x, n, threshold, period);
  T := Semigroup(List(GeneratorsOfSemigroup(S), map));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       x -> AsMatrix(IsNTPMatrix,
                                                     x,
                                                     n,
                                                     threshold,
                                                     period),
                                       x -> AsTransformation(x));
end);

#############################################################################
## Standard examples.
#############################################################################

InstallMethod(FullTropicalMaxPlusMonoid, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(dim, threshold)
  local gens, i, j;

  if dim <> 2 then
    ErrorNoReturn("Semigroups: FullTropicalMaxPlusMonoid: usage,\n",
                  "the dimension must be 2,");
  fi;

  gens := [Matrix(IsTropicalMaxPlusMatrix, [[-infinity, 0],
                                            [-infinity, -infinity]],
                                            threshold),
           Matrix(IsTropicalMaxPlusMatrix, [[-infinity, 0],
                                            [0, -infinity]],
                                            threshold),
           Matrix(IsTropicalMaxPlusMatrix, [[-infinity, 0],
                                            [0, 0]],
                                            threshold),
           Matrix(IsTropicalMaxPlusMatrix, [[-infinity, 1],
                                            [0, -infinity]],
                                            threshold)];

  for i in [1 .. threshold] do
    Add(gens, Matrix(IsTropicalMaxPlusMatrix,
                     [[-infinity, 0], [0, i]],
                     threshold));
    for j in [1 .. i] do
      Add(gens, Matrix(IsTropicalMaxPlusMatrix,
                       [[0, j], [i, 0]],
                       threshold));
    od;
  od;

  return Monoid(gens);
end);

InstallMethod(FullTropicalMinPlusMonoid, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(dim, threshold)
  local gens, i, j, k;

  if dim = 2  then
    gens := [Matrix(IsTropicalMinPlusMatrix, [[infinity, 0],
                                              [0, infinity]],
                                              threshold),
             Matrix(IsTropicalMinPlusMatrix, [[infinity, 0],
                                              [1, infinity]],
                                              threshold),
             Matrix(IsTropicalMinPlusMatrix, [[infinity, 0],
                                              [infinity, infinity]],
                                              threshold)];
    for i in [0 .. threshold] do
      Add(gens, Matrix(IsTropicalMinPlusMatrix,
                       [[infinity, 0], [0, i]],
                       threshold));
    od;
  elif dim = 3 then
    gens := [Matrix(IsTropicalMinPlusMatrix,
                    [[infinity, infinity, 0],
                     [0, infinity, infinity],
                     [infinity, 0, infinity]],
                    threshold),
             Matrix(IsTropicalMinPlusMatrix,
                    [[infinity, infinity, 0],
                     [infinity, 0, infinity],
                     [0, infinity, infinity]],
                    threshold),
             Matrix(IsTropicalMinPlusMatrix,
                    [[infinity, infinity, 0],
                     [infinity, 0, infinity],
                     [infinity, infinity, infinity]],
                    threshold),
             Matrix(IsTropicalMinPlusMatrix,
                    [[infinity, infinity, 0],
                     [infinity, 0, infinity],
                     [1, infinity, infinity]],
                    threshold)];

    for i in [0 .. threshold] do
      Add(gens, Matrix(IsTropicalMinPlusMatrix,
                       [[infinity, infinity, 0],
                        [infinity, 0, infinity],
                        [0, i, infinity]],
                       threshold));
      Add(gens, Matrix(IsTropicalMinPlusMatrix,
                       [[infinity, 0, i],
                        [i, infinity, 0],
                        [0, i, infinity]],
                        threshold));
      for j in [1 .. i] do
        Add(gens, Matrix(IsTropicalMinPlusMatrix,
                         [[infinity, 0, 0],
                          [0, infinity, i],
                          [0, j, infinity]],
                         threshold));
      od;

      for j in [1 .. threshold] do
        Add(gens, Matrix(IsTropicalMinPlusMatrix,
                         [[infinity, 0, 0],
                          [0, infinity, i],
                          [j, 0, infinity]],
                         threshold));
      od;
    od;

    for i in [1 .. threshold] do
      for j in [i .. threshold] do
        for k in [1 .. j - 1] do
          Add(gens, Matrix(IsTropicalMinPlusMatrix,
                           [[infinity, 0, i],
                            [j, infinity, 0],
                            [0, k, infinity]],
                           threshold));
        od;
      od;
    od;
  else
    ErrorNoReturn("Semigroups: FullTropicalMinPlusMonoid: usage,\n",
                  "the dimension must be 2 or 3,");
  fi;

  return Monoid(gens);
end);
