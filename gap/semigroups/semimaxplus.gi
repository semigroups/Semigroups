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

# fallback method: isomorphism from an arbitrary semigroup to a matrix
# semigroup via a transformation semigroup

for _IsXMatrix in ["IsNTPMatrix",
                   "IsMaxPlusMatrix",
                   "IsMinPlusMatrix",
                   "IsTropicalMaxPlusMatrix",
                   "IsTropicalMinPlusMatrix",
                   "IsProjectiveMaxPlusMatrix",
                   "IsIntegerMatrix"] do

  _IsXSemigroup := Concatenation(_IsXMatrix, "Semigroup");
  _IsXMonoid    := Concatenation(_IsXMatrix, "Monoid");

  InstallMethod(IsomorphismSemigroup,
  Concatenation("for ", _IsXSemigroup, " and a semigroup"),
  [EvalString(_IsXSemigroup), IsSemigroup],
  SEMIGROUPS.DefaultIsomorphismSemigroup);

  InstallMethod(IsomorphismMonoid,
  Concatenation("for ", _IsXMonoid, " and a semigroup"),
  [EvalString(_IsXMonoid), IsSemigroup],
  SEMIGROUPS.DefaultIsomorphismMonoid);

od;

Unbind(_IsXSemigroup);
Unbind(_IsXMonoid);

# isomorphism from a transformation semigroup to a matrix
# semigroup.

# so that the value of _IsXMatrix is retained as a local variable

_InstallIsomorphismMonoid := function(filter)
  local IsXSemigroup, IsXMonoid;

  IsXSemigroup := Concatenation(filter, "Semigroup");
  IsXMonoid := Concatenation(filter, "Monoid");

  InstallMethod(IsomorphismMonoid,
  Concatenation("for ", IsXMonoid, " and a monoid"),
  [EvalString(IsXMonoid), IsMonoid],
  function(filter, S)
    return IsomorphismSemigroup(EvalString(IsXSemigroup), S);
  end);

end;

_InstallIsomorphismSemigroup := function(filter)
  local IsXSemigroup, IsXMatrix;
  IsXMatrix := filter;
  IsXSemigroup := Concatenation(filter, "Semigroup");

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
  _InstallIsomorphismSemigroup(_IsXMatrix);
  _InstallIsomorphismMonoid(_IsXMatrix);
od;

_InstallIsomorphismSemigroup := function(filter)
  local IsXSemigroup, IsXMatrix;
  IsXMatrix := filter;
  IsXSemigroup := Concatenation(filter, "Semigroup");

  InstallMethod(IsomorphismSemigroup,
  Concatenation("for ", IsXSemigroup,
                " and a transformation semigroup with generators"),
  [EvalString(IsXSemigroup),
   IsTransformationSemigroup and HasGeneratorsOfSemigroup],
  function(filt, S)
    local n, map, T;

    n    := Maximum(DegreeOfTransformationSemigroup(S), 1);
    map  := x -> AsMatrix(EvalString(IsXMatrix), x, n, 1);
    T := Semigroup(List(GeneratorsOfSemigroup(S), map));
    UseIsomorphismRelation(S, T);

    return MagmaIsomorphismByFunctionsNC(S,
                                         T,
                                         map,
                                         AsTransformation);
  end);
end;

# isomorphism from a transformation semigroup to a matrix
# semigroup.

for _IsXMatrix in ["IsTropicalMaxPlusMatrix",
                   "IsTropicalMinPlusMatrix"] do
  _InstallIsomorphismSemigroup(_IsXMatrix);
  _InstallIsomorphismMonoid(_IsXMatrix);
od;

_InstallIsomorphismMonoid("IsNTPMatrix");

InstallMethod(IsomorphismSemigroup,
"for IsNTPMatrixSemigroup and transformation semigroup with gens",
[IsNTPMatrixSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filt, S)
  local n, map, T;

  n := Maximum(DegreeOfTransformationSemigroup(S), 1);
  map := x -> AsMatrix(EvalString(IsNTPMatrix), x, n, 1, 1);
  T := Semigroup(List(GeneratorsOfSemigroup(S), map));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       x -> AsMatrix(IsNTPMatrix,
                                                     x,
                                                     n,
                                                     1,
                                                     1),
                                       x -> AsTransformation(x));
end);

Unbind(_InstallIsomorphismSemigroup);
Unbind(_InstallIsomorphismMonoid);
Unbind(_IsXMatrix);

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
