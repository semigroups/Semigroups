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

InstallMethod(IsomorphismSemigroup,
"for IsMaxPlusMatrixSemigroup and a semigroup",
[IsMaxPlusMatrixSemigroup, IsSemigroup],
SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsMinPlusMatrixSemigroup and a semigroup",
[IsMinPlusMatrixSemigroup, IsSemigroup],
SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsTropicalMaxPlusMatrixSemigroup and a semigroup",
[IsTropicalMaxPlusMatrixSemigroup, IsSemigroup],
SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsTropicalMinPlusMatrixSemigroup and a semigroup",
[IsTropicalMinPlusMatrixSemigroup, IsSemigroup],
SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsProjectiveMaxPlusMatrixSemigroup and a semigroup",
[IsProjectiveMaxPlusMatrixSemigroup, IsSemigroup],
SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsNTPMatrixSemigroup and a semigroup",
[IsNTPMatrixSemigroup, IsSemigroup],
SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsIntegerMatrixSemigroup and a semigroup",
[IsIntegerMatrixSemigroup, IsSemigroup],
SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsMaxPlusMatrixSemigroup and a transformation semigroup with generators",
[IsMaxPlusMatrixSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filt, S)
  local deg, gens;

  deg := DegreeOfTransformationSemigroup(S);
  gens := List(GeneratorsOfSemigroup(S),
               x -> AsMatrix(IsMaxPlusMatrix, x, deg));
  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(gens),
                                       x -> AsMatrix(IsMaxPlusMatrix, x, deg),
                                       x -> AsTransformation(x));
end);

InstallMethod(IsomorphismSemigroup,
"for IsMinPlusMatrixSemigroup and a transformation semigroup with generators",
[IsMinPlusMatrixSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filt, S)
  local deg, gens;

  deg := DegreeOfTransformationSemigroup(S);
  gens := List(GeneratorsOfSemigroup(S),
               x -> AsMatrix(IsMinPlusMatrix, x, deg));
  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(gens),
                                       x -> AsMatrix(IsMinPlusMatrix, x, deg),
                                       x -> AsTransformation(x));
end);

InstallMethod(IsomorphismSemigroup,
"for IsTropicalMaxPlusMatrixSemigroup and transformation semigroup with gens",
[IsTropicalMaxPlusMatrixSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filt, S)
  local deg, gens;

  deg := DegreeOfTransformationSemigroup(S);
  gens := List(GeneratorsOfSemigroup(S),
               x -> AsMatrix(IsTropicalMaxPlusMatrix, x, deg, 1));
  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(gens),
                                       x -> AsMatrix(IsTropicalMaxPlusMatrix,
                                                     x,
                                                     deg,
                                                     1),
                                       x -> AsTransformation(x));
end);

InstallMethod(IsomorphismSemigroup,
"for IsTropicalMinPlusMatrixSemigroup and transformation semigroup with gens",
[IsTropicalMinPlusMatrixSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filt, S)
  local deg, gens;

  deg := DegreeOfTransformationSemigroup(S);
  gens := List(GeneratorsOfSemigroup(S),
               x -> AsMatrix(IsTropicalMinPlusMatrix, x, deg, 1));
  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(gens),
                                       x -> AsMatrix(IsTropicalMinPlusMatrix,
                                                     x,
                                                     deg,
                                                     1),
                                       x -> AsTransformation(x));
end);

InstallMethod(IsomorphismSemigroup,
"for IsProjectiveMaxPlusMatrixSemigroup and transformation semigroup with gens",
[IsProjectiveMaxPlusMatrixSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filt, S)
  local deg, gens;

  deg := DegreeOfTransformationSemigroup(S);
  gens := List(GeneratorsOfSemigroup(S),
               x -> AsMatrix(IsProjectiveMaxPlusMatrix, x, deg));
  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(gens),
                                       x -> AsMatrix(IsProjectiveMaxPlusMatrix,
                                                     x,
                                                     deg),
                                       x -> AsTransformation(x));
end);

InstallMethod(IsomorphismSemigroup,
"for IsNTPMatrixSemigroup and transformation semigroup with gens",
[IsNTPMatrixSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filt, S)
  local deg, gens;

  deg := DegreeOfTransformationSemigroup(S);
  gens := List(GeneratorsOfSemigroup(S),
               x -> AsMatrix(IsNTPMatrix, x, deg, 1, 1));
  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(gens),
                                       x -> AsMatrix(IsNTPMatrix,
                                                     x,
                                                     deg,
                                                     1,
                                                     1),
                                       x -> AsTransformation(x));
end);

InstallMethod(IsomorphismSemigroup,
"for IsIntegerMatrixSemigroup and transformation semigroup with gens",
[IsIntegerMatrixSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(filt, S)
  local deg, gens;

  deg := DegreeOfTransformationSemigroup(S);
  gens := List(GeneratorsOfSemigroup(S),
               x -> AsMatrix(IsIntegerMatrix, x, deg));
  return MagmaIsomorphismByFunctionsNC(S,
                                       Semigroup(gens),
                                       x -> AsMatrix(IsIntegerMatrix,
                                                     x,
                                                     deg),
                                       x -> AsTransformation(x));
end);

InstallMethod(FullTropicalMaxPlusMonoid, "for pos int and pos int",
[IsPosInt, IsPosInt],
function(dim, threshold)
  local gens, i, j;

  if dim <> 2 then
    ErrorNoReturn();
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
    Add(gens, Matrix(IsTropicalMaxPlusMatrix, [[-infinity, 0], [0, i]], threshold));
    for j in [1 .. i] do
      Add(gens, Matrix(IsTropicalMaxPlusMatrix, [[0, j], [i, 0]], threshold));
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
                                              [0, infinity]], threshold),
             Matrix(IsTropicalMinPlusMatrix, [[infinity, 0],
                                              [1, infinity]], threshold),
             Matrix(IsTropicalMinPlusMatrix, [[infinity, 0],
                                              [infinity, infinity]], threshold)];
    for i in [0 .. threshold] do
      Add(gens, Matrix(IsTropicalMinPlusMatrix, [[infinity, 0], [0, i]], threshold));
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
    ErrorNoReturn();
  fi;

  return Monoid(gens);
end);
