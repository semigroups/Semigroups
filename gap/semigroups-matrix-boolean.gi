############################################################################
##
#W  semibool.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for semigroups of boolean matrices.

InstallMethod(RandomBooleanMatSemigroup, "for a pos ints",
[IsPosInt, IsPosInt],
function(nrgens, dim)
  local gens, i;
  gens := [];
  for i in [1 .. nrgens] do
    Add(gens, RandomBooleanMat(dim));
  od;
  return Semigroup(gens);
end);

InstallMethod(RandomBooleanMatMonoid, "for a pos ints",
[IsPosInt, IsPosInt],
function(nrgens, dim)
  local gens, i;
  gens := [];
  for i in [1 .. nrgens] do
    Add(gens, RandomBooleanMat(dim));
  od;
  return Monoid(gens);
end);

# not relevant for ideals
# FIXME this doesn't yet work!
#InstallMethod(IsomorphismTransformationSemigroup,
#"for semigroup of boolean matrices with generators",
#[IsBooleanMatSemigroup and HasGeneratorsOfSemigroup],
#function(S)
#
#  act := function(vec, mat) # return the ith row of mat
#    out := EmptyPlist(mat![1]);
#    for j in [1 .. mat![1]] do
#      out[j] := mat![(i - 1) * mat![1] + j];
#    od;
#    return out;
#  end;
#
#  n := DimensionOfBooleanMat(GeneratorsOfSemigroup(S)[1]);
#  pts := [];
#
#  for i in [1 .. n] do
#    # TODO improve this to keep track of those things seen before.
#    o := Enumerate(Orb(S, i, act));
#    pts := Union(pts, AsList(o));
#  od;
#
#  pos := List([1 .. n], x -> Position(pts, []));
#  t := Semigroup(List(GeneratorsOfSemigroup(s),
#                      x -> TransformationOpNC(x, pts, OnPoints)));
#  # gaplint: ignore 3
#  return MappingByFunction(s, t,
#           x -> TransformationOpNC(x, pts, OnPoints),
#           x -> BinaryRelationOnPoints(List([1 .. n], i -> pts[pos[i] ^ x])));
#end);
