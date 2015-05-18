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
  return Semigroup(Set([1 .. nrgens], x -> RandomBooleanMat(dim)));
end);

InstallMethod(RandomBooleanMatMonoid, "for a pos ints",
[IsPosInt, IsPosInt],
function(nrgens, dim)
  return Monoid(Set([1 .. nrgens], x -> RandomBooleanMat(dim)));
end);

InstallMethod(IsomorphismTransformationSemigroup,
"for a boolean matrix semigroup generators",
[IsBooleanMatSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local n, pts, o, pos, T, i;

  n := Length(Representative(S)![1]);
  pts := EmptyPlist(2 ^ n);

  for i in [1 .. n] do
    o := Enumerate(Orb(S, BlistList([1 .. n], [i]), OnBlists)); 
    pts := Union(pts, AsList(o));
  od;
  ShrinkAllocationPlist(pts);
  pos := List([1 .. n], x -> Position(pts, BlistList([1 .. n], [x])));
  T := Semigroup(List(GeneratorsOfSemigroup(S),
                      x -> TransformationOpNC(x, pts, OnBlists)));
  # gaplint: ignore 3
  return MappingByFunction(S, T,
           x -> TransformationOpNC(x, pts, OnBlists),
           x -> BooleanMatNC(List([1 .. n], i -> pts[pos[i] ^ x])));
end);

GossipMonoid := function(n)
  local gens, x, i, j;
  
  gens := [];
  for i in [1 .. n - 1] do
    for j in [i + 1 .. n] do
      x := List([1 .. n], k -> BlistList([1 .. n], [k]));
      x[i][j] := true;
      x[j][i] := true;
      Add(gens, BooleanMatNC(x));
    od;
  od;

  return Monoid(gens);
end;
