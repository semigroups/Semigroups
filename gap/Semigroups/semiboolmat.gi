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

InstallMethod(IsomorphismBooleanMatSemigroup, 
"for a transformation semigroup", [IsTransformationSemigroup], 
function(S)
  local n, T;
  n := DegreeOfTransformationSemigroup(S);
  T := Semigroup(List(GeneratorsOfSemigroup(S), x -> AsBooleanMat(x, n)));
  return MappingByFunction(S, T, x -> AsBooleanMat(x, n), AsTransformation);
end);

InstallMethod(IsomorphismBooleanMatSemigroup, 
"for a semigroup", [IsSemigroup], 
function(S)
  local iso1, inv1, iso2, inv2;

  iso1 := IsomorphismTransformationSemigroup(S);
  inv1 := InverseGeneralMapping(iso1);
  iso2 := IsomorphismBooleanMatSemigroup(Range(iso1));
  inv2 := InverseGeneralMapping(iso2);

  return MappingByFunction(S, Range(iso2), 
                           x -> (x ^ iso1) ^ iso2, 
                           x -> (x ^ inv2) ^ inv1);
end);

InstallMethod(AsBooleanMatSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  return Range(IsomorphismBooleanMatSemigroup(S));
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
