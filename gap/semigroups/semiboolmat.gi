############################################################################
##
#W  semiboolmat.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for semigroups of boolean matrices.

#############################################################################
## 1. Isomorphisms etc.
#############################################################################

InstallMethod(IsomorphismBooleanMatSemigroup,
"for a transformation semigroup", [IsTransformationSemigroup],
function(S)
  local n, T;
  n := Maximum(1, DegreeOfTransformationSemigroup(S));
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
    o := Enumerate(Orb(S, BlistList([1 .. n], [i]), OnBlist));
    pts := Union(pts, AsList(o));
  od;
  ShrinkAllocationPlist(pts);
  pos := List([1 .. n], x -> Position(pts, BlistList([1 .. n], [x])));
  T := Semigroup(List(GeneratorsOfSemigroup(S),
                      x -> TransformationOpNC(x, pts, OnBlist)));
  # gaplint: ignore 3
  return MappingByFunction(S, T,
           x -> TransformationOpNC(x, pts, OnBlist),
           x -> BooleanMatNC(List([1 .. n], i -> pts[pos[i] ^ x])));
end);

#############################################################################
## 2. Standard examples - known generators
#############################################################################

InstallMethod(RegularBooleanMatMonoid, "for a pos int",
[IsPosInt],
function(n)
  local gens, i, j;

  if n = 1 then
    return Monoid(BooleanMat([[true]]), BooleanMat([[false]]));
  fi;

  gens := [];

  gens[2] := List([1 .. n], x -> BlistList([1 .. n], []));
  for j in [1 .. n - 1] do
    gens[2][j][j + 1] := true;
  od;
  gens[2][n][1] := true;

  for i in [3, 4] do
    gens[i] := List([1 .. n], x -> BlistList([1 .. n], []));
    for j in [1 .. n - 1] do
      gens[i][j][j] := true;
    od;
  od;
  gens[3][n][1] := true;
  gens[3][n][n] := true;

  Apply(gens, BooleanMat);

  gens[1] := AsBooleanMat((1, 2), n);

  return Monoid(gens);
end);

InstallMethod(GossipMonoid, "for a positive integer",
[IsPosInt],
function(n)
  local gens, i, j, x, m;

  gens := [];
  for i in [1 .. n - 1] do
    for j in [i + 1 .. n] do
      x := List([1 .. n], k -> BlistList([1 .. n], [k]));
      x[i][j] := true;
      x[j][i] := true;
      Add(gens, BooleanMat(x));
    od;
  od;

  m := Monoid(gens);
  SetNrIdempotents(m, Bell(n));
  return m;
end);

InstallMethod(UnitriangularBooleanMatrixMonoid, "for a positive integer",
[IsPosInt],
function(n)
  local gens, x, i, j;

  gens := [];
  for i in [1 .. n - 1] do
    for j in [i + 1 .. n] do
      x := List([1 .. n], k -> BlistList([1 .. n], [k]));
      x[i][j] := true;
      Add(gens, BooleanMat(x));
    od;
  od;

  return Monoid(gens);
end);

InstallMethod(TriangularBooleanMatrixMonoid, "for a positive integer",
[IsPosInt],
function(n)
  local gens, x, i;

  gens := [];
  for i in [1 .. n] do
    x := List([1 .. n], k -> BlistList([1 .. n], [k]));
    x[i][i] := false;
    Add(gens, BooleanMat(x));
  od;

  return Monoid(UnitriangularBooleanMatrixMonoid(n), gens);
end);

#############################################################################
## 3. Standard examples - calculated generators
#############################################################################

InstallMethod(ReflexiveBooleanMatMonoid, "for a positive integer",
[IsPosInt],
function(n)
  if not IsBound(SEMIGROUPS.GENERATORS.Reflex) then
    SEMIGROUPS.GENERATORS.Reflex :=
      ReadGenerators(Concatenation(SEMIGROUPS.PackageDir,
                                   "/data/gens/reflex.pickle.gz"));
  fi;

  if not IsBound(SEMIGROUPS.GENERATORS.Reflex[n]) then
    ErrorNoReturn("Semigroups: ReflexiveBooleanMatMonoid:\n ",
                  "generators for this monoid are only known up to ",
                  String(Length(SEMIGROUPS.GENERATORS.Reflex[n])), ",");
  fi;

  return Monoid(SEMIGROUPS.GENERATORS.Reflex[n]);
end);

InstallMethod(HallMonoid, "for a positive integer",
[IsPosInt],
function(n)
  if not IsBound(SEMIGROUPS.GENERATORS.Hall) then
    SEMIGROUPS.GENERATORS.Hall :=
      ReadGenerators(Concatenation(SEMIGROUPS.PackageDir,
                                   "/data/gens/hall.pickle.gz"));
  fi;

  if not IsBound(SEMIGROUPS.GENERATORS.Hall[n]) then
    ErrorNoReturn("Semigroups: HallMonoid:\n ",
                  "generators for this monoid are only known up to ",
                  String(Length(SEMIGROUPS.GENERATORS.Hall[n])), ",");
  fi;

  return Monoid(SEMIGROUPS.GENERATORS.Hall[n]);
end);

InstallMethod(FullBooleanMatMonoid, "for a positive integer",
[IsPosInt],
function(n)
  if not IsBound(SEMIGROUPS.GENERATORS.FullBool) then
    SEMIGROUPS.GENERATORS.FullBool :=
      ReadGenerators(Concatenation(SEMIGROUPS.PackageDir,
                                   "/data/gens/fullbool.pickle.gz"));
  fi;

  if not IsBound(SEMIGROUPS.GENERATORS.FullBool[n]) then
    ErrorNoReturn("Semigroups: FullBooleanMatMonoid:\n ",
                  "generators for this monoid are only known up to ",
                  String(Length(SEMIGROUPS.GENERATORS.FullBool[n])), ",");
  fi;

  return Monoid(SEMIGROUPS.GENERATORS.FullBool[n]);
end);
