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
## ?. Random
#############################################################################

InstallMethod(RandomSemigroupCons,
"for IsBooleanMatSemigroup and list",
[IsBooleanMatSemigroup, IsList],
function(filt, params)
  return Semigroup(List([1 .. params[1]], i -> RandomMatrix(IsBooleanMat,
                                                            params[2])));
end);

InstallMethod(RandomMonoidCons,
"for IsBooleanMatMonoid and list",
[IsBooleanMatMonoid, IsList],
function(filt, params)
  return Monoid(List([1 .. params[1]], i -> RandomMatrix(IsBooleanMat,
                                                         params[2])));
end);

InstallMethod(RandomInverseSemigroupCons,
"for IsBooleanMatSemigroup and list",
[IsBooleanMatSemigroup, IsList],
SEMIGROUPS.DefaultRandomInverseSemigroup);

InstallMethod(RandomInverseMonoidCons,
"for IsBooleanMatMonoid and list",
[IsBooleanMatMonoid, IsList],
SEMIGROUPS.DefaultRandomInverseSemigroup);

#############################################################################
## 1. Isomorphisms
#############################################################################

# fallback method: via a transformation semigroup

InstallMethod(IsomorphismSemigroup,
"for IsBooleanMatSemigroup and a semigroup",
[IsBooleanMatSemigroup, IsSemigroup],
SEMIGROUPS.DefaultIsomorphismSemigroup);

InstallMethod(IsomorphismSemigroup,
"for IsBooleanMatSemigroup and a boolean mat semigroup",
[IsBooleanMatSemigroup, IsBooleanMatSemigroup],
function(filter, S)
  return MagmaIsomorphismByFunctionsNC(S, S, IdFunc, IdFunc);
end);

# It seems necessary that the method below occurs after the fallback method, in
# order that it be selected.

InstallMethod(IsomorphismSemigroup,
"for IsBooleanMatSemigroup and a transformation semigroup",
[IsBooleanMatSemigroup, IsTransformationSemigroup],
function(filter, S)
  local n, T;
  n := Maximum(1, DegreeOfTransformationSemigroup(S));
  T := Semigroup(List(GeneratorsOfSemigroup(S), x -> AsBooleanMat(x, n)));
  UseIsomorphismRelation(S, T);
  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       x -> AsBooleanMat(x, n),
                                       AsTransformation);
end);

# If the second argument here is a transformation semigroup, then
# DefaultIsomorphismMonoid uses IsomorphismTransformationMonoid, which detects
# the MultiplicativeNeutralElement of the second argument, and reduces the
# degree accordingly.

InstallMethod(AsMonoid, "for a boolean mat semigroup", 
[IsBooleanMatSemigroup],
function(S)
  return Range(IsomorphismMonoid(IsBooleanMatMonoid, S));
end);

InstallMethod(IsomorphismMonoid, "for IsBooleanMatMonoid and a semigroup",
[IsBooleanMatMonoid, IsSemigroup], SEMIGROUPS.DefaultIsomorphismMonoid);

InstallMethod(IsomorphismMonoid,
"for IsBooleanMatMonoid and a boolean mat monoid",
[IsBooleanMatMonoid, IsBooleanMatMonoid],
function(filter, S)
  return MagmaIsomorphismByFunctionsNC(S, S, IdFunc, IdFunc);
end);

InstallMethod(IsomorphismMonoid, "for IsBooleanMatMonoid and a monoid",
[IsBooleanMatMonoid, IsMonoid],
function(filter, S)
  return IsomorphismSemigroup(IsBooleanMatSemigroup, S);
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
