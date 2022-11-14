#############################################################################
##
##  attributes/semifp.gi
##  Copyright (C) 2017                                      Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(IndecomposableElements, "for an fp semigroup", [IsFpSemigroup],
function(S)
  local gens, rels, decomposable, uf, lens, pos1, pos2, t1, t2, x, rel;

  if HasIsSurjectiveSemigroup(S) then
    return [];
  fi;

  gens := GeneratorsOfSemigroup(FreeSemigroupOfFpSemigroup(S));
  rels := RelationsOfFpSemigroup(S);
  decomposable := BlistList(gens, []);
  uf := PartitionDS(IsPartitionDS, Length(gens));

  for rel in rels do
    lens := List(rel, Length);
    if lens[1] = 1 and lens[2] = 1 and rel[1] <> rel[2] then
      # relation of the form x[i] = x[j]:
      # a generator is repeated, so record this information using UF.
      pos1 := Position(gens, rel[1]);
      pos2 := Position(gens, rel[2]);
      t1 := Representative(uf, pos1);
      t2 := Representative(uf, pos2);
      Unite(uf, pos1, pos2);
      x := Representative(uf, pos1);
      decomposable[x] := decomposable[t1] or decomposable[t2];
    elif (lens[1] = 1 or lens[2] = 1) and rel[1] <> rel[2] then
      # Relation gives non-trivial decomposition of some generator x[i]
      pos1 := Position(gens, rel[Position(lens, 1)]);
      decomposable[Representative(uf, pos1)] := true;
    fi;
  od;
  return Set(Filtered(PartsOfPartitionDS(uf), x -> not decomposable[x[1]]),
             x -> GeneratorsOfSemigroup(S)[x[1]]);
end);

InstallMethod(AntiIsomorphismDualFpSemigroup, "for an fp semigroup",
[IsFpSemigroup],
function(S)
  local F, R, T, map, inv;
  F := FreeSemigroupOfFpSemigroup(S);
  R := List(RelationsOfFpSemigroup(S), x -> List(x, Reversed));
  T := F / R;
  map := function(x)
    local word;
    word := Reversed(UnderlyingElement(x));
    return ElementOfFpSemigroup(FamilyObj(Representative(T)), word);
  end;
  inv := function(x)
    local word;
    word := Reversed(UnderlyingElement(x));
    return ElementOfFpSemigroup(FamilyObj(Representative(S)), word);
  end;
  return MappingByFunction(S, T, map, inv);
end);

InstallMethod(AntiIsomorphismDualFpMonoid, "for an fp monoid",
[IsFpMonoid],
function(S)
  local F, R, T, map, inv;
  F := FreeMonoidOfFpMonoid(S);
  R := List(RelationsOfFpMonoid(S), x -> List(x, Reversed));
  T := F / R;
  map := function(x)
    local word;
    word := Reversed(UnderlyingElement(x));
    return ElementOfFpMonoid(FamilyObj(Representative(T)), word);
  end;
  inv := function(x)
    local word;
    word := Reversed(UnderlyingElement(x));
    return ElementOfFpMonoid(FamilyObj(Representative(S)), word);
  end;
  return MappingByFunction(S, T, map, inv);
end);
