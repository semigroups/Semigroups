#############################################################################
##
#W  semifp.gi
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

SEMIGROUPS.InitFpSemigroup := function(S)
  local rels, nrgens, out, next, ext, k, rel, i, j;

  Assert(1, IsFpSemigroup(S) or IsFpMonoid(S));

  if IsBound(S!.__fp_semigroup_relations) then 
    return;
  fi;

  if IsFpMonoid(S) then 
    rels := RelationsOfFpSemigroup(AsSemigroup(IsFpSemigroup, S));
    nrgens := Length(GeneratorsOfMonoid(S)) + 1;
  else
    rels := RelationsOfFpSemigroup(S);
    nrgens := Length(GeneratorsOfSemigroup(S));
  fi;

  out := [];

  for rel in rels do 
    next := [[], []];
    for i in [1, 2] do
      ext := ExtRepOfObj(rel[i]);
      for j in [1, 3 .. Length(ext) - 1] do 
        k := 0;
        while k < ext[j + 1] do 
          Add(next[i], ext[j]);
          k := k + 1;
        od;
      od;
    od;
    Add(out, next);
  od;

  S!.__fp_semigroup_relations := out;
  S!.__fp_semigroup_nrgens    := nrgens; 
  S!.report                   := SEMIGROUPS.DefaultOptionsRec.report;
end;

InstallMethod(ExtRepOfObj, "for an element of an fp semigroup", 
[IsElementOfFpSemigroup], 
function(x)
  return ExtRepOfObj(UnderlyingElement(x));
end);

InstallMethod(ExtRepOfObj, "for an element of an fp monoid", 
[IsElementOfFpMonoid], 
function(x)
  return ExtRepOfObj(UnderlyingElement(x));
end);

InstallMethod(Size, "for an fp semigroup", [IsFpSemigroup], 
function(S)
  SEMIGROUPS.InitFpSemigroup(S);
  return FP_SEMI_SIZE(S);
end);

InstallMethod(Size, "for an fp monoid", [IsFpMonoid], 
function(S)
  SEMIGROUPS.InitFpSemigroup(S);
  return FP_SEMI_SIZE(S);
end);

InstallMethod(\= , "for two elements of an f.p. semigroup",
IsIdenticalObj, [IsElementOfFpSemigroup, IsElementOfFpSemigroup],
function(x1, x2)
  local S;
  S := FpSemigroupOfElementOfFpSemigroup(x1);
  SEMIGROUPS.InitFpSemigroup(S);
  return FP_SEMI_EQ(S, ExtRepOfObj(x1), ExtRepOfObj(x2));
end);

InstallMethod(\= , "for two elements of an f.p. monoid",
IsIdenticalObj, [IsElementOfFpMonoid, IsElementOfFpMonoid],
function(x1, x2)
  local map;
  map := IsomorphismFpSemigroup(FpMonoidOfElementOfFpMonoid(x1));
  return x1 ^ map = x2 ^ map;
end);

InstallMethod(\< , "for two elements of a f.p. semigroup",
IsIdenticalObj, [IsElementOfFpSemigroup, IsElementOfFpSemigroup],
function(x1, x2)
  local S;
  S := FpSemigroupOfElementOfFpSemigroup(x1);
  SEMIGROUPS.InitFpSemigroup(S);
  return FP_SEMI_COSET_ID(S, ExtRepOfObj(x1))
    < FP_SEMI_COSET_ID(S, ExtRepOfObj(x2));
end);

InstallMethod(\< , "for two elements of a f.p. monoid",
IsIdenticalObj, [IsElementOfFpMonoid, IsElementOfFpMonoid],
function(x1, x2)
  local map;
  map := IsomorphismFpSemigroup(FpMonoidOfElementOfFpMonoid(x1));
  return x1 ^ map < x2 ^ map;
end);

#TODO AsSSortedList, RightCayleyGraph, any more?

InstallMethod(ViewString, "for an f.p. semigroup element",
[IsElementOfFpSemigroup], String);

InstallMethod(ViewString, "for an f.p. monoid element",
[IsElementOfFpMonoid], String);

#InstallMethod(ViewObj, "for an f.p. monoid", 
#[IsFpMonoid and HasGeneratorsOfMonoid], 
#function(M)
#  Print(ViewString(M));
#end);
#
#InstallMethod(ViewString, "for an f.p. monoid", 
#[IsFpMonoid and HasGeneratorsOfMonoid],
#function(M)
#  local str;
#
#  str := "<fp monoid with ";
#  Append(str, String(Length(GeneratorsOfMonoid(M))));
#  Append(str, " generator");
#  if Length(GeneratorsOfMonoid(M)) > 1 then
#    Append(str, "s");
#  fi;
#  Append(str, " and ");
#  Append(str, String(Length(RelationsOfFpMonoid(M))));
#  Append(str, " relation");
#  if Length(RelationsOfFpMonoid(M)) > 1 then
#    Append(str, "s");
#  fi;
#  Append(str, ">");
#
#  return PRINT_STRINGIFY(str);
#end);
#
#InstallMethod(ViewObj, "for an f.p. semigroup", 
#[IsFpSemigroup and HasGeneratorsOfSemigroup], 
#function(M)
#  Print(ViewString(M));
#end);
#
#InstallMethod(ViewString, "for an f.p. semigroup", 
#[IsFpSemigroup and HasGeneratorsOfSemigroup],
#function(M)
#  local str;
#
#  str := "<fp monoid with ";
#  Append(str, String(Length(GeneratorsOfSemigroup(M))));
#  Append(str, " generator");
#  if Length(GeneratorsOfSemigroup(M)) > 1 then
#    Append(str, "s");
#  fi;
#  Append(str, " and ");
#  Append(str, String(Length(RelationsOfFpSemigroup(M))));
#  Append(str, " relation");
#  if Length(RelationsOfFpSemigroup(M)) > 1 then
#    Append(str, "s");
#  fi;
#  Append(str, ">");
#
#  return PRINT_STRINGIFY(str);
#end);

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons, 
[IsFpSemigroup, IsList],
function(filt, params)
  if Length(params) < 1 then # nr gens
    params[1] := Random([1 .. 20]);
  fi;
  if not IsPosInt(params[1]) then 
    ErrorNoReturn();
  fi;
  return params;
end);

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons, 
[IsFpMonoid, IsList],
function(filt, params)
  return SEMIGROUPS_ProcessRandomArgsCons(IsFpSemigroup, params);
end);

# FIXME this doesn't work very well

InstallMethod(RandomSemigroupCons, "for IsFpSemigroup and a list",
[IsFpSemigroup, IsList],
function(filt, params)
  return AsSemigroup(IsFpSemigroup,
                     CallFuncList(RandomSemigroup,
                                  Concatenation([IsTransformationSemigroup],
                                                 params)));
end);

# FIXME this doesn't work very well

InstallMethod(RandomMonoidCons, "for IsFpMonoid and a list",
[IsFpMonoid, IsList],
function(filt, params)
  return AsMonoid(IsFpMonoid,
                  CallFuncList(RandomMonoid,
                               Concatenation([IsTransformationMonoid],
                                              params)));
end);

# FIXME this doesn't work very well

InstallMethod(RandomInverseSemigroupCons, "for IsFpSemigroup and a list",
[IsFpSemigroup, IsList],
function(filt, params)
  return AsSemigroup(IsFpSemigroup,
                     CallFuncList(RandomInverseSemigroup,
                                  Concatenation([IsPartialPermSemigroup],
                                                params)));
end);

# FIXME this doesn't work very well

InstallMethod(RandomInverseMonoidCons, "for IsFpMonoid and a list",
[IsFpMonoid, IsList],
function(filt, params)
  return AsMonoid(IsFpMonoid,
                     CallFuncList(RandomInverseMonoid,
                                  Concatenation([IsPartialPermMonoid],
                                                params)));
end);

InstallMethod(IsomorphismSemigroup, "for IsFpSemigroup and a semigroup",
[IsFpSemigroup, IsSemigroup],
function(filt, S)
  return IsomorphismFpSemigroup(S);
end);

InstallMethod(AsMonoid, "for an fp semigroup",
[IsFpSemigroup],
function(S)
  if MultiplicativeNeutralElement(S) = fail then
    return fail; # so that we do the same as the GAP/ref manual says
  fi;
  return Range(IsomorphismMonoid(IsFpMonoid, S));
end);

InstallMethod(IsomorphismMonoid, "for IsFpMonoid and a semigroup",
[IsFpMonoid, IsSemigroup],
function(filt, S)
  return IsomorphismFpMonoid(S);
end);

# same method for ideals

InstallMethod(IsomorphismFpSemigroup, "for a semigroup",
[IsSemigroup], 3,
function(S)
  local rules, F, A, rels, Q, B, map, inv;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  rules := EN_SEMI_RELATIONS(S);

  F := FreeSemigroup(Length(GeneratorsOfSemigroup(S)));
  A := GeneratorsOfSemigroup(F);
  rels := List(rules, x -> [EvaluateWord(A, x[1]), EvaluateWord(A, x[2])]);

  Q := F / rels;
  B := GeneratorsOfSemigroup(Q);

  map := x -> EvaluateWord(B, MinimalFactorization(S, x));
  inv := x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfSemigroup(S));

  return MagmaIsomorphismByFunctionsNC(S, Q, map, inv);
end);

# same method for ideals

InstallMethod(IsomorphismFpMonoid, "for a semigroup",
[IsSemigroup], 8,
function(S)
  local sgens, mgens, F, A, start, lookup, spos, mpos, pos, rules, rels,
  convert, word, is_redundant, Q, map, inv, i, rule;

  if not IsMonoidAsSemigroup(S) then
    ErrorNoReturn("Semigroups: IsomorphismFpMonoid: usage,\n",
                  "the semigroup given as first argument must have a ",
                  "multiplicative neutral element,");
  elif not IsFinite(S) then
    TryNextMethod();
  fi;

  sgens := GeneratorsOfSemigroup(S);
  mgens := Filtered(sgens,
                    x -> x <> MultiplicativeNeutralElement(S));

  F := FreeMonoid(Length(mgens));
  A := GeneratorsOfMonoid(F);
  start := [1 .. Length(sgens)] * 0;
  lookup := [];
  # make sure we map duplicate generators to the correct values
  for i in [1 .. Length(sgens)] do
    spos := Position(sgens, sgens[i]);
    mpos := Position(mgens, sgens[i], start[spos]);
    lookup[i] := mpos;
    if spos <> fail then 
      start[spos] := mpos;
    fi;
  od;
            
  pos := Position(lookup, fail);

  rules := EN_SEMI_RELATIONS(S);
  rels := [];

  # convert a word in GeneratorsOfSemigroup to a word in GeneratorsOfMonoid
  convert := function(word)
    local out, i;
    out := One(F);
    for i in word do
      if lookup[i] <> fail then
        out := out * A[lookup[i]];
      fi;
    od;
    return out;
  end;

  if mgens = sgens then
    # the identity is not a generator, so to avoid adjoining an additional
    # identity in the output, we must add a relation equating the identity with
    # a word in the generators.
    word := MinimalFactorization(S, MultiplicativeNeutralElement(S));
    Add(rels, [convert(word), One(F)]);
    # Note that the previously line depends on Factorization always giving a
    # factorization in the GeneratorsOfSemigroup(S), and not in
    # GeneratorsOfMonoid(S) if S happens to be a monoid.
  fi;

  # check if a rule is a consequence of the relation (word = one)
  is_redundant := function(rule)
    local prefix, suffix, i;
    if not IsBound(word) or Length(rule[1]) < Length(word) then
      return false;
    fi;

    # check if <word> is a prefix
    prefix := true;
    for i in [1 .. Length(word)] do
      if word[i] <> rule[1][i] then
        prefix := false;
        break;
      fi;
    od;
    if prefix then
      return rule[1]{[Length(word) + 1 .. Length(rule[1])]} = rule[2];
    fi;

    # check if <word> is a suffix
    suffix := true;
    for i in [1 .. Length(word)] do
      if word[i] <> rule[1][i] then
        suffix := false;
        break;
      fi;
    od;
    if suffix then
      return rule[1]{[1 .. Length(rule[1]) - Length(word)]} = rule[2];
    fi;
    return false;
  end;

  for rule in rules do
    # only include non-redundant rules
    if (Length(rule[1]) <> 2
        or (rule[1][1] <> pos and rule[1][Length(rule[1])] <> pos))
        and (not is_redundant(rule)) then
      Add(rels, [convert(rule[1]), convert(rule[2])]);
    fi;
  od;

  Q := F / rels;

  if sgens = mgens then
    map := x -> EvaluateWord(GeneratorsOfMonoid(Q),
                             MinimalFactorization(S, x));
  else
    map := x -> EvaluateWord(GeneratorsOfSemigroup(Q),
                             MinimalFactorization(S, x));
  fi;

  inv := function(x)
    if not IsOne(UnderlyingElement(x)) then
      return MappedWord(UnderlyingElement(x), A, mgens);
    fi;
    return MultiplicativeNeutralElement(S);
  end;

  return MagmaIsomorphismByFunctionsNC(S, Q, map, inv);
end);

InstallMethod(AssignGeneratorVariables, "for a free semigroup",
[IsFreeSemigroup],
function(S)
  DoAssignGenVars(GeneratorsOfSemigroup(S));
end);

InstallMethod(AssignGeneratorVariables, "for an free monoid",
[IsFreeMonoid],
function(S)
  DoAssignGenVars(GeneratorsOfMonoid(S));
end);

InstallMethod(IsomorphismFpSemigroup, "for a group",
[IsGroup],
function(G)
  local iso1, inv1, iso2, inv2;

  if IsFpGroup(G) or IsTrivial(G) or not IsFinite(G) then
    TryNextMethod();
  fi;

  iso1 := IsomorphismFpGroup(G);
  inv1 := InverseGeneralMapping(iso1);
  # TODO the method for IsomorphismFpSemigroup uses the generators of G and
  # their inverses, since we know that G is finite this could be avoided.
  iso2 := IsomorphismFpSemigroup(Range(iso1));
  inv2 := InverseGeneralMapping(iso2);

  return MagmaIsomorphismByFunctionsNC(G,
                                       Range(iso2),
                                       x -> (x ^ iso1) ^ iso2,
                                       x -> (x ^ inv2) ^ inv1);
end);

InstallMethod(IsomorphismFpMonoid, "for a group",
[IsGroup],
function(G)
  local iso1, inv1, iso2, inv2;

  if IsFpGroup(G) or IsTrivial(G) or not IsFinite(G) then
    TryNextMethod();
  fi;

  iso1 := IsomorphismFpGroup(G);
  inv1 := InverseGeneralMapping(iso1);
  # TODO the method for IsomorphismFpMonoid uses the generators of G and their
  # inverses, since we know that G is finite this could be avoided.
  iso2 := IsomorphismFpMonoid(Range(iso1));
  inv2 := InverseGeneralMapping(iso2);

  return MagmaIsomorphismByFunctionsNC(G,
                                       Range(iso2),
                                       x -> (x ^ iso1) ^ iso2,
                                       x -> (x ^ inv2) ^ inv1);
end);
