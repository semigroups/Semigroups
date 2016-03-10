#############################################################################
##
#W  semifp.gd
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

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

# same method for ideals

InstallMethod(IsomorphismFpSemigroup, "for a semigroup",
[IsSemigroup], 3,
function(S)
  local rules, F, A, rels, Q, B, map, inv;

  if not IsFinite(S) then
    TryNextMethod();
  fi;

  rules := SEMIGROUP_RELATIONS(GenericSemigroupData(S));

  F := FreeSemigroup(Length(GeneratorsOfSemigroup(S)));
  A := GeneratorsOfSemigroup(F);
  rels := List(rules, x -> [EvaluateWord(A, x[1]), EvaluateWord(A, x[2])]);

  Q := F / rels;
  B := GeneratorsOfSemigroup(Q);

  map := x -> EvaluateWord(B, Factorization(S, x));
  inv := x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfSemigroup(S));

  return MagmaIsomorphismByFunctionsNC(S, Q, map, inv);
end);

# same method for ideals

InstallMethod(IsomorphismFpMonoid, "for a semigroup",
[IsSemigroup], 8,
function(S)
  local gens, F, A, lookup, pos, data, rules, rels, convert, Q, B, map, inv,
  rule;

  if not IsMonoidAsSemigroup(S) then 
    ErrorNoReturn("Semigroups: IsomorphismFpMonoid: usage,\n", 
                  "the semigroup given as first argument must have a ",
                  "multiplicative neutral element,");
  elif not IsFinite(S) then
    TryNextMethod();
  fi;
  
  if IsMonoid(S) then 
    gens := GeneratorsOfMonoid(S);
  else
    gens := Filtered(GeneratorsOfSemigroup(S), 
                     x -> x <> MultiplicativeNeutralElement(S));
  fi;

  F := FreeMonoid(Length(gens));
  A := GeneratorsOfMonoid(F);
  lookup := List(GeneratorsOfSemigroup(S),
                 x -> Position(gens, x));
  pos := Position(lookup, fail);

  data := GenericSemigroupData(S);
  rules := SEMIGROUP_RELATIONS(data);
  rels := [];

  convert := function(word)
    local out, i;
    out := MultiplicativeNeutralElement(F);
    for i in word do
      if lookup[i] <> fail then
        out := out * A[lookup[i]];
      fi;
    od;
    return out;
  end;

  for rule in rules do
    # only include non-redundant rules
    if Length(rule[1]) <> 2
        or (rule[1][1] <> pos and rule[1][Length(rule[1])] <> pos) then
      Add(rels, [convert(rule[1]), convert(rule[2])]);
    fi;
  od;

  Q := F / rels;
  B := GeneratorsOfSemigroup(Q);

  map := x -> EvaluateWord(B, Factorization(S, x));
  inv := x -> MappedWord(UnderlyingElement(x), A, GeneratorsOfMonoid(S));

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
