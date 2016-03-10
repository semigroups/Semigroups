#############################################################################
##
#W  semifp.gd
#Y  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(ViewString, "for an f.p. semigroup element",
[IsElementOfFpSemigroup], String);

InstallMethod(ViewString, "for an f.p. monoid element",
[IsElementOfFpMonoid], String);

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
  local sgens, mgens, F, A, lookup, pos, rules, rels, convert, word,
   is_redundant, Q, B, map, inv, rule;

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
  lookup := List(sgens, x -> Position(mgens, x));
  pos := Position(lookup, fail);

  rules := SEMIGROUP_RELATIONS(GenericSemigroupData(S));
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
    word := Factorization(S, MultiplicativeNeutralElement(S));
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
