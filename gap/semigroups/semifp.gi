#############################################################################
##
##  semifp.gi
##  Copyright (C) 2015                                  James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

SEMIGROUPS.InitFpSemigroup := function(S)
  local semi, rels, F;

  Assert(1, IsFpSemigroup(S) or IsFpMonoid(S));

  if IsBound(S!.cong) then
    return;
  fi;

  if IsFpMonoid(S) then
    S!.iso := IsomorphismSemigroup(IsFpSemigroup, S);
    semi := Range(S!.iso);
    rels := RelationsOfFpSemigroup(semi);
    F := FreeSemigroupOfFpSemigroup(semi);
  else
    rels := RelationsOfFpSemigroup(S);
    F := FreeSemigroupOfFpSemigroup(S);
  fi;

  S!.cong := SemigroupCongruenceByGeneratingPairs(F, rels);
  S!.report := SEMIGROUPS.DefaultOptionsRec.report;
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
  return NrEquivalenceClasses(S!.cong);
end);

InstallMethod(Size, "for an fp monoid", [IsFpMonoid],
function(S)
  SEMIGROUPS.InitFpSemigroup(S);
  return NrEquivalenceClasses(S!.cong);
end);

InstallMethod(\=, "for two elements of an f.p. semigroup",
IsIdenticalObj, [IsElementOfFpSemigroup, IsElementOfFpSemigroup],
function(x1, x2)
  local S;
  S := FpSemigroupOfElementOfFpSemigroup(x1);
  SEMIGROUPS.InitFpSemigroup(S);
  return [UnderlyingElement(x1), UnderlyingElement(x2)] in S!.cong;
end);

InstallMethod(\=, "for two elements of an f.p. monoid",
IsIdenticalObj, [IsElementOfFpMonoid, IsElementOfFpMonoid],
function(x1, x2)
  local M;
  M := FpMonoidOfElementOfFpMonoid(x1);
  SEMIGROUPS.InitFpSemigroup(M);
  return x1 ^ M!.iso = x2 ^ M!.iso;
end);

InstallMethod(\<, "for two elements of a f.p. semigroup",
IsIdenticalObj, [IsElementOfFpSemigroup, IsElementOfFpSemigroup],
function(x1, x2)
  local S, class1, class2;
  S := FpSemigroupOfElementOfFpSemigroup(x1);
  SEMIGROUPS.InitFpSemigroup(S);
  class1 := EquivalenceClassOfElement(S!.cong, UnderlyingElement(x1));
  class2 := EquivalenceClassOfElement(S!.cong, UnderlyingElement(x2));
  return class1 < class2;
end);

InstallMethod(\<, "for two elements of a f.p. monoid",
IsIdenticalObj, [IsElementOfFpMonoid, IsElementOfFpMonoid],
function(x1, x2)
  local map;
  map := IsomorphismFpSemigroup(FpMonoidOfElementOfFpMonoid(x1));
  return x1 ^ map < x2 ^ map;
end);

# TODO AsSSortedList, RightCayleyDigraph, any more?

InstallMethod(ViewString, "for an f.p. semigroup element",
[IsElementOfFpSemigroup], String);

InstallMethod(ViewString, "for an f.p. monoid element",
[IsElementOfFpMonoid], String);

# InstallMethod(ViewObj, "for an f.p. monoid",
# [IsFpMonoid and HasGeneratorsOfMonoid],
# function(M)
#   Print(ViewString(M));
# end);
#
# InstallMethod(ViewString, "for an f.p. monoid",
# [IsFpMonoid and HasGeneratorsOfMonoid],
# function(M)
#   local str;
#
#   str := "<fp monoid with ";
#   Append(str, String(Length(GeneratorsOfMonoid(M))));
#   Append(str, " generator");
#   if Length(GeneratorsOfMonoid(M)) > 1 then
#     Append(str, "s");
#   fi;
#   Append(str, " and ");
#   Append(str, String(Length(RelationsOfFpMonoid(M))));
#   Append(str, " relation");
#   if Length(RelationsOfFpMonoid(M)) > 1 then
#     Append(str, "s");
#   fi;
#   Append(str, ">");
#
#   return PRINT_STRINGIFY(str);
# end);
#
# InstallMethod(ViewObj, "for an f.p. semigroup",
# [IsFpSemigroup and HasGeneratorsOfSemigroup],
# function(M)
#   Print(ViewString(M));
# end);
#
# InstallMethod(ViewString, "for an f.p. semigroup",
# [IsFpSemigroup and HasGeneratorsOfSemigroup],
# function(M)
#   local str;
#
#   str := "<fp monoid with ";
#   Append(str, String(Length(GeneratorsOfSemigroup(M))));
#   Append(str, " generator");
#   if Length(GeneratorsOfSemigroup(M)) > 1 then
#     Append(str, "s");
#   fi;
#   Append(str, " and ");
#   Append(str, String(Length(RelationsOfFpSemigroup(M))));
#   Append(str, " relation");
#   if Length(RelationsOfFpSemigroup(M)) > 1 then
#     Append(str, "s");
#   fi;
#   Append(str, ">");
#
#   return PRINT_STRINGIFY(str);
# end);

InstallMethod(SEMIGROUPS_ProcessRandomArgsCons,
[IsFpSemigroup, IsList],
function(filt, params)
  if Length(params) < 1 then  # nr gens
    params[1] := Random(1, 20);
    params[2] := Random(1, 8);
  elif Length(params) < 2 then  # degree
    params[2] := Random(1, 8);
  fi;
  if not ForAll(params, IsPosInt) then
    ErrorNoReturn("Semigroups: SEMIGROUPS_ProcessRandomArgsCons: ",
                  "usage,\nthe parameter must be pos ints,");
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
    return fail;  # so that we do the same as the GAP/ref manual says
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

  map := x -> EvaluateWord(B, Factorization(S, x));
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
                  "the first argument (a semigroup) must ",
                  "satisfy `IsMonoidAsSemigroup`,");
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
    if mpos <> fail then
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

  if sgens = mgens then
    map := x -> EvaluateWord(GeneratorsOfMonoid(Q),
                             Factorization(S, x));
  else
    map := x -> EvaluateWord(GeneratorsOfSemigroup(Q),
                             Factorization(S, x));
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

SEMIGROUPS.ExtRepObjToWord := function(ext_rep_obj)
  local n, word, val, pow, i;
  n    := Length(ext_rep_obj);
  word := [];
  for i in [1, 3 .. n - 1] do
    val := ext_rep_obj[i];
    pow := ext_rep_obj[i + 1];
    while pow > 0 do
      Add(word, val);
      pow := pow - 1;
    od;
  od;
  return word;
end;

SEMIGROUPS.WordToExtRepObj := function(word)
  local ext_rep_obj, i, j;
  ext_rep_obj := [];
  i           := 1;
  j           := 1;

  while i <= Length(word) do
    Add(ext_rep_obj, word[i]);
    Add(ext_rep_obj, 1);
    i := i + 1;
    while i <= Length(word) and word[i] = ext_rep_obj[j] do
      ext_rep_obj[j + 1] := ext_rep_obj[j + 1] + 1;
      i := i + 1;
    od;
    j := j + 2;
  od;
  return ext_rep_obj;
end;

SEMIGROUPS.ExtRepObjToString := function(ext_rep_obj)
  local alphabet, out, i;
  alphabet := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  out := "";
  for i in [1, 3 .. Length(ext_rep_obj) - 1] do
    if ext_rep_obj[i] > Length(alphabet) then
      ErrorNoReturn("SEMIGROUPS.ExtRepObjToString: the maximum value in an ",
                    "odd position of the argument must be at most ",
                    Length(alphabet), ",");
    fi;
    Add(out, alphabet[ext_rep_obj[i]]);
    if ext_rep_obj[i + 1] > 1 then
      Append(out, " ^ ");
      Append(out, String(ext_rep_obj[i + 1]));
    fi;
  od;
  return out;
end;

SEMIGROUPS.WordToString := function(word)
  local alphabet, out, letter;
  alphabet := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  out := "";
  for letter in word do
    if letter > Length(alphabet) then
      ErrorNoReturn("SEMIGROUPS.WordToString: the maximum value in the",
                    " argument be at most ", Length(alphabet), ",");
    fi;
    Add(out, alphabet[letter]);
  od;
  return out;
end;

# The following method could disappear if there are methods for Green's
# relations etc so that the other method in attr.gi can be used.
#
# InstallMethod(MultiplicativeNeutralElement, "for an fp semigroup",
# [IsFpSemigroup],
# function(S)
#   local e;
#
#   if not IsFinite(S) then
#     TryNextMethod();
#   fi;
#   for e in Idempotents(S) do
#     if ForAll(GeneratorsOfSemigroup(S), x -> x * e = x and e * x = x) then
#       return e;
#     fi;
#   od;
#   return fail;
# end);

InstallMethod(Factorization, "for an fp semigroup and element",
IsCollsElms, [IsFpSemigroup, IsElementOfFpSemigroup],
function(S, x)
  return SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(x));
end);

# This method is based on the following paper
# Presentations of Factorizable Inverse Monoids
# David Easdown, James East, and D. G. FitzGerald
# July 19, 2004
InstallMethod(IsomorphismFpSemigroup,
"for an inverse partial perm semigroup",
[IsPartialPermSemigroup and IsInverseActingSemigroupRep],
function(M)
  local add_to_odd_positions, S, SS, G, GG, s, g, F, rels, fam, alpha,
  beta, lhs, rhs, map, H, o, comp, U, rhs_list, conj, MF, T, inv, rel, x, y, m,
  i;

  if not IsFactorisableInverseMonoid(M) then
    TryNextMethod();
  fi;

  add_to_odd_positions := function(list, s)
    list{[1, 3 .. Length(list) - 1]} := list{[1, 3 .. Length(list) - 1]} + s;
    return list;
  end;

  S := Semigroup(IdempotentGeneratedSubsemigroup(M));
  SS := GeneratorsOfSemigroup(S);
  G := GroupOfUnits(M);
  GG := GeneratorsOfSemigroup(G);
  s := Length(GeneratorsOfSemigroup(S));
  g := Length(GeneratorsOfSemigroup(G));

  F := FreeSemigroup(s + g);
  rels := [];
  fam := ElementsFamily(FamilyObj(F));

  # R_S - semigroup relations for the idempotent generated subsemigroup
  alpha := IsomorphismFpSemigroup(S);
  for rel in RelationsOfFpSemigroup(Image(alpha)) do
    Add(rels, [ObjByExtRep(fam, ExtRepOfObj(rel[1])),
               ObjByExtRep(fam, ExtRepOfObj(rel[2]))]);
  od;

  # R_G - semigroup relations for the group of units
  beta  := IsomorphismFpSemigroup(G);
  for rel in RelationsOfFpSemigroup(Image(beta)) do
    lhs := add_to_odd_positions(ShallowCopy(ExtRepOfObj(rel[1])), s);
    rhs := add_to_odd_positions(ShallowCopy(ExtRepOfObj(rel[2])), s);
    Add(rels, [ObjByExtRep(fam, lhs), ObjByExtRep(fam, rhs)]);
  od;

  # R_product - see page 4 of the paper
  for x in [1 .. s] do
    for y in [1 .. g] do
      rhs := Factorization(S, SS[x] ^ (GG[y] ^ -1));
      Add(rels, [F.(s + y) * F.(x),
                 EvaluateWord(GeneratorsOfSemigroup(F), rhs) * F.(s + y)]);
    od;
  od;

  map := InverseGeneralMapping(IsomorphismPermGroup(G));
  H   := Source(map);
  o   := Enumerate(LambdaOrb(M));
  # R_tilde - see page 4 of the paper
  for m in [2 .. Length(OrbSCC(o))] do
    comp := OrbSCC(o)[m];
    U := SmallGeneratingSet(Stabilizer(H,
                                       PartialPerm(o[comp[1]], o[comp[1]]),
                                       OnRight));

    for i in comp do
      rhs_list := Factorization(S, PartialPerm(o[i], o[i]));
      rhs := EvaluateWord(GeneratorsOfSemigroup(F), rhs_list);
      conj := MappingPermListList(o[comp[1]], o[i]);
      for x in List(U, x -> x ^ conj) do
        lhs := ShallowCopy(rhs_list);
        Append(lhs, Factorization(G, x ^ map) + s);
        Add(rels, [EvaluateWord(GeneratorsOfSemigroup(F), lhs), rhs]);
      od;
    od;
  od;

  # Relation to indentify One(G) and One(S)
  Add(rels, [EvaluateWord(GeneratorsOfSemigroup(F),
                          Factorization(G, One(G)) + s),
             EvaluateWord(GeneratorsOfSemigroup(F),
                          Factorization(S, One(S)))]);

  MF := F / rels;  # FpSemigroup which is isomorphic to M, with different gens.
  fam := ElementsFamily(FamilyObj(MF));
  T := Semigroup(Concatenation(SS, GG));  # M with isomorphic generators to MF

  map := x -> ElementOfFpSemigroup(fam, EvaluateWord(GeneratorsOfSemigroup(F),
                                                     Factorization(T, x)));
  inv := x -> EvaluateWord(GeneratorsOfSemigroup(T),
                           SEMIGROUPS.ExtRepObjToWord(ExtRepOfObj(x)));

  return MagmaIsomorphismByFunctionsNC(M, MF, map, inv);
end);

InstallMethod(ParseRelations,
"for a list of free generators and a string",
[IsDenseList, IsString],
function(gens, inputstring)
    local newinputstring, g, chartoel, RemoveBrackets, ParseRelation, output;

    for g in gens do
      if not (Size(String(g)) = 1 and String(g)[1]
         in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") then
        ErrorNoReturn(Concatenation(
        "expected the first argument to be a list of a free semigroup ",
        "generators represented by single English letter but found ",
        "the generator ", String(g)));
      fi;
    od;

    newinputstring := Filtered(inputstring, x -> not x = ' ');
    for g in gens do
      newinputstring := ReplacedString(newinputstring,
                        [String(g)[1], '^'], ['(', String(g)[1], ')', '^']);
    od;

    RemoveBrackets := function(word)
        local i, product, lbracket, rbracket, nestcount, index, p, chartoel;
        if word = "" then
            ErrorNoReturn(Concatenation("expected the second argument to be",
                          " a string listing the relations of a semigroup",
                          " but found an = symbol which isn't pairing two",
                          " words"));
        fi;

        # if the number of left brackets is different from the number of right
        # brackets they can't possibly pair up
        if Number(word, x -> x = '(') <> Number(word, x -> x = ')') then
            ErrorNoReturn(Concatenation("expected the number of open brackets",
                          " to match the number of closed brackets"));
        fi;

        # if the ^ is at the end of the string there is no exponent.
        # if the ^ is at the start of the string there is no base.
        if word[1] = '^' then
            ErrorNoReturn(Concatenation("expected ^ to be preceded by a ) or",
                          " a generator but found beginning of string"));
        elif word[Size(word)] = '^' then
            ErrorNoReturn(Concatenation("expected ^ to be followed by a ",
                          "positive integer but found end of string"));
        fi;
        # checks that all ^s have an exponent.
        for index in [1 .. Size(word)] do
            if word[index] = '^' then
                if not word[index + 1] in "0123456789" then
                  ErrorNoReturn(Concatenation("expected ^ to be followed by",
                  " a positive integer but found ", [word[index + 1]]));
                fi;
                if word[index - 1] in "0123456789^(" then
                  ErrorNoReturn(Concatenation(
                  "expected ^ to be preceded by a ) or a generator",
                  " but found ", [word[index - 1]]));
                fi;
            fi;
        od;

        # converts a character to the element it represents
        chartoel := function(char)
            local i;
            for i in [1 .. Size(gens)] do
                if char = String(gens[i])[1] then
                    return gens[i];
                fi;
            od;
            ErrorNoReturn(Concatenation("expected a free semigroup generator",
                          " but found ", [char]));
        end;

        # i acts as a pointer to positions in the string.
        product := "";
        i := 1;
        while i <= Size(word) do
            # if there are no brackets the character is left as it is.
            if not word[i] = '(' then
                if product = "" then
                  product := chartoel(word[i]);
                else
                  product := product * chartoel(word[i]);
                fi;
            else
                lbracket := i;
                rbracket := -1;
                # tracks how 'deep' the position of i is in terms of nested
                # brackets
                nestcount := 0;
                i := i + 1;
                while i <= Size(word) do
                    if word[i] = '(' then
                        nestcount := nestcount + 1;
                    elif word[i] = ')' then
                        if nestcount = 0 then
                            rbracket := i;
                            break;
                        else
                            nestcount := nestcount - 1;
                        fi;
                    fi;
                    i := i + 1;
                od;
                # if rbracket is not followed by ^ then the value inside the
                # bracket is appended (recursion is used to remove any brackets
                # in this value)
                if rbracket = Size(word) or (not word[rbracket + 1] = '^') then
                    if product = "" then
                      product := RemoveBrackets(
                                 word{[lbracket + 1 .. rbracket - 1]});
                    else
                      product := product *
                                 RemoveBrackets(
                                 word{[lbracket + 1 .. rbracket - 1]});
                    fi;
                # if rbracket is followed by ^ then the value inside the
                # bracket is appended the given number of time
                else
                    i := i + 2;
                    while i <= Size(word) do
                        if word[i] in "0123456789" then
                            i := i + 1;
                        else
                            break;
                        fi;
                    od;

                    p := Int(word{[rbracket + 2 .. i - 1]});
                    if p = 0 then
                      ErrorNoReturn(Concatenation("expected ^ to be followed",
                      " by a positive integer but found 0"));
                    fi;
                    if product = "" then
                       product := RemoveBrackets(word{[lbracket + 1 ..
                                                       rbracket - 1]}) ^ p;
                    else
                       product := product *
                                  RemoveBrackets(word{[lbracket + 1 ..
                                                       rbracket - 1]}) ^ p;
                    fi;
                    i := i - 1;
                fi;
            fi;
            i := i + 1;
        od;
        return product;
    end;

    ParseRelation := x -> List(SplitString(x, "="), RemoveBrackets);
    output := List(SplitString(newinputstring, ","), ParseRelation);
    if ForAny(output, x -> Size(x) = 1) then
      ErrorNoReturn(Concatenation("expected the second argument to be",
                    " a string listing the relations of a semigroup",
                    " but found an = symbol which isn't pairing two",
                    " words"));
    fi;
    output := Filtered(output, x -> Size(x) >= 2);
    output := List(output,
                   x -> List([1 .. Size(x) - 1], y -> [x[y], x[y + 1]]));
    return Concatenation(output);
end);

