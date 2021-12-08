###########################################################################
##
##  cong.gi
##  Copyright (C) 2021                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
###########################################################################
##

# TODO: A method for MeetXSemigroupCongruences

# A congruence belongs to this category if it can use libsemigroups to compute
# things about itself.
DeclareCategory("CanComputeCppCongruence", IsAnyCongruenceCategory);

# A semigroup satisfies this property if its congruences should belong to
# CanComputeCppCongruence.
DeclareProperty("CanComputeCppCongruences", IsSemigroup);

DeclareGlobalFunction("CppCongruence");

DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsHomogeneousList]);
DeclareOperation("CongruenceWordToClassIndex",
                 [CanComputeCppCongruence, IsMultiplicativeElement]);

DeclareOperation("CongruenceLessNC",
                 [CanComputeCppCongruence,
                  IsMultiplicativeElement,
                  IsMultiplicativeElement]);

InstallTrueMethod(CanComputeCppCongruences,
                  IsSemigroup and CanComputeFroidurePin);

InstallTrueMethod(CanComputeCppCongruences, IsFpSemigroup);
InstallTrueMethod(CanComputeCppCongruences, IsFpMonoid);

InstallTrueMethod(CanComputeCppCongruences, HasIsFreeSemigroup and IsFreeSemigroup);
InstallTrueMethod(CanComputeCppCongruences, HasIsFreeMonoid and IsFreeMonoid);

BindGlobal("_KindString",
function(C)
  Assert(1, CanComputeCppCongruence(C));
  Assert(1, IsAnyCongruenceCategory(C));

  if IsCongruenceCategory(C) then
    return "twosided";
  elif IsLeftCongruenceCategory(C) then
    return "left";
  elif IsRightCongruenceCategory(C) then
    return "right";
  fi;
end);

InstallMethod(SemigroupCongruenceByGeneratingPairs,
"for a semigroup with CanComputeCppCongruences and a list",
[IsSemigroup and CanComputeCppCongruences, IsList],
RankFilter(IsList and IsEmpty),
function(S, pairs)
  local filt;
  filt := IsCongruenceCategory and CanComputeCppCongruence;
  return AnyCongruenceByGeneratingPairs(S, pairs, filt);
end);

InstallMethod(LeftSemigroupCongruenceByGeneratingPairs,
"for a semigroup with CanComputeCppCongruences and a list",
[IsSemigroup and CanComputeCppCongruences, IsList],
RankFilter(IsList and IsEmpty),
function(S, pairs)
  local filt;
  filt := IsLeftCongruenceCategory and CanComputeCppCongruence;
  return AnyCongruenceByGeneratingPairs(S, pairs, filt);
end);

InstallMethod(RightSemigroupCongruenceByGeneratingPairs,
"for a semigroup with CanComputeCppCongruences and a list",
[IsSemigroup and CanComputeCppCongruences, IsList],
RankFilter(IsList and IsEmpty),
function(S, pairs)
  local filt;
  filt := IsRightCongruenceCategory and CanComputeCppCongruence;
  return AnyCongruenceByGeneratingPairs(S, pairs, filt);
end);

###########################################################################
## Methods using the libsemigroups object directly
###########################################################################

BindGlobal("CppCongruenceConstructor",
function(S)
  local N;
  if IsTransformationSemigroup(S) then
    if DegreeOfTransformationSemigroup(S) <= 16 and
        IsBound(LIBSEMIGROUPS_HPCOMBI_ENABLED) then
      return libsemigroups.Congruence.make_from_froidurepin_leasttransf;
    elif DegreeOfTransformationSemigroup(S) <= 65536 then
      return libsemigroups.Congruence.make_from_froidurepin_transfUInt2;
    elif DegreeOfTransformationSemigroup(S) <= 18446744073709551616 then
      return libsemigroups.Congruence.make_from_froidurepin_transfUInt4;
    else
      Error("transformation degree is too high!");
    fi;
  elif IsPartialPermSemigroup(S) then
    N := Maximum(DegreeOfPartialPermSemigroup(S),
                 CodegreeOfPartialPermSemigroup(S));
    if N <= 16 and IsBound(LIBSEMIGROUPS_HPCOMBI_ENABLED) then
      return libsemigroups.Congruence.make_from_froidurepin_leastpperm;
    elif N <= 65536 then
      return libsemigroups.Congruence.make_from_froidurepin_ppermUInt2;
    elif N <= 18446744073709551616 then
      return libsemigroups.Congruence.make_from_froidurepin_ppermUInt4;
    else
      Error("partial perm degree is too high!");
    fi;
  elif IsMatrixOverSemiringSemigroup(S) then
    if IsBooleanMatSemigroup(S)
        and DimensionOfMatrixOverSemiring(Representative(S)) <= 8 then
      return libsemigroups.Congruence.make_from_froidurepin_bmat8;
    fi;
    # Is this right?
    return libsemigroups.Congruence.make_from_froidurepin_bmat;
  elif IsBipartitionSemigroup(S) then
    return libsemigroups.Congruence.make_from_froidurepin_bipartition;
  elif IsPBRSemigroup(S) then
    return libsemigroups.Congruence.make_from_froidurepin_pbr;
  else
    Error("Something has gone wrong, should not have ",
          "been able to reach here!");
  fi;
end);

InstallGlobalFunction(CppCongruence,
function(C)
  local S, CC, factor, N, tc, table, add_pair, pair;

  Assert(1, CanComputeCppCongruence(C));

  if IsBound(C!.CppCongruence)
      and IsValidGapbind14Object(C!.CppCongruence) then
    return C!.CppCongruence;
  fi;
  Unbind(C!.CppCongruence);

  S  := Range(C);
  if CanComputeCppFroidurePin(S) then
    CC := CppCongruenceConstructor(S)([_KindString(C), CppFroidurePin(S)]);
    factor := MinimalFactorization;
  elif IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or IsFpMonoid(S) or (HasIsFreeMonoid(S) and IsFreeMonoid(S)) then
    CC := libsemigroups.Congruence.make_from_fpsemigroup([_KindString(C),
                                                       CppFpSemigroup(S)]);
    factor := Factorization;
  elif CanComputeGapFroidurePin(S) then
    N := Length(GeneratorsOfSemigroup(Range(C)));
    tc := libsemigroups.ToddCoxeter.make([_KindString(C)]);
    libsemigroups.ToddCoxeter.set_number_of_generators(tc, N);
    if IsLeftCongruenceCategory(C) then
      table := LeftCayleyGraphSemigroup(Range(C)) - 1;
    else
      table := RightCayleyGraphSemigroup(Range(C)) - 1;
    fi;
    libsemigroups.ToddCoxeter.prefill(tc, table);
    CC := libsemigroups.Congruence.make_from_table([_KindString(C),
                                                    "none"]);
    libsemigroups.Congruence.set_number_of_generators(CC, N);
    libsemigroups.Congruence.add_runner(CC, tc);
    factor := MinimalFactorization;
  else
    Error("Something has gone wrong, should not have ",
          "been able to reach here!");
  fi;
  add_pair := libsemigroups.Congruence.add_pair;
  for pair in GeneratingPairsOfAnyCongruence(C) do
    add_pair(CC, factor(S, pair[1]) - 1, factor(S, pair[2]) - 1);
  od;
  C!.CppCongruence := CC;
  return CC;
end);

InstallMethod(CongruenceWordToClassIndex,
"for CanComputeCppCongruence and hom. list",
[CanComputeCppCongruence, IsHomogeneousList],
# {C, word} -> WordToClassIndex(CppCongruence(C), word - 1) + 1);
function(C, word)
  local CC;
  CC := CppCongruence(C);
  return libsemigroups.Congruence.word_to_class_index(CC, word - 1) + 1;
end);

InstallMethod(CongruenceWordToClassIndex,
"for CanComputeCppCongruence and hom. list",
[CanComputeCppCongruence, IsMultiplicativeElement],
function(C, x)
  return CongruenceWordToClassIndex(C, MinimalFactorization(Range(C), x));
end);

InstallMethod(CongruenceTestMembershipNC,
"for CanComputeCppCongruence and two mult. elements",
[CanComputeCppCongruence, IsMultiplicativeElement, IsMultiplicativeElement],
100,
function(C, elm1, elm2)
  local S, pos1, pos2, lookup, word1, word2, CC;

  S    := Range(C);
  if CanComputeFroidurePin(S) then
    pos1 := PositionCanonical(S, elm1);
    pos2 := PositionCanonical(S, elm2);
    if HasEquivalenceRelationLookup(C) then
      lookup := EquivalenceRelationLookup(C);
      return lookup[pos1] = lookup[pos2];
    else
      word1 := MinimalFactorization(S, pos1);
      word2 := MinimalFactorization(S, pos2);
    fi;
  elif IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or IsFpMonoid(S) or (HasIsFreeMonoid(S) and IsFreeMonoid(S)) then
    word1 := Factorization(S, elm1);
    word2 := Factorization(S, elm2);
  else
    TryNextMethod();
  fi;
  CC := CppCongruence(C);
  return libsemigroups.Congruence.contains(CC, word1 - 1, word2 - 1);
end);

InstallMethod(CongruenceLessNC,
"for CanComputeCppCongruence and two mult. elements",
[CanComputeCppCongruence, IsMultiplicativeElement, IsMultiplicativeElement],
100,
function(C, elm1, elm2)
  local S, pos1, pos2, lookup, word1, word2, CC;

  S := Range(C);
  if CanComputeFroidurePin(S) then
    pos1 := PositionCanonical(S, elm1);
    pos2 := PositionCanonical(S, elm2);
    if HasEquivalenceRelationLookup(C) then
      lookup := EquivalenceRelationLookup(C);
      return lookup[pos1] < lookup[pos2];
    else
      word1 := MinimalFactorization(S, pos1);
      word2 := MinimalFactorization(S, pos2);
    fi;
  elif IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or IsFpMonoid(S) or (HasIsFreeMonoid(S) and IsFreeMonoid(S)) then
    word1 := Factorization(S, elm1);
    word2 := Factorization(S, elm2);
  else
    TryNextMethod();
  fi;
  CC := CppCongruence(C);
  return libsemigroups.Congruence.less(CC, word1 - 1, word2 - 1);
end);

InstallMethod(NrEquivalenceClasses,
"for CanComputeCppCongruence",
[CanComputeCppCongruence], 100,
function(C)
  local result;
  result := libsemigroups.Congruence.number_of_classes(CppCongruence(C));
  if result = -2 then
    return infinity;
  fi;
  return result;
end);

# Methods for congruence classes

InstallMethod(\<,
"for congruence classes of CanComputeCppCongruence", IsIdenticalObj,
[IsAnyCongruenceClass,
 IsAnyCongruenceClass],
function(class1, class2)
  local C, word1, word2, CC;

  C := EquivalenceClassRelation(class1);
  if C <> EquivalenceClassRelation(class2) then
    return false;
  fi;
  word1 := Factorization(Range(C), Representative(class1));
  word2 := Factorization(Range(C), Representative(class2));
  CC := CppCongruence(C);
  return libsemigroups.Congruence.less(CC, word1 - 1, word2 - 1);
end);

InstallMethod(EquivalenceClasses,
"for CanComputeCppCongruence",
[CanComputeCppCongruence], 100,
function(C)
  local result, CC, gens, class_index_to_word, rep, i;

  if NrEquivalenceClasses(C) = infinity then
    Error("the argument (a congruence) must have a finite ",
                  "number of classes");
  fi;

  result := EmptyPlist(NrEquivalenceClasses(C));
  CC := CppCongruence(C);
  gens := GeneratorsOfSemigroup(Range(C));
  class_index_to_word := libsemigroups.Congruence.class_index_to_word;
  for i in [1 .. NrEquivalenceClasses(C)] do
    rep := EvaluateWord(gens, class_index_to_word(CC, i - 1) + 1);
    result[i] := EquivalenceClassOfElementNC(C, rep);
  od;
  return result;
end);

InstallMethod(EquivalenceRelationPartition,
"for CanComputeCppCongruence",
[CanComputeCppCongruence], 100,
function(C)
  local S, CC, ntc, gens, class, i, j;
  S := Range(C);
  if not IsFinite(S) or CanComputeCppFroidurePin(S) then
    CC := CppCongruence(C);
    ntc := libsemigroups.Congruence.ntc(CC) + 1;
    gens := GeneratorsOfSemigroup(S);
    for i in [1 .. Length(ntc)] do
      class := ntc[i];
      for j in [1 .. Length(class)] do
        class[j] := EvaluateWord(gens, class[j]);
      od;
    od;
    return ntc;
  elif CanComputeGapFroidurePin(S) then
    # in this case libsemigroups.Congruence.ntc doesn't work
    return Filtered(EquivalenceRelationPartitionWithSingletons(C),
                    x -> Size(x) > 1);
   else
    Error("shouldn't have been able to reach here!");
  fi;
end);

###########################################################################
## Methods NOT using the libsemigroups object directly but that use an
## operation or function that only applies to CanComputeCppCongruence
###########################################################################

InstallMethod(EquivalenceRelationLookup, "for CanComputeCppCongruence",
[CanComputeCppCongruence], 100,
function(C)
  local S, N, lookup, i;
  S := Range(C);
  if not IsFinite(S) then
    Error("the argument (a congruence) must have finite range");
  fi;
  N := Size(S);
  lookup := EmptyPlist(N);
  for i in [1 .. N] do
    lookup[i] := CongruenceWordToClassIndex(C, Factorization(S, i));
  od;
  return lookup;
end);

# TODO refactor
# Not sure if this needs to be here or not
InstallMethod(ImagesElm,
"for CanComputeCppCongruence and a multiplicative element",
[CanComputeCppCongruence, IsMultiplicativeElement],
function(cong, elm)
  local lookup, id, part, pos;

  if HasIsFinite(Range(cong)) and IsFinite(Range(cong))
      and CanComputeFroidurePin(Range(cong)) then
    lookup := EquivalenceRelationCanonicalLookup(cong);
    id     := lookup[PositionCanonical(Range(cong), elm)];
    part   := EquivalenceRelationPartitionWithSingletons(cong);
    return part[id];
  elif IsFpSemigroup(Range(cong))
      or (HasIsFreeSemigroup(Range(cong)) and IsFreeSemigroup(Range(cong)))
      or IsFpMonoid(Range(cong))
      or (HasIsFreeSemigroup(Range(cong)) and IsFreeMonoid(Range(cong))) then
    part := EquivalenceRelationPartition(cong);
    pos := PositionProperty(part, l -> [elm, l[1]] in cong);
    if pos = fail then
      return [elm];  # singleton
    fi;
    return part[pos];  # non-singleton
  else
    Error("shouldn't have been able to reach here!");
  fi;
end);

#############################################################################
#############################################################################
# Congruence classes
#############################################################################
#############################################################################

# TODO move to cong.gi
# TODO move to cong.gi

# TODO move to cong.gi
InstallMethod(\in,
"for a mult. elt. and a congruence class of CanComputeCppCongruence",
[IsMultiplicativeElement, IsAnyCongruenceClass],
function(elm, class)
  return [elm, Representative(class)] in EquivalenceClassRelation(class);
end);

# TODO move to cong.gi
InstallMethod(\=,
"for congruence classes of CanComputeCppCongruence", IsIdenticalObj,
[IsAnyCongruenceClass,
 IsAnyCongruenceClass],
function(class1, class2)
  local cong;
  cong := EquivalenceClassRelation(class1);
  if cong <> EquivalenceClassRelation(class2) then
    return false;
  fi;
  return [Representative(class1), Representative(class2)] in cong;
end);

# TODO move to cong.gi
InstallMethod(AsList,
"for a congruence class of CanComputeCppCongruence",
[IsAnyCongruenceClass],
function(class)
  return ImagesElm(EquivalenceClassRelation(class), Representative(class));
end);

# TODO move to cong.gi
InstallMethod(Enumerator, "for a congruence class of CanComputeCppCongruence",
[IsAnyCongruenceClass], AsList);

# TODO move to cong.gi
InstallMethod(Size,
"for a congruence class of CanComputeCppCongruence",
[IsAnyCongruenceClass and CanComputeCppCongruence],
function(class)
  local cong, part, id;
  # cong := EquivalenceClassRelation(class);
  #if HasIsFinite(Range(cong)) and IsFinite(Range(cong))
  #    and CanComputeCppFroidurePin(Range(cong)) then
  #  part := EquivalenceRelationPartitionWithSingletons(cong);
  #  id   := CongruenceWordToClassIndex(cong, Representative(class));
  #  return Size(part[id]);
  # elif IsFpSemigroup(Range(cong))
  #     or (HasIsFreeSemigroup(Range(cong)) and IsFreeSemigroup(Range(cong)))
  #     or IsFpMonoid(Range(cong))
  #     or (HasIsFreeMonoid(Range(cong)) and IsFreeMonoid(Range(cong))) then
  return Size(AsList(class));
  # else
  #   Error("shouldn't have been able to reach here!");
  # fi;
end);
