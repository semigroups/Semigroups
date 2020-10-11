###########################################################################
##
##  libsemigroups/cong.gi
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
###########################################################################
##

## This file contains the interface to libsemigroups Congruence objects. There
## is no declaration/implementation file because no other part of the
## Semigroups package should directly use any of the functionality in
## libsemigroups, only via the functions specified in this file.

# TODO: A method for MeetXSemigroupCongruences

###########################################################################
# Categories + properties + true methods
###########################################################################

# A semigroup satisfies this property if its congruences should belong to
# CanUseLibsemigroupsCongruence.
DeclareProperty("CanUseLibsemigroupsCongruences", IsSemigroup);

InstallTrueMethod(CanUseLibsemigroupsCongruences,
                  IsSemigroup and CanUseFroidurePin);
InstallTrueMethod(CanUseLibsemigroupsCongruences,
                  IsFpSemigroup);
InstallTrueMethod(CanUseLibsemigroupsCongruences,
                  IsFpMonoid);
InstallTrueMethod(CanUseLibsemigroupsCongruences,
                  HasIsFreeSemigroup and IsFreeSemigroup);
InstallTrueMethod(CanUseLibsemigroupsCongruences,
                  HasIsFreeMonoid and IsFreeMonoid);

###########################################################################
# Functions/methods that are declared in this file and that use the
# libsemigroups object directly
###########################################################################

DeclareAttribute("LibsemigroupsCongruenceConstructor",
IsSemigroup and CanUseLibsemigroupsCongruences);

# Construct a libsemigroups::Congruence from some GAP object

InstallMethod(LibsemigroupsCongruenceConstructor,
"for a transformation semigroup with CanUseLibsemigroupsCongruences",
[IsTransformationSemigroup and CanUseLibsemigroupsCongruences],
function(S)
  local N;
  N := DegreeOfTransformationSemigroup(S);
  if N <= 16 and IsBound(LIBSEMIGROUPS_HPCOMBI_ENABLED) then
    return libsemigroups.Congruence.make_from_froidurepin_leasttransf;
  elif N <= 2 ^ 16 then
    return libsemigroups.Congruence.make_from_froidurepin_transfUInt2;
  elif N <= 2 ^ 32 then
    return libsemigroups.Congruence.make_from_froidurepin_transfUInt4;
  else
    # Cannot currently test the next line
    Error("transformation degree is too high!");
  fi;
end);

InstallMethod(LibsemigroupsCongruenceConstructor,
"for a partial perm semigroup with CanUseLibsemigroupsCongruences",
[IsPartialPermSemigroup and CanUseLibsemigroupsCongruences],
function(S)
  local N;
  N := Maximum(DegreeOfPartialPermSemigroup(S),
               CodegreeOfPartialPermSemigroup(S));
  if N <= 16 and IsBound(LIBSEMIGROUPS_HPCOMBI_ENABLED) then
    return libsemigroups.Congruence.make_from_froidurepin_leastpperm;
  elif N <= 2 ^ 16 then
    return libsemigroups.Congruence.make_from_froidurepin_ppermUInt2;
  elif N <= 2 ^ 32 then
    return libsemigroups.Congruence.make_from_froidurepin_ppermUInt4;
  else
    # Cannot currently test the next line
    Error("partial perm degree is too high!");
  fi;
end);

InstallMethod(LibsemigroupsCongruenceConstructor,
"for a boolean matrix semigroup with CanUseLibsemigroupsCongruences",
[IsBooleanMatSemigroup and CanUseLibsemigroupsCongruences],
function(S)
  if DimensionOfMatrixOverSemiring(Representative(S)) <= 8 then
    return libsemigroups.Congruence.make_from_froidurepin_bmat8;
  fi;
  return libsemigroups.Congruence.make_from_froidurepin_bmat;
end);

InstallMethod(LibsemigroupsCongruenceConstructor,
"for a matrix semigroup with CanUseLibsemigroupsCongruences",
[IsMatrixOverSemiringSemigroup and CanUseLibsemigroupsCongruences],
function(S)
  # Why does this work for types other than boolean matrices?
  return libsemigroups.Congruence.make_from_froidurepin_bmat;
end);

InstallMethod(LibsemigroupsCongruenceConstructor,
"for a bipartition semigroup with CanUseLibsemigroupsCongruences",
[IsBipartitionSemigroup and CanUseLibsemigroupsCongruences],
function(S)
  return libsemigroups.Congruence.make_from_froidurepin_bipartition;
end);

InstallMethod(LibsemigroupsCongruenceConstructor,
"for a PBR semigroup and CanUseLibsemigroupsCongruences",
[IsPBRSemigroup and CanUseLibsemigroupsCongruences],
function(S)
  return libsemigroups.Congruence.make_from_froidurepin_pbr;
end);

# Get the libsemigroups::Congruence object associated to a GAP object

BindGlobal("LibsemigroupsCongruence",
function(C)
  local S, make, CC, factor, N, tc, table, add_pair, pair;

  Assert(1, CanUseLibsemigroupsCongruence(C));

  if IsBound(C!.LibsemigroupsCongruence)
      and IsValidGapbind14Object(C!.LibsemigroupsCongruence) then
      # Tested in workspace tests
    return C!.LibsemigroupsCongruence;
  fi;
  Unbind(C!.LibsemigroupsCongruence);

  S  := Range(C);
  if IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or IsFpMonoid(S) or (HasIsFreeMonoid(S) and IsFreeMonoid(S)) then
    make := libsemigroups.Congruence.make_from_fpsemigroup;
    CC := make([AnyCongruenceString(C), LibsemigroupsFpSemigroup(S)]);
    factor := Factorization;
  elif CanUseLibsemigroupsFroidurePin(S) then
    CC := LibsemigroupsCongruenceConstructor(S)([AnyCongruenceString(C),
                                       LibsemigroupsFroidurePin(S)]);
    factor := MinimalFactorization;
  elif CanUseGapFroidurePin(S) then
    N := Length(GeneratorsOfSemigroup(Range(C)));
    tc := libsemigroups.ToddCoxeter.make([AnyCongruenceString(C)]);
    libsemigroups.ToddCoxeter.set_number_of_generators(tc, N);
    if IsLeftCongruenceCategory(C) then
      table := LeftCayleyGraphSemigroup(Range(C)) - 1;
    else
      table := RightCayleyGraphSemigroup(Range(C)) - 1;
    fi;
    libsemigroups.ToddCoxeter.prefill(tc, table);
    CC := libsemigroups.Congruence.make_from_table([AnyCongruenceString(C),
                                                    "none"]);
    libsemigroups.Congruence.set_number_of_generators(CC, N);
    libsemigroups.Congruence.add_runner(CC, tc);
    factor := MinimalFactorization;
  else
    # Shouldn't be possible to reach the next line, and can't currently test it
    Assert(0, false);
  fi;
  add_pair := libsemigroups.Congruence.add_pair;
  for pair in GeneratingPairsOfAnyCongruence(C) do
    add_pair(CC, factor(S, pair[1]) - 1, factor(S, pair[2]) - 1);
  od;
  C!.LibsemigroupsCongruence := CC;
  return CC;
end);

########################################################################

DeclareOperation("CongruenceWordToClassIndex",
                 [CanUseLibsemigroupsCongruence, IsHomogeneousList]);
DeclareOperation("CongruenceWordToClassIndex",
                 [CanUseLibsemigroupsCongruence, IsMultiplicativeElement]);

InstallMethod(CongruenceWordToClassIndex,
"for CanUseLibsemigroupsCongruence and hom. list",
[CanUseLibsemigroupsCongruence, IsHomogeneousList],
function(C, word)
  local CC;
  CC := LibsemigroupsCongruence(C);
  return libsemigroups.Congruence.word_to_class_index(CC, word - 1) + 1;
end);

InstallMethod(CongruenceWordToClassIndex,
"for CanUseLibsemigroupsCongruence and hom. list",
[CanUseLibsemigroupsCongruence, IsMultiplicativeElement],
function(C, x)
  return CongruenceWordToClassIndex(C, MinimalFactorization(Range(C), x));
end);

########################################################################

InstallMethod(CongruenceLessNC,
 "for CanUseLibsemigroupsCongruence and two mult. elements",
 [CanUseLibsemigroupsCongruence,
  IsMultiplicativeElement,
  IsMultiplicativeElement],
function(C, elm1, elm2)
  local S, pos1, pos2, lookup, word1, word2, CC;

  S := Range(C);
  if CanUseFroidurePin(S) then
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
    # Cannot currently test the next line
    Assert(0, false);
  fi;
  CC := LibsemigroupsCongruence(C);
  return libsemigroups.Congruence.less(CC, word1 - 1, word2 - 1);
end);

###########################################################################
# Functions/methods that are declared elsewhere and that use the
# libsemigroups object directly
###########################################################################

InstallMethod(NrEquivalenceClasses, "for CanUseLibsemigroupsCongruence",
[CanUseLibsemigroupsCongruence], 100,
function(C)
  local number_of_classes, result;
  number_of_classes := libsemigroups.Congruence.number_of_classes;
  result := number_of_classes(LibsemigroupsCongruence(C));
  if result = -2 then
    return infinity;
  fi;
  return result;
end);

InstallMethod(CongruenceTestMembershipNC,
"for CanUseLibsemigroupsCongruence and two mult. elements",
[CanUseLibsemigroupsCongruence,
 IsMultiplicativeElement,
 IsMultiplicativeElement],
100,
function(C, elm1, elm2)
  local S, pos1, pos2, lookup, word1, word2, CC;

  S    := Range(C);
  if IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or IsFpMonoid(S) or (HasIsFreeMonoid(S) and IsFreeMonoid(S)) then
    word1 := Factorization(S, elm1);
    word2 := Factorization(S, elm2);
  elif CanUseFroidurePin(S) then
    pos1 := PositionCanonical(S, elm1);
    pos2 := PositionCanonical(S, elm2);
    if HasEquivalenceRelationLookup(C) then
      lookup := EquivalenceRelationLookup(C);
      return lookup[pos1] = lookup[pos2];
    else
      word1 := MinimalFactorization(S, pos1);
      word2 := MinimalFactorization(S, pos2);
    fi;
  else
    # Cannot currently test the next line
    Assert(0, false);
  fi;
  CC := LibsemigroupsCongruence(C);
  return libsemigroups.Congruence.contains(CC, word1 - 1, word2 - 1);
end);

InstallMethod(EquivalenceRelationPartition, "for CanUseLibsemigroupsCongruence",
[CanUseLibsemigroupsCongruence], 100,
function(C)
  local S, CC, ntc, gens, class, i, j;
  S := Range(C);
  if not IsFinite(S) or CanUseLibsemigroupsFroidurePin(S) then
    CC := LibsemigroupsCongruence(C);
    ntc := libsemigroups.Congruence.ntc(CC) + 1;
    gens := GeneratorsOfSemigroup(S);
    for i in [1 .. Length(ntc)] do
      class := ntc[i];
      for j in [1 .. Length(class)] do
        class[j] := EvaluateWord(gens, class[j]);
      od;
    od;
    return ntc;
  elif CanUseGapFroidurePin(S) then
    # in this case libsemigroups.Congruence.ntc doesn't work
    return Filtered(EquivalenceRelationPartitionWithSingletons(C),
                    x -> Size(x) > 1);
  fi;
  # Cannot currently test the next line
  Assert(0, false);
end);

# Methods for congruence classes

InstallMethod(\<,
"for congruence classes of CanUseLibsemigroupsCongruence", IsIdenticalObj,
[IsAnyCongruenceClass, IsAnyCongruenceClass],
1,  # to beat the method in congruences/cong.gi for IsAnyCongruenceClass
function(class1, class2)
  local C, word1, word2, CC;

  C := EquivalenceClassRelation(class1);
  if C <> EquivalenceClassRelation(class2) then
    return false;
  elif not CanUseLibsemigroupsCongruence(C) then
    TryNextMethod();
  fi;

  word1 := Factorization(Range(C), Representative(class1));
  word2 := Factorization(Range(C), Representative(class2));
  CC := LibsemigroupsCongruence(C);
  return libsemigroups.Congruence.less(CC, word1 - 1, word2 - 1);
end);

InstallMethod(EquivalenceClasses, "for CanUseLibsemigroupsCongruence",
[CanUseLibsemigroupsCongruence], 100,
function(C)
  local result, CC, gens, class_index_to_word, rep, i;

  if NrEquivalenceClasses(C) = infinity then
    Error("the argument (a congruence) must have a finite ",
                  "number of classes");
  fi;

  result := EmptyPlist(NrEquivalenceClasses(C));
  CC := LibsemigroupsCongruence(C);
  gens := GeneratorsOfSemigroup(Range(C));
  class_index_to_word := libsemigroups.Congruence.class_index_to_word;
  for i in [1 .. NrEquivalenceClasses(C)] do
    rep := EvaluateWord(gens, class_index_to_word(CC, i - 1) + 1);
    result[i] := EquivalenceClassOfElementNC(C, rep);
  od;
  return result;
end);

###########################################################################
# Methods NOT using libsemigroups object directly but that use an
# operation or function that only applies to CanUseLibsemigroupsCongruence
###########################################################################

InstallMethod(EquivalenceRelationLookup, "for CanUseLibsemigroupsCongruence",
[CanUseLibsemigroupsCongruence], 100,
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

InstallMethod(ImagesElm,
"for CanUseLibsemigroupsCongruence and a multiplicative element",
[CanUseLibsemigroupsCongruence, IsMultiplicativeElement],
function(cong, elm)
  local lookup, id, part, pos;

  if HasIsFinite(Range(cong)) and IsFinite(Range(cong))
      and CanUseFroidurePin(Range(cong)) then
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
  fi;
  # Shouldn't be able to reach here
  Assert(0, false);
end);

# These constructors exist so that the resulting congruences belong to the
# correct categories, namely CanUseLibsemigroupsCongruence.

InstallMethod(SemigroupCongruenceByGeneratingPairs,
"for a semigroup with CanUseLibsemigroupsCongruences and a list",
[IsSemigroup and CanUseLibsemigroupsCongruences, IsList],
ToBeat([IsSemigroup and CanUseLibsemigroupsCongruences, IsList],
       [IsSemigroup and CanUseLibsemigroupsCongruences, IsList and IsEmpty]),
function(S, pairs)
  local filt;
  filt := IsCongruenceCategory and CanUseLibsemigroupsCongruence;
  return _AnyCongruenceByGeneratingPairs(S, pairs, filt);
end);

InstallMethod(LeftSemigroupCongruenceByGeneratingPairs,
"for a semigroup with CanUseLibsemigroupsCongruences and a list",
[IsSemigroup and CanUseLibsemigroupsCongruences, IsList],
ToBeat([IsSemigroup and CanUseLibsemigroupsCongruences, IsList],
       [IsSemigroup and CanUseLibsemigroupsCongruences, IsList and IsEmpty]),
function(S, pairs)
  local filt;
  filt := IsLeftCongruenceCategory and CanUseLibsemigroupsCongruence;
  return _AnyCongruenceByGeneratingPairs(S, pairs, filt);
end);

InstallMethod(RightSemigroupCongruenceByGeneratingPairs,
"for a semigroup with CanUseLibsemigroupsCongruences and a list",
[IsSemigroup and CanUseLibsemigroupsCongruences, IsList],
ToBeat([IsSemigroup and CanUseLibsemigroupsCongruences, IsList],
       [IsSemigroup and CanUseLibsemigroupsCongruences, IsList and IsEmpty]),
function(S, pairs)
  local filt;
  filt := IsRightCongruenceCategory and CanUseLibsemigroupsCongruence;
  return _AnyCongruenceByGeneratingPairs(S, pairs, filt);
end);
