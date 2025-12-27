###########################################################################
##
##  libsemigroups/cong.gi
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
###########################################################################
##

## This file contains the interface to libsemigroups Congruence objects.

###########################################################################
# Categories + properties + true methods
###########################################################################

# Unless explicitly excluded below, any left, right, or 2-sided congruence
# satisfies CanUseLibsemigroupsCongruence if its range has generators and
# CanUseFroidurePin, and the congruence has GeneratingPairs. The main reason to
# exclude some types of congruences from having CanUseLibsemigroupsCongruence
# is because there are better methods specifically for that type of congruence,
# and to avoid inadvertently computing a libsemigroups Congruence object.
# Note that any congruence with generating pairs whose range/source has
# CanUseFroidurePin, can use libsemigroups Congruence objects in theory (and
# they could also in practice), but we exclude this, for the reasons above.
#
# The fundamental operation for Congruences that satisfy
# CanUseLibsemigroupsCongruence is EquivalenceRelationPartition, and any type
# of congruence not satisfying CanUseLibsemigroupsCongruence should implement
# EquivalenceRelationPartition, and then the other methods will be available.

InstallImmediateMethod(CanUseLibsemigroupsCongruence,
                       IsLeftRightOrTwoSidedCongruence
                       and HasRange,
                       0,
                       C -> CanUseFroidurePin(Range(C))
                       and HasGeneratorsOfSemigroup(Range(C))
                       or (HasIsFreeSemigroup(Range(C))
                           and IsFreeSemigroup(Range(C)))
                       or (HasIsFreeMonoid(Range(C))
                           and IsFreeMonoid(Range(C))));

InstallMethod(CanUseLibsemigroupsCongruence,
"for a left, right, or 2-sided congruence that can compute partition",
[CanComputeEquivalenceRelationPartition],
ReturnFalse);

# TODO(later) remove CanUseLibsemigroupsCongruences?

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
InstallTrueMethod(CanUseLibsemigroupsCongruence,
                  IsInverseSemigroupCongruenceByKernelTrace);

###########################################################################
# Functions/methods that are declared in this file and that use the
# libsemigroups object directly
###########################################################################

# Get the libsemigroups::Congruence object associated to a GAP object

BindGlobal("LibsemigroupsCongruence",
function(C)
  local S, kind, p, CC, Factorize2Args, fp, factor, add_generating_pair, pair;

  Assert(1, CanUseLibsemigroupsCongruence(C));

  if IsBound(C!.LibsemigroupsCongruence)
      and IsValidGapbind14Object(C!.LibsemigroupsCongruence) then
      # Tested in workspace tests
    return C!.LibsemigroupsCongruence;
  fi;
  Unbind(C!.LibsemigroupsCongruence);

  S := Range(C);
  kind := CongruenceHandednessString(C);

  if IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or IsFpMonoid(S) or (HasIsFreeMonoid(S) and IsFreeMonoid(S)) then
    p := LibsemigroupsPresentation(S);
    if IsLeftMagmaCongruence(C) and not IsRightMagmaCongruence(C) then
      p := libsemigroups.Presentation.copy(p);
      libsemigroups.presentation_reverse(p);
    fi;
    CC := libsemigroups.Congruence.make(kind, p);
    Factorize2Args := Factorization;
  elif CanUseLibsemigroupsFroidurePin(S) and not IsQuotientSemigroup(S) then
    Enumerate(S);
    fp := LibsemigroupsFroidurePin(S);
    if kind = "left" then
      CC := libsemigroups.froidure_pin_to_left_congruence(fp);
    elif kind = "right" then
      CC := libsemigroups.froidure_pin_to_right_congruence(fp);
    else
      CC := libsemigroups.froidure_pin_to_2_sided_congruence(fp);
    fi;
    Factorize2Args := MinimalFactorization;
  elif CanUseLibsemigroupsFroidurePin(S) and IsQuotientSemigroup(S) then
    Enumerate(S);
    fp := LibsemigroupsFroidurePin(S);
    if kind = "left" then
    # TODO impl
      CC := libsemigroups.shared_ptr_froidure_pin_to_left_congruence(fp);
    elif kind = "right" then
    # TODO impl
      CC := libsemigroups.shared_ptr_froidure_pin_to_right_congruence(fp);
    else
      CC := libsemigroups.shared_ptr_froidure_pin_to_2_sided_congruence(fp);
    fi;
    Factorize2Args := MinimalFactorization;
  elif CanUseGapFroidurePin(S) then
    RUN_FROIDURE_PIN(GapFroidurePin(S), -1, InfoLevel(InfoSemigroups) > 0);
    CC := libsemigroups.gap_froidure_pin_to_congruence(kind, GapFroidurePin(S));
    Factorize2Args := MinimalFactorization;
  else
    TryNextMethod();
  fi;
  if IsLeftMagmaCongruence(C) and not IsRightMagmaCongruence(C) then
    factor := x -> Reversed(Factorize2Args(S, x) - 1);
  else
    factor := x -> Factorize2Args(S, x) - 1;
  fi;
  add_generating_pair := libsemigroups.Congruence.add_generating_pair;
  for pair in GeneratingPairsOfLeftRightOrTwoSidedCongruence(C) do
    add_generating_pair(CC, factor(pair[1]), factor(pair[2]));
  od;
  C!.LibsemigroupsCongruence := CC;
  return CC;
end);

########################################################################

InstallMethod(CongruenceLessNC,
"for CanUseLibsemigroupsCongruence and two mult. elements",
[CanUseLibsemigroupsCongruence,
 IsMultiplicativeElement,
 IsMultiplicativeElement],
function(C, elm1, elm2)
  local S, pos1, pos2, lookup, word1, word2;

  S := Range(C);
  if CanUseFroidurePin(S) then
    pos1 := PositionCanonical(S, elm1);
    pos2 := PositionCanonical(S, elm2);
    if HasEquivalenceRelationCanonicalLookup(C) then
      lookup := EquivalenceRelationCanonicalLookup(C);
      return lookup[pos1] < lookup[pos2];
    else
      word1 := MinimalFactorization(S, pos1);
      word2 := MinimalFactorization(S, pos2);
    fi;
  elif IsFpSemigroup(S) or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or IsFpMonoid(S) or (HasIsFreeMonoid(S) and IsFreeMonoid(S))
      or IsQuotientSemigroup(S) then
    word1 := Factorization(S, elm1);
    word2 := Factorization(S, elm2);
  else
    # Cannot currently test the next line
    Assert(0, false);
  fi;
  return CongruenceReduce(C, word1) < CongruenceReduce(C, word2);
end);

########################################################################

InstallMethod(CongruenceReduce,
"for CanUseLibsemigroupsCongruence and hom. list",
[CanUseLibsemigroupsCongruence, IsHomogeneousList],
function(C, word)
  local CC;
  CC := LibsemigroupsCongruence(C);
  if IsLeftMagmaCongruence(C) and not IsRightMagmaCongruence(C) then
    word := Reversed(word);
  fi;
  return libsemigroups.Congruence.reduce(CC, word - 1) + 1;
end);

InstallMethod(CongruenceReduce,
"for CanUseLibsemigroupsCongruence and mult. elt.",
[CanUseLibsemigroupsCongruence, IsMultiplicativeElement],
{C, x} -> CongruenceReduce(C, MinimalFactorization(Range(C), x)));

###########################################################################
# Functions/methods that are declared elsewhere and that use the
# libsemigroups object directly
###########################################################################

InstallMethod(NrEquivalenceClasses,
"for CanUseLibsemigroupsCongruence with known generating pairs",
[CanUseLibsemigroupsCongruence and
 HasGeneratingPairsOfLeftRightOrTwoSidedCongruence],
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
"for CanUseLibsemigroupsCongruence with known gen. pairs and 2 mult. elts",
[CanUseLibsemigroupsCongruence and
 HasGeneratingPairsOfLeftRightOrTwoSidedCongruence,
 IsMultiplicativeElement,
 IsMultiplicativeElement],
100,
function(C, elm1, elm2)
  local S, pos1, pos2, lookup, word1, word2, CC;

  S := Range(C);
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
  if IsLeftMagmaCongruence(C) and not IsRightMagmaCongruence(C) then
    word1 := Reversed(word1);
    word2 := Reversed(word2);
  fi;
  CC := LibsemigroupsCongruence(C);
  return libsemigroups.Congruence.contains(CC, word1 - 1, word2 - 1);
end);

InstallMethod(EquivalenceRelationPartition,
"for CanUseLibsemigroupsCongruence with known generating pairs",
[CanUseLibsemigroupsCongruence and
 HasGeneratingPairsOfLeftRightOrTwoSidedCongruence],
function(C)
  local S, CC, reverse, words, ntc, super, gens, class, i, j;

  S := Range(C);
  if not IsFinite(S) or CanUseLibsemigroupsFroidurePin(S) then
    CC := LibsemigroupsCongruence(C);
    if IsLeftMagmaCongruence(C) and not IsRightMagmaCongruence(C) then
      reverse := Reversed;
    else
      reverse := IdFunc;
    fi;
    if IsFinite(S) then
      words := List(S, x -> reverse(Factorization(S, x) - 1));
      ntc := libsemigroups.congruence_non_trivial_classes(CC, words) + 1;
    elif IsFpSemigroup(S) or IsFreeSemigroup(S) or IsFpMonoid(S) or IsFreeMonoid(S) then
      super := LibsemigroupsCongruence(UnderlyingCongruence(S));
      ntc := libsemigroups.infinite_congruence_non_trivial_classes(super, CC) + 1;
    else
      TryNextMethod();
    fi;
    gens := GeneratorsOfSemigroup(S);
    for i in [1 .. Length(ntc)] do
      class := ntc[i];
      for j in [1 .. Length(class)] do
        class[j] := EvaluateWord(gens, reverse(class[j]));
      od;
    od;
    return ntc;
  elif CanUseGapFroidurePin(S) then
    # in this case libsemigroups.Congruence.ntc doesn't work, because S is not
    # represented in the libsemigroups object
    return Filtered(EquivalenceRelationPartitionWithSingletons(C),
                    x -> Size(x) > 1);
  fi;
  TryNextMethod();
end);

# Methods for congruence classes

InstallMethod(\<,
"for congruence classes of CanUseLibsemigroupsCongruence", IsIdenticalObj,
[IsLeftRightOrTwoSidedCongruenceClass, IsLeftRightOrTwoSidedCongruenceClass],
1,  # to beat the method in congruences/cong.gi for
    # IsLeftRightOrTwoSidedCongruenceClass
function(class1, class2)
  local C, word1, word2;

  C := EquivalenceClassRelation(class1);
  if not CanUseLibsemigroupsCongruence(C)
      or not HasGeneratingPairsOfLeftRightOrTwoSidedCongruence(C) then
    TryNextMethod();
  elif C <> EquivalenceClassRelation(class2) then
    return false;
  fi;

  word1 := Factorization(Range(C), Representative(class1));
  word2 := Factorization(Range(C), Representative(class2));
  return CongruenceReduce(C, word1) < CongruenceReduce(C, word2);
end);

InstallMethod(EquivalenceClasses,
"for CanUseLibsemigroupsCongruence with known generating pairs",
[CanUseLibsemigroupsCongruence and
 HasGeneratingPairsOfLeftRightOrTwoSidedCongruence],
function(C)
  local result, CC, gens, i, reverse, rep, word;

  if NrEquivalenceClasses(C) = infinity then
    ErrorNoReturn("the argument (a congruence) must have a finite ",
                  "number of classes");
  fi;

  result := EmptyPlist(NrEquivalenceClasses(C));
  CC := LibsemigroupsCongruence(C);
  gens := GeneratorsOfSemigroup(Range(C));
  i := 0;
  if IsLeftMagmaCongruence(C) and not IsRightMagmaCongruence(C) then
    reverse := Reversed;
  else
    reverse := IdFunc;
  fi;
  for word in libsemigroups.congruence_normal_forms(CC) do
    i := i + 1;
    rep := EvaluateWord(gens, reverse(word + 1));
    result[i] := EquivalenceClassOfElementNC(C, rep);
  od;
  return result;
end);

###########################################################################
# Methods NOT using libsemigroups object directly but that use an
# operation or function that only applies to CanUseLibsemigroupsCongruence
###########################################################################

InstallMethod(EquivalenceRelationPartitionWithSingletons,
"for CanUseLibsemigroupsCongruence with known generating pairs",
[CanUseLibsemigroupsCongruence and
 HasGeneratingPairsOfLeftRightOrTwoSidedCongruence],
function(C)
  local map, next, part, CC, word, i, x;

  if not IsFinite(Range(C)) then
    ErrorNoReturn("the argument (a congruence) must have finite range");
  fi;

  map := HashMap();
  next := 1;
  part := [];
  for x in Range(C) do
    word := CongruenceReduce(C, x);
    if not word in map then
       map[word] := next;
       next := next + 1;
    fi;
    i := map[word];

    if not IsBound(part[i]) then
      part[i] := [];
    fi;
    Add(part[i], x);
  od;

  return part;
end);

InstallMethod(ImagesElm,
"for CanUseLibsemigroupsCongruence with known gen. pairs and a mult. elt.",
[CanUseLibsemigroupsCongruence and
 HasGeneratingPairsOfLeftRightOrTwoSidedCongruence, IsMultiplicativeElement],
function(cong, elm)
  local lookup, id, part, pos;

  if HasIsFinite(Range(cong)) and IsFinite(Range(cong))
      and CanUseFroidurePin(Range(cong)) then
    lookup := EquivalenceRelationCanonicalLookup(cong);
    id     := lookup[PositionCanonical(Range(cong), elm)];
    return EnumeratorCanonical(Range(cong)){Positions(lookup, id)};
  elif IsFpSemigroup(Range(cong))
      or (HasIsFreeSemigroup(Range(cong)) and IsFreeSemigroup(Range(cong)))
      or IsFpMonoid(Range(cong))
      or (HasIsFreeMonoid(Range(cong)) and IsFreeMonoid(Range(cong)))
      or IsQuotientSemigroup(Range(cong)) then
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
