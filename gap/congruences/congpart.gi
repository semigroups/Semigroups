############################################################################
##
##  congruences/congpart.gi
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
############################################################################
##
## This file contains implementations for left, right, and two-sided
## congruences that can compute EquivalenceRelationPartition. Please read the
## comments in congruences/congpart.gd.

#############################################################################
# Constructor by generating pairs
#############################################################################

BindGlobal("_AnyCongruenceByGeneratingPairs",
function(S, pairs, filt)
  local fam, C, pair;

  for pair in pairs do
    if not IsList(pair) or Length(pair) <> 2 then
      Error("the 2nd argument <pairs> must consist of lists of ",
            "length 2");
    elif not pair[1] in S or not pair[2] in S then
      Error("the 2nd argument <pairs> must consist of lists of ",
            "elements of the 1st argument <S> (a semigroup)");
    fi;
  od;

  # Create the Object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));

  C := Objectify(NewType(fam, filt and IsAttributeStoringRep),
                 rec());
  SetSource(C, S);
  SetRange(C, S);
  return C;
end);

InstallMethod(SemigroupCongruenceByGeneratingPairs,
"for a semigroup and a list",
[IsSemigroup, IsList],
19,  # to beat the library method for IsList and IsEmpty
function(S, pairs)
  local filt, C;
  if not (CanUseFroidurePin(S) or IsFpSemigroup(S) or IsFpMonoid(S)
      or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or (HasIsFreeMonoid(S) and IsFreeMonoid(S))) then
    TryNextMethod();
  fi;
  filt := IsSemigroupCongruence
          and IsMagmaCongruence
          and CanComputeEquivalenceRelationPartition;
  C := _AnyCongruenceByGeneratingPairs(S, pairs, filt);
  SetGeneratingPairsOfMagmaCongruence(C, pairs);
  return C;
end);

InstallMethod(LeftSemigroupCongruenceByGeneratingPairs,
"for a semigroup and a list",
[IsSemigroup, IsList],
19,  # to beat the library method for IsList and IsEmpty
function(S, pairs)
  local filt, C;
  if not (CanUseFroidurePin(S) or IsFpSemigroup(S) or IsFpMonoid(S)
      or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or (HasIsFreeMonoid(S) and IsFreeMonoid(S))) then
    TryNextMethod();
  fi;
  filt := IsLeftSemigroupCongruence
          and IsLeftMagmaCongruence
          and CanComputeEquivalenceRelationPartition;
  C := _AnyCongruenceByGeneratingPairs(S, pairs, filt);
  SetGeneratingPairsOfLeftMagmaCongruence(C, pairs);
  return C;
end);

InstallMethod(RightSemigroupCongruenceByGeneratingPairs,
"for a semigroup and a list",
[IsSemigroup, IsList],
19,  # to beat the library method for IsList and IsEmpty
function(S, pairs)
  local filt, C;
  if not (CanUseFroidurePin(S) or IsFpSemigroup(S) or IsFpMonoid(S)
      or (HasIsFreeSemigroup(S) and IsFreeSemigroup(S))
      or (HasIsFreeMonoid(S) and IsFreeMonoid(S))) then
    TryNextMethod();
  fi;
  filt := IsRightSemigroupCongruence
          and IsRightMagmaCongruence
          and CanComputeEquivalenceRelationPartition;
  C := _AnyCongruenceByGeneratingPairs(S, pairs, filt);
  SetGeneratingPairsOfRightMagmaCongruence(C, pairs);
  return C;
end);

############################################################################
# Congruence attributes that do use FroidurePin directly
############################################################################

InstallMethod(EquivalenceRelationPartitionWithSingletons,
"for left, right, or 2-sided congruence that can compute partition",
[CanComputeEquivalenceRelationPartition],
function(C)
  local S, lookup, result, enum, i;

  S := Range(C);
  if not IsFinite(S) then
    ErrorNoReturn("the argument (a ",
                  CongruenceHandednessString(C),
                  " congruence) must have finite range");
  elif not CanUseFroidurePin(S) then
    # CanUseFroidurePin is required because PositionCanonical is not a thing
    # for other types of semigroups.
    TryNextMethod();
  fi;
  lookup := EquivalenceRelationCanonicalLookup(C);
  result := List([1 .. NrEquivalenceClasses(C)], x -> []);
  enum := EnumeratorCanonical(S);
  for i in [1 .. Size(S)] do
    Add(result[lookup[i]], enum[i]);
  od;

  return result;
end);

InstallMethod(EquivalenceRelationLookup,
"for a left, right, or 2-sided congruence that can compute partition",
[CanComputeEquivalenceRelationPartition],
function(C)
  local S, lookup, class, nr, elm;
  S := Range(C);
  if not IsFinite(S) then
    ErrorNoReturn("the argument (a ",
                  CongruenceHandednessString(C),
                  " congruence) must have finite range");
  elif not CanUseFroidurePin(S) then
    # CanUseFroidurePin is required because PositionCanonical is not a thing
    # for other types of semigroups.
    TryNextMethod();
  fi;

  lookup := [1 .. Size(S)];
  for class in EquivalenceRelationPartition(C) do
    nr := PositionCanonical(S, Representative(class));
    for elm in class do
      lookup[PositionCanonical(S, elm)] := nr;
    od;
  od;
  return lookup;
end);

InstallMethod(EquivalenceRelationCanonicalPartition,
"for a left, right, or 2-sided congruence that can compute partition",
[CanComputeEquivalenceRelationPartition],
function(C)
  local S, result, cmp, i;
  S := Range(C);
  if not IsFinite(S) then
    ErrorNoReturn("the argument (a ",
                  CongruenceHandednessString(C),
                  " congruence) must have finite range");
  elif not CanUseFroidurePin(S) then
    # CanUseFroidurePin is required because PositionCanonical is not a thing
    # for other types of semigroups.
    TryNextMethod();
  fi;
  result := ShallowCopy(EquivalenceRelationPartition(C));
  cmp := {x, y} -> PositionCanonical(S, x) < PositionCanonical(S, y);
  for i in [1 .. Length(result)] do
    result[i] := ShallowCopy(result[i]);
    Sort(result[i], cmp);
  od;
  Sort(result, {x, y} -> cmp(Representative(x), Representative(y)));
  return result;
end);

############################################################################
# Congruence attributes/operations/etc that don't require CanUseFroidurePin
############################################################################

InstallMethod(EquivalenceRelationPartition,
"for left, right, or 2-sided congruence that can compute partition",
[CanComputeEquivalenceRelationPartition],
function(C)
  return Filtered(EquivalenceRelationPartitionWithSingletons(C),
                  x -> Size(x) > 1);
end);

InstallMethod(EquivalenceRelationCanonicalLookup,
"for a left, right, or 2-sided congruence that can compute partition",
[CanComputeEquivalenceRelationPartition],
function(C)
  local S, lookup;
  S := Range(C);
  if not IsFinite(S) then
    ErrorNoReturn("the argument (a ",
                  CongruenceHandednessString(C),
                  " congruence) must have finite range");
  fi;
  lookup := EquivalenceRelationLookup(C);
  return FlatKernelOfTransformation(Transformation(lookup), Length(lookup));
end);

InstallMethod(NonTrivialEquivalenceClasses,
"for a left, right, or 2-sided congruence that can compute partition",
[CanComputeEquivalenceRelationPartition],
function(C)
  local part, nr_classes, classes, i;
  part := EquivalenceRelationPartition(C);
  nr_classes := Length(part);
  classes := EmptyPlist(nr_classes);
  for i in [1 .. nr_classes] do
    classes[i] := EquivalenceClassOfElementNC(C, part[i][1]);
    SetAsList(classes[i], part[i]);
  od;
  return classes;
end);

InstallMethod(EquivalenceClasses,
"for left, right, or 2-sided congruence that can compute partition",
[CanComputeEquivalenceRelationPartition],
function(C)
  local part, classes, i;
  part := EquivalenceRelationPartitionWithSingletons(C);
  classes := [];
  for i in [1 .. Length(part)] do
    classes[i] := EquivalenceClassOfElementNC(C, part[i][1]);
    SetAsList(classes[i], part[i]);
  od;
  return classes;
end);

InstallMethod(NrEquivalenceClasses,
"for a left, right, or 2-sided congruence that can compute partition",
[CanComputeEquivalenceRelationPartition],
C -> Length(EquivalenceClasses(C)));

BindGlobal("_GeneratingPairsOfLeftRight2SidedCongDefault",
function(XCongruenceByGeneratingPairs, C)
  local result, pairs, class, i, j;

  result := XCongruenceByGeneratingPairs(Source(C), []);

  for class in EquivalenceRelationPartition(C) do
    for i in [1 .. Length(class) - 1] do
      for j in [i + 1 .. Length(class)] do
        if not [class[i], class[j]] in result then
          pairs := GeneratingPairsOfLeftRightOrTwoSidedCongruence(result);
          pairs := Concatenation(pairs, [[class[i], class[j]]]);
          result := XCongruenceByGeneratingPairs(Source(C), pairs);
        fi;
      od;
    od;
  od;
  return GeneratingPairsOfLeftRightOrTwoSidedCongruence(result);
end);

InstallMethod(GeneratingPairsOfRightMagmaCongruence,
"for a right congruence that can compute partition",
[CanComputeEquivalenceRelationPartition and IsRightMagmaCongruence],
function(C)
  return _GeneratingPairsOfLeftRight2SidedCongDefault(
            RightSemigroupCongruenceByGeneratingPairs, C);
end);

InstallMethod(GeneratingPairsOfLeftMagmaCongruence,
"for a left congruence that can compute partition",
[CanComputeEquivalenceRelationPartition and IsLeftMagmaCongruence],
function(C)
  return _GeneratingPairsOfLeftRight2SidedCongDefault(
            LeftSemigroupCongruenceByGeneratingPairs, C);
end);

InstallMethod(GeneratingPairsOfMagmaCongruence,
"for a congruence that can compute partition",
[CanComputeEquivalenceRelationPartition and IsMagmaCongruence],
function(C)
  return _GeneratingPairsOfLeftRight2SidedCongDefault(
            SemigroupCongruenceByGeneratingPairs, C);
end);

########################################################################
# Comparison operators
########################################################################

InstallMethod(\=,
"for left, right, or 2-sided congruences with known generating pairs",
[CanComputeEquivalenceRelationPartition and
 HasGeneratingPairsOfLeftRightOrTwoSidedCongruence,
 CanComputeEquivalenceRelationPartition and
 HasGeneratingPairsOfLeftRightOrTwoSidedCongruence],
function(lhop, rhop)
  local lpairs, rpairs;
  if CongruenceHandednessString(lhop) <> CongruenceHandednessString(rhop) then
    TryNextMethod();
  fi;

  lpairs := GeneratingPairsOfLeftRightOrTwoSidedCongruence(lhop);
  rpairs := GeneratingPairsOfLeftRightOrTwoSidedCongruence(rhop);
  return Range(lhop) = Range(rhop)
         and ForAll(lpairs, x -> CongruenceTestMembershipNC(rhop, x[1], x[2]))
         and ForAll(rpairs, x -> CongruenceTestMembershipNC(lhop, x[1], x[2]));
end);

InstallMethod(\=, "for a left, right, or 2-sided semigroup congruence",
[IsLeftRightOrTwoSidedCongruence, IsLeftRightOrTwoSidedCongruence],
function(lhop, rhop)
  local S;
  S := Range(lhop);
  if S <> Range(rhop) then
    return false;
  elif HasIsFinite(S) and IsFinite(S) then
    return EquivalenceRelationCanonicalLookup(lhop) =
           EquivalenceRelationCanonicalLookup(rhop);
  fi;
  return EquivalenceRelationCanonicalPartition(lhop) =
         EquivalenceRelationCanonicalPartition(rhop);
end);

# This is declared only for CanComputeEquivalenceRelationPartition because no
# other types of congruence have CongruenceTestMembershipNC implemented.
InstallMethod(\in,
"for pair of elements and left, right, or 2-sided congruence",
[IsListOrCollection, CanComputeEquivalenceRelationPartition],
function(pair, C)
  local S, string;

  if Size(pair) <> 2 then
    ErrorNoReturn("the 1st argument (a list) does not have length 2");
  fi;

  S      := Range(C);
  string := CongruenceHandednessString(C);
  if not (pair[1] in S and pair[2] in S) then
    ErrorNoReturn("the items in the 1st argument (a list) do not all belong",
                  " to the range of the 2nd argument (a ",
                  string,
                  " semigroup congruence)");
  elif CanEasilyCompareElements(pair[1]) and pair[1] = pair[2] then
    return true;
  fi;
  return CongruenceTestMembershipNC(C, pair[1], pair[2]);
end);

# This is implemented only for CanComputeEquivalenceRelationPartition because
# no other types of congruence have CongruenceTestMembershipNC implemented.
InstallMethod(IsSubrelation,
"for left, right, or 2-sided congruences with known generating pairs",
[CanComputeEquivalenceRelationPartition
 and HasGeneratingPairsOfLeftRightOrTwoSidedCongruence,
 CanComputeEquivalenceRelationPartition
 and HasGeneratingPairsOfLeftRightOrTwoSidedCongruence],
function(lhop, rhop)
  # Only valid for certain combinations of types
  if CongruenceHandednessString(lhop) <> CongruenceHandednessString(rhop)
      and not IsMagmaCongruence(lhop) then
    TryNextMethod();
  fi;

  # We use CongruenceTestMembershipNC instead of \in because using \in causes a
  # 33% slow down in tst/standard/congruences/conglatt.tst
  return Range(lhop) = Range(rhop)
    and ForAll(GeneratingPairsOfLeftRightOrTwoSidedCongruence(rhop),
               x -> CongruenceTestMembershipNC(lhop, x[1], x[2]));
end);

############################################################################
# Operators
############################################################################

BindGlobal("_MeetXCongruences",
function(XCongruenceByGeneratingPairs, GeneratingPairsOfXCongruence, lhop, rhop)
  local result, lhop_nr_pairs, rhop_nr_pairs, cong1, cong2, pairs, class, i, j;

  if Range(lhop) <> Range(rhop) then
    Error("cannot form the meet of congruences over different semigroups");
  elif lhop = rhop then
    return lhop;
  elif IsSubrelation(lhop, rhop) then
    return rhop;
  elif IsSubrelation(rhop, lhop) then
    return lhop;
  fi;
  result := XCongruenceByGeneratingPairs(Source(lhop), []);

  lhop_nr_pairs := Sum(EquivalenceRelationPartition(lhop),
                       x -> Binomial(Size(x), 2));
  rhop_nr_pairs := Sum(EquivalenceRelationPartition(rhop),
                       x -> Binomial(Size(x), 2));

  if lhop_nr_pairs < rhop_nr_pairs then
    cong1 := lhop;
    cong2 := rhop;
  else
    cong1 := rhop;
    cong2 := lhop;
  fi;

  for class in EquivalenceRelationPartition(cong1) do
    for i in [1 .. Length(class) - 1] do
      for j in [i + 1 .. Length(class)] do
        if [class[i], class[j]] in cong2
            and not [class[i], class[j]] in result then
          pairs := GeneratingPairsOfXCongruence(result);
          pairs := Concatenation(pairs, [[class[i], class[j]]]);
          result := XCongruenceByGeneratingPairs(Source(cong1), pairs);
        fi;
      od;
    od;
  od;
  return result;
end);

InstallMethod(MeetLeftSemigroupCongruences,
"for semigroup congruences that can compute partition",
[CanComputeEquivalenceRelationPartition and IsLeftSemigroupCongruence,
 CanComputeEquivalenceRelationPartition and IsLeftSemigroupCongruence],
function(lhop, rhop)
  return _MeetXCongruences(LeftSemigroupCongruenceByGeneratingPairs,
                           GeneratingPairsOfLeftMagmaCongruence,
                           lhop,
                           rhop);
end);

InstallMethod(MeetRightSemigroupCongruences,
"for semigroup congruences that can compute partition",
[CanComputeEquivalenceRelationPartition and IsRightSemigroupCongruence,
 CanComputeEquivalenceRelationPartition and IsRightSemigroupCongruence],
function(lhop, rhop)
  return _MeetXCongruences(RightSemigroupCongruenceByGeneratingPairs,
                           GeneratingPairsOfRightMagmaCongruence,
                           lhop,
                           rhop);
end);

InstallMethod(MeetSemigroupCongruences,
"for semigroup congruences that can compute partition",
[CanComputeEquivalenceRelationPartition and IsSemigroupCongruence,
 CanComputeEquivalenceRelationPartition and IsSemigroupCongruence],
function(lhop, rhop)
  return _MeetXCongruences(SemigroupCongruenceByGeneratingPairs,
                           GeneratingPairsOfSemigroupCongruence,
                           lhop,
                           rhop);
end);
