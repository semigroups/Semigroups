############################################################################
##
##  cong.gi
##  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains some general functions, operations and attributes of
## semigroup congruences.  Methods for specific types of congruence are
## implemented in the following files:
##
##       conginv.gi     - Inverse semigroups
##       congpairs.gi   - Congruences with generating pairs
##       congrees.gi    - Rees congruences
##       congrms.gi     - (0-)simple Rees matrix semigroups
##       congsimple.gi  - (0-)simple semigroups
##       conguniv.gi    - Universal congruences
##
## cong.gd contains declarations for many of these.
##

# Fallback method

InstallMethod(NrEquivalenceClasses, "for a semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local classes;
  classes := EquivalenceClasses(cong);
  # Note: EquivalenceClasses may exclude all singletons due to a bug in GAP.
  # This is a workaround which adds any missing singletons.
  return Length(classes) + Size(Range(cong)) - Sum(classes, Size);
end);

InstallMethod(\in,
"for dense list and left semigroup congruence",
[IsDenseList, IsLeftSemigroupCongruence],
function(pair, cong)
  local S;
  S := Range(cong);
  if Size(pair) <> 2 then
    ErrorNoReturn("Semigroups: \\in (for a relation): usage,\n",
                  "the first arg <pair> must be a list of length 2,");
  elif not (pair[1] in S and pair[2] in S) then
    ErrorNoReturn("Semigroups: \\in (for a relation): usage,\n",
                  "elements of the first arg <pair> must be\n",
                  "in the range of the second arg <cong>,");
  elif CanEasilyCompareElements(pair[1]) and pair[1] = pair[2] then
    return true;
  fi;
  return CongruenceTestMembershipNC(cong, pair[1], pair[2]);
end);

InstallMethod(\in,
"for dense list and right semigroup congruence",
[IsDenseList, IsRightSemigroupCongruence],
function(pair, cong)
  local S;
  S := Range(cong);
  if Size(pair) <> 2 then
    ErrorNoReturn("Semigroups: \\in (for a relation): usage,\n",
                  "the first arg <pair> must be a list of length 2,");
  elif not (pair[1] in S and pair[2] in S) then
    ErrorNoReturn("Semigroups: \\in (for a relation): usage,\n",
                  "elements of the first arg <pair> must be\n",
                  "in the range of the second arg <cong>,");
  elif CanEasilyCompareElements(pair[1]) and pair[1] = pair[2] then
    return true;
  fi;
  return CongruenceTestMembershipNC(cong, pair[1], pair[2]);
end);

BindGlobal("_GenericCongEquality",
function(cong1, cong2)
  local S;
  S := Range(cong1);
  if S <> Range(cong2) then
    return false;
  fi;
  if HasIsFinite(S) and IsFinite(S) then
    return EquivalenceRelationCanonicalLookup(cong1) =
           EquivalenceRelationCanonicalLookup(cong2);
  fi;
  return EquivalenceRelationCanonicalPartition(cong1) =
         EquivalenceRelationCanonicalPartition(cong2);
end);

InstallMethod(\=, "for two left semigroup congruences",
[IsLeftSemigroupCongruence, IsLeftSemigroupCongruence],
_GenericCongEquality);

InstallMethod(\=, "for a left and a right semigroup congruence",
[IsLeftSemigroupCongruence, IsRightSemigroupCongruence],
_GenericCongEquality);

InstallMethod(\=, "for a right and a left semigroup congruence",
[IsRightSemigroupCongruence, IsLeftSemigroupCongruence],
_GenericCongEquality);

InstallMethod(\=, "for two right semigroup congruences",
[IsRightSemigroupCongruence, IsRightSemigroupCongruence],
_GenericCongEquality);

# Since two-sided congs are both left and right, this covers all cases

MakeReadWriteGlobal("_GenericCongEquality");
Unbind(_GenericCongEquality);

InstallMethod(\=, "for two semigroup congruences with generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence,
 IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(c1, c2)
  return Range(c1) = Range(c2)
         and ForAll(GeneratingPairsOfSemigroupCongruence(c1), p -> p in c2)
         and ForAll(GeneratingPairsOfSemigroupCongruence(c2), p -> p in c1);
end);

# Multiplication for congruence classes: only makes sense for 2-sided

InstallMethod(\*,
"for two congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  if EquivalenceClassRelation(class1) <> EquivalenceClassRelation(class2) then
    ErrorNoReturn("Semigroups: \\*: usage,\n",
                  "the args must be classes of the same congruence,");
  fi;
  return EquivalenceClassOfElementNC(EquivalenceClassRelation(class1),
                                     Representative(class1) *
                                     Representative(class2));
end);

InstallMethod(\=,
"for two congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2)
         and Representative(class1) in class2;
end);

InstallMethod(\<,
"for two congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2)
    and RepresentativeSmallest(class1) < RepresentativeSmallest(class2);
end);

InstallGlobalFunction(SemigroupCongruence,
function(arg)
  local S, opts, s_opts, x, pairs, cong;
  if not Length(arg) >= 2 then
    ErrorNoReturn("Semigroups: SemigroupCongruence: usage,\n",
                  "at least 2 arguments are required,");
  fi;
  if not IsSemigroup(arg[1]) then
    ErrorNoReturn("Semigroups: SemigroupCongruence: usage,\n",
                  "1st argument <S> must be a semigroup,");
  fi;
  S := arg[1];

  # Set up any options
  if IsRecord(arg[Length(arg)]) then
    opts := arg[Length(arg)];
    arg := arg{[1 .. Length(arg) - 1]};
  else
    opts := rec();
  fi;
  s_opts := SEMIGROUPS.OptionsRec(S);
  for x in RecNames(s_opts) do
    if not IsBound(opts.(x)) then
      opts.(x) := s_opts.(x);
    fi;
  od;

  if IsHomogeneousList(arg[2]) then
    # We should have a list of generating pairs
    if Length(arg) = 2 then
      pairs := arg[2];
      if not IsEmpty(pairs) and not IsList(pairs[1]) then
        pairs := [pairs];
      fi;
    elif Length(arg) > 2 then
      pairs := arg{[2 .. Length(arg)]};
    fi;
    if not ForAll(pairs, p -> Size(p) = 2) then
      ErrorNoReturn("Semigroups: SemigroupCongruence: usage,\n",
                    "<pairs> should be a list of lists of size 2,");
    fi;
    if not ForAll(pairs, p -> p[1] in S and p[2] in S) then
      ErrorNoReturn("Semigroups: SemigroupCongruence: usage,\n",
                    "each pair should contain ",
                    "elements from the semigroup <S>,");
    fi;

    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);

    # Decide which representation to use
    if not IsFinite(S) then
      return SemigroupCongruenceByGeneratingPairs(S, pairs);
    elif ((HasIsSimpleSemigroup(S) or IsActingSemigroup(S)
           or HasSize(S) or IsReesMatrixSemigroup(S))
          and IsSimpleSemigroup(S)) or
         ((HasIsZeroSimpleSemigroup(S) or IsActingSemigroup(S)
           or HasSize(S) or IsReesZeroMatrixSemigroup(S))
          and IsZeroSimpleSemigroup(S)) then
      return SEMIGROUPS.SimpleCongFromPairs(S, pairs);
    elif IsInverseSemigroup(S) and IsGeneratorsOfInverseSemigroup(S) and
         Size(S) >= opts.cong_by_ker_trace_threshold then
      cong := SemigroupCongruenceByGeneratingPairs(S, pairs);
      cong := AsInverseSemigroupCongruenceByKernelTrace(cong);
      SetGeneratingPairsOfMagmaCongruence(cong, pairs);
      return cong;
    else
      return SemigroupCongruenceByGeneratingPairs(S, pairs);
    fi;
  elif IsGeneralMapping(arg[2]) and
      ((IsRMSCongruenceByLinkedTriple(arg[3]) and IsSimpleSemigroup(S))
       or (IsRZMSCongruenceByLinkedTriple(arg[3]) and IsZeroSimpleSemigroup(S)))
      then
    # We should have a congruence of an isomorphic RMS/RZMS
    if Range(arg[2]) = Range(arg[3]) and S = Source(arg[2]) then
      return SEMIGROUPS.SimpleCongFromRMSCong(S, arg[2], arg[3]);
    else
      ErrorNoReturn("Semigroups: SemigroupCongruence: usage,\n",
                    "<cong> should be over a Rees (0-)matrix semigroup ",
                    "isomorphic to <S> via <iso>,");
    fi;
  elif HasIsSemigroupIdeal(arg[2])
      and IsSemigroupIdeal(arg[2])
      and Parent(arg[2]) = S then
    return ReesCongruenceOfSemigroupIdeal(arg[2]);
  elif Length(arg) = 3
      and IsInverseSemigroup(arg[2])
      and IsGeneratorsOfInverseSemigroup(arg[2])
      and IsDenseList(arg[3])
      and IsInverseSemigroup(S)
      and IsGeneratorsOfInverseSemigroup(S) then
    # We should have the kernel and trace of a congruence on an inverse
    # semigroup
    return InverseSemigroupCongruenceByKernelTrace(S, arg[2], arg[3]);
  else
    ErrorNoReturn("Semigroups: SemigroupCongruence: usage,\n",
                  "the arguments are not valid for this function,");
  fi;
end);

InstallGlobalFunction(LeftSemigroupCongruence,
function(arg)
  local S, pairs;
  if not Length(arg) >= 2 then
    ErrorNoReturn("Semigroups: LeftSemigroupCongruence: usage,\n",
                  "at least 2 arguments are required,");
  fi;
  if not IsSemigroup(arg[1]) then
    ErrorNoReturn("Semigroups: LeftSemigroupCongruence: usage,\n",
                  "1st argument <S> must be a semigroup,");
  fi;
  S := arg[1];

  if IsHomogeneousList(arg[2]) then
    # We should have a list of generating pairs
    if Length(arg) = 2 then
      pairs := arg[2];
      if not IsEmpty(pairs) and not IsList(pairs[1]) then
        pairs := [pairs];
      fi;
    elif Length(arg) > 2 then
      pairs := arg{[2 .. Length(arg)]};
    fi;
    if not ForAll(pairs, p -> Size(p) = 2) then
      ErrorNoReturn("Semigroups: LeftSemigroupCongruence: usage,\n",
                    "<pairs> should be a list of lists of size 2,");
    fi;
    if not ForAll(pairs, p -> p[1] in S and p[2] in S) then
      ErrorNoReturn("Semigroups: LeftSemigroupCongruence: usage,\n",
                    "each pair should contain elements from the semigroup ",
                    "<S>,");
    fi;
    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);
    return LeftSemigroupCongruenceByGeneratingPairs(S, pairs);
  else
    ErrorNoReturn("Semigroups: LeftSemigroupCongruence: usage,\n",
                  "the arguments are not valid for this function,");
  fi;
end);

InstallGlobalFunction(RightSemigroupCongruence,
function(arg)
  local S, pairs;
  if not Length(arg) >= 2 then
    ErrorNoReturn("Semigroups: RightSemigroupCongruence: usage,\n",
                  "at least 2 arguments are required,");
  fi;
  if not IsSemigroup(arg[1]) then
    ErrorNoReturn("Semigroups: RightSemigroupCongruence: usage,\n",
                  "1st argument <S> must be a semigroup,");
  fi;
  S := arg[1];

  if IsHomogeneousList(arg[2]) then
    # We should have a list of generating pairs
    if Length(arg) = 2 then
      pairs := arg[2];
      if not IsEmpty(pairs) and not IsList(pairs[1]) then
        pairs := [pairs];
      fi;
    elif Length(arg) > 2 then
      pairs := arg{[2 .. Length(arg)]};
    fi;
    if not ForAll(pairs, p -> Size(p) = 2) then
      ErrorNoReturn("Semigroups: RightSemigroupCongruence: usage,\n",
                    "<pairs> should be a list of lists of size 2,");
    fi;
    if not ForAll(pairs, p -> p[1] in S and p[2] in S) then
      ErrorNoReturn("Semigroups: RightSemigroupCongruence: usage,\n",
                    "each pair should contain elements from the semigroup ",
                    "<S>,");
    fi;
    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);
    return RightSemigroupCongruenceByGeneratingPairs(S, pairs);
  else
    ErrorNoReturn("Semigroups: RightSemigroupCongruence: usage,\n",
                  "the arguments are not valid for this function,");
  fi;
end);

InstallMethod(ViewObj,
"for a left semigroup congruence",
[IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence],
function(cong)
  Print("<left semigroup congruence over ");
  ViewObj(Range(cong));
  Print(" with ",
        Size(GeneratingPairsOfLeftSemigroupCongruence(cong)),
        " generating pairs>");
end);

InstallMethod(ViewObj,
"for a right semigroup congruence",
[IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(cong)
  Print("<right semigroup congruence over ");
  ViewObj(Range(cong));
  Print(" with ",
        Size(GeneratingPairsOfRightSemigroupCongruence(cong)),
        " generating pairs>");
end);

InstallMethod(ViewObj,
"for a semigroup congruence",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong)
  Print("<semigroup congruence over ");
  ViewObj(Range(cong));
  Print(" with ",
        Size(GeneratingPairsOfSemigroupCongruence(cong)),
        " generating pairs>");
end);

InstallMethod(ViewObj,
"for a congruence class",
[IsCongruenceClass],
function(class)
  Print("<congruence class of ");
  ViewObj(Representative(class));
  Print(">");
end);

InstallMethod(ViewObj,
"for a left congruence class",
[IsLeftCongruenceClass],
function(class)
  Print("<left congruence class of ");
  ViewObj(Representative(class));
  Print(">");
end);

InstallMethod(ViewObj,
"for a right congruence class",
[IsRightCongruenceClass],
function(class)
  Print("<right congruence class of ");
  ViewObj(Representative(class));
  Print(">");
end);

InstallMethod(CongruenceClasses,
"for a semigroup congruence",
[IsSemigroupCongruence],
EquivalenceClasses);

InstallMethod(LeftCongruenceClasses,
"for a left semigroup congruence",
[IsLeftSemigroupCongruence],
EquivalenceClasses);

InstallMethod(RightCongruenceClasses,
"for a right semigroup congruence",
[IsRightSemigroupCongruence],
EquivalenceClasses);

InstallMethod(NrCongruenceClasses,
"for a semigroup congruence",
[IsSemigroupCongruence],
NrEquivalenceClasses);

InstallMethod(NrLeftCongruenceClasses,
"for a left semigroup congruence",
[IsLeftSemigroupCongruence],
NrEquivalenceClasses);

InstallMethod(NrRightCongruenceClasses,
"for a right semigroup congruence",
[IsRightSemigroupCongruence],
NrEquivalenceClasses);

InstallMethod(NonTrivialCongruenceClasses,
"for a semigroup congruence",
[IsSemigroupCongruence],
NonTrivialEquivalenceClasses);

InstallMethod(NonTrivialLeftCongruenceClasses,
"for a left semigroup congruence",
[IsLeftSemigroupCongruence],
NonTrivialEquivalenceClasses);

InstallMethod(NonTrivialRightCongruenceClasses,
"for a right semigroup congruence",
[IsRightSemigroupCongruence],
NonTrivialEquivalenceClasses);

InstallMethod(CongruenceClassOfElement,
"for a semigroup congruence and multiplicative element",
[IsSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  return EquivalenceClassOfElement(cong, elm);
end);

InstallMethod(LeftCongruenceClassOfElement,
"for a left semigroup congruence and multiplicative element",
[IsLeftSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  return EquivalenceClassOfElement(cong, elm);
end);

InstallMethod(RightCongruenceClassOfElement,
"for a right semigroup congruence and multiplicative element",
[IsRightSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  return EquivalenceClassOfElement(cong, elm);
end);

InstallMethod(IsSuperrelation,
"for two semigroup congruences",
[IsSemigroupCongruence, IsSemigroupCongruence],
function(cong1, cong2)
  return IsSubrelation(cong2, cong1);
end);

InstallMethod(OnLeftCongruenceClasses,
"for a left congruence class and a multiplicative element",
[IsLeftCongruenceClass, IsMultiplicativeElement],
function(class, elm)
  local cong;
  cong := EquivalenceClassRelation(class);
  return EquivalenceClassOfElementNC(cong, elm * Representative(class));
end);

InstallMethod(OnRightCongruenceClasses,
"for a right congruence class and a multiplicative element",
[IsRightCongruenceClass, IsMultiplicativeElement],
function(class, elm)
  local cong;
  cong := EquivalenceClassRelation(class);
  return EquivalenceClassOfElementNC(cong, Representative(class) * elm);
end);

SEMIGROUPS._GenericCongLookup := function(cong)
  local S, lookup, class, nr, elm;

  S := Range(cong);
  lookup := [1 .. Size(S)];
  for class in NonTrivialEquivalenceClasses(cong) do
    nr := PositionCanonical(S, Representative(class));
    for elm in class do
      lookup[PositionCanonical(S, elm)] := nr;
    od;
  od;
  return lookup;
end;

InstallMethod(EquivalenceRelationLookup,
"for a semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  if not IsFinite(Range(cong)) then
    ErrorNoReturn("Semigroups: EquivalenceRelationLookup: usage,\n",
                  "<cong> must be over a finite semigroup,");
  fi;
  return SEMIGROUPS._GenericCongLookup(cong);
end);

InstallMethod(EquivalenceRelationLookup,
"for a left semigroup congruence",
[IsLeftSemigroupCongruence],
function(cong)
  if not IsFinite(Range(cong)) then
    ErrorNoReturn("Semigroups: EquivalenceRelationLookup: usage,\n",
                  "<cong> must be over a finite semigroup,");
  fi;
  return SEMIGROUPS._GenericCongLookup(cong);
end);

InstallMethod(EquivalenceRelationLookup,
"for a right semigroup congruence",
[IsRightSemigroupCongruence],
function(cong)
  if not IsFinite(Range(cong)) then
    ErrorNoReturn("Semigroups: EquivalenceRelationLookup: usage,\n",
                  "<cong> must be over a finite semigroup,");
  fi;
  return SEMIGROUPS._GenericCongLookup(cong);
end);

BindGlobal("_GenericCongCanonicalLookup",
function(cong)
  local lookup, max, dictionary, next, out, i, new_nr;
  lookup := EquivalenceRelationLookup(cong);
  max := Maximum(lookup);
  # We do not know whether the maximum is NrEquivalenceClasses(cong)
  dictionary := ListWithIdenticalEntries(max, 0);
  next := 1;
  out := EmptyPlist(max);
  for i in [1 .. Length(lookup)] do
    new_nr := dictionary[lookup[i]];
    if new_nr = 0 then
      dictionary[lookup[i]] := next;
      new_nr := next;
      next := next + 1;
    fi;
    out[i] := new_nr;
  od;
  return out;
end);

InstallMethod(EquivalenceRelationCanonicalLookup,
"for a left semigroup congruence",
[IsLeftSemigroupCongruence],
_GenericCongCanonicalLookup);

InstallMethod(EquivalenceRelationCanonicalLookup,
"for a right semigroup congruence",
[IsRightSemigroupCongruence],
_GenericCongCanonicalLookup);

MakeReadWriteGlobal("_GenericCongCanonicalLookup");
Unbind(_GenericCongCanonicalLookup);

InstallMethod(EquivalenceRelationCanonicalPartition,
"for a left semigroup congruence",
[IsLeftSemigroupCongruence],
cong -> Set(EquivalenceRelationPartition(cong), Set));

InstallMethod(EquivalenceRelationCanonicalPartition,
"for a right semigroup congruence",
[IsRightSemigroupCongruence],
cong -> Set(EquivalenceRelationPartition(cong), Set));
