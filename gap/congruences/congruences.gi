############################################################################
##
#W  congruences.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
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
## congruences.gd contains declarations for many of these.
##

InstallMethod(\= , "for a left and a right semigroup congruence",
[IsLeftSemigroupCongruence, IsRightSemigroupCongruence],
function(c1, c2)
  return Range(c1) = Range(c2)
    and EquivalenceRelationLookup(c1) = EquivalenceRelationLookup(c2);
end);

InstallMethod(\= , "for a right and a left semigroup congruence",
[IsRightSemigroupCongruence, IsLeftSemigroupCongruence],
function(c1, c2)
  return Range(c1) = Range(c2)
    and EquivalenceRelationLookup(c1) = EquivalenceRelationLookup(c2);
end);

# Multiplication for congruence classes: only makes sense for 2-sided

InstallMethod(\*,
"for two congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  if EquivalenceClassRelation(class1) <> EquivalenceClassRelation(class2) then
    ErrorNoReturn("Semigroups: \*: usage,\n",
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
  local S, pairs;
  if not Length(arg) >= 2 then
    ErrorNoReturn("Semigroups: SemigroupCongruence: usage,\n",
                  "at least 2 arguments are required,");
  fi;
  if not IsSemigroup(arg[1]) then
    ErrorNoReturn("Semigroups: SemigroupCongruence: usage,\n",
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
    if not IsFinite(S) then
      return SemigroupCongruenceByGeneratingPairs(S, pairs);
    elif IsSimpleSemigroup(S) or IsZeroSimpleSemigroup(S) then
      return SEMIGROUPS.SimpleCongFromPairs(S, pairs);
    elif IsInverseSemigroup(S) then
      return SEMIGROUPS.InverseCongFromPairs(S, pairs);
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
      and IsDenseList(arg[3])
      and IsInverseSemigroup(S) then
    # We should have the kernel and trace of a congruence on an inverse
    # semigroup
    return InverseSemigroupCongruenceByKernelTrace(S, arg[2], arg[3]);
  else
    TryNextMethod();
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
                    "each pair should contain elements from the semigroup <S>,");
    fi;
    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);
    return LeftSemigroupCongruenceByGeneratingPairs(S, pairs);
  else
    TryNextMethod();
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
                    "each pair should contain elements from the semigroup <S>,");
    fi;
    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);
    return RightSemigroupCongruenceByGeneratingPairs(S, pairs);
  else
    TryNextMethod();
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
"for a semigroup congruence and associative element",
[IsSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  return EquivalenceClassOfElement(cong, elm);
end);

InstallMethod(LeftCongruenceClassOfElement,
"for a left semigroup congruence and associative element",
[IsLeftSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  return EquivalenceClassOfElement(cong, elm);
end);

InstallMethod(RightCongruenceClassOfElement,
"for a right semigroup congruence and associative element",
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
"for a left congruence class and an associative element",
[IsLeftCongruenceClass, IsMultiplicativeElement],
function(class, elm)
  local cong;
  cong := EquivalenceClassRelation(class);
  return EquivalenceClassOfElementNC(cong, elm * Representative(class));
end);

InstallMethod(OnRightCongruenceClasses,
"for a right congruence class and an associative element",
[IsRightCongruenceClass, IsMultiplicativeElement],
function(class, elm)
  local cong;
  cong := EquivalenceClassRelation(class);
  return EquivalenceClassOfElementNC(cong, Representative(class) * elm);
end);
