############################################################################
##
##  congruences/cong.gi
##  Copyright (C) 2015-2021                               Michael C. Young
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

# Where possible, we only provide methods in this file for
# IsAnyCongruenceCategory and IsAnyCongruenceClass, and not arbitrary
# congruences.

########################################################################
# 0. Categories
########################################################################

InstallMethod(AnyCongruenceCategory, "for a right congruence category",
[IsRightCongruenceCategory], C -> IsRightCongruenceCategory);

InstallMethod(AnyCongruenceCategory, "for a left congruence category",
[IsLeftCongruenceCategory], C -> IsLeftCongruenceCategory);

InstallMethod(AnyCongruenceCategory, "for a 2-sided congruence category",
[IsCongruenceCategory], C -> IsCongruenceCategory);

InstallMethod(AnyCongruenceString, "for a right congruence category",
[IsRightCongruenceCategory], C -> "right");

InstallMethod(AnyCongruenceString, "for a left congruence category",
[IsLeftCongruenceCategory], C -> "left");

InstallMethod(AnyCongruenceString, "for a 2-sided congruence category",
[IsCongruenceCategory], C -> "2-sided");

########################################################################
# Flexible functions for creating congruences
########################################################################

InstallGlobalFunction(SemigroupCongruence,
function(arg)
  local S, opts, s_opts, x, pairs, cong;
  if not Length(arg) >= 2 then
    ErrorNoReturn("at least 2 arguments are required");
  elif not IsSemigroup(arg[1]) then
    ErrorNoReturn("the 1st argument is not a semigroup");
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
      ErrorNoReturn("the 2nd argument (a list of lists) contains lists ",
                    "of size not equal to 2");
    elif not ForAll(pairs, p -> p[1] in S and p[2] in S) then
      ErrorNoReturn("the 2nd argument (a list of lists) contains items ",
                    "that do not belong to the 1st argument (a semigroup)");
    fi;

    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);

    # Decide which representation to use
    if not IsFinite(S)
        or ((HasIsSimpleSemigroup(S) or IsActingSemigroup(S)
           or HasSize(S) or IsReesMatrixSemigroup(S))
          and IsSimpleSemigroup(S)) or
         ((HasIsZeroSimpleSemigroup(S) or IsActingSemigroup(S)
           or HasSize(S) or IsReesZeroMatrixSemigroup(S))
          and IsZeroSimpleSemigroup(S)) then
      return SemigroupCongruenceByGeneratingPairs(S, pairs);
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
      return CongruenceByIsomorphism(arg[2], arg[3]);
    else
      ErrorNoReturn("the range of the 3rd argument (a congruence) is ",
                    "not a Rees (0-)matrix semigroup isomorphic to the ",
                    "1st argument");
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
  fi;
  ErrorNoReturn("the arguments are not valid for this function");
end);

BindGlobal("_LeftOrRightCong",
function(CongruenceConstructor, arg)
  local S, pairs;
  if not Length(arg) >= 2 then
    ErrorNoReturn("at least 2 arguments are required");
  elif not IsSemigroup(arg[1]) then
    ErrorNoReturn("the 1st argument is not a semigroup");
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
      ErrorNoReturn("the 2nd argument (a list of lists) contains lists ",
                    "of size not equal to 2");
    elif not ForAll(pairs, p -> p[1] in S and p[2] in S) then
      ErrorNoReturn("the 2nd argument (a list of lists) contains items ",
                    "that do not belong to the 1st argument (a semigroup)");
    fi;
    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);
    return CongruenceConstructor(S, pairs);
  else
    ErrorNoReturn("the arguments are not valid for this function");
  fi;
end);

InstallGlobalFunction(LeftSemigroupCongruence,
function(arg)
  return _LeftOrRightCong(LeftSemigroupCongruenceByGeneratingPairs, arg);
end);

InstallGlobalFunction(RightSemigroupCongruence,
function(arg)
  return _LeftOrRightCong(RightSemigroupCongruenceByGeneratingPairs, arg);
end);

########################################################################
# Congruence attributes
########################################################################

InstallMethod(NonTrivialEquivalenceClasses, "for IsAnyCongruenceCategory",
[IsAnyCongruenceCategory],
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

InstallMethod(EquivalenceRelationLookup, "for IsAnyCongruenceCategory",
[IsAnyCongruenceCategory],
function(C)
  local S, lookup, class, nr, elm;
  S := Range(C);
  if not (IsSemigroup(S) and IsFinite(S)) then
    ErrorNoReturn("the range of the argument (an equivalence relation) ",
                  "is not a finite semigroup");
  elif not CanComputeFroidurePin(S) then
    # PositionCanonical requires CanComputeFroidurePin
    TryNextMethod();
  fi;

  lookup := [1 .. Size(S)];
  for class in NonTrivialEquivalenceClasses(C) do
    nr := PositionCanonical(S, Representative(class));
    for elm in class do
      lookup[PositionCanonical(S, elm)] := nr;
    od;
  od;
  return lookup;
end);

InstallMethod(EquivalenceRelationCanonicalLookup,
"for IsAnyCongruenceCategory", [IsAnyCongruenceCategory],
function(C)
  local S, lookup, max, dictionary, next, out, new_nr, i;
  S := Range(C);
  if not (IsSemigroup(S) and IsFinite(S)) then
    ErrorNoReturn("the range of the argument (an equivalence relation) ",
                  "is not a finite semigroup");
  elif not CanComputeFroidurePin(S) then
    TryNextMethod();
  fi;
  lookup := EquivalenceRelationLookup(C);
  max := Maximum(lookup);
  # We do not know whether the maximum is NrEquivalenceClasses(C)
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

InstallMethod(EquivalenceRelationCanonicalPartition,
"for IsAnyCongruenceCategory", [IsAnyCongruenceCategory],
C -> Set(EquivalenceRelationPartition(C), Set));

InstallMethod(EquivalenceRelationPartitionWithSingletons,
"for IsAnyCongruenceCategory", [IsAnyCongruenceCategory],
function(C)
  local en, partition, lookup, i;
  if not CanComputeFroidurePin(Range(C)) then
    # This is required because EnumeratorCanonical is not a thing for other
    # types of congruences.
    TryNextMethod();
  elif not IsFinite(Range(C)) then
    Error("the argument (a congruence) must have finite range");
  fi;
  en        := EnumeratorCanonical(Range(C));
  partition := List([1 .. NrEquivalenceClasses(C)], x -> []);
  lookup    := EquivalenceRelationCanonicalLookup(C);
  for i in [1 .. Length(lookup)] do
    Add(partition[lookup[i]], en[i]);
  od;

  return partition;
end);

########################################################################
# Congruence operators
########################################################################

InstallMethod(\in,
"for homog. list and IsAnyCongruenceCategory",
[IsHomogeneousList, IsAnyCongruenceCategory],
function(pair, C)
  local S, string;

  if Size(pair) <> 2 then
    ErrorNoReturn("the 1st argument (a list) does not have length 2");
  fi;

  S      := Range(C);
  string := AnyCongruenceString(C);
  if not (pair[1] in S and pair[2] in S) then
    ErrorNoReturn("the items in the 1st argument (a list) do not all belong to ",
                  "the range of the 2nd argument (a ", string, " semigroup ",
                  "congruence)");
  elif CanEasilyCompareElements(pair[1]) and pair[1] = pair[2] then
    return true;
  fi;
  return CongruenceTestMembershipNC(C, pair[1], pair[2]);
end);

InstallMethod(\=, "for IsAnyCongruenceCategory",
[IsAnyCongruenceCategory, IsAnyCongruenceCategory],
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

InstallMethod(IsSuperrelation, "for semigroup congruences",
[IsAnyCongruenceCategory, IsAnyCongruenceCategory],
{lhop, rhop} -> IsSubrelation(rhop, lhop));

InstallMethod(MeetSemigroupCongruences, "for semigroup congruences",
[IsSemigroupCongruence, IsSemigroupCongruence],
function(lhop, rhop)
  if Range(lhop) <> Range(rhop) then
    Error("cannot form the meet of congruences over different semigroups");
  elif lhop = rhop then
    return lhop;
  fi;
  # TODO actually implement a method
  TryNextMethod();
end);

########################################################################
# Congruence classes
########################################################################

InstallMethod(EquivalenceClassOfElement,
"for IsAnyCongruenceCategory and multiplicative element",
[IsAnyCongruenceCategory, IsMultiplicativeElement],
function(C, x)
  if not x in Range(C) then
    Error("the 2nd argument (a mult. elt.) does not belong to the range ",
          "of the 1st argument (a congruence)");
  fi;
  return EquivalenceClassOfElementNC(C, x);
end);

InstallMethod(EquivalenceClassOfElementNC,
"for IsAnyCongruenceCategory and multiplicative element",
[IsAnyCongruenceCategory, IsMultiplicativeElement],
function(C, x)
  local filt, class;

  filt := IsAnyCongruenceClass;

  if IsCongruenceCategory(C) then
    filt := filt and IsCongruenceClass;
  elif IsLeftCongruenceCategory(C) then
    filt := filt and IsLeftCongruenceClass;
  else
    Assert(1, IsRightCongruenceCategory(C));
    filt := filt and IsRightCongruenceClass;
  fi;

  class := Objectify(NewType(FamilyObj(Range(C)), filt), rec());
  SetParentAttr(class, Range(C));
  SetEquivalenceClassRelation(class, C);
  SetRepresentative(class, x);
  if HasIsFinite(Range(C)) and IsFinite(Range(C)) then
    SetIsFinite(class, true);
  fi;

  return class;
end);

########################################################################
# Congruence class attributes
########################################################################

InstallMethod(AsList, "for IsAnyCongruenceClass", [IsAnyCongruenceClass],
C -> ImagesElm(EquivalenceClassRelation(C), Representative(C)));

InstallMethod(Enumerator, "for IsAnyCongruenceClass", [IsAnyCongruenceClass],
AsList);

InstallMethod(Size, "for IsAnyCongruenceClass", [IsAnyCongruenceClass],
C -> Size(AsList(C)));

########################################################################
# Congruence class operators
########################################################################

# Multiplication for congruence classes: only makes sense for 2-sided
InstallMethod(\*, "for two congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(lhop, rhop)
  if EquivalenceClassRelation(lhop) <> EquivalenceClassRelation(rhop) then
    ErrorNoReturn("the arguments (cong. classes) are not classes of the same ",
                  "congruence");
  fi;
  return EquivalenceClassOfElementNC(EquivalenceClassRelation(lhop),
                                     Representative(lhop) *
                                     Representative(rhop));
end);

InstallMethod(\=,
"for IsAnyCongruenceClass and IsAnyCongruenceClass",
IsIdenticalObj,
[IsAnyCongruenceClass, IsAnyCongruenceClass],
function(lhop, rhop)
  return EquivalenceClassRelation(lhop) = EquivalenceClassRelation(rhop)
    and [Representative(lhop), Representative(rhop)]
    in EquivalenceClassRelation(lhop);
end);

InstallMethod(\<, "for two congruence classes",
[IsAnyCongruenceClass, IsAnyCongruenceClass],
function(lhop, rhop)
  return EquivalenceClassRelation(lhop) = EquivalenceClassRelation(rhop)
    and RepresentativeSmallest(lhop) < RepresentativeSmallest(rhop);
end);

InstallMethod(\in,
"for a mult. elt. and IsAnyCongruenceClass",
[IsMultiplicativeElement, IsAnyCongruenceClass],
function(x, class)
  local C;
  C := EquivalenceClassRelation(class);
  return x in Range(C) and [x, Representative(class)] in C;
end);

InstallMethod(ViewObj, "for IsAnyCongruenceClass", [IsAnyCongruenceClass],
function(C)
  local string;
  if IsCongruenceCategory(EquivalenceClassRelation(C)) then
    string := "";
  else
    string := Concatenation(AnyCongruenceString(EquivalenceClassRelation(C)),
                            " ");
  fi;
  Print("<", string, "congruence class of ");
  ViewObj(Representative(C));
  Print(">");
end);

########################################################################
# Congruence class actions
########################################################################

InstallMethod(OnLeftCongruenceClasses,
"for a left congruence class and a multiplicative element",
[IsLeftCongruenceClass, IsMultiplicativeElement],
function(class, x)
  local C;
  C := EquivalenceClassRelation(class);
  return EquivalenceClassOfElementNC(C, x * Representative(class));
end);

InstallMethod(OnRightCongruenceClasses,
"for a right congruence class and a multiplicative element",
[IsRightCongruenceClass, IsMultiplicativeElement],
function(class, x)
  local C;
  C := EquivalenceClassRelation(class);
  return EquivalenceClassOfElementNC(C, Representative(class) * x);
end);
