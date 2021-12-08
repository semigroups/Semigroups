############################################################################
##
##  cong.gi
##  Copyright (C) 2015-2021                              Michael C. Young
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

InstallMethod(AnyCongruenceCategory, "for a right congruence category", 
[IsRightCongruenceCategory], C -> IsRightCongruenceCategory);

InstallMethod(AnyCongruenceCategory, "for a left congruence category", 
[IsLeftCongruenceCategory], C -> IsLeftCongruenceCategory);

InstallMethod(AnyCongruenceCategory, "for a 2-sided congruence category", 
[IsCongruenceCategory], C -> IsCongruenceCategory);

########################################################################
# 1. Constructors
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
  else
    ErrorNoReturn("the arguments are not valid for this function");
  fi;
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
# 2. Attributes
########################################################################

InstallMethod(NrEquivalenceClasses, "for a semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  local classes;
  classes := EquivalenceClasses(cong);
  # Note: EquivalenceClasses may exclude all singletons due to a bug in GAP.
  # This is a workaround which adds any missing singletons.
  return Length(classes) + Size(Range(cong)) - Sum(classes, Size);
end);

InstallMethod(NonTrivialEquivalenceClasses, "for an equivalence relation",
[IsEquivalenceRelation],
x -> Filtered(EquivalenceClasses(x), y -> Size(y) > 1));

InstallMethod(NonTrivialEquivalenceClasses, "for IsAnyCongruenceCategory",
[IsAnyCongruenceCategory],
function(cong)
  local part, nr_classes, classes, i;
  part := EquivalenceRelationPartition(cong);
  nr_classes := Length(part);
  classes := EmptyPlist(nr_classes);
  for i in [1 .. nr_classes] do
    classes[i] := EquivalenceClassOfElementNC(cong, part[i][1]);
    SetAsList(classes[i], part[i]);
  od;
  return classes;
end);

InstallMethod(EquivalenceRelationLookup, "for an equivalence relation",
[IsEquivalenceRelation],
function(equiv)
  local S, lookup, class, nr, elm;
  S := Range(equiv);
  if not (IsSemigroup(S) and IsFinite(S)) then
    ErrorNoReturn("the range of the argument (an equivalence relation) ",
                  "is not a finite semigroup");
  elif not CanComputeFroidurePin(S) then
    TryNextMethod();
  fi;

  lookup := [1 .. Size(S)];
  for class in NonTrivialEquivalenceClasses(equiv) do
    nr := PositionCanonical(S, Representative(class));
    for elm in class do
      lookup[PositionCanonical(S, elm)] := nr;
    od;
  od;
  return lookup;
end);

InstallMethod(EquivalenceRelationCanonicalLookup,
"for an equivalence relation",
[IsEquivalenceRelation],
function(equiv)
  local S, lookup, max, dictionary, next, out, new_nr, i;
  S := Range(equiv);
  if not (IsSemigroup(S) and IsFinite(S)) then
    ErrorNoReturn("the range of the argument (an equivalence relation) ",
                  "is not a finite semigroup");
  elif not CanComputeFroidurePin(S) then
    TryNextMethod();
  fi;
  lookup := EquivalenceRelationLookup(equiv);
  max := Maximum(lookup);
  # We do not know whether the maximum is NrEquivalenceClasses(equiv)
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
"for a left semigroup congruence",
[IsLeftSemigroupCongruence],
cong -> Set(EquivalenceRelationPartition(cong), Set));

InstallMethod(EquivalenceRelationCanonicalPartition,
"for a right semigroup congruence",
[IsRightSemigroupCongruence],
cong -> Set(EquivalenceRelationPartition(cong), Set));

InstallMethod(EquivalenceRelationPartitionWithSingletons,
"for IsAnyCongruenceCategory", [IsAnyCongruenceCategory], 100, # TODO check if this is required
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
# 3. Operators
########################################################################

BindGlobal("_GenericCongIn",
function(string, pair, cong)
  local S;
  Assert(1, IsString(string));
  Assert(1, IsDenseList(pair));
  Assert(1, IsLeftSemigroupCongruence(cong)
            or IsRightSemigroupCongruence(cong));
  S := Range(cong);
  if Size(pair) <> 2 then
    ErrorNoReturn("the 1st argument (a list) does not have length 2");
  elif not (pair[1] in S and pair[2] in S) then
    ErrorNoReturn("the items in the 1st argument (a list) do not all belong to ",
                  "the range of the 2nd argument (a ", string, " semigroup ",
                  "congruence)");
  elif CanEasilyCompareElements(pair[1]) and pair[1] = pair[2] then
    return true;
  fi;
  return CongruenceTestMembershipNC(cong, pair[1], pair[2]);
end);

InstallMethod(\in,
"for dense list and left semigroup congruence",
[IsDenseList, IsLeftSemigroupCongruence],
{pair, cong} -> _GenericCongIn("left", pair, cong));

InstallMethod(\in,
"for dense list and right semigroup congruence",
[IsDenseList, IsRightSemigroupCongruence],
{pair, cong} -> _GenericCongIn("right", pair, cong));

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

# TODO(now) shouldn't this be in another file?
InstallMethod(\=, "for two semigroup congruences with generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence,
 IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(c1, c2)
  return Range(c1) = Range(c2)
         and ForAll(GeneratingPairsOfSemigroupCongruence(c1), p -> p in c2)
         and ForAll(GeneratingPairsOfSemigroupCongruence(c2), p -> p in c1);
end);

InstallMethod(IsSuperrelation, "for semigroup congruences",
[IsSemigroupCongruence, IsSemigroupCongruence],
{C1, C2} -> IsSubrelation(C2, C1));

InstallMethod(IsSuperrelation, "for left semigroup congruences",
[IsLeftSemigroupCongruence, IsLeftSemigroupCongruence],
{C1, C2} -> IsSubrelation(C2, C1));

InstallMethod(IsSuperrelation, "for right semigroup congruences",
[IsRightSemigroupCongruence, IsRightSemigroupCongruence],
{C1, C2} -> IsSubrelation(C2, C1));

########################################################################
# 4. Congruence classes
########################################################################

# Multiplication for congruence classes: only makes sense for 2-sided
InstallMethod(\*, "for two congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  if EquivalenceClassRelation(class1) <> EquivalenceClassRelation(class2) then
    ErrorNoReturn("the arguments are not classes of the same congruence");
  fi;
  return EquivalenceClassOfElementNC(EquivalenceClassRelation(class1),
                                     Representative(class1) *
                                     Representative(class2));
end);

# FIXME why isn't there a method of IsLeftCongruenceClass and
# IsRightCongruenceClass

InstallMethod(\=, "for two congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2)
         and Representative(class1) in class2;
end);

InstallMethod(\<, "for two congruence classes",
[IsCongruenceClass, IsCongruenceClass],
function(class1, class2)
  return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2)
    and RepresentativeSmallest(class1) < RepresentativeSmallest(class2);
end);

InstallMethod(\in,
"for multiplicative element and congruence class",
[IsMultiplicativeElement, IsInverseSemigroupCongruenceClassByKernelTrace],
function(elm, class)
  local cong;
  cong := EquivalenceClassRelation(class);
  return elm in Range(cong) and [elm, class!.rep] in cong;
end);

BindGlobal("_ViewCongObj",
function(string, C)
  Print("<", string, "congruence class of ");
  ViewObj(Representative(C));
  Print(">");
  return fail;
end);

InstallMethod(ViewObj, "for a congruence class", [IsCongruenceClass],
C -> _ViewCongObj("", C));

InstallMethod(ViewObj, "for a left congruence class", [IsLeftCongruenceClass],
C -> _ViewCongObj("left ", C));

InstallMethod(ViewObj, "for a right congruence class",
[IsRightCongruenceClass], C -> _ViewCongObj("right ", C));

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
