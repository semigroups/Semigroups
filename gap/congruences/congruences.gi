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
##       inverse.gi - Inverse semigroups
##       pairs.gi   - Congruences with generating pairs
##       rees.gi    - Rees congruences
##       reesmat.gi - (0-)simple Rees matrix semigroups
##       simple.gi  - (0-)simple semigroups
##       univ.gi    - Universal congruences
##
## congruences.gd contains declarations for many of these.
##

InstallGlobalFunction(SemigroupCongruence,
function(arg)
  local S, pairs, cong;
  if not Length(arg) >= 2 then
    ErrorMayQuit("Semigroups: SemigroupCongruence: usage,\n",
                 "at least 2 arguments are required,");
  fi;
  if not IsSemigroup(arg[1]) then
    ErrorMayQuit("Semigroups: SemigroupCongruence: usage,\n",
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
      ErrorMayQuit("Semigroups: SemigroupCongruence: usage,\n",
                   "<pairs> should be a list of lists of size 2,");
    fi;
    if not ForAll(pairs, p -> p[1] in S and p[2] in S) then
      ErrorMayQuit("Semigroups: SemigroupCongruence: usage,\n",
                   "each pair should contain elements from the semigroup <S>,");
    fi;
    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);
    if not IsFinite(S) then
      return SemigroupCongruenceByGeneratingPairs(S, pairs);
    elif IsSimpleSemigroup(S) or IsZeroSimpleSemigroup(S) then
      return SEMIGROUPS_SimpleCongFromPairs(S, pairs);
    elif (HasIsSemilattice(S) or IsActingSemigroup(S))
        and IsSemilattice(S) then
      cong := SemigroupCongruenceByGeneratingPairs(S, pairs);
      SetIsSemilatticeCongruence(cong, true);
      return cong;
    elif IsInverseSemigroup(S) then
      return SEMIGROUPS_InverseCongFromPairs(S, pairs);
    else
      return SemigroupCongruenceByGeneratingPairs(S, pairs);
    fi;
  elif IsGeneralMapping(arg[2])
     and ((IsRMSCongruenceByLinkedTriple(arg[3]) and IsSimpleSemigroup(S)) or
          (IsRZMSCongruenceByLinkedTriple(arg[3]) and IsZeroSimpleSemigroup(S)))
      then
    # We should have a congruence of an isomorphic RMS/RZMS
    if Range(arg[2]) = Range(arg[3]) and S = Source(arg[2]) then
      return SEMIGROUPS_SimpleCongFromRMSCong(S, arg[2], arg[3]);
    else
      ErrorMayQuit("Semigroups: SemigroupCongruence: usage,\n",
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

#

InstallGlobalFunction(LeftSemigroupCongruence,
function(arg)
  local S, pairs;
  if not Length(arg) >= 2 then
    ErrorMayQuit("Semigroups: LeftSemigroupCongruence: usage,\n",
                 "at least 2 arguments are required,");
  fi;
  if not IsSemigroup(arg[1]) then
    ErrorMayQuit("Semigroups: LeftSemigroupCongruence: usage,\n",
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
      ErrorMayQuit("Semigroups: LeftSemigroupCongruence: usage,\n",
                   "<pairs> should be a list of lists of size 2,");
    fi;
    if not ForAll(pairs, p -> p[1] in S and p[2] in S) then
      ErrorMayQuit("Semigroups: LeftSemigroupCongruence: usage,\n",
                   "each pair should contain elements from the semigroup <S>,");
    fi;
    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);
    return LeftSemigroupCongruenceByGeneratingPairs(S, pairs);
  else
    TryNextMethod();
  fi;
end);

#

InstallGlobalFunction(RightSemigroupCongruence,
function(arg)
  local S, pairs;
  if not Length(arg) >= 2 then
    ErrorMayQuit("Semigroups: RightSemigroupCongruence: usage,\n",
                 "at least 2 arguments are required,");
  fi;
  if not IsSemigroup(arg[1]) then
    ErrorMayQuit("Semigroups: RightSemigroupCongruence: usage,\n",
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
      ErrorMayQuit("Semigroups: RightSemigroupCongruence: usage,\n",
                   "<pairs> should be a list of lists of size 2,");
    fi;
    if not ForAll(pairs, p -> p[1] in S and p[2] in S) then
      ErrorMayQuit("Semigroups: RightSemigroupCongruence: usage,\n",
                   "each pair should contain elements from the semigroup <S>,");
    fi;
    # Remove any reflexive pairs
    pairs := Filtered(pairs, p -> p[1] <> p[2]);
    return RightSemigroupCongruenceByGeneratingPairs(S, pairs);
  else
    TryNextMethod();
  fi;
end);
