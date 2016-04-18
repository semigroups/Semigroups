############################################################################
##
#W  simple-cong.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on finite (0-)simple semigroups,
## using isomorphisms to Rees (0-)matrix semigroups and methods in
## congruences/reesmat.gd/gi.  These functions are not intended for direct
## use by an end-user.
##

InstallGlobalFunction(SemigroupCongruence,
function(arg)
  local s, pairs;
  if not Length(arg) >= 2 then
    Error("Semigroups: SemigroupCongruence: usage,\n",
          "at least 2 arguments are required,");
    return;
  fi;
  if not IsSemigroup(arg[1]) then
    Error("Semigroups: SemigroupCongruence: usage,\n",
          "1st argument <s> must be a semigroup,");
    return;
  fi;
  s := arg[1];

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
      Error("Semigroups: SemigroupCongruence: usage,\n",
            "<pairs> should be a list of lists of size 2,");
      return;
    fi;
    if not ForAll(pairs, p -> p[1] in s and p[2] in s) then
      Error("Semigroups: SemigroupCongruence: usage,\n",
            "each pair should contain elements from the semigroup <s>,");
      return;
    fi;
    if ((HasIsSimpleSemigroup(s) or IsActingSemigroup(s)) and
        IsSimpleSemigroup(s)) or
       ((HasIsZeroSimpleSemigroup(s) or IsActingSemigroup(s)) and
        IsZeroSimpleSemigroup(s)) then
      return SEMIGROUPS_SimpleCongFromPairs(s, pairs);
    elif (HasIsInverseSemigroup(s) or IsActingSemigroup(s))
        and IsInverseSemigroup(s) then
      return SEMIGROUPS_InverseCongFromPairs(s, pairs);
    else
      return SemigroupCongruenceByGeneratingPairs(s, pairs);
    fi;
  elif (IsRMSCongruenceByLinkedTriple(arg[2]) and IsSimpleSemigroup(s)) or
    (IsRZMSCongruenceByLinkedTriple(arg[2]) and IsZeroSimpleSemigroup(s)) then
    # We should have a congruence of an isomorphic RMS/RZMS
    if Range(IsomorphismReesMatrixSemigroup(s)) = Range(arg[2]) then
      return SEMIGROUPS_SimpleCongFromRMSCong(s, arg[2]);
    else
      Error("Semigroups: SemigroupCongruence: usage,\n",
            "<cong> should be over a Rees (0-)matrix semigroup ",
            "isomorphic to <s>");
      return;
    fi;
  elif IsSemigroupIdeal(arg[2]) and Parent(arg[2]) = s then
    return ReesCongruenceOfSemigroupIdeal(arg[2]);
  elif Length(arg) = 3 and
    IsInverseSemigroup(arg[2]) and
    IsDenseList(arg[3]) and
    IsInverseSemigroup(s) then
    # We should have the kernel and trace of a congruence on an inverse
    # semigroup
    return InverseSemigroupCongruenceByKernelTrace(s, arg[2], arg[3]);
  else
    TryNextMethod();
  fi;
end);

#

InstallGlobalFunction(SEMIGROUPS_SimpleCongFromPairs,
function(S, pairs)
  local iso, r, rmspairs, pcong, rmscong, cong;

  # If s is a RMS/RZMS, then just create the linked triple congruence
  if IsReesMatrixSemigroup(S) then
    # gaplint: ignore 2
    cong := AsRMSCongruenceByLinkedTriple(
            SemigroupCongruenceByGeneratingPairs(S, pairs));
  elif IsReesZeroMatrixSemigroup(S) then
    # gaplint: ignore 2
    cong := AsRZMSCongruenceByLinkedTriple(
            SemigroupCongruenceByGeneratingPairs(S, pairs));
  else
    # Otherwise, create a SIMPLECONG
    if IsSimpleSemigroup(S) then
      iso := IsomorphismReesMatrixSemigroup(S);
    else
      iso := IsomorphismReesZeroMatrixSemigroup(S);
    fi;
    r := Range(iso);
    rmspairs := List(pairs, p -> [p[1] ^ iso, p[2] ^ iso]);
    pcong := SemigroupCongruenceByGeneratingPairs(r, rmspairs);
    if IsReesMatrixSemigroup(r) then
      rmscong := AsRMSCongruenceByLinkedTriple(pcong);
    else  #elif IsReesZeroMatrixSemigroup(r) then
      rmscong := AsRZMSCongruenceByLinkedTriple(pcong);
    fi;
    # Special case for the universal congruence
    if IsUniversalSemigroupCongruence(rmscong) then
      cong := UniversalSemigroupCongruence(S);
    else
      cong := SEMIGROUPS_SimpleCongFromRMSCong(S, iso, rmscong);
    fi;
  fi;
  SetGeneratingPairsOfMagmaCongruence(cong, pairs);
  return cong;
end);

#

InstallGlobalFunction(SEMIGROUPS_SimpleCongFromRMSCong,
function(S, iso, rmscong)
  local r, fam, cong;
  r := Range(rmscong);

  # Construct the object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, SEMIGROUPS_CongSimple),
                    rec(rmscong := rmscong, iso := iso));
  SetSource(cong, S);
  SetRange(cong, S);
  return cong;
end);

#

InstallGlobalFunction(SEMIGROUPS_SimpleClassFromRMSclass,
function(cong, rmsclass)
  local fam, class;
  fam := FamilyObj(Range(cong));
  class := Objectify(NewType(fam, SEMIGROUPS_CongClassSimple),
                     rec(rmsclass := rmsclass, iso := cong!.iso));
  SetParentAttr(class, cong);
  SetRepresentative(class, Representative(rmsclass) ^
                           InverseGeneralMapping(cong!.iso));
  SetEquivalenceClassRelation(class, cong);
  return class;
end);

#

InstallMethod(ViewObj,
"for a (0-)simple semigroup congruence",
[SEMIGROUPS_CongSimple],
function(cong)
  Print("<semigroup congruence over ");
  ViewObj(Range(cong));
  Print(" with linked triple (",
        StructureDescription(cong!.rmscong!.n:short), ",",
        Size(cong!.rmscong!.colBlocks), ",",
        Size(cong!.rmscong!.rowBlocks), ")>");
end);

#

InstallMethod(CongruencesOfSemigroup,
"for a (0-)simple or simple semigroup",
[IsSemigroup],
function(S)
  local iso, R, congs, i;
  if not (IsFinite(S) and (IsSimpleSemigroup(S)
                           or IsZeroSimpleSemigroup(S))) then
    TryNextMethod();
  fi;
  if IsReesMatrixSemigroup(S) or IsReesZeroMatrixSemigroup(S) then
    return CongruencesOfSemigroup(S);
  fi;
  iso := IsomorphismReesMatrixSemigroup(S);
  R := Range(iso);
  congs := ShallowCopy(CongruencesOfSemigroup(R));
  for i in [1 .. Length(congs)] do
    if IsUniversalSemigroupCongruence(congs[i]) then
      congs[i] := UniversalSemigroupCongruence(S);
    else
      congs[i] := SEMIGROUPS_SimpleCongFromRMSCong(S, iso, congs[i]);
    fi;
  od;
  return congs;
end);

#

InstallMethod(\=,
"for two (0-)simple semigroup congruences",
[SEMIGROUPS_CongSimple, SEMIGROUPS_CongSimple],
function(cong1, cong2)
  return (Range(cong1) = Range(cong2) and cong1!.rmscong = cong2!.rmscong);
end);

#

InstallMethod(JoinMagmaCongruences,
"for two (0-)simple semigroup congruences",
[SEMIGROUPS_CongSimple, SEMIGROUPS_CongSimple],
function(cong1, cong2)
  local join;
  if Range(cong1) <> Range(cong2) or cong1!.iso <> cong2!.iso then
    ErrorNoReturn("Semigroups: JoinMagmaCongruences: usage,\n",
                  "<cong1> and <cong2> must be over the same semigroup,");
  fi;
  join := JoinSemigroupCongruences(cong1!.rmscong, cong2!.rmscong);
  return SEMIGROUPS_SimpleCongFromRMSCong(Range(cong1), cong1!.iso, join);
end);

#

InstallMethod(MeetMagmaCongruences,
"for two (0-)simple semigroup congruences",
[SEMIGROUPS_CongSimple, SEMIGROUPS_CongSimple],
function(cong1, cong2)
  local meet;
  if Range(cong1) <> Range(cong2) or cong1!.iso <> cong2!.iso then
    ErrorNoReturn("Semigroups: MeetMagmaCongruences: usage,\n",
                  "<cong1> and <cong2> must be over the same semigroup,");
  fi;
  meet := MeetSemigroupCongruences(cong1!.rmscong, cong2!.rmscong);
  return SEMIGROUPS_SimpleCongFromRMSCong(Range(cong1), cong1!.iso, meet);
end);

#

InstallMethod(\in,
"for an associative element collection and a (0-)simple semigroup congruence",
[IsAssociativeElementCollection, SEMIGROUPS_CongSimple],
function(pair, cong)
  local S;
  # Check for validity
  if Size(pair) <> 2 then
    return false;
  fi;
  S := Range(cong);
  if not ForAll(pair, x -> x in S) then
    return false;
  fi;
  return [pair[1] ^ cong!.iso, pair[2] ^ cong!.iso] in cong!.rmscong;
end);

#

InstallMethod(ImagesElm,
"for a (0-)simple semigroup congruence and an associative element",
[SEMIGROUPS_CongSimple, IsAssociativeElement],
function(cong, elm)
  return List(ImagesElm(cong!.rmscong, elm ^ cong!.iso),
              x -> x ^ InverseGeneralMapping(cong!.iso));
end);

#

InstallMethod(EquivalenceClasses,
"for a (0-)simple semigroup congruence",
[SEMIGROUPS_CongSimple],
function(cong)
  return List(EquivalenceClasses(cong!.rmscong),
              c -> SEMIGROUPS_SimpleClassFromRMSclass(cong, c));
end);

#

InstallMethod(EquivalenceClassOfElement,
"for a (0-)simple semigroup congruence and associative element",
[SEMIGROUPS_CongSimple, IsAssociativeElement],
function(cong, elm)
  if elm in Range(cong) then
    return EquivalenceClassOfElementNC(cong, elm);
  else
    ErrorNoReturn("Semigroups: EquivalenceClassOfElement: usage,\n",
                  "<elm> must be an element of the range of <cong>");
  fi;
end);

#

InstallMethod(EquivalenceClassOfElementNC,
"for a (0-)simple semigroup congruence and associative element",
[SEMIGROUPS_CongSimple, IsAssociativeElement],
function(cong, elm)
  local class;
  class := EquivalenceClassOfElementNC(cong!.rmscong, elm ^ cong!.iso);
  return SEMIGROUPS_SimpleClassFromRMSclass(cong, class);
end);

#

InstallMethod(NrCongruenceClasses,
"for a (0-simple) semigroup congruence",
[SEMIGROUPS_CongSimple],
function(cong)
  return NrCongruenceClasses(cong!.rmscong);
end);

#

InstallMethod(\in,
"for an associative element and a (0-)simple semigroup congruence class",
[IsAssociativeElement, SEMIGROUPS_CongClassSimple],
function(elm, class)
  return (elm ^ EquivalenceClassRelation(class)!.iso in class!.rmsclass);
end);

#

InstallMethod(\*,
"for two (0-)simple semigroup congruence classes",
[SEMIGROUPS_CongClassSimple, SEMIGROUPS_CongClassSimple],
function(c1, c2)
  return SEMIGROUPS_SimpleClassFromRMSclass(EquivalenceClassRelation(c1),
                                            c1!.rmsclass * c2!.rmsclass);
end);

#

InstallMethod(Size,
"for a (0-)simple semigroup congruence class",
[SEMIGROUPS_CongClassSimple],
function(class)
  return Size(class!.rmsclass);
end);

#

InstallMethod(\=,
"for two (0-)simple semigroup congruence classes",
[SEMIGROUPS_CongClassSimple, SEMIGROUPS_CongClassSimple],
function(c1, c2)
  return EquivalenceClassRelation(c1) = EquivalenceClassRelation(c2) and
         c1!.rmsclass = c2!.rmsclass;
end);

#

InstallMethod(GeneratingPairsOfMagmaCongruence,
"for a (0-)simple semigroup congruence",
[SEMIGROUPS_CongSimple],
function(cong)
  local map;
  map := InverseGeneralMapping(cong!.iso);
  return List(GeneratingPairsOfMagmaCongruence(cong!.rmscong),
               x -> [x[1] ^ map, x[2] ^ map]);
end);

#

InstallMethod(CanonicalRepresentative,
"for a (0-)simple semigroup congruence class",
[SEMIGROUPS_CongClassSimple],
function(class)
  return CanonicalRepresentative(class!.rmsclass) ^ class!.iso;
end);
