############################################################################
##
##  congruences/congsimple.gi
##  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on finite (0-)simple semigroups,
## using isomorphisms to Rees (0-)matrix semigroups and methods in
## congruences/reesmat.gd/gi.  These functions are not intended for direct # use
## by an end-user. JDM: say what?
##

InstallMethod(ViewObj,
"for a (0-)simple semigroup congruence",
[IsSimpleSemigroupCongruence],
function(cong)
  Print("<semigroup congruence over ");
  ViewObj(Range(cong));
  Print(" with linked triple (",
        StructureDescription(cong!.rmscong!.n:short), ",",
        Size(cong!.rmscong!.colBlocks), ",",
        Size(cong!.rmscong!.rowBlocks), ")>");
end);

InstallMethod(CongruencesOfSemigroup,
"for a (0-)simple or simple semigroup",
[IsSemigroup],
1,  # Try this before the method in congpairs.gi
function(S)
  local iso, R, congs, i;
  if not (IsFinite(S) and (IsSimpleSemigroup(S)
                           or IsZeroSimpleSemigroup(S))) then
    TryNextMethod();
  fi;
  if IsReesMatrixSemigroup(S) or IsReesZeroMatrixSemigroup(S) then
    return CongruencesOfSemigroup(S);
  fi;
  if IsSimpleSemigroup(S) then
    iso := IsomorphismReesMatrixSemigroup(S);
  else
    iso := IsomorphismReesZeroMatrixSemigroup(S);
  fi;
  R := Range(iso);
  congs := ShallowCopy(CongruencesOfSemigroup(R));
  if IsReesMatrixSemigroup(R) then
    # The universal congruence has a linked triple
    return List(congs,
                cong -> SEMIGROUPS.SimpleCongFromRMSCong(S, iso, cong));
  else
    # The universal congruence has no linked triple
    for i in [1 .. Length(congs)] do
      if IsRZMSCongruenceByLinkedTriple(congs[i]) then
        congs[i] := SEMIGROUPS.SimpleCongFromRMSCong(S, iso, congs[i]);
      else
        congs[i] := UniversalSemigroupCongruence(S);
      fi;
    od;
    return congs;
  fi;
end);

InstallMethod(\=,
"for two (0-)simple semigroup congruences",
[IsSimpleSemigroupCongruence, IsSimpleSemigroupCongruence],
function(cong1, cong2)
  return (Range(cong1) = Range(cong2) and cong1!.rmscong = cong2!.rmscong);
end);

InstallMethod(JoinSemigroupCongruences,
"for two (0-)simple semigroup congruences",
[IsSimpleSemigroupCongruence, IsSimpleSemigroupCongruence],
function(cong1, cong2)
  local join;
  if Range(cong1) <> Range(cong2) or cong1!.iso <> cong2!.iso then
    ErrorNoReturn("Semigroups: JoinSemigroupCongruences: usage,\n",
                  "<cong1> and <cong2> must be over the same semigroup,");
  fi;
  join := JoinSemigroupCongruences(cong1!.rmscong, cong2!.rmscong);
  return SEMIGROUPS.SimpleCongFromRMSCong(Range(cong1), cong1!.iso, join);
end);

InstallMethod(MeetSemigroupCongruences,
"for two (0-)simple semigroup congruences",
[IsSimpleSemigroupCongruence, IsSimpleSemigroupCongruence],
function(cong1, cong2)
  local meet;
  if Range(cong1) <> Range(cong2) or cong1!.iso <> cong2!.iso then
    ErrorNoReturn("Semigroups: MeetSemigroupCongruences: usage,\n",
                  "<cong1> and <cong2> must be over the same semigroup,");
  fi;
  meet := MeetSemigroupCongruences(cong1!.rmscong, cong2!.rmscong);
  return SEMIGROUPS.SimpleCongFromRMSCong(Range(cong1), cong1!.iso, meet);
end);

InstallMethod(CongruenceTestMembershipNC,
"for (0-)simple semigroup congruence and two multiplicative elements",
[IsSimpleSemigroupCongruence, IsMultiplicativeElement, IsMultiplicativeElement],
function(cong, elm1, elm2)
  return [elm1 ^ cong!.iso, elm2 ^ cong!.iso] in cong!.rmscong;
end);

InstallMethod(ImagesElm,
"for a (0-)simple semigroup congruence and a multiplicative element",
[IsSimpleSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  return List(ImagesElm(cong!.rmscong, elm ^ cong!.iso),
              x -> x ^ InverseGeneralMapping(cong!.iso));
end);

InstallMethod(EquivalenceClasses,
"for a (0-)simple semigroup congruence",
[IsSimpleSemigroupCongruence],
function(cong)
  return List(EquivalenceClasses(cong!.rmscong),
              c -> SEMIGROUPS.SimpleClassFromRMSclass(cong, c));
end);

InstallMethod(EquivalenceClassOfElement,
"for a (0-)simple semigroup congruence and multiplicative element",
[IsSimpleSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    ErrorNoReturn("Semigroups: EquivalenceClassOfElement: usage,\n",
                  "<elm> must be an element of the range of <cong>,");
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

InstallMethod(EquivalenceClassOfElementNC,
"for a (0-)simple semigroup congruence and multiplicative element",
[IsSimpleSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  local class;
  class := EquivalenceClassOfElementNC(cong!.rmscong, elm ^ cong!.iso);
  return SEMIGROUPS.SimpleClassFromRMSclass(cong, class);
end);

InstallMethod(NrEquivalenceClasses,
"for a (0-)simple semigroup congruence",
[IsSimpleSemigroupCongruence],
function(cong)
  return NrEquivalenceClasses(cong!.rmscong);
end);

InstallMethod(\in,
"for a multiplicative element and a (0-)simple semigroup congruence class",
[IsMultiplicativeElement, IsSimpleSemigroupCongruenceClass],
function(elm, class)
  return (elm ^ EquivalenceClassRelation(class)!.iso in class!.rmsclass);
end);

InstallMethod(Enumerator,
"for a (0-)simple semigroup congruence class",
[IsSimpleSemigroupCongruenceClass],
function(class)
  return ImagesElm(EquivalenceClassRelation(class), Representative(class));
end);

InstallMethod(\*,
"for two (0-)simple semigroup congruence classes",
[IsSimpleSemigroupCongruenceClass, IsSimpleSemigroupCongruenceClass],
function(c1, c2)
  return SEMIGROUPS.SimpleClassFromRMSclass(EquivalenceClassRelation(c1),
                                            c1!.rmsclass * c2!.rmsclass);
end);

InstallMethod(Size,
"for a (0-)simple semigroup congruence class",
[IsSimpleSemigroupCongruenceClass],
function(class)
  return Size(class!.rmsclass);
end);

InstallMethod(\=,
"for two (0-)simple semigroup congruence classes",
[IsSimpleSemigroupCongruenceClass, IsSimpleSemigroupCongruenceClass],
function(c1, c2)
  return EquivalenceClassRelation(c1) = EquivalenceClassRelation(c2) and
         c1!.rmsclass = c2!.rmsclass;
end);

InstallMethod(GeneratingPairsOfSemigroupCongruence,
"for a (0-)simple semigroup congruence",
[IsSimpleSemigroupCongruence],
function(cong)
  local map;
  map := InverseGeneralMapping(cong!.iso);
  return List(GeneratingPairsOfSemigroupCongruence(cong!.rmscong),
               x -> [x[1] ^ map, x[2] ^ map]);
end);

InstallMethod(CanonicalRepresentative,
"for a (0-)simple semigroup congruence class",
[IsSimpleSemigroupCongruenceClass],
function(class)
  return CanonicalRepresentative(class!.rmsclass) ^
         InverseGeneralMapping(class!.iso);
end);

InstallMethod(IsSubrelation,
"for two (0-)simple semigroup congruences",
[IsSimpleSemigroupCongruence, IsSimpleSemigroupCongruence],
function(cong1, cong2)
  return IsSubrelation(cong1!.rmscong, cong2!.rmscong);
end);

InstallMethod(EquivalenceRelationCanonicalLookup,
"for a (0-)simple semigroup congruence",
# FIXME Why does the string say (0-)simple and not simple?
[IsSimpleSemigroupCongruence],
function(cong)
  local S, rmstable, nrclasses, iso, elms, table, newnums, next, rmsclass, i;

  S         := Range(cong);
  rmstable  := EquivalenceRelationCanonicalLookup(cong!.rmscong);
  nrclasses := NrEquivalenceClasses(cong!.rmscong);
  iso       := cong!.iso;
  elms      := AsListCanonical(S);

  # Renumber the entries so we start at 1
  table   := EmptyPlist(Length(elms));
  newnums := EmptyPlist(nrclasses);
  next    := 1;

  for i in [1 .. Length(elms)] do
    rmsclass := rmstable[PositionCanonical(Range(iso), elms[i] ^ iso)];
    if not IsBound(newnums[rmsclass]) then
      newnums[rmsclass] := next;
      next := next + 1;
    fi;
    table[i] := newnums[rmsclass];
  od;
  return table;
end);
