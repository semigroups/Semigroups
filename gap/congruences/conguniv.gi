############################################################################
##
##  congruences/conguniv.gi
##  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for the unique universal congruence on a
## semigroup, that is the relation SxS on a semigroup S.
##

InstallMethod(UniversalSemigroupCongruence,
"for a semigroup",
[IsSemigroup],
function(S)
  local fam, cong;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam,
                            IsSemigroupCongruence and IsAttributeStoringRep),
                    rec());
  SetSource(cong, S);
  SetRange(cong, S);
  SetIsUniversalSemigroupCongruence(cong, true);
  return cong;
end);

InstallMethod(IsUniversalSemigroupCongruence,
"for a semigroup congruence",
[IsSemigroupCongruence],
function(cong)
  return NrEquivalenceClasses(cong) = 1;
end);

InstallImmediateMethod(IsUniversalSemigroupCongruence,
IsSemigroupCongruence and HasNrEquivalenceClasses, 0,
function(cong)
  return NrEquivalenceClasses(cong) = 1;
end);

InstallMethod(IsUniversalSemigroupCongruence,
"for a semigroup congruence",
[IsRMSCongruenceByLinkedTriple],
function(cong)
  return Length(cong!.colBlocks) = 1 and
         Length(cong!.rowBlocks) = 1 and
         cong!.n = UnderlyingSemigroup(Range(cong));
end);

InstallImmediateMethod(IsUniversalSemigroupCongruence,
IsRZMSCongruenceByLinkedTriple, 0,
ReturnFalse);

InstallMethod(EquivalenceRelationCanonicalLookup,
"for a universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  return ListWithIdenticalEntries(Size(Range(cong)), 1);
end);

InstallMethod(EquivalenceRelationPartition,
"for a universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  return [AsList(Range(cong))];
end);

InstallMethod(ViewObj,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  Print("<universal semigroup congruence over ");
  ViewObj(Range(cong));
  Print(">");
end);

InstallMethod(\=,
"for two universal semigroup congruences",
[IsUniversalSemigroupCongruence, IsUniversalSemigroupCongruence],
function(cong1, cong2)
  return Range(cong1) = Range(cong2);
end);

InstallMethod(\=,
"for universal congruence and RZMS congruence by linked triple",
[IsUniversalSemigroupCongruence, IsRZMSCongruenceByLinkedTriple],
function(ucong, cong)
  return false;
end);

InstallMethod(\=,
"for RZMS congruence by linked triple and universal congruence",
[IsRZMSCongruenceByLinkedTriple, IsUniversalSemigroupCongruence],
function(cong, ucong)
  return false;
end);

InstallMethod(\=,
"for universal congruence and semigroup congruence with generating pairs",
[IsUniversalSemigroupCongruence,
 IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(ucong, pcong)
  return Range(ucong) = Range(pcong) and NrEquivalenceClasses(pcong) = 1;
end);

InstallMethod(\=,
"for universal congruence and semigroup congruence with generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence,
 IsUniversalSemigroupCongruence],
function(pcong, ucong)
  return Range(ucong) = Range(pcong) and NrEquivalenceClasses(pcong) = 1;
end);

InstallMethod(CongruenceTestMembershipNC,
"for universal semigroup congruence and two multiplicative elements",
[IsUniversalSemigroupCongruence,
 IsMultiplicativeElement, IsMultiplicativeElement],
function(pair, elm1, elm2)
  return true;
end);

InstallMethod(IsSubrelation,
"for a universal semigroup congruence and a semigroup congruence",
[IsUniversalSemigroupCongruence, IsSemigroupCongruence],
function(uni, cong)
  if Range(uni) <> Range(cong) then
    ErrorNoReturn("Semigroups: IsSubrelation: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;
  return true;
end);

InstallMethod(IsSubrelation,
"for a semigroup congruence and a universal semigroup congruence",
[IsSemigroupCongruence, IsUniversalSemigroupCongruence],
function(cong, uni)
  if Range(uni) <> Range(cong) then
    ErrorNoReturn("Semigroups: IsSubrelation: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;
  return uni = cong;
end);

InstallMethod(ImagesElm,
"for universal semigroup congruence and element",
[IsUniversalSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    ErrorNoReturn("Semigroups: ImagesElm: usage,\n",
                  "the second argument <elm> must be in <cong>'s semigroup,");
  fi;
  return Elements(Range(cong));
end);

InstallMethod(NrEquivalenceClasses,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  return 1;
end);

InstallMethod(JoinSemigroupCongruences,
"for semigroup congruence and universal congruence",
[IsSemigroupCongruence, IsUniversalSemigroupCongruence],
function(cong, ucong)
  if Range(cong) <> Range(ucong) then
    ErrorNoReturn("Semigroups: JoinSemigroupCongruences: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;
  return ucong;
end);

InstallMethod(JoinSemigroupCongruences,
"for universal congruence and semigroup congruence",
[IsUniversalSemigroupCongruence, IsSemigroupCongruence],
function(ucong, cong)
  if Range(cong) <> Range(ucong) then
    ErrorNoReturn("Semigroups: JoinSemigroupCongruences: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;
  return ucong;
end);

InstallMethod(MeetSemigroupCongruences,
"for semigroup congruence and universal congruence",
[IsSemigroupCongruence, IsUniversalSemigroupCongruence],
function(cong, ucong)
  if Range(cong) <> Range(ucong) then
    ErrorNoReturn("Semigroups: MeetSemigroupCongruences: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;
  return cong;
end);

InstallMethod(MeetSemigroupCongruences,
"for universal congruence and semigroup congruence",
[IsUniversalSemigroupCongruence, IsSemigroupCongruence],
function(ucong, cong)
  if Range(cong) <> Range(ucong) then
    ErrorNoReturn("Semigroups: MeetSemigroupCongruences: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;
  return cong;
end);

InstallMethod(EquivalenceClasses,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  return [EquivalenceClassOfElement(cong, Representative(Range(cong)))];
end);

InstallMethod(EquivalenceClassOfElement,
"for universal semigroup congruence and associative element",
[IsUniversalSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  # Check that the arguments make sense
  if not elm in Range(cong) then
    ErrorNoReturn("Semigroups: EquivalenceClassOfElement: usage,\n",
                  "the second argument <elm> must be ",
                  "in the semigroup of 1st argument <cong>,");
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

InstallMethod(EquivalenceClassOfElementNC,
"for universal semigroup congruence and associative element",
[IsUniversalSemigroupCongruence, IsMultiplicativeElement],
function(cong, elm)
  local fam, class;
  fam := CollectionsFamily(FamilyObj(elm));
  class := Objectify(NewType(fam, IsUniversalSemigroupCongruenceClass), rec());
  SetParentAttr(class, Range(cong));
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

InstallMethod(\in,
"for associative element and universal semigroup congruence class",
[IsMultiplicativeElement, IsUniversalSemigroupCongruenceClass],
function(elm, class)
  return elm in Parent(class);
end);

InstallMethod(\*,
"for two universal semigroup congruence classes",
[IsUniversalSemigroupCongruenceClass, IsUniversalSemigroupCongruenceClass],
function(c1, c2)
  if EquivalenceClassRelation(c1) <> EquivalenceClassRelation(c2) then
    ErrorNoReturn("Semigroups: \\*: usage,\n",
                  "the args <c1> and <c2> must be over the same congruence,");
  fi;
  return c1;
end);

InstallMethod(Size,
"for universal semigroup congruence class",
[IsUniversalSemigroupCongruenceClass],
function(class)
  return Size(Range(EquivalenceClassRelation(class)));
end);

InstallMethod(\=,
"for two universal semigroup congruence classes",
[IsUniversalSemigroupCongruenceClass, IsUniversalSemigroupCongruenceClass],
function(c1, c2)
  return EquivalenceClassRelation(c1) = EquivalenceClassRelation(c2);
end);

InstallMethod(GeneratingPairsOfMagmaCongruence,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  local S, it, z, x, m, iso, r, n, colBlocks, rowBlocks, rmscong, pairs, d;
  S := Range(cong);
  if Size(S) = 1 then
    return [];
  fi;
  it := Iterator(S);
  z := MultiplicativeZero(S);
  if z <> fail then
    if IsZeroSimpleSemigroup(S) then
      # Just link zero to any non-zero element
      x := NextIterator(it);
      if x = z then
        return [[z, NextIterator(it)]];
      else
        return [[x, z]];
      fi;
    else
      # Link zero to a representative of each maximal D-class
      return List(MaximalDClasses(S), cl -> [z, Representative(cl)]);
    fi;
  fi;

  # Otherwise we have no zero: use the minimal ideal
  m := MinimalIdeal(S);

  # Use the linked triple
  iso := IsomorphismReesMatrixSemigroup(m);
  r := Range(iso);
  n := UnderlyingSemigroup(r);
  colBlocks := [[1 .. Size(Matrix(r)[1])]];
  rowBlocks := [[1 .. Size(Matrix(r))]];
  rmscong := RMSCongruenceByLinkedTriple(r, n, colBlocks, rowBlocks);
  cong := SEMIGROUPS.SimpleCongFromRMSCong(m, iso, rmscong);
  pairs := ShallowCopy(GeneratingPairsOfSemigroupCongruence(cong));

  if IsSimpleSemigroup(S) then
    return pairs;
  fi;
  # We must relate each maximal D-class to the minimal ideal
  z := GeneratorsOfSemigroupIdeal(m)[1];
  for d in MaximalDClasses(S) do
    Add(pairs, [z, Representative(d)]);
  od;
  return pairs;
end);
