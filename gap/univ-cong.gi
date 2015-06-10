############################################################################
##
#W  univ-cong.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for the unique universal congruence on a
## semigroup.
##

#

InstallMethod(UniversalSemigroupCongruence,
"for a semigroup",
[IsSemigroup],
function(s)
  local fam, cong;
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(s)),
                               ElementsFamily(FamilyObj(s)));
  cong := Objectify(NewType(fam, IsUniversalSemigroupCongruence), rec());
  SetSource(cong, s);
  SetRange(cong, s);
  return cong;
end);

#

InstallMethod(ViewObj,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  Print("<universal semigroup congruence over ");
  ViewObj(Range(cong));
  Print(">");
end);

#

InstallMethod(\=,
"for two universal semigroup congruences",
[IsUniversalSemigroupCongruence, IsUniversalSemigroupCongruence],
function(cong1, cong2)
  return(Range(cong1) = Range(cong2));
end);

#

InstallMethod(\=,
"for universal congruence and RZMS congruence by linked triple",
[IsUniversalSemigroupCongruence, IsRZMSCongruenceByLinkedTriple],
function(ucong, cong)
  return false;
end);

#

InstallMethod(\=,
"for RZMS congruence by linked triple and universal congruence",
[IsRZMSCongruenceByLinkedTriple, IsUniversalSemigroupCongruence],
function(cong, ucong)
  return false;
end);

#

InstallMethod(\in,
"for dense list and universal semigroup congruence",
[IsDenseList, IsUniversalSemigroupCongruence],
function(pair, cong)
  return(Size(pair) = 2
          and pair[1] in Range(cong)
          and pair[2] in Range(cong));
end);

#

InstallMethod(ImagesElm,
"for universal semigroup congruence and element",
[IsUniversalSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    Error("Semigroups: ImagesElm: usage,\n",
          "the second argument <elm> must be in <cong>'s semigroup");
    return;
  fi;
  return Elements(Range(cong));
end);

#

InstallMethod(NrCongruenceClasses,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  return 1;
end);

#

InstallMethod(JoinSemigroupCongruences,
"for two universal semigroup congruences",
[IsUniversalSemigroupCongruence, IsUniversalSemigroupCongruence],
function(ucong1, ucong2)
  return ucong1;
end);

#

InstallMethod(JoinSemigroupCongruences,
"for RZMS congruence by linked triple and universal congruence",
[IsRZMSCongruenceByLinkedTriple, IsUniversalSemigroupCongruence],
function(cong, ucong)
  return ucong;
end);

#

InstallMethod(JoinSemigroupCongruences,
"for universal congruence and RZMS congruence by linked triple",
[IsUniversalSemigroupCongruence, IsRZMSCongruenceByLinkedTriple],
function(ucong, cong)
  return ucong;
end);

#

InstallMethod(MeetSemigroupCongruences,
"for two universal semigroup congruences",
[IsUniversalSemigroupCongruence, IsUniversalSemigroupCongruence],
function(ucong1, ucong2)
  return ucong1;
end);

#

InstallMethod(MeetSemigroupCongruences,
"for RZMS congruence by linked triple and universal congruence",
[IsRZMSCongruenceByLinkedTriple, IsUniversalSemigroupCongruence],
function(cong, ucong)
  return cong;
end);

#

InstallMethod(MeetSemigroupCongruences,
"for universal congruence and RZMS congruence by linked triple",
[IsUniversalSemigroupCongruence, IsRZMSCongruenceByLinkedTriple],
function(ucong, cong)
  return cong;
end);

#

InstallMethod(EquivalenceClasses,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  return [EquivalenceClassOfElement(cong, Representative(Range(cong)))];
end);

#

InstallMethod(EquivalenceClassOfElement,
"for universal semigroup congruence and associative element",
[IsUniversalSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  # Check that the arguments make sense
  if not elm in Range(cong) then
    Error("Semigroups: EquivalenceClassOfElement: usage,\n",
          "the second argument <elm> must be ",
          "in the semigroup of 1st argument <cong>,");
    return;
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

#

InstallMethod(EquivalenceClassOfElementNC,
"for universal semigroup congruence and associative element",
[IsUniversalSemigroupCongruence, IsAssociativeElement],
function(cong, elm)
  local fam, class;
  fam := CollectionsFamily(FamilyObj(elm));
  class := Objectify(NewType(fam, IsUniversalSemigroupCongruenceClass), rec());
  SetParentAttr(class, cong);
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, elm);
  return class;
end);

#

InstallMethod(\in,
"for associative element and universal semigroup congruence class",
[IsAssociativeElement, IsUniversalSemigroupCongruenceClass],
function(elm, class)
  return(elm in Range(ParentAttr(class)));
end);

#

InstallMethod(\*,
"for two universal semigroup congruence classes",
[IsUniversalSemigroupCongruenceClass, IsUniversalSemigroupCongruenceClass],
function(c1, c2)
  if ParentAttr(c1) <> ParentAttr(c2) then
    Error("Semigroups: \*: usage,\n",
          "the args <c1> and <c2> must be over the same congruence");
    return;
  fi;
  return c1;
end);

#

InstallMethod(Size,
"for universal semigroup congruence class",
[IsUniversalSemigroupCongruenceClass],
function(class)
  return 1;
end);

#

InstallMethod(\=,
"for two universal semigroup congruence classes",
[IsUniversalSemigroupCongruenceClass, IsUniversalSemigroupCongruenceClass],
function(c1, c2)
  return(ParentAttr(c1) = ParentAttr(c2));
end);

#

InstallMethod(GeneratingPairsOfMagmaCongruence,
"for universal semigroup congruence",
[IsUniversalSemigroupCongruence],
function(cong)
  local s, it, z, x, m, r, n, colBlocks, rowBlocks, rmscong, pairs, d;
  s := Range(cong);
  if Size(s) = 1 then
    return [];
  fi;
  it := Iterator(s);
  z := MultiplicativeZero(s);
  if z <> fail then
    if IsZeroSimpleSemigroup(s) then
      # Just link zero to any non-zero element
      x := NextIterator(it);
      if x = z then
        return [[z, NextIterator(it)]];
      else
        return [[x, z]];
      fi;
    else
      # Link zero to a representative of each maximal D-class
      return List(MaximalDClasses(s), cl -> [z, Representative(cl)]);
    fi;
  else
    # Use the minimal ideal
    m := MinimalIdeal(s);

    # Use the linked triple
    r := Range(IsomorphismReesMatrixSemigroup(m));
    n := UnderlyingSemigroup(r);
    colBlocks := [[1 .. Size(Matrix(r)[1])]];
    rowBlocks := [[1 .. Size(Matrix(r))]];
    rmscong := RMSCongruenceByLinkedTriple(r, n, colBlocks, rowBlocks);
    cong := SEMIGROUPS_SimpleCongFromRMSCong(m, rmscong);
    pairs := ShallowCopy(GeneratingPairsOfSemigroupCongruence(cong));

    if IsSimpleSemigroup(s) then
      # m = s, so we are done
    else
      # We must relate each maximal D-class to the minimal ideal
      z := GeneratorsOfSemigroupIdeal(m)[1];
      for d in MaximalDClasses(s) do
        Add(pairs, [z, Representative(d)]);
      od;
    fi;
    return pairs;
  fi;
end);
