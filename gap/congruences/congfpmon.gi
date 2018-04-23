############################################################################
##
##  congruences/congfpmon.gi
##  Copyright (C) 2017                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences (left, right, or 2-sided) defined
## by generating pairs on finitely presented monoids.  The approach is, on
## creation, to take an isomorphism to an fp semigroup, and then to call the
## standard methods defined in congpairs.gd/gi to answer any questions about the
## congruence.
##

InstallMethod(SemigroupCongruenceByGeneratingPairs,
"for an fp monoid and a list of generating pairs",
[IsFpMonoid, IsList], RankFilter(IsList and IsEmpty),
function(M, genpairs)
  local pair, iso, semipairs, semicong, fam, cong;
  # Check that the pairs are all lists of length 2
  for pair in genpairs do
    if not IsList(pair) or Length(pair) <> 2 then
      ErrorNoReturn("Semigroups: SemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of length 2,");
    elif not pair[1] in M or not pair[2] in M then
      ErrorNoReturn("Semigroups: SemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of elements of <M>,");
    fi;
  od;

  # Make an isomorphism to an fp semigroup
  iso := IsomorphismSemigroup(IsFpSemigroup, M);
  semipairs := List(genpairs, p -> [p[1] ^ iso, p[2] ^ iso]);
  semicong := SemigroupCongruenceByGeneratingPairs(Range(iso), semipairs);

  # Create the Object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(M)),
                               ElementsFamily(FamilyObj(M)));
  cong := Objectify(NewType(fam, IsFpMonoidCongruence
                                 and IsSemigroupCongruence),
                    rec(iso := iso, semicong := semicong));
  SetSource(cong, M);
  SetRange(cong, M);
  SetGeneratingPairsOfMagmaCongruence(cong, genpairs);
  return cong;
end);

InstallMethod(LeftSemigroupCongruenceByGeneratingPairs,
"for an fp monoid and a list of generating pairs",
[IsFpMonoid, IsList], RankFilter(IsList and IsEmpty),
function(M, genpairs)
  local pair, iso, semipairs, semicong, fam, cong;
  # Check that the pairs are all lists of length 2
  for pair in genpairs do
    if not IsList(pair) or Length(pair) <> 2 then
      ErrorNoReturn("Semigroups: LeftSemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of length 2,");
    elif not pair[1] in M or not pair[2] in M then
      ErrorNoReturn("Semigroups: LeftSemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of elements of <M>,");
    fi;
  od;

  # Make an isomorphism to an fp semigroup
  iso := IsomorphismSemigroup(IsFpSemigroup, M);
  semipairs := List(genpairs, p -> [p[1] ^ iso, p[2] ^ iso]);
  semicong := LeftSemigroupCongruenceByGeneratingPairs(Range(iso), semipairs);

  # Create the Object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(M)),
                               ElementsFamily(FamilyObj(M)));
  cong := Objectify(NewType(fam, IsFpMonoidCongruence
                                 and IsLeftSemigroupCongruence),
                    rec(iso := iso, semicong := semicong));
  SetSource(cong, M);
  SetRange(cong, M);
  SetGeneratingPairsOfLeftMagmaCongruence(cong, genpairs);
  return cong;
end);

InstallMethod(RightSemigroupCongruenceByGeneratingPairs,
"for an fp monoid and a list of generating pairs",
[IsFpMonoid, IsList], RankFilter(IsList and IsEmpty),
function(M, genpairs)
  local pair, iso, semipairs, semicong, fam, cong;
  # Check that the pairs are all lists of length 2
  for pair in genpairs do
    if not IsList(pair) or Length(pair) <> 2 then
      ErrorNoReturn("Semigroups: RightSemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of length 2,");
    elif not pair[1] in M or not pair[2] in M then
      ErrorNoReturn("Semigroups: RightSemigroupCongruenceByGeneratingPairs: ",
                    "usage,\n<pairs> must all be lists of elements of <M>,");
    fi;
  od;

  # Make an isomorphism to an fp semigroup
  iso := IsomorphismSemigroup(IsFpSemigroup, M);
  semipairs := List(genpairs, p -> [p[1] ^ iso, p[2] ^ iso]);
  semicong := RightSemigroupCongruenceByGeneratingPairs(Range(iso), semipairs);

  # Create the Object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(M)),
                               ElementsFamily(FamilyObj(M)));
  cong := Objectify(NewType(fam, IsFpMonoidCongruence
                                 and IsRightSemigroupCongruence),
                    rec(iso := iso, semicong := semicong));
  SetSource(cong, M);
  SetRange(cong, M);
  SetGeneratingPairsOfRightMagmaCongruence(cong, genpairs);
  return cong;
end);

InstallMethod(\=,
"for two fp monoid congruences",
[IsFpMonoidCongruence, IsFpMonoidCongruence],
function(cong1, cong2)
  return Range(cong1) = Range(cong2) and
         cong1!.semicong = cong2!.semicong;
end);

InstallMethod(JoinSemigroupCongruences,
"for two 2-sided fp monoid congruences",
[IsFpMonoidCongruence and IsSemigroupCongruence,
 IsFpMonoidCongruence and IsSemigroupCongruence],
function(cong1, cong2)
  local M, iso, join;
  M := Range(cong1);
  iso := cong1!.iso;
  if M <> Range(cong2) or iso <> cong2!.iso then
    ErrorNoReturn("Semigroups: JoinSemigroupCongruences: usage,\n",
                  "<cong1> and <cong2> must be over the same semigroup,");
  fi;
  join := JoinSemigroupCongruences(cong1!.semicong, cong2!.semicong);
  return SEMIGROUPS.FpMonCongFromFpSemiCong(M, iso, join);
end);

InstallMethod(JoinLeftSemigroupCongruences,
"for two left fp monoid congruences",
[IsFpMonoidCongruence and IsLeftSemigroupCongruence,
 IsFpMonoidCongruence and IsLeftSemigroupCongruence],
function(cong1, cong2)
  local M, iso, join;
  M := Range(cong1);
  iso := cong1!.iso;
  if M <> Range(cong2) or iso <> cong2!.iso then
    ErrorNoReturn("Semigroups: JoinLeftSemigroupCongruences: usage,\n",
                  "<cong1> and <cong2> must be over the same semigroup,");
  fi;
  join := JoinLeftSemigroupCongruences(cong1!.semicong, cong2!.semicong);
  return SEMIGROUPS.FpMonCongFromFpSemiCong(M, iso, join);
end);

InstallMethod(JoinRightSemigroupCongruences,
"for two right fp monoid congruences",
[IsFpMonoidCongruence and IsRightSemigroupCongruence,
 IsFpMonoidCongruence and IsRightSemigroupCongruence],
function(cong1, cong2)
  local M, iso, join;
  M := Range(cong1);
  iso := cong1!.iso;
  if M <> Range(cong2) or iso <> cong2!.iso then
    ErrorNoReturn("Semigroups: JoinRightSemigroupCongruences: usage,\n",
                  "<cong1> and <cong2> must be over the same semigroup,");
  fi;
  join := JoinRightSemigroupCongruences(cong1!.semicong, cong2!.semicong);
  return SEMIGROUPS.FpMonCongFromFpSemiCong(M, iso, join);
end);

InstallMethod(CongruenceTestMembershipNC,
"for fp monoid congruence and two multiplicative elements",
[IsFpMonoidCongruence, IsMultiplicativeElement, IsMultiplicativeElement],
function(cong, elm1, elm2)
  return [elm1 ^ cong!.iso, elm2 ^ cong!.iso] in cong!.semicong;
end);

InstallMethod(ImagesElm,
"for an fp monoid congruence and a multiplicative element",
[IsFpMonoidCongruence, IsMultiplicativeElement],
function(cong, elm)
  return List(ImagesElm(cong!.semicong, elm ^ cong!.iso),
              x -> x ^ InverseGeneralMapping(cong!.iso));
end);

InstallMethod(EquivalenceClasses,
"for an fp monoid congruence",
[IsFpMonoidCongruence],
function(cong)
  return List(EquivalenceClasses(cong!.semicong),
              c -> SEMIGROUPS.FpMonClassFromFpSemiClass(cong, c));
end);

InstallMethod(EquivalenceClassOfElement,
"for an fp monoid congruence and multiplicative element",
[IsFpMonoidCongruence, IsMultiplicativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    ErrorNoReturn("Semigroups: EquivalenceClassOfElement: usage,\n",
                  "<elm> must be an element of the range of <cong>,");
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

InstallMethod(EquivalenceClassOfElementNC,
"for an fp monoid congruence and multiplicative element",
[IsFpMonoidCongruence, IsMultiplicativeElement],
function(cong, elm)
  local class;
  class := EquivalenceClassOfElementNC(cong!.semicong, elm ^ cong!.iso);
  return SEMIGROUPS.FpMonClassFromFpSemiClass(cong, class);
end);

InstallMethod(NrEquivalenceClasses,
"for an fp monoid congruence",
[IsFpMonoidCongruence],
function(cong)
  return NrEquivalenceClasses(cong!.semicong);
end);

InstallMethod(\in,
"for a multiplicative element and an fp monoid congruence class",
[IsMultiplicativeElement, IsFpMonoidCongruenceClass],
function(elm, class)
  return (elm ^ EquivalenceClassRelation(class)!.iso in class!.semiclass);
end);

InstallMethod(Enumerator,
"for an fp monoid congruence class",
[IsFpMonoidCongruenceClass],
function(class)
  local iso, semienum, conv_out, conv_in;
  iso := EquivalenceClassRelation(class)!.iso;
  semienum := Enumerator(class!.semiclass);
  conv_out := function(enum, elt)
    return elt ^ InverseGeneralMapping(iso);
  end;
  conv_in := function(enum, elt)
    return elt ^ iso;
  end;
  return EnumeratorByEnumerator(class, semienum, conv_out, conv_in, [], rec());
end);

InstallMethod(\*,
"for two fp monoid congruence classes",
[IsFpMonoidCongruenceClass, IsFpMonoidCongruenceClass],
function(c1, c2)
  return SEMIGROUPS.FpMonClassFromFpSemiClass(EquivalenceClassRelation(c1),
                                              c1!.semiclass * c2!.semiclass);
end);

InstallMethod(Size,
"for an fp monoid congruence class",
[IsFpMonoidCongruenceClass],
function(class)
  return Size(class!.semiclass);
end);

InstallMethod(\=,
"for two fp monoid congruence classes",
[IsFpMonoidCongruenceClass, IsFpMonoidCongruenceClass],
function(c1, c2)
  return EquivalenceClassRelation(c1) = EquivalenceClassRelation(c2) and
         c1!.semiclass = c2!.semiclass;
end);

InstallMethod(IsSubrelation,
"for two fp monoid congruences",
[IsFpMonoidCongruence, IsFpMonoidCongruence],
function(cong1, cong2)
  return Range(cong1) = Range(cong2) and
         IsSubrelation(cong1!.semicong, cong2!.semicong);
end);

InstallMethod(EquivalenceRelationLookup,
"for an fp monoid congruence",
[IsFpMonoidCongruence],
function(cong)
  local iso, mon_enum, semi_enum, semilookup;
  iso := cong!.iso;
  mon_enum := AsSet(Range(cong));
  semi_enum := AsSet(Range(iso));
  semilookup := EquivalenceRelationLookup(cong!.semicong);
  return List(mon_enum, elm -> semilookup[Position(semi_enum, elm ^ iso)]);
end);

InstallMethod(EquivalenceRelationPartition,
"for an fp monoid congruence",
[IsFpMonoidCongruence],
function(cong)
  local semi_part, inv_map;
  semi_part := EquivalenceRelationPartition(cong!.semicong);
  inv_map := InverseGeneralMapping(cong!.iso);
  return List(semi_part, block -> List(block, elm -> elm ^ inv_map));
end);
