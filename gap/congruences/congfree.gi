############################################################################
##
#W  congruences/congfree.gi
#Y  Copyright (C) 2016                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on free semigroups and free
## monoids, using methods which create the corresponding finitely presented
## semigroup and calling methods on those.  This is not an ideal way of treating
## such congruences, but since an FP semigroup or monoid in GAP is not treated
## simply as a quotient semigroup, and its elements are not regarded as
## congruence classes, it is not possible to implement congruences in the usual
## way.
##

# Not implemented because they might be infinite:
# ImagesElm, Enumerator/Size (class)
#
# Not implemented because they should use other methods:
# ViewObj, PrintObj, \* (classes), EquivalenceRelationLookup,
# EquivalenceRelationCanonicalLookup

InstallMethod(SemigroupCongruenceByGeneratingPairs,
"for a free semigroup and a list of generating pairs", IsElmsColls,
[IsFreeSemigroup, IsList],
function(F, genpairs)
  local pair, fam, fpsemi, epi, cong;
  # Check input
  for pair in genpairs do
    if not IsList(pair) or Length(pair) <> 2 then
      ErrorNoReturn("Semigroups: SemigroupCongruenceByGeneratingPairs: usage,",
                    "\n<pairs> must all be lists of length 2,");
    elif not pair[1] in F or not pair[2] in F then
      ErrorNoReturn("Semigroups: SemigroupCongruenceByGeneratingPairs: usage,",
                    "\n<pairs> must all be lists of elements of <F>,");
    fi;
  od;

  # Construct the object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(F)),
                               ElementsFamily(FamilyObj(F)));
  fpsemi := FactorFreeSemigroupByRelations(F, genpairs);
  fam := FamilyObj(fpsemi.1);
  epi := w -> ElementOfFpSemigroup(fam, w);  # epimorphism from F to fpsemi
  cong := Objectify(NewType(fam, IsFreeCongruence),
                    rec(fpsemi := fpsemi, epi := epi));
  SetSource(cong, F);
  SetRange(cong, F);
  SetGeneratingPairsOfMagmaCongruence(cong, Immutable(genpairs));
  return cong;
end);

InstallMethod(SemigroupCongruenceByGeneratingPairs,
"for a free monoid and a list of generating pairs", IsElmsColls,
[IsFreeMonoid, IsList],
function(F, genpairs)
  local pair, fam, fpsemi, epi, cong;
  # Check input
  for pair in genpairs do
    if not IsList(pair) or Length(pair) <> 2 then
      ErrorNoReturn("Semigroups: SemigroupCongruenceByGeneratingPairs: usage,",
                    "\n<pairs> must all be lists of length 2,");
    elif not pair[1] in F or not pair[2] in F then
      ErrorNoReturn("Semigroups: SemigroupCongruenceByGeneratingPairs: usage,",
                    "\n<pairs> must all be lists of elements of <F>,");
    fi;
  od;

  # Construct the object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(F)),
                               ElementsFamily(FamilyObj(F)));
  fpsemi := FactorFreeMonoidByRelations(F, genpairs);
  fam := FamilyObj(fpsemi.1);
  epi := w -> ElementOfFpMonoid(fam, w);  # epimorphism from F to fpsemi
  cong := Objectify(NewType(fam, IsFreeCongruence),
                    rec(fpsemi := fpsemi, epi := epi));
  SetSource(cong, F);
  SetRange(cong, F);
  SetGeneratingPairsOfMagmaCongruence(cong, Immutable(genpairs));
  return cong;
end);

InstallMethod(\in,
"for a multiplicative element collection and a free congruence",
[IsMultiplicativeElementCollection, IsFreeCongruence],
function(pair, cong)
  if Size(pair) <> 2 then
    ErrorNoReturn("Semigroups: \\in (for a congruence): usage,\n",
                  "the first arg <pair> must be a list of length 2,");
  elif not (pair[1] in Range(cong) and pair[2] in Range(cong)) then
    ErrorNoReturn("Semigroups: \\in (for a congruence): usage,\n",
                  "elements of the first arg <pair> must be\n",
                  "in the range of the second arg <cong>,");
  elif CanEasilyCompareElements(pair[1]) and pair[1] = pair[2] then
    return true;
  fi;
  return cong!.epi(pair[1]) = cong!.epi(pair[2]);
end);

InstallMethod(FreeCongruenceClassType,
"for a free congruence",
[IsFreeCongruence],
function(cong)
  return NewType(FamilyObj(Range(cong)), IsFreeCongruenceClass);
end);

InstallMethod(EquivalenceClasses,
"for a free congruence",
[IsFreeCongruence],
function(cong)
  return List(AsList(cong!.fpsemi),
              x -> EquivalenceClassOfElementNC(cong, UnderlyingElement(x)));
end);

InstallMethod(EquivalenceClassOfElement,
"for a free congruence and multiplicative element",
[IsFreeCongruence, IsMultiplicativeElement],
function(cong, elm)
  if not elm in Range(cong) then
    ErrorNoReturn("Semigroups: EquivalenceClassOfElement: usage,\n",
                  "the second arg <elm> must be in the ",
                  "semigroup of the first arg <cong>,");
  fi;
  return EquivalenceClassOfElementNC(cong, elm);
end);

InstallMethod(EquivalenceClassOfElementNC,
"for a free congruence and multiplicative element",
[IsFreeCongruence, IsMultiplicativeElement],
function(cong, elm)
  local fpelm, class;
  fpelm := cong!.epi(elm);
  class := Objectify(FreeCongruenceClassType(cong),
                     rec(fpelm := fpelm));
  SetParentAttr(class, Range(cong));
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, UnderlyingElement(fpelm));
  return class;
end);

InstallMethod(NrEquivalenceClasses,
"for a free congruence",
[IsFreeCongruence],
function(cong)
  return Size(cong!.fpsemi);
end);

InstallMethod(\in,
"for an multiplicative element and a free congruence class",
[IsMultiplicativeElement, IsFreeCongruenceClass],
function(elm, class)
  local cong;
  cong := EquivalenceClassRelation(class);
  if not elm in Range(cong) then
    return false;
  fi;
  return cong!.epi(elm) = class!.fpelm;
end);

InstallMethod(\=,
"for two free congruence classes",
[IsFreeCongruenceClass, IsFreeCongruenceClass],
function(class1, class2)
  return EquivalenceClassRelation(class1) = EquivalenceClassRelation(class2)
         and class1!.fpelm = class2!.fpelm;
end);

InstallMethod(IsSubrelation,
"for two free congruences",
[IsFreeCongruence, IsFreeCongruence],
function(cong1, cong2)
  # Check semigroup
  if Range(cong1) <> Range(cong2) then
    ErrorNoReturn("Semigroups: IsSubrelation: usage,\n",
                  "congruences must be defined over the same semigroup,");
  fi;

  return ForAll(GeneratingPairsOfSemigroupCongruence(cong2),
                pair -> pair in cong1);
end);
