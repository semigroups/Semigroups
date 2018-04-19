############################################################################
##
##  congruences/congfpmon.gd
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

DeclareCategory("IsFpMonoidCongruence",
                IsEquivalenceRelation and IsAttributeStoringRep,
                RankFilter(IsSemigroupCongruence));

DeclareCategory("IsFpMonoidCongruenceClass",
                IsEquivalenceClass and IsAttributeStoringRep and
                IsAssociativeElement,
                RankFilter(IsCongruenceClass));

SEMIGROUPS.FpMonCongFromFpSemiCong := function(M, iso, semicong)
  local filt, set_pairs, inv_map, pairs, fam, cong;
  if semicong!.type = "twosided" then
    filt := IsSemigroupCongruence;
    set_pairs := SetGeneratingPairsOfMagmaCongruence;
  elif semicong!.type = "left" then
    filt := IsLeftSemigroupCongruence;
    set_pairs := SetGeneratingPairsOfLeftMagmaCongruence;
  else  # semicong!.type = "right"
    filt := IsRightSemigroupCongruence;
    set_pairs := SetGeneratingPairsOfRightMagmaCongruence;
  fi;

  inv_map := InverseGeneralMapping(iso);
  pairs := List(semicong!.genpairs,
                pair -> [pair[1] ^ inv_map, pair[2] ^ inv_map]);

  # Create the Object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(M)),
                               ElementsFamily(FamilyObj(M)));
  cong := Objectify(NewType(fam, IsFpMonoidCongruence and filt),
                    rec(iso := iso, semicong := semicong));
  SetSource(cong, M);
  SetRange(cong, M);
  set_pairs(cong, pairs);
  return cong;
end;

SEMIGROUPS.FpMonClassFromFpSemiClass := function(cong, semiclass)
  local filt, fam, class;
  if IsCongruenceClass(semiclass) then
    filt := IsCongruenceClass;
  elif IsLeftCongruenceClass(semiclass) then
    filt := IsLeftCongruenceClass;
  else
    filt := IsRightCongruenceClass;
  fi;

  fam := FamilyObj(Range(cong));
  class := Objectify(NewType(fam, IsFpMonoidCongruenceClass and filt),
                     rec(semiclass := semiclass, iso := cong!.iso));
  SetParentAttr(class, Range(cong));
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, Representative(semiclass) ^
                           InverseGeneralMapping(cong!.iso));
  return class;
end;
