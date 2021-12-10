############################################################################
##
##  congruences/congsimple.gd
##  Copyright (C) 2015                                   Michael C. Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains methods for congruences on finite (0-)simple semigroups,
## using isomorphisms to Rees (0-)matrix semigroups and methods in
## congruences/reesmat.gd/gi.

DeclareCategory("IsSimpleSemigroupCongruence",
                IsCongruenceCategory and IsAttributeStoringRep and IsFinite);

DeclareCategory("IsSimpleSemigroupCongruenceClass",
                IsAnyCongruenceClass and IsCongruenceClass and
                IsAttributeStoringRep and IsAssociativeElement);

SEMIGROUPS.SimpleCongFromRMSCong := function(S, iso, rmscong)
  local fam, cong;
  # Construct the object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, 
                            IsSimpleSemigroupCongruence),
                    rec(rmscong := rmscong, iso := iso));
  SetSource(cong, S);
  SetRange(cong, S);
  return cong;
end;

SEMIGROUPS.SimpleClassFromRMSclass := function(cong, rmsclass)
  local fam, class;
  fam := FamilyObj(Range(cong));
  class := Objectify(NewType(fam, IsSimpleSemigroupCongruenceClass),
                     rec(rmsclass := rmsclass, iso := cong!.iso));
  SetParentAttr(class, Range(cong));
  SetEquivalenceClassRelation(class, cong);
  SetRepresentative(class, Representative(rmsclass) ^
                           InverseGeneralMapping(cong!.iso));
  return class;
end;
