############################################################################
##
##  congruences/congsimple.gd
##  Copyright (C) 2015                                   Michael C. Torpey
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

DeclareCategory("IsSimpleSemigroupCongruence",
                IsSemigroupCongruence and IsAttributeStoringRep and IsFinite);

DeclareCategory("IsSimpleSemigroupCongruenceClass",
                IsCongruenceClass and IsAttributeStoringRep and
                IsAssociativeElement);

SEMIGROUPS.SimpleCongFromPairs :=
function(S, pairs)
  local iso, r, rmspairs, pcong, rmscong, cong;

  # If s is a RMS/RZMS, then just create the linked triple congruence
  if IsReesMatrixSemigroup(S) then
    cong := AsRMSCongruenceByLinkedTriple(
            SemigroupCongruenceByGeneratingPairs(S, pairs));
  elif IsReesZeroMatrixSemigroup(S) then
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
    else  # elif IsReesZeroMatrixSemigroup(r) then
      rmscong := AsRZMSCongruenceByLinkedTriple(pcong);
    fi;
    # Special case for the universal congruence
    if IsUniversalSemigroupCongruence(rmscong) then
      cong := UniversalSemigroupCongruence(S);
    else
      cong := SEMIGROUPS.SimpleCongFromRMSCong(S, iso, rmscong);
    fi;
  fi;
  SetGeneratingPairsOfMagmaCongruence(cong, pairs);
  return cong;
end;

#

SEMIGROUPS.SimpleCongFromRMSCong := function(S, iso, rmscong)
  local fam, cong;
  # Construct the object
  fam := GeneralMappingsFamily(ElementsFamily(FamilyObj(S)),
                               ElementsFamily(FamilyObj(S)));
  cong := Objectify(NewType(fam, IsSimpleSemigroupCongruence),
                    rec(rmscong := rmscong, iso := iso));
  SetSource(cong, S);
  SetRange(cong, S);
  return cong;
end;

#

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
