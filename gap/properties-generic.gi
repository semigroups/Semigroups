###############################################################################
##
#W  properties-generic.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# same method for ideals

InstallMethod(IsBand, "for a semigroup",
[IsSemigroup],
function(S)
  if HasParent(S) and HasIsBand(Parent(S)) and IsBand(Parent(S)) then
    return true;
  else
    return IsCompletelyRegularSemigroup(S) and IsHTrivial(S);
  fi;
end);

# same method for ideals

InstallMethod(IsBand, "for an inverse semigroup", [IsInverseSemigroup],
IsSemilatticeAsSemigroup);

# same method for ideals

InstallMethod(IsBlockGroup, "for a finite semigroup", 
[IsSemigroup and IsFinite],
function(S)
  local iter, D;

  if HasParent(S) and HasIsBlockGroup(Parent(S))
    and IsBlockGroup(Parent(S)) then
    return true;
  elif HasIsInverseSemigroup(S) and IsInverseSemigroup(S) then
    Info(InfoSemigroups, 2, "inverse semigroup");
    return true;
  elif (HasIsRegularSemigroup(S) and IsRegularSemigroup(S)) and
   (HasIsInverseSemigroup(S) and not IsInverseSemigroup(S)) then
    Info(InfoSemigroups, 2, "regular but non-inverse semigroup");
    return false;
  fi;

  iter := IteratorOfDClasses(S);

  for D in iter do
    if IsRegularDClass(D) and
       (ForAny(RClasses(D), x -> NrIdempotents(x) > 1)
        or NrRClasses(D) <> NrLClasses(D)) then
      return false;
    fi;
  od;
  return true;
end);

#

InstallMethod(IsGroupAsSemigroup, "for a semigroup",
[IsSemigroup],
function(S)
  if HasParent(S) and HasIsGroupAsSemigroup(Parent(S))
    and IsGroupAsSemigroup(Parent(S)) then
    return true;
  fi;
  return NrRClasses(S) = 1 and NrLClasses(S) = 1;
end);

