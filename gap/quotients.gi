#############################################################################
##
#W  quotients.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(GeneratorsOfSemigroup, "for a quotient semigroup",
[IsQuotientSemigroup],
function(S)
  local T;
  T := QuotientSemigroupPreimage(S);
  return DuplicateFreeList(Images(QuotientSemigroupHomomorphism(S),
                                  GeneratorsOfSemigroup(T)));
end);

InstallMethod(\*, "for associative coll coll and congruence class",
[IsAssociativeElementCollColl, IsCongruenceClass],
function(list, nonlist)
  if ForAll(list, IsCongruenceClass) then
    return PROD_LIST_SCL_DEFAULT(list, nonlist);
  fi;
  TryNextMethod();
end);

InstallMethod(\*, "for congruence class and associative coll coll",
[IsCongruenceClass, IsAssociativeElementCollColl],
function(nonlist, list)
  if ForAll(list, IsCongruenceClass) then
    return PROD_SCL_LIST_DEFAULT(nonlist, list);
  fi;
  TryNextMethod();
end);

InstallMethod(\/, "for a semigroup and an ideal",
[IsSemigroup, IsSemigroupIdeal],
function(S, I)
  return S / ReesCongruenceOfSemigroupIdeal(I);
end);
