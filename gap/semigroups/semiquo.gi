#############################################################################
##
##  semiquo.gi
##  Copyright (C) 2014-16                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(ViewObj, "for a quotient semigroup",
[IsQuotientSemigroup],
function(S)
  Print("<quotient of ");
  ViewObj(QuotientSemigroupCongruence(S));
  Print(">");
end);

InstallMethod(OneImmutable, "for a quotient semigroup",
[IsQuotientSemigroup],
function(S)
  return One(QuotientSemigroupPreimage(S)) ^ QuotientSemigroupHomomorphism(S);
end);

InstallMethod(GeneratorsOfSemigroup, "for a quotient semigroup",
[IsQuotientSemigroup],
function(S)
  local T;
  T := QuotientSemigroupPreimage(S);
  return DuplicateFreeList(Images(QuotientSemigroupHomomorphism(S),
                                  GeneratorsOfSemigroup(T)));
end);

InstallMethod(\*, "for multiplicative coll coll and congruence class",
[IsMultiplicativeElementCollColl, IsCongruenceClass],
function(list, nonlist)
  if ForAll(list, IsCongruenceClass) then
    return PROD_LIST_SCL_DEFAULT(list, nonlist);
  fi;
  TryNextMethod();
end);

InstallMethod(\*, "for congruence class and multiplicative coll coll",
[IsCongruenceClass, IsMultiplicativeElementCollColl],
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

InstallMethod(Size, "for a quotient semigroup",
[IsQuotientSemigroup and IsFinite],
function(q)
  local cong;
  cong := QuotientSemigroupCongruence(q);
  return NrCongruenceClasses(cong);
end);
