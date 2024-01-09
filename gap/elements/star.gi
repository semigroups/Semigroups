#############################################################################
##
##  elements/star.gi
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(Star, "for an associative element with star",
[IsAssociativeElementWithStar],
function(elm)
  elm := StarOp(elm);
  MakeImmutable(elm);
  return elm;
end);

InstallMethod(IsStarSemigroup, "for a semigroup of elements with star",
[IsSemigroup and IsAssociativeElementWithStarCollection],
S -> ForAll(GeneratorsOfSemigroup(S), x -> Star(x) in S));
