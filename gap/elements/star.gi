#############################################################################
##
##  star.gi
##  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# star

InstallMethod(Star, "for an associative element with star",
[IsAssociativeElementWithStar],
function(elm)
  elm := StarOp(elm);
  MakeImmutable(elm);
  return elm;
end);

InstallMethod(IsStarSemigroup, "for a semigroup of elements with star",
[IsSemigroup and IsAssociativeElementWithStarCollection],
function(S)
  return ForAll(GeneratorsOfSemigroup(S), x -> Star(x) in S);
end);
