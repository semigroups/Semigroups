#############################################################################
##
#W  ideals-properties.gi
#Y  Copyright (C) 2013-14                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


InstallMethod(IsTrivial, "for a semigroup ideal",
[IsSemigroupIdeal],
function(I)
  local gens;
    gens := GeneratorsOfSemigroupIdeal(I);
  return MultiplicativeZero(I) = gens[1] and ForAll(gens, x -> gens[1] = x);
end);
