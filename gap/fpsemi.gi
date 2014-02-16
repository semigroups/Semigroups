#############################################################################
##
#W  fpsemi.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(IsomorphismFpSemigroup, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 
function(S)
  FroidurePinExtendedAlg(S);
  return IsomorphismFpSemigroup(S);
end);

#EOF
