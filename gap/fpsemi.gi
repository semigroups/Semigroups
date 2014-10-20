#############################################################################
##
#W  fpsemi.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# a different method is required for ideals (or MonoidByAdjoiningIdentity
# requires a method for acting semigroup ideals)

InstallMethod(IsomorphismFpSemigroup, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup], 
function(S)
  FroidurePinExtendedAlg(S);
  return IsomorphismFpSemigroup(S);
end);

#EOF
