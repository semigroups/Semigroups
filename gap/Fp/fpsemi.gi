#############################################################################
##
#W  fpsemi.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(AsFpSemigroup, "for a semigroup",
[IsSemigroup], S -> Range(IsomorphismFpSemigroup(S)));

InstallMethod(AsFpMonoid, "for a monoid",
[IsMonoid], S -> Range(IsomorphismFpMonoid(S)));
