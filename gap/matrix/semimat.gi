############################################################################
##
#W  semimat.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##                                                         Markus Pfeiffer
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##


InstallMethod(OneMutable, "for ring element coll coll coll",
[IsRingElementCollCollColl], x-> One(Representative(x)));

InstallMethod(IsGroupAsSemigroup, "for a matrix semigroup",
[IsMatrixSemigroup],
s-> IsGroupAsSemigroup(Range(IsomorphismTransformationSemigroup(s))));

