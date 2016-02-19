#############################################################################
##
#W  obsolete.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

SEMIGROUPS.PrintObsolete := function(old, new)
  Print("#I  `", old, "` is no longer supported\n",
        "#I  use `", new, "` instead!\n");
end;

InstallMethod(RandomTransformationSemigroup, "for pos ints",
[IsPosInt, IsPosInt],
function(nrgens, deg)
  SEMIGROUPS.PrintObsolete("RandomTransformationSemigroup", "RandomSemigroup");
  return RandomSemigroup(IsTransformationSemigroup, nrgens, deg);
end);

InstallMethod(RandomPartialPermSemigroup, "for pos ints",
[IsPosInt, IsPosInt],
function(nrgens, deg)
  SEMIGROUPS.PrintObsolete("RandomTransformationSemigroup", "RandomSemigroup");
  return RandomSemigroup(IsPartialPermSemigroup, nrgens, deg);
end);

InstallMethod(DotDClasses, "for a semigroup",
[IsSemigroup],
function(S)
  SEMIGROUPS.PrintObsolete("DotDClasses", "DotString");
  return DotString(S);
end);

InstallMethod(PartialTransformationSemigroup, "for a positive integer",
[IsPosInt],
function(n)
  SEMIGROUPS.PrintObsolete("PartialTransformationSemigroup",
                           "PartialTransformationMonoid");
  return PartialTransformationMonoid(n);
end);
