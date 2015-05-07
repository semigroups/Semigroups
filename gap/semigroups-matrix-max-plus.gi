############################################################################
##
#W  semigroups-matrix-max-plus.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains implementations for semigroups of max-plus, min-plus,
# tropical max-plus, and tropical min-plus matrices.

InstallMethod(RandomMaxPlusMatrixSemigroup, "for a pos ints",
[IsPosInt, IsPosInt],
function(nrgens, dim)
  return Semigroup(Set([1 .. nrgens], x -> RandomMaxPlusMatrix(dim)));
end);

InstallMethod(RandomMaxPlusMatrixMonoid, "for a pos ints",
[IsPosInt, IsPosInt],
function(nrgens, dim)
  return Monoid(Set([1 .. nrgens], x -> RandomMaxPlusMatrix(dim)));
end);

InstallMethod(RandomMinPlusMatrixSemigroup, "for a pos ints",
[IsPosInt, IsPosInt],
function(nrgens, dim)
  return Semigroup(Set([1 .. nrgens], x -> RandomMinPlusMatrix(dim)));
end);

InstallMethod(RandomMinPlusMatrixMonoid, "for a pos ints",
[IsPosInt, IsPosInt],
function(nrgens, dim)
  return Monoid(Set([1 .. nrgens], x -> RandomMinPlusMatrix(dim)));
end);

InstallMethod(RandomTropicalMaxPlusMatrixSemigroup, "for a pos ints",
[IsPosInt, IsPosInt, IsPosInt],
function(nrgens, dim, threshold)
  return Semigroup(Set([1 .. nrgens], 
                   x -> RandomTropicalMaxPlusMatrix(dim, threshold)));
end);

InstallMethod(RandomTropicalMaxPlusMatrixMonoid, "for a pos ints",
[IsPosInt, IsPosInt, IsPosInt],
function(nrgens, dim, threshold)
  return Monoid(Set([1 .. nrgens], 
                x -> RandomTropicalMaxPlusMatrix(dim, threshold)));
end);

InstallMethod(RandomTropicalMinPlusMatrixSemigroup, "for a pos ints",
[IsPosInt, IsPosInt, IsPosInt],
function(nrgens, dim, threshold)
  return Semigroup(Set([1 .. nrgens], 
                   x -> RandomTropicalMinPlusMatrix(dim, threshold)));
end);

InstallMethod(RandomTropicalMinPlusMatrixMonoid, "for a pos ints",
[IsPosInt, IsPosInt, IsPosInt],
function(nrgens, dim, threshold)
  return Monoid(Set([1 .. nrgens], 
                x -> RandomTropicalMinPlusMatrix(dim, threshold)));
end);
