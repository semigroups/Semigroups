############################################################################
##
#W  congruences/conglattice.gi
#Y  Copyright (C) 2015                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for a lattice of congruences.
##
## When the congruences of a semigroup are computed, they form a lattice with
## respect to containment.  The information about how the congruences lie in
## this lattice is stored in an IsCongruenceLattice object (a positional object
## based on a list) and can be retrieved from this object with the following
## methods
##
## Most of these methods simply redirect to the first entry of the object, the
## actual list of lists of integers which defines the containments in the
## lattice.  Component 2 in the list is the list of congruences, which is hidden
## when this object is used.
##
##

InstallMethod(\[\],
"for a congruence lattice",
[IsCongruenceLattice, IsObject],
function(latt, x)
  return latt![1][x];
end);

InstallMethod(ViewObj,
"for a congruence lattice",
[IsCongruenceLattice],
function(latt)
  ViewObj(latt![1]);
end);

InstallMethod(PrintObj,
"for a congruence lattice",
[IsCongruenceLattice],
function(latt)
  PrintObj(latt![1]);
end);

InstallMethod(Length,
"for a congruence lattice",
[IsCongruenceLattice],
function(latt)
  return Length(latt![1]);
end);

InstallMethod(IsBound\[\],
"for a congruence lattice and a positive integer",
[IsCongruenceLattice, IsPosInt],
function(latt, x)
  return IsBound(latt![1][x]);
end);
