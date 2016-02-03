###########################################################################
##
#W  froidure-pin.gd
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for accessing the kernel level version of the
# Froidure-Pin algorithm for enumerating arbitrary semigroups.

#  For details see:
#
#  V. Froidure, and J.-E. Pin, Algorithms for computing finite semigroups.
#  Foundations of computational mathematics (Rio de Janeiro, 1997), 112-126,
#  Springer, Berlin,  1997.

# Note that an acting semigroup can have generic data but not the other way
# around.

# SEMIGROUPS.IsCCSemigroup: returns <true> if the argument is a semigroup to
# which we can apply the C++ code.

DeclareProperty("IsSemigroupEnumerator", IsEnumeratorByFunctions);

DeclareOperation("PositionSortedOp",
                 [IsSemigroup and HasGeneratorsOfSemigroup,
                  IsAssociativeElement]);

SEMIGROUPS.IsCCSemigroup := function(S)
  return IsTransformationSemigroup(S)
           or IsPartialPermSemigroup(S)
           or IsBipartitionSemigroup(S)
           or IsBooleanMatSemigroup(S)
           or IsPBRSemigroup(S)
           or IsMatrixOverSemiringSemigroup(S);
end;

# SEMIGROUPS.DegreeOfSemigroup: returns the size of the container required in
# the C++ code by elements of the semigroup.

SEMIGROUPS.DegreeOfSemigroup := function(arg)
  local S, coll;

  S := arg[1];
  if Length(arg) = 1 then
    coll := [Representative(S)];
  elif Length(arg) = 2 then
    coll := arg[2];
  else
    ErrorNoReturn("Semigroups: SEMIGROUPS.DegreeOfSemigroup:\n",
                 "unknown error,");
  fi;

  if IsTransformationSemigroup(S) then
    return Maximum(DegreeOfTransformationSemigroup(S),
                   DegreeOfTransformationCollection(coll));
  elif IsPartialPermSemigroup(S) then
    return Maximum(DegreeOfPartialPermSemigroup(S),
                   CodegreeOfPartialPermSemigroup(S),
                   DegreeOfPartialPermCollection(coll),
                   CodegreeOfPartialPermCollection(coll));
  elif IsMatrixOverSemiringSemigroup(S) then
    return DimensionOfMatrixOverSemiring(Representative(S));
  elif IsBipartitionSemigroup(S) then
    return DegreeOfBipartitionSemigroup(S);
  elif IsPBRSemigroup(S) then
    return DegreeOfPBRSemigroup(S);
  else
    ErrorNoReturn("Semigroups: SEMIGROUPS.DegreeOfSemigroup:\n",
                 "unknown error,");
  fi;
end;

DeclareCategory("IsGenericSemigroupData", IsList);

# TODO remove everything from here down
DeclareAttribute("GenericSemigroupData", IsSemigroup, "mutable");

DeclareOperation("Enumerate", [IsGenericSemigroupData]);
DeclareOperation("Enumerate", [IsGenericSemigroupData, IsCyclotomic]);
DeclareOperation("Enumerate", [IsGenericSemigroupData,
                               IsCyclotomic,
                               IsFunction]);
