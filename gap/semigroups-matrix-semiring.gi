############################################################################
##
#W  semisemiringmat.gi
#Y  Copyright (C) 2015                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for semigroups of boolean matrices.

InstallMethod(ViewString, "for a matrix over semiring semigroup with generators",
[IsMatrixOverSemiringSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local str, nrgens, n;

  str := "\><";

  if HasIsTrivial(S) and IsTrivial(S) then
    Append(str, "\>trivial\< ");
  else
    if HasIsCommutative(S) and IsCommutative(S) then
      Append(str, "\>commutative\< ");
    fi;
  fi;
  if not IsGroup(S) then
    if (HasIsTrivial(S) and IsTrivial(S)) or IsGroup(S) then
    elif HasIsZeroSimpleSemigroup(S) and IsZeroSimpleSemigroup(S) then
      Append(str, "\>0-simple\< ");
    elif HasIsSimpleSemigroup(S) and IsSimpleSemigroup(S) then
      Append(str, "\>simple\< ");
    fi;

    if HasIsInverseSemigroup(S) and IsInverseSemigroup(S) then
      Append(str, "\>inverse\< ");
    elif HasIsRegularSemigroup(S)
        and not (HasIsSimpleSemigroup(S) and IsSimpleSemigroup(S)) then
      if IsRegularSemigroup(S) then
        Append(str, "\>regular\< ");
      else
        Append(str, "\>non-regular\< ");
      fi;
    fi;
  fi;

  if HasIsMonoid(S) and IsMonoid(S) then
    Append(str, "monoid ");
    if HasIsInverseSemigroup(S) and IsInverseSemigroup(S) then
      nrgens := Length(GeneratorsOfInverseMonoid(S));
    else
      nrgens := Length(GeneratorsOfMonoid(S));
    fi;
  else
    Append(str, "semigroup ");
    if HasIsInverseSemigroup(S) and IsInverseSemigroup(S) then
      nrgens := Length(GeneratorsOfInverseSemigroup(S));
    else
      nrgens := Length(GeneratorsOfSemigroup(S));
    fi;
  fi;

  n := String(DimensionOfMatrixOverSemiring(Representative(S)));
  Append(str, "of ");
  Append(str, Concatenation(n, "x", n));
  Append(str, " ");
  Append(str, TypeViewStringOfMatrixOverSemiring(Representative(S)));
  Append(str, " matrices with ");

  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens > 1 or nrgens = 0 then
    Append(str, "s\<");
  else
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);
