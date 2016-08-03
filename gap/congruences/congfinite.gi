############################################################################
##
#W  congruences/congfinite.gi
#Y  Copyright (C) 2016                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for any finite semigroup congruence with
## generating pairs, using the Todd-Coxeter algorithms in Semigroups++.
##
#############################################################################

SEMIGROUPS.cong_make_cpp_cong := function(cong)
  local data, type, genpairs;
  data := GenericSemigroupData(Range(cong));
  if HasGeneratingPairsOfMagmaCongruence(cong) then
    type := "twosided";
    genpairs := GeneratingPairsOfSemigroupCongruence(cong);
  elif HasGeneratingPairsOfRightMagmaCongruence(cong) then
    type := "right";
    genpairs := GeneratingPairsOfRightSemigroupCongruence(cong);
  elif HasGeneratingPairsOfLeftMagmaCongruence(cong) then
    type := "left";
    genpairs := GeneratingPairsOfLeftSemigroupCongruence(cong);
  fi;
  return FINITE_CONG_BY_GEN_PAIRS(type, data, genpairs);
end;

InstallMethod(\in,
Concatenation("for a multiplicative element collection and ",
              "a right semigroup congruence with generating pairs"),
[IsMultiplicativeElementCollection,
 IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(pair, cong)
  local S, pairs;
  S := Range(cong);
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  pairs := GeneratingPairsOfRightSemigroupCongruence(cong);
  pairs := List(pairs, x ->
                [Factorization(S, x[1]), Factorization(S, x[2])]);
  return FINITE_CONG_PAIR_IN("right",
                             GenericSemigroupData(S),
                             pairs,
                             MinimalFactorization(S, pair[1]),
                             MinimalFactorization(S, pair[2]));
end);

InstallMethod(NrEquivalenceClasses,
"for a right semigroup congruence with generating pairs",
[IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(cong)
  local S, pairs;
  S := Range(cong);
  if not IsFinite(S) then
    TryNextMethod();
  fi;

  pairs := GeneratingPairsOfRightSemigroupCongruence(cong);
  pairs := List(pairs, x ->
           [Factorization(S, x[1]), Factorization(S, x[2])]);
  return FINITE_CONG_NR_CLASSES("right", GenericSemigroupData(S), pairs);
end);

InstallMethod(NrEquivalenceClasses,
"for a left semigroup congruence with generating pairs",
[IsLeftSemigroupCongruence and HasGeneratingPairsOfLeftMagmaCongruence],
function(cong)
  local S, pairs;

  S := Range(cong);
  if not IsFinite(S) then
    TryNextMethod();
  fi;

  pairs := GeneratingPairsOfLeftSemigroupCongruence(cong);
  pairs := List(pairs, x ->
           [Factorization(S, x[1]), Factorization(S, x[2])]);
  return FINITE_CONG_NR_CLASSES("left", GenericSemigroupData(S), pairs);
end);

InstallMethod(NrEquivalenceClasses,
"for a semigroup congruence with generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong)
  local S, pairs;

  S := Range(cong);
  if not IsFinite(S) then
    TryNextMethod();
  fi;

  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  pairs := List(pairs, x ->
           [Factorization(S, x[1]), Factorization(S, x[2])]);
  return FINITE_CONG_NR_CLASSES("twosided", GenericSemigroupData(S), pairs);
end);
