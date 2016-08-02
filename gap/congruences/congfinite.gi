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

InstallMethod(NrEquivalenceClasses,
"for a right semigroup congruence with generating pairs",
[IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(cong)
  local S, pairs, extra;
  S := Range(cong);
  if not IsFinite(S) then
    TryNextMethod();
  fi;

  pairs := GeneratingPairsOfRightSemigroupCongruence(cong);
  extra := List(pairs, x ->
                [Factorization(S, x[1]), Factorization(S, x[2])]);
  return FINITE_CONG_NR_CLASSES(GenericSemigroupData(S), extra);
end);

InstallMethod(NrEquivalenceClasses,
"for a semigroup congruence with generating pairs",
[IsSemigroupCongruence and HasGeneratingPairsOfMagmaCongruence],
function(cong)
  local S, pairs, extra;
  S := Range(cong);
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  extra := List(pairs, x ->
                [Factorization(S, x[1]), Factorization(S, x[2])]);
  return FINITE_CONG_NR_CLASSES(GenericSemigroupData(S), extra);
end);

InstallMethod(\in,
Concatenation("for a multiplicative element collection and ",
              "a right semigroup congruence with generating pairs"),
[IsMultiplicativeElementCollection,
 IsRightSemigroupCongruence and HasGeneratingPairsOfRightMagmaCongruence],
function(pair, cong)
  S := Range(cong);
  if not IsFinite(S) then
    TryNextMethod();
  fi;
  pairs := GeneratingPairsOfSemigroupCongruence(cong);
  extra := List(pairs, x ->
                [Factorization(S, x[1]), Factorization(S, x[2])]);
  return FINITE_CONG_PAIR_IN(GenericSemigroupData(S),
                             extra,
                             MinimalFactorization(S, x),
                             MinimalFactorization(S, y));
end);
