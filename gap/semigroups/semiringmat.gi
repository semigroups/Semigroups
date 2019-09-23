############################################################################
##
##  semigroups/semiringmat.gi
##  Copyright (C) 2015-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for semigroups of matrices over semirings.

InstallMethod(SemigroupViewStringSuffix,
"for a matrix over semiring semigroup with generators",
[IsMatrixOverSemiringSemigroup],
function(S)
  local n, type;
  n    := DimensionOfMatrixOverSemiring(Representative(S));
  type := SEMIGROUPS_TypeViewStringOfMatrixOverSemiring(Representative(S));
  return StringFormatted("{}x{} {} matrices ", n, n, type);
end);

InstallMethod(SemigroupViewStringSuffix,
"for an integer matrix semigroup",
[IsIntegerMatrixSemigroup],
function(S)
  return StringFormatted("{1}x{1} {2} matrices ",
                         NrRows(Representative(S)),
                         "integer");
end);

InstallMethod(SemigroupViewStringSuffix,
"for an integer matrix monoid",
[IsIntegerMatrixMonoid],
function(S)
  return StringFormatted("{1}x{1} {2} matrices ",
                         NrRows(Representative(S)),
                         "integer");
end);
