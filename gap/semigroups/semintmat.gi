############################################################################
##
##  semigroups/semintmat.gi
##  Copyright (C) 2022                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Note that IsIntegerMatrixSemigroup does not imply
# IsMatrixOverSemiringSemigroup and so we require this method too.
InstallMethod(SemigroupViewStringSuffix,
"for an integer matrix semigroup",
[IsIntegerMatrixSemigroup],
function(S)
  return StringFormatted("{1}x{1} {2} matrices ",
                         NrRows(Representative(S)),
                         "integer");
end);

InstallMethod(IsomorphismSemigroup,
"for IsIntegerMatrixSemigroup and a transf. semigroup with generators",
[IsIntegerMatrixSemigroup,
 IsTransformationSemigroup and HasGeneratorsOfSemigroup],
1,  # to beat the default method
function(_, S)
  local n, map, T;

  n    := Maximum(DegreeOfTransformationSemigroup(S), 1);
  map  := x -> Matrix(Integers, x, n);
  T := Semigroup(List(GeneratorsOfSemigroup(S), map));
  UseIsomorphismRelation(S, T);

  return MagmaIsomorphismByFunctionsNC(S,
                                       T,
                                       map,
                                       AsTransformation);
end);

# IsomorphismMonoid for IsIntegerMatrixMonoid uses a generic method defined in
# semigroups/semimaxplus.gi

InstallMethod(IsFinite,
"for a semigroup of matrices of positive integers",
[IsIntegerMatrixSemigroup],
6,  # to beat the method for semigroups with CanUseLibsemigroupsFroidurePin
function(S)
  local gens, ET, mat, i, j;

  gens := GeneratorsOfSemigroup(S);

  for mat in gens do
    for i in [1 .. NrRows(mat)] do
      for j in [1 .. NrCols(mat)] do
        if mat[i][j] < 0 then
          TryNextMethod();
        fi;
      od;
    od;
  od;

  ET := Idempotents(Semigroup(List(gens,
                                   x -> AsMatrix(IsNTPMatrix, x, 1, 2))));

  for mat in ET do
    mat := Matrix(Integers, mat);
    if mat ^ 2 <> mat ^ 3 then
      return false;
    fi;
  od;

  return true;
end);
