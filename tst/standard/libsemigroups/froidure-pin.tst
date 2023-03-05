#############################################################################
##
#W  standard/libsemigroups/froidure-pin.tst
#Y  Copyright (C) 2022                                   James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local F, N, R, S, acting, add_generator, at, closure, coll, copy
#@local current_position, en, enumerate, factorisation, fast_product
#@local final_letter, finished, first_letter, generator, idempotents
#@local is_idempotent, it, left_cayley_graph, list, make, nr
#@local number_of_generators, number_of_idempotents, opts, position
#@local position_to_sorted_position, prefix, rels, right_cayley_graph, rules
#@local size, sorted_at, sorted_position, suffix, x
gap> START_TEST("Semigroups package: standard/libsemigroups/froidure-pin.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# FroidurePinMemFnRec
gap> FroidurePinMemFnRec(FullTransformationSemigroup(1));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(Semigroup(ConstantTransformation(17, 1)));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(Semigroup(ConstantTransformation(65537, 1)));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(SymmetricInverseMonoid(1));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(SymmetricInverseMonoid(17));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(SymmetricInverseMonoid(65537));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(FullBooleanMatMonoid(2));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(RegularBooleanMatMonoid(9));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(FullTropicalMinPlusMonoid(2, 2));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(FullTropicalMaxPlusMonoid(2, 2));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(Semigroup(Matrix(IsProjectiveMaxPlusMatrix, [[1]])));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  final_letter := function( arg1, arg2 ) ... end, 
  finished := function( arg1 ) ... end, 
  first_letter := function( arg1, arg2 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, make := function(  ) ... end,
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  prefix := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end, 
  suffix := function( arg1, arg2 ) ... end )

# HasLibsemigroupsFroidurePin
gap> S := FullTransformationMonoid(2);
<full transformation monoid of degree 2>
gap> HasLibsemigroupsFroidurePin(S);
false
gap> IsomorphismFpSemigroup(S);;
gap> HasLibsemigroupsFroidurePin(S);
true

# Size etc
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> Size(S);
16
gap> IsFinite(S);
true
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> Size(S);
infinity
gap> IsFinite(S);
false
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> IsFinite(S);
true
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> IsFinite(S);
false
gap> S := Semigroup(Matrix(Integers,
>                   [[-1, 0, 0], [0, -1, 0], [0, 0, -1]]));
<commutative semigroup of 3x3 integer matrices with 1 generator>
gap> IsFinite(S);
true

# AsSet
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> AsSet(S);
[ Matrix(IsBooleanMat, [[0, 0], [0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0], [0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0], [1, 0]]), 
  Matrix(IsBooleanMat, [[0, 0], [1, 1]]), 
  Matrix(IsBooleanMat, [[0, 1], [0, 0]]), 
  Matrix(IsBooleanMat, [[0, 1], [0, 1]]), 
  Matrix(IsBooleanMat, [[0, 1], [1, 0]]), 
  Matrix(IsBooleanMat, [[0, 1], [1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0], [0, 0]]), 
  Matrix(IsBooleanMat, [[1, 0], [0, 1]]), 
  Matrix(IsBooleanMat, [[1, 0], [1, 0]]), 
  Matrix(IsBooleanMat, [[1, 0], [1, 1]]), 
  Matrix(IsBooleanMat, [[1, 1], [0, 0]]), 
  Matrix(IsBooleanMat, [[1, 1], [0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1], [1, 0]]), 
  Matrix(IsBooleanMat, [[1, 1], [1, 1]]) ]
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> AsSet(S);
Error, the argument (a semigroup) is not finite
gap> S := SymmetricInverseMonoid(2);
<symmetric inverse monoid of degree 2>
gap> AsSet(S);
[ <empty partial perm>, <identity partial perm on [ 1 ]>, [1,2], [2,1], 
  <identity partial perm on [ 2 ]>, <identity partial perm on [ 1, 2 ]>, 
  (1,2) ]

# AsListCanonical
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> AsListCanonical(S);
[ Matrix(IsBooleanMat, [[1, 0], [0, 1]]), 
  Matrix(IsBooleanMat, [[0, 1], [1, 0]]), 
  Matrix(IsBooleanMat, [[1, 0], [1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0], [0, 0]]), 
  Matrix(IsBooleanMat, [[1, 1], [1, 0]]), 
  Matrix(IsBooleanMat, [[0, 0], [1, 0]]), 
  Matrix(IsBooleanMat, [[0, 1], [1, 1]]), 
  Matrix(IsBooleanMat, [[1, 0], [1, 0]]), 
  Matrix(IsBooleanMat, [[0, 1], [0, 0]]), 
  Matrix(IsBooleanMat, [[1, 1], [0, 1]]), 
  Matrix(IsBooleanMat, [[0, 0], [0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1], [1, 1]]), 
  Matrix(IsBooleanMat, [[0, 1], [0, 1]]), 
  Matrix(IsBooleanMat, [[1, 1], [0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0], [0, 0]]), 
  Matrix(IsBooleanMat, [[0, 0], [1, 1]]) ]
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> AsListCanonical(S);
Error, the argument (a semigroup) is not finite
gap> S := SymmetricInverseMonoid(2);
<symmetric inverse monoid of degree 2>
gap> AsListCanonical(S);
[ <identity partial perm on [ 1, 2 ]>, (1,2), <identity partial perm on [ 1 ]>
    , [2,1], [1,2], <identity partial perm on [ 2 ]>, <empty partial perm> ]

# PositionCanonical
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> List(AsListCanonical(S), x -> PositionCanonical(S, x)) = [1 .. 16];
true
gap> PositionCanonical(S, Matrix(IsBooleanMat, [[0, 0, 0], [1, 0, 0], [1, 1, 1]]));
fail
gap> S := SymmetricInverseMonoid(2);;
gap> List(AsListCanonical(S), x -> PositionCanonical(S, x)) = [1 .. Size(S)];
true
gap> PositionCanonical(S, PartialPerm([1 .. 3]));
fail
gap> S := FullTransformationMonoid(2);;
gap> List(AsListCanonical(S), x -> PositionCanonical(S, x)) = [1 .. Size(S)];
true
gap> PositionCanonical(S, AsTransformation((1, 2, 3)));
fail
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 2, s1], [s1 * s2, s2], [s2 ^ 2, s2 * s1]];;
gap> S := F / rels;
<fp semigroup with 2 generators and 3 relations of length 12>
gap> PositionCanonical(S, 1);
fail

# Position
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> List(AsListCanonical(S), x -> Position(S, x)) = [1 .. 16];
true
gap> Position(S, Matrix(IsBooleanMat, [[0, 0, 0], [1, 0, 0], [1, 1, 1]]));
fail
gap> S := SymmetricInverseMonoid(2);;
gap> List(AsListCanonical(S), x -> Position(S, x)) = [1 .. Size(S)];
true
gap> Position(S, PartialPerm([1 .. 3]));
fail
gap> S := FullTransformationMonoid(2);;
gap> List(AsListCanonical(S), x -> Position(S, x)) = [1 .. Size(S)];
true
gap> Position(S, AsTransformation((1, 2, 3)));
fail
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> Enumerate(S, 8194);
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> en := Enumerator(S);
<enumerator of <commutative semigroup of 3x3 max-plus matrices with 1 
 generator>>
gap> en[100];
Matrix(IsMaxPlusMatrix, [[388, 394, 386], [394, 400, 392], [390, 396, 388]])
gap> PositionCanonical(S, en[100]);
100

# PostitionSorted
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> List(AsListCanonical(S), x -> PositionSorted(S, x));
[ 10, 7, 12, 9, 15, 3, 8, 11, 5, 14, 2, 16, 6, 13, 1, 4 ]
gap> PositionSorted(S, Matrix(IsBooleanMat, [[0, 0, 0], [1, 0, 0], [1, 1, 1]]));
fail
gap> S := SymmetricInverseMonoid(2);;
gap> list := AsListCanonical(S);
[ <identity partial perm on [ 1, 2 ]>, (1,2), <identity partial perm on [ 1 ]>
    , [2,1], [1,2], <identity partial perm on [ 2 ]>, <empty partial perm> ]
gap> copy := ShallowCopy(list);;
gap> Sort(copy, {x, y} -> PositionSorted(S, x) < PositionSorted(S, y));
gap> SortedList(list) = copy;
true
gap> PositionSorted(S, PartialPerm([1 .. 3]));
fail
gap> S := FullTransformationMonoid(2);;
gap> list := AsListCanonical(S);
[ IdentityTransformation, Transformation( [ 2, 1 ] ), 
  Transformation( [ 1, 1 ] ), Transformation( [ 2, 2 ] ) ]
gap> copy := ShallowCopy(list);;
gap> Sort(copy, {x, y} -> PositionSorted(S, x) < PositionSorted(S, y));
gap> SortedList(list) = copy;
true
gap> PositionSorted(S, AsTransformation((1, 2, 3)));
fail
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> PositionSorted(S, S.1);
Error, the 1st argument (a semigroup) is not finite

# \in
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> ForAll(S, x -> x in S);
true

# NrIdempotents, Idempotents
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> NrIdempotents(S);
Error, the argument (a semigroup) is not finite
gap> Idempotents(S);
Error, the argument (a semigroup) is not finite
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> NrIdempotents(S);
11
gap> ForAll(Idempotents(S), x -> x ^ 2 = x);
true
gap> Length(Idempotents(S));
11
gap> Number(S, x -> x ^ 2 = x);
11

# MinimalFactorization
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> MinimalFactorization(S, 
>                         Matrix(IsMaxPlusMatrix, [[388, 394, 386], 
>                                                  [394, 400, 392], 
>                                                  [390, 396, 388]]));
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
gap> S := RegularBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> MinimalFactorization(S, Matrix(IsBooleanMat, [[0, 0, 0], [1, 0, 0], [1, 1, 1]]));
Error, the 2nd argument (a mult. elt.) must belong to the 1st argument (a semi\
group)
gap> MinimalFactorization(S, 7);
[ 3, 2 ]

# LeftCayleyGraphSemigroup/Digraph
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> LeftCayleyGraphSemigroup(S);
Error, the argument (a semigroup) is not finite
gap> LeftCayleyDigraph(S);
Error, the argument (a semigroup) is not finite
gap> RightCayleyGraphSemigroup(S);
Error, the argument (a semigroup) is not finite
gap> RightCayleyDigraph(S);
Error, the argument (a semigroup) is not finite
gap> S := RegularBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> LeftCayleyDigraph(S);
<immutable multidigraph with 16 vertices, 64 edges>
gap> RightCayleyDigraph(S);
<immutable multidigraph with 16 vertices, 64 edges>

# EnumeratorSorted
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> EnumeratorSorted(S); 
Error, the argument (a semigroup) is not finite
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> N := EnumeratorSorted(S); 
<enumerator of <monoid of size 16, 2x2 boolean matrices with 3 generators>>
gap> ForAll(N, x -> N[Position(N, x)] = x);
true
gap> ForAll([1 .. Length(N)], x -> Position(N, N[x]) = x);
true
gap> ForAll(N, x -> x in N);
true
gap> ForAll([1 .. Length(N)], x -> IsBound(N[x]));
true
gap> IsBound(N[Length(N) + 1]);
false
gap> S := SymmetricInverseMonoid(2);
<symmetric inverse monoid of degree 2>
gap> N := EnumeratorSorted(S); 
[ <empty partial perm>, <identity partial perm on [ 1 ]>, [1,2], [2,1], 
  <identity partial perm on [ 2 ]>, <identity partial perm on [ 1, 2 ]>, 
  (1,2) ]
gap> ForAll(N, x -> N[Position(N, x)] = x);
true
gap> ForAll([1 .. Length(N)], x -> Position(N, N[x]) = x);
true

# EnumeratorCanonical
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> N := EnumeratorCanonical(S); 
<enumerator of <commutative semigroup of 3x3 max-plus matrices with 1 
 generator>>
gap> ForAll([1 .. 1000], x -> Position(N, N[x]) = x);
true
gap> ForAll([1 .. 1000], x -> N[x] in N);
true
gap> ForAll([1 .. 1000], x -> IsBound(N[x]));
true
gap> N{[1001 .. 1003]};
[ Matrix(IsMaxPlusMatrix, [[3992, 3998, 3990], [3998, 4004, 3996], 
      [3994, 4000, 3992]]), 
  Matrix(IsMaxPlusMatrix, [[3996, 4002, 3994], [4002, 4008, 4000], 
      [3998, 4004, 3996]]), 
  Matrix(IsMaxPlusMatrix, [[4000, 4006, 3998], [4006, 4012, 4004], 
      [4002, 4008, 4000]]) ]
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> N := EnumeratorCanonical(S); 
<enumerator of <monoid of 2x2 boolean matrices with 3 generators>>
gap> ForAll(N, x -> N[Position(N, x)] = x);
true
gap> ForAll([1 .. Length(N)], x -> Position(N, N[x]) = x);
true
gap> ForAll(N, x -> x in N);
true
gap> ForAll([1 .. Length(N)], x -> IsBound(N[x]));
true
gap> IsBound(N[Length(N) + 1]);
false
gap> N[Length(N) + 1];
fail
gap> S := SymmetricInverseMonoid(2);
<symmetric inverse monoid of degree 2>
gap> N := EnumeratorCanonical(S); 
<enumerator of <symmetric inverse monoid of degree 2>>
gap> ForAll(N, x -> N[Position(N, x)] = x);
true
gap> ForAll([1 .. Length(N)], x -> Position(N, N[x]) = x);
true
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> AsListCanonical(S);;
gap> N := EnumeratorCanonical(S);;
gap> ForAll(N, x -> N[Position(N, x)] = x);
true
gap> ForAll([1 .. Length(N)], x -> Position(N, N[x]) = x);
true
gap> ForAll(N, x -> x in N);
true
gap> ForAll([1 .. Length(N)], x -> IsBound(N[x]));
true
gap> IsBound(N[Length(N) + 1]);
false
gap> N[Length(N) + 1];
Error, List Element: <list>[17] must have an assigned value
gap> S := FreeBand(2);
<free band on the generators [ x1, x2 ]>
gap> Enumerator(S){[3 .. 5]};
[ x1x2, x2x1, x1x2x1 ]
gap> F := FreeSemigroup(2);; AssignGeneratorVariables(F);;
gap> rels := [[s1 ^ 2, s1], [s1 * s2, s2], [s2 ^ 2, s2 * s1]];;
gap> S := F / rels;
<fp semigroup with 2 generators and 3 relations of length 12>
gap> EnumeratorCanonical(S)[4];
fail
gap> 

# IteratorSorted
gap> S := SymmetricInverseMonoid(2);
<symmetric inverse monoid of degree 2>
gap> it := IteratorSorted(S);
<iterator>
gap> nr := 0;
0
gap> for x in it do nr := nr + 1; od;
gap> nr = Size(S);
true

# MultiplicationTable
gap> S := FullBooleanMatMonoid(1);
<commutative monoid of 1x1 boolean matrices with 1 generator>
gap> MultiplicationTable(S);
[ [ 1, 1 ], [ 1, 2 ] ]
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> MultiplicationTable(S);
Error, the argument (a semigroup) is not finite

# ClosureSemigroupOrMonoidNC
gap> S := Semigroup(Matrix(IsBooleanMat, [[0, 0, 0], [1, 0, 0], [1, 1, 1]]));
<commutative semigroup of 3x3 boolean matrices with 1 generator>
gap> coll := [Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 0], [1, 0, 1]]),
> Matrix(IsBooleanMat, [[1, 1, 0], [1, 0, 1], [1, 1, 0]]), 
> Matrix(IsBooleanMat, [[1, 1, 1], [1, 1, 0], [0, 1, 0]]),
> Matrix(IsBooleanMat, [[0, 1, 1], [1, 1, 1], [0, 0, 0]]), 
> Matrix(IsBooleanMat, [[0, 0, 1], [0, 1, 0], [0, 0, 0]]),
> Matrix(IsBooleanMat, [[1, 1, 0], [1, 1, 1], [1, 0, 1]]), 
> Matrix(IsBooleanMat, [[0, 1, 1], [0, 1, 0], [1, 1, 1]]),
> Matrix(IsBooleanMat, [[1, 1, 0], [1, 1, 1], [0, 0, 1]]), 
> Matrix(IsBooleanMat, [[0, 1, 0], [0, 0, 1], [1, 0, 1]])];;
gap> S := ClosureSemigroupOrMonoidNC(Semigroup, S, coll, rec());
<semigroup of 3x3 boolean matrices with 8 generators>
gap> S := ClosureSemigroupOrMonoidNC(Semigroup, S, coll, rec());
<semigroup of 3x3 boolean matrices with 8 generators>

# ClosureSemigroupOrMonoidNC
gap> opts := rec(acting := false);
rec( acting := false )
gap> S := Semigroup(Transformation([2, 1, 1]),
>                   Transformation([3, 1, 1]),
>                   Transformation([3, 3, 1]), opts);
<transformation semigroup of degree 3 with 3 generators>
gap> S := ClosureSemigroupOrMonoidNC(Semigroup, S,
> AsList(FullTransformationMonoid(3)), opts);
<transformation monoid of degree 3 with 5 generators>
gap> S := ClosureSemigroupOrMonoidNC(Semigroup, S,
> AsList(FullTransformationMonoid(3)), opts);
<transformation monoid of degree 3 with 5 generators>
gap> S := ClosureSemigroupOrMonoidNC(Semigroup, S,
> [Transformation([1, 2, 1, 3])], opts);
<transformation monoid of degree 4 with 6 generators>
gap> S := Semigroup(PartialPerm([1, 2, 4], [1, 2, 3]),
>                   PartialPerm([1, 3, 4], [3, 1, 2]),
>                   PartialPerm([1, 2], [1, 4]),
>                   PartialPerm([1, 3, 4], [4, 2, 1]), opts);
<partial perm semigroup of rank 4 with 4 generators>
gap> S := ClosureSemigroupOrMonoidNC(Semigroup, S,
> [PartialPerm([1, 2, 3, 4], [1, 3, 2, 4])], opts);
<partial perm semigroup of rank 4 with 5 generators>

# RulesOfSemigroup
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> RulesOfSemigroup(S);
[ [ [ 1, 1 ], [ 1 ] ], [ [ 1, 2 ], [ 2 ] ], [ [ 1, 3 ], [ 3 ] ], 
  [ [ 1, 4 ], [ 4 ] ], [ [ 2, 1 ], [ 2 ] ], [ [ 2, 2 ], [ 1 ] ], 
  [ [ 3, 1 ], [ 3 ] ], [ [ 3, 3 ], [ 3 ] ], [ [ 4, 1 ], [ 4 ] ], 
  [ [ 4, 3 ], [ 4 ] ], [ [ 4, 4 ], [ 4 ] ], [ [ 2, 3, 4 ], [ 3, 4 ] ], 
  [ [ 3, 2, 4 ], [ 2, 4 ] ], [ [ 2, 3, 2, 3 ], [ 3, 2, 3 ] ], 
  [ [ 2, 4, 2, 4 ], [ 4, 2, 4 ] ], [ [ 3, 2, 3, 2 ], [ 3, 2, 3 ] ], 
  [ [ 3, 4, 2, 3 ], [ 3, 2, 3 ] ], [ [ 3, 4, 2, 4 ], [ 4, 2, 4 ] ], 
  [ [ 4, 2, 3, 2 ], [ 4, 2, 3 ] ], [ [ 4, 2, 4, 2 ], [ 4, 2, 4 ] ] ]
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> RulesOfSemigroup(S);
Error, the argument (a semigroup) is not finite

# IdempotentsSubset
gap> S := FullBooleanMatMonoid(2);
<monoid of 2x2 boolean matrices with 3 generators>
gap> IdempotentsSubset(S, [2 .. 10]);
[ 3, 4, 8, 10 ]
gap> S := Semigroup(Matrix(IsMaxPlusMatrix,
> [[1, -infinity, 2], [-2, 4, -infinity], [1, 0, 3]]));
<commutative semigroup of 3x3 max-plus matrices with 1 generator>
gap> IdempotentsSubset(S, [666 .. 1000]);
Error, the 1st argument (a semigroup) is not finite

# MultiplicationTable for fp semigroup/monoid
gap> F := FreeMonoid("a", "b");;
gap> R := [[F.1 ^ 3, F.1], [F.2 ^ 2, F.2], [F.1 * F.2 * F.1 * F.2, F.1]];;
gap> S := F / R;;
gap> MultiplicationTable(S);
[ [ 1, 2, 3, 4 ], [ 2, 2, 2, 2 ], [ 3, 4, 3, 4 ], [ 4, 4, 4, 4 ] ]

# Issue #869, when the size of an fp semigroup/monoid equals its number of
# generators.
gap> F := FreeSemigroup(6);
<free semigroup on the generators [ s1, s2, s3, s4, s5, s6 ]>
gap> R :=
> [[F.1 ^ 2, F.1], [F.1 * F.2, F.1], [F.1 * F.3, F.1], [F.1 * F.4, F.1],
>  [F.1 * F.5, F.5], [F.1 * F.6, F.6], [F.2 * F.1, F.1], [F.2 ^ 2, F.1],
>  [F.2 * F.3, F.1], [F.2 * F.4, F.2], [F.2 * F.5, F.5], [F.2 * F.6, F.6],
>  [F.3 * F.1, F.3], [F.3 * F.2, F.3], [F.3 ^ 2, F.3], [F.3 * F.4, F.3],
>  [F.3 * F.5, F.5], [F.3 * F.6, F.6], [F.4 * F.1, F.1], [F.4 * F.2, F.2],
>  [F.4 * F.3, F.3], [F.4 ^ 2, F.4], [F.4 * F.5, F.5], [F.4 * F.6, F.6],
>  [F.5 * F.1, F.5], [F.5 * F.2, F.5], [F.5 * F.3, F.5], [F.5 * F.4, F.5],
>  [F.5 ^ 2, F.5], [F.5 * F.6, F.5], [F.6 * F.1, F.6], [F.6 * F.2, F.6],
>  [F.6 * F.3, F.6], [F.6 * F.4, F.6], [F.6 * F.5, F.6], [F.6 ^ 2, F.6]];;
gap> S := F / R;
<fp semigroup with 6 generators and 36 relations of length 114>
gap> MultiplicationTable(S);
[ [ 1, 1, 1, 1, 5, 6 ], [ 1, 1, 1, 2, 5, 6 ], [ 3, 3, 3, 3, 5, 6 ], 
  [ 1, 2, 3, 4, 5, 6 ], [ 5, 5, 5, 5, 5, 5 ], [ 6, 6, 6, 6, 6, 6 ] ]

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/libsemigroups/froidure-pin.tst");
