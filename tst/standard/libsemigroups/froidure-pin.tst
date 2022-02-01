#############################################################################
##
#W  standard/libsemigroups/froidure-pin.tst
#Y  Copyright (C) 2022                                   James D. Mitchell
##                                                       
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
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
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(Semigroup(ConstantTransformation(17, 1)));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(Semigroup(ConstantTransformation(65537, 1)));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(SymmetricInverseMonoid(1));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(SymmetricInverseMonoid(17));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(SymmetricInverseMonoid(65537));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(FullBooleanMatMonoid(2));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(RegularBooleanMatMonoid(9));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(FullTropicalMinPlusMonoid(2, 2));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(FullTropicalMaxPlusMonoid(2, 2));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )
gap> FroidurePinMemFnRec(Semigroup(Matrix(IsProjectiveMaxPlusMatrix, [[1]])));
rec( add_generator := function( arg1, arg2 ) ... end, 
  at := function( arg1, arg2 ) ... end, 
  closure := function( arg1, arg2 ) ... end, copy := function( arg1 ) ... end,
  current_position := function( arg1, arg2 ) ... end, 
  enumerate := function( arg1, arg2 ) ... end, 
  factorisation := function( arg1, arg2 ) ... end, 
  fast_product := function( arg1, arg2, arg3 ) ... end, 
  finished := function( arg1 ) ... end, 
  generator := function( arg1, arg2 ) ... end, 
  idempotents := function( arg1 ) ... end, 
  is_idempotent := function( arg1, arg2 ) ... end, 
  left_cayley_graph := function( arg1 ) ... end, 
  make := function( arg1 ) ... end, 
  number_of_generators := function( arg1 ) ... end, 
  number_of_idempotents := function( arg1 ) ... end, 
  position := function( arg1, arg2 ) ... end, 
  position_to_sorted_position := function( arg1, arg2 ) ... end, 
  right_cayley_graph := function( arg1 ) ... end, 
  rules := function( arg1 ) ... end, size := function( arg1 ) ... end, 
  sorted_at := function( arg1, arg2 ) ... end, 
  sorted_position := function( arg1, arg2 ) ... end )

# HasCppFroidurePin
gap> S := FullTransformationMonoid(2);
<full transformation monoid of degree 2>
gap> HasCppFroidurePin(S);
false
gap> IsomorphismFpSemigroup(S);;
gap> HasCppFroidurePin(S);
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

# SEMIGROUPS_UnbindVariables
gap> Unbind(BruteForceInverseCheck);
gap> Unbind(BruteForceIsoCheck);
gap> Unbind(F);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(T);
gap> Unbind(map);
gap> Unbind(rels);
gap> Unbind(x);
gap> Unbind(y);

# 
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/libsemigroups/froidure-pin.tst");
