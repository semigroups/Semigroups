/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ semigroups.
 *
 */

#ifndef SRC_INTERFACE_H_
#define SRC_INTERFACE_H_

/*******************************************************************************
 *
*******************************************************************************/

Obj enumerate_semigroup(Obj, Obj, Obj, Obj, Obj);

// GAP level functions

Obj SEMIGROUP_ADD_GENERATORS(Obj, Obj, Obj);
Obj SEMIGROUP_CAYLEY_TABLE(Obj, Obj);
Obj SEMIGROUP_CLOSURE(Obj, Obj, Obj, Obj);
Obj SEMIGROUP_CURRENT_MAX_WORD_LENGTH(Obj, Obj);
Obj SEMIGROUP_CURRENT_NR_RULES(Obj, Obj);
Obj SEMIGROUP_CURRENT_SIZE(Obj, Obj);
Obj SEMIGROUP_AS_LIST(Obj, Obj);
Obj SEMIGROUP_AS_SET(Obj, Obj);
Obj SEMIGROUP_ELEMENT_NUMBER(Obj, Obj, Obj);
Obj SEMIGROUP_ELEMENT_NUMBER_SORTED(Obj, Obj, Obj);
Obj SEMIGROUP_ENUMERATE(Obj, Obj, Obj);
Obj SEMIGROUP_FACTORIZATION(Obj, Obj, Obj);
Obj SEMIGROUP_IS_DONE(Obj, Obj);
Obj SEMIGROUP_LEFT_CAYLEY_GRAPH(Obj, Obj);
Obj SEMIGROUP_LENGTH_ELEMENT(Obj, Obj, Obj);
Obj SEMIGROUP_NEXT_ITERATOR(Obj, Obj);
Obj SEMIGROUP_NEXT_ITERATOR_SORTED(Obj, Obj);
Obj SEMIGROUP_IS_DONE_ITERATOR(Obj, Obj);
Obj SEMIGROUP_IS_DONE_ITERATOR_CC(Obj, Obj);
Obj SEMIGROUP_MAX_WORD_LENGTH_BY_RANK(Obj, Obj);
Obj SEMIGROUP_NR_IDEMPOTENTS(Obj, Obj);
Obj SEMIGROUP_POSITION(Obj, Obj, Obj);
Obj SEMIGROUP_POSITION_CURRENT(Obj, Obj, Obj);
Obj SEMIGROUP_POSITION_SORTED(Obj, Obj, Obj);
Obj SEMIGROUP_RELATIONS(Obj, Obj);
Obj SEMIGROUP_RIGHT_CAYLEY_GRAPH(Obj, Obj);
Obj SEMIGROUP_SIZE(Obj, Obj);

Obj FP_SEMI_SIZE(Obj, Obj);
Obj FP_SEMI_WORD_PROBLEM(Obj, Obj, Obj, Obj);

Obj SEMIGROUP_CONGRUENCE(Obj, Obj, Obj);

#endif // SRC_INTERFACE_H_
