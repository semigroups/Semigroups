//
// Semigroups package for GAP
// Copyright (C) 2016 James D. Mitchell
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

// This file contains declarations relating to enumerable semigroups

#ifndef SEMIGROUPS_SRC_SEMIGRP_H_
#define SEMIGROUPS_SRC_SEMIGRP_H_

#include "data.h"          // FIXME remove this
#include "src/compiled.h"  // GAP headers

typedef Obj en_semi_obj_t;

// C++ functions

static inline en_semi_t en_semi_get_type(en_semi_obj_t es) {
  assert(TNUM_OBJ(es) == T_SEMI && SUBTYPE_OF_T_SEMI(es) == T_SEMI_SUBTYPE_ENSEMI);
  return static_cast<en_semi_t>(reinterpret_cast<UInt>(ADDR_OBJ(es)[1]));
}

static inline Semigroup* en_semi_get_cpp(en_semi_obj_t es) {
  assert(TNUM_OBJ(es) == T_SEMI && SUBTYPE_OF_T_SEMI(es) == T_SEMI_SUBTYPE_ENSEMI);
  assert(en_semi_get_type(es) != UNKNOWN);
  return CLASS_OBJ<Semigroup*>(es, 2);
}

static inline Converter* en_semi_get_converter(en_semi_obj_t es) {
  assert(TNUM_OBJ(es) == T_SEMI && SUBTYPE_OF_T_SEMI(es) == T_SEMI_SUBTYPE_ENSEMI);
  assert(en_semi_get_type(es) != UNKNOWN);
  return CLASS_OBJ<Converter*>(es, 3);
}

Semigroup* semi_obj_get_semi_cpp(gap_semigroup_t so);
gap_prec_t semi_obj_get_fropin(gap_semigroup_t so);
en_semi_t semi_obj_get_type(gap_semigroup_t so);

// GAP level functions for IsEnumerableSemigroup

gap_semigroup_t
EN_SEMI_ADD_GENERATORS(Obj self, gap_semigroup_t so, gap_plist_t coll);
gap_plist_t EN_SEMI_AS_LIST(Obj self, gap_semigroup_t so);
gap_plist_t EN_SEMI_AS_SET(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_CURRENT_MAX_WORD_LENGTH(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_CURRENT_NR_RULES(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_CURRENT_SIZE(Obj self, gap_semigroup_t so);
gap_plist_t EN_SEMI_CAYLEY_TABLE(Obj self, gap_semigroup_t so);
gap_semigroup_t EN_SEMI_CLOSURE(Obj             self,
                                gap_semigroup_t new_so,
                                gap_semigroup_t old_so,
                                gap_plist_t     plist);
gap_element_t
EN_SEMI_ELEMENT_NUMBER(Obj self, gap_semigroup_t so, gap_int_t pos);
gap_element_t
EN_SEMI_ELEMENT_NUMBER_SORTED(Obj self, gap_semigroup_t so, gap_int_t pos);
gap_semigroup_t
EN_SEMI_ENUMERATE(Obj self, gap_semigroup_t so, gap_int_t limit);
gap_plist_t EN_SEMI_FACTORIZATION(Obj self, gap_semigroup_t so, gap_int_t pos);
gap_plist_t EN_SEMI_LEFT_CAYLEY_GRAPH(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_LENGTH_ELEMENT(Obj self, gap_semigroup_t so, gap_int_t pos);
gap_bool_t EN_SEMI_IS_DONE(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_NR_IDEMPOTENTS(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_POSITION(Obj self, gap_semigroup_t so, gap_element_t x);
gap_int_t
EN_SEMI_POSITION_CURRENT(Obj self, gap_semigroup_t so, gap_element_t x);
gap_int_t
EN_SEMI_POSITION_SORTED(Obj self, gap_semigroup_t so, gap_element_t x);
gap_plist_t EN_SEMI_RELATIONS(Obj self, gap_semigroup_t so);
gap_plist_t EN_SEMI_RIGHT_CAYLEY_GRAPH(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_SIZE(Obj self, gap_semigroup_t so);

// Iterators

gap_bool_t EN_SEMI_IS_DONE_ITERATOR(Obj self, gap_prec_t iter);
gap_element_t EN_SEMI_NEXT_ITERATOR(Obj self, gap_prec_t iter);
gap_element_t EN_SEMI_NEXT_ITERATOR_SORTED(Obj self, gap_prec_t iter);

#endif  // SEMIGROUPS_SRC_SEMIGRP_H_
