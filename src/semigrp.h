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

#include "converter.h"
#include "libsemigroups/src/semigroups.h"
#include "pkg.h"
#include "rnams.h"
#include "src/compiled.h"  // GAP headers

using libsemigroups::Semigroup;

// Typedef for readability, an en_semi_obj_t should be an Obj of TNUM_OBJ =
// T_SEMI and SUBTYPE_OF_T_SEMI = T_SEMI_SUBTYPE_ENSEMI

typedef Obj en_semi_obj_t;

// Enum for types of enumerable semigroups, to be stored in the en_semi_obj_t
// associated to an enumerable semigroup.

enum en_semi_t {
  UNKNOWN,
  TRANS2,
  TRANS4,
  PPERM2,
  PPERM4,
  BOOL_MAT,
  BIPART,
  MAX_PLUS_MAT,
  MIN_PLUS_MAT,
  TROP_MAX_PLUS_MAT,
  TROP_MIN_PLUS_MAT,
  PROJ_MAX_PLUS_MAT,
  NTP_MAT,
  INT_MAT,
  PBR_TYPE
};

// C++ functions

size_t semi_obj_get_batch_size(gap_semigroup_t so);
bool semi_obj_get_report(gap_semigroup_t so);
gap_list_t semi_obj_get_gens(gap_semigroup_t so);
gap_rec_t semi_obj_get_fropin(gap_semigroup_t so);
en_semi_t semi_obj_get_type(gap_semigroup_t so);
Semigroup* semi_obj_get_semi_cpp(gap_semigroup_t so);

static inline en_semi_t en_semi_get_type(en_semi_obj_t es) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(es) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(es) == T_SEMI_SUBTYPE_ENSEMI);
  return static_cast<en_semi_t>(reinterpret_cast<UInt>(ADDR_OBJ(es)[1]));
}

static inline gap_semigroup_t en_semi_get_semi_obj(en_semi_obj_t es) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(es) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(es) == T_SEMI_SUBTYPE_ENSEMI);
  SEMIGROUPS_ASSERT(en_semi_get_type(es) != UNKNOWN);
  return ADDR_OBJ(es)[2];
}

static inline size_t en_semi_get_degree(en_semi_obj_t es) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(es) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(es) == T_SEMI_SUBTYPE_ENSEMI);
  SEMIGROUPS_ASSERT(en_semi_get_type(es) != UNKNOWN);
  return CLASS_OBJ<size_t>(es, 3);
}

Converter* en_semi_get_converter(en_semi_obj_t es);
Semigroup* en_semi_get_semi_cpp(en_semi_obj_t es);

// GAP level functions for IsEnumerableSemigroupRep

gap_list_t EN_SEMI_AS_LIST(Obj self, gap_semigroup_t so);
gap_list_t EN_SEMI_AS_SET(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_CURRENT_MAX_WORD_LENGTH(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_CURRENT_NR_RULES(Obj self, gap_semigroup_t so);
gap_int_t
EN_SEMI_CURRENT_POSITION(Obj self, gap_semigroup_t so, gap_element_t x);
gap_int_t EN_SEMI_CURRENT_SIZE(Obj self, gap_semigroup_t so);
gap_list_t EN_SEMI_CAYLEY_TABLE(Obj self, gap_semigroup_t so);
gap_semigroup_t EN_SEMI_CLOSURE(Obj             self,
                                gap_semigroup_t new_so,
                                gap_semigroup_t old_so,
                                gap_list_t      plist);
gap_semigroup_t
EN_SEMI_CLOSURE_DEST(Obj self, gap_semigroup_t so, gap_list_t coll);
gap_element_t
EN_SEMI_ELEMENT_NUMBER(Obj self, gap_semigroup_t so, gap_int_t pos);
gap_element_t
EN_SEMI_ELEMENT_NUMBER_SORTED(Obj self, gap_semigroup_t so, gap_int_t pos);
gap_list_t EN_SEMI_ELMS_LIST(Obj self, gap_semigroup_t so, gap_list_t poslist);
gap_semigroup_t
EN_SEMI_ENUMERATE(Obj self, gap_semigroup_t so, gap_int_t limit);
gap_list_t EN_SEMI_FACTORIZATION(Obj self, gap_semigroup_t so, gap_int_t pos);
gap_list_t EN_SEMI_LEFT_CAYLEY_GRAPH(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_LENGTH_ELEMENT(Obj self, gap_semigroup_t so, gap_int_t pos);
gap_list_t EN_SEMI_IDEMPOTENTS(Obj self, gap_semigroup_t so);
gap_list_t EN_SEMI_IDEMS_SUBSET(Obj self, gap_semigroup_t so, gap_list_t list);
gap_bool_t EN_SEMI_IS_DONE(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_NR_IDEMPOTENTS(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_POSITION(Obj self, gap_semigroup_t so, gap_element_t x);
gap_int_t
EN_SEMI_POSITION_SORTED(Obj self, gap_semigroup_t so, gap_element_t x);
gap_list_t EN_SEMI_RELATIONS(Obj self, gap_semigroup_t so);
gap_list_t EN_SEMI_RIGHT_CAYLEY_GRAPH(Obj self, gap_semigroup_t so);
gap_int_t EN_SEMI_SIZE(Obj self, gap_semigroup_t so);

// Iterators

gap_bool_t EN_SEMI_IS_DONE_ITERATOR(Obj self, gap_rec_t iter);
gap_element_t EN_SEMI_NEXT_ITERATOR(Obj self, gap_rec_t iter);
gap_element_t EN_SEMI_NEXT_ITERATOR_SORTED(Obj self, gap_rec_t iter);

#endif  // SEMIGROUPS_SRC_SEMIGRP_H_
