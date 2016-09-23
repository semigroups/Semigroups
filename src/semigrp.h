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

#ifndef SEMIGROUPS_SRC_SEMIGRP_H_
#define SEMIGROUPS_SRC_SEMIGRP_H_

#include "src/compiled.h" // GAP headers
#include "data.h" // FIXME remove this

// C++ functions

static inline Semigroup* en_semi_get_cpp(Obj en_semi) {
  return CLASS_OBJ<Semigroup*>(en_semi, 2);
}

static inline Converter* en_semi_get_converter(Obj en_semi) {
  return CLASS_OBJ<Converter*>(en_semi, 3);
}

static inline en_semi_t en_semi_get_type(Obj en_semi) {
  return static_cast<en_semi_t>(reinterpret_cast<UInt>(ADDR_OBJ(en_semi)[1]));
}

Semigroup* semi_obj_get_semi_cpp(gap_semigroup_t S);
Obj semi_obj_get_fropin(gap_semigroup_t S);
en_semi_t semi_obj_get_type(gap_semigroup_t S);

// GAP level functions
Obj EN_SEMI_ADD_GENERATORS(Obj self, gap_semigroup_t S, gap_plist_t coll);
Obj EN_SEMI_FACTORIZATION(Obj self, gap_semigroup_t S, gap_int_t pos);
Obj EN_SEMI_IS_DONE_ITERATOR(Obj self, Obj iter);
Obj EN_SEMI_LEFT_CAYLEY_GRAPH(Obj self, gap_semigroup_t S);
Obj EN_SEMI_LENGTH_ELEMENT(Obj self, gap_semigroup_t S, gap_int_t pos);
Obj EN_SEMI_NR_IDEMPOTENTS(Obj self, gap_semigroup_t S);
Obj EN_SEMI_POSITION_CURRENT(Obj self, gap_semigroup_t S, gap_element_t x);
Obj EN_SEMI_POSITION(Obj self, gap_element_t S, gap_element_t x);
Obj EN_SEMI_RELATIONS(Obj self, gap_semigroup_t S);
Obj EN_SEMI_RIGHT_CAYLEY_GRAPH(Obj self, gap_semigroup_t S);
Obj EN_SEMI_SIZE(Obj self, gap_semigroup_t S);

#endif  // SEMIGROUPS_SRC_SEMIGRP_H_
