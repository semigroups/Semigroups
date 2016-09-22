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

#ifndef SRC_INTERFACE_H_
#define SRC_INTERFACE_H_

#include "src/compiled.h"

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
Obj SEMIGROUP_IS_DONE(Obj, Obj);
Obj SEMIGROUP_NEXT_ITERATOR(Obj, Obj);
Obj SEMIGROUP_NEXT_ITERATOR_SORTED(Obj, Obj);
Obj SEMIGROUP_IS_DONE_ITERATOR_CC(Obj, Obj);
Obj SEMIGROUP_POSITION_SORTED(Obj, Obj, Obj);
Obj SEMIGROUP_RELATIONS(Obj, Obj);

#endif // SRC_INTERFACE_H_
