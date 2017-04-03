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

#ifndef SEMIGROUPS_SRC_FROPIN_H_
#define SEMIGROUPS_SRC_FROPIN_H_

#include "pkg.h"
#include "src/compiled.h"

#define ELM_PLIST2(plist, i, j) ELM_PLIST(ELM_PLIST(plist, i), j)

// TODO(JDM) write doc
size_t fropin_prod_by_reduction(gap_rec_t fp, size_t i, size_t j);

// TODO(JDM) write doc
gap_rec_t fropin(Obj        data,  // can be gap_semigroup_t or a prec
                 gap_int_t  limit,
                 gap_func_t lookfunc,
                 gap_bool_t looking);

// TODO(JDM) use gap_foo_t and write doc
Obj SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS(Obj, Obj, Obj);
Obj FIND_HCLASSES(Obj, Obj, Obj);

#endif  // SEMIGROUPS_SRC_FROPIN_H_
