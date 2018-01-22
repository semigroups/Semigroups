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

#ifndef SEMIGROUPS_SRC_CONGPAIRS_H_
#define SEMIGROUPS_SRC_CONGPAIRS_H_

// Inclusion of <cstdef> appears to be required to prevent travis from issuing
// the warning:
//
//     /usr/include/c++/5/cstddef:51:11: error: ‘::max_align_t’ has not been
//     declared
//
// according to:
//
// https://stackoverflow.com/questions/35110786/how-to-fix-the-error-max-align-t

#include <cstddef>

#include "src/compiled.h"

// GAP level functions

Obj CONG_PAIRS_NR_CLASSES(Obj, Obj);
Obj CONG_PAIRS_IN(Obj, Obj, Obj, Obj);
Obj CONG_PAIRS_LESS_THAN(Obj, Obj, Obj, Obj);
Obj CONG_PAIRS_LOOKUP_PART(Obj, Obj);
Obj CONG_PAIRS_ELM_COSET_ID(Obj, Obj, Obj);
Obj CONG_PAIRS_NONTRIVIAL_CLASSES(Obj, Obj);

#endif  // SEMIGROUPS_SRC_CONGPAIRS_H_
