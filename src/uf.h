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

// This file uses UF, a class in libsemigroups used to make an equivalence
// relation on the integers {1 .. n}, using the UNION-FIND METHOD: new pairs can
// be added and the appropriate classes combined quickly.

#ifndef SEMIGROUPS_SRC_UF_H_
#define SEMIGROUPS_SRC_UF_H_

#include "pkg.h"
#include "src/compiled.h"

// GAP level functions

Obj UF_NEW(Obj self, Obj size);
Obj UF_COPY(Obj self, Obj uf);
Obj UF_SIZE(Obj self, Obj uf);
Obj UF_FIND(Obj self, Obj uf, Obj i);
Obj UF_UNION(Obj self, Obj uf, Obj pair);
Obj UF_FLATTEN(Obj self, Obj uf);
Obj UF_TABLE(Obj self, Obj uf);
Obj UF_BLOCKS(Obj self, Obj uf);
Obj UF_NR_BLOCKS(Obj self, Obj uf);
Obj UF_BLOCK_REPS(Obj self, Obj uf);
Obj UF_JOIN(Obj self, Obj uf1, Obj uf2);

#endif  // SEMIGROUPS_SRC_UF_H_
