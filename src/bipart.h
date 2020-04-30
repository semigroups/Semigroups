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

#ifndef SEMIGROUPS_SRC_BIPART_H_
#define SEMIGROUPS_SRC_BIPART_H_

// GAP headers
#include "compiled.h"

// Semigroups pkg headers
#include "pkg.h"
#include "semigroups-debug.h"

#include "libsemigroups/element.hpp"

using libsemigroups::Bipartition;
using libsemigroups::Blocks;

// C functions

inline Bipartition* bipart_get_cpp(Obj x) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(x) == T_BIPART);
  return reinterpret_cast<Bipartition*>(ADDR_OBJ(x)[0]);
}

inline Blocks* blocks_get_cpp(Obj x) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(x) == T_BLOCKS);
  return reinterpret_cast<Blocks*>(ADDR_OBJ(x)[0]);
}

Obj bipart_new_obj(Bipartition*);

// GAP level functions

Int BIPART_EQ(Obj, Obj);
Int BIPART_LT(Obj, Obj);
Obj BIPART_PROD(Obj, Obj);

Obj BIPART_NC(Obj, Obj);
Obj BIPART_EXT_REP(Obj, Obj);
Obj BIPART_INT_REP(Obj, Obj);
Obj BIPART_HASH(Obj, Obj, Obj);
Obj BIPART_DEGREE(Obj, Obj);
Obj BIPART_RANK(Obj, Obj, Obj);
Obj BIPART_NR_BLOCKS(Obj, Obj);
Obj BIPART_NR_LEFT_BLOCKS(Obj, Obj);
Obj BIPART_PERM_LEFT_QUO(Obj, Obj, Obj);
Obj BIPART_LEFT_PROJ(Obj, Obj);
Obj BIPART_RIGHT_PROJ(Obj, Obj);
Obj BIPART_STAR(Obj, Obj);
Obj BIPART_LAMBDA_CONJ(Obj, Obj, Obj);
Obj BIPART_STAB_ACTION(Obj, Obj, Obj);
Obj BIPART_LEFT_BLOCKS(Obj, Obj);
Obj BIPART_RIGHT_BLOCKS(Obj, Obj);

Int BLOCKS_EQ(Obj, Obj);
Int BLOCKS_LT(Obj, Obj);

Obj BLOCKS_NC(Obj, Obj);
Obj BLOCKS_EXT_REP(Obj, Obj);
Obj BLOCKS_HASH(Obj, Obj, Obj);
Obj BLOCKS_DEGREE(Obj, Obj);
Obj BLOCKS_RANK(Obj, Obj);
Obj BLOCKS_NR_BLOCKS(Obj, Obj);
Obj BLOCKS_PROJ(Obj, Obj);
Obj BLOCKS_E_TESTER(Obj, Obj, Obj);
Obj BLOCKS_E_CREATOR(Obj, Obj, Obj);
Obj BLOCKS_LEFT_ACT(Obj, Obj, Obj);
Obj BLOCKS_RIGHT_ACT(Obj, Obj, Obj);
Obj BLOCKS_INV_LEFT(Obj, Obj, Obj);
Obj BLOCKS_INV_RIGHT(Obj, Obj, Obj);

Obj BIPART_NR_IDEMPOTENTS(Obj, Obj, Obj, Obj, Obj, Obj);

#endif  // SEMIGROUPS_SRC_BIPART_H_
