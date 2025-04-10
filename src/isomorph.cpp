//
// Semigroups package for GAP
// Copyright (C) 2025 Pramoth Ragavan
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
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

#include "isomorph.hpp"
#include "gap_all.h"
#include "semigroups-debug.hpp"  // for SEMIGROUPS_ASSERT

Obj PermuteMultiplicationTableNC(Obj self, Obj output, Obj table, Obj p) {
  SEMIGROUPS_ASSERT(IS_LIST(output));
  SEMIGROUPS_ASSERT(IS_LIST(table));
  SEMIGROUPS_ASSERT(IS_PERM(p));
  SEMIGROUPS_ASSERT(LEN_LIST(output) == LEN_LIST(table));

  UInt n = LEN_LIST(table);

  for (UInt i = 1; i <= n; i++) {
    SEMIGROUPS_ASSERT(IS_LIST(ELM_LIST(table, i)))
    SEMIGROUPS_ASSERT(IS_LIST(ELM_LIST(output, i)))
    SEMIGROUPS_ASSERT(LEN_LIST(ELM_LIST(table, i)) == n);
    SEMIGROUPS_ASSERT(LEN_LIST(ELM_LIST(output, i)) == n);
  }

  if (TNUM_OBJ(p) == T_PERM2) {
    UInt deg_p = DEG_PERM2(p);
    Obj  q     = STOREDINV_PERM(p);
    if (q == 0) {
      q = INV(p);
      SET_STOREDINV_PERM(p, q);
    }
    SEMIGROUPS_ASSERT(TNUM_OBJ(q) == T_PERM2);
    UInt deg_q = DEG_PERM2(q);
    for (UInt i = 1; i <= n; i++) {
      Obj  row = ELM_LIST(output, i);
      UInt ii  = IMAGE(i - 1, CONST_ADDR_PERM2(q), deg_q) + 1;

      for (UInt j = 1; j <= n; j++) {
        UInt home    = INT_INTOBJ(ELM_LIST(
            ELM_LIST(table, ii), IMAGE(j - 1, CONST_ADDR_PERM2(q), deg_q) + 1));
        UInt new_val = IMAGE(home - 1, CONST_ADDR_PERM2(p), deg_p) + 1;
        ASS_LIST(row, j, INTOBJ_INT(new_val));
      }
    }
  } else {
    UInt deg_p = DEG_PERM4(p);
    Obj  q     = STOREDINV_PERM(p);
    if (q == 0) {
      q = INV(p);
      SET_STOREDINV_PERM(p, q);
    }
    SEMIGROUPS_ASSERT(TNUM_OBJ(q) == T_PERM4);
    UInt deg_q = DEG_PERM4(q);
    for (UInt i = 1; i <= n; i++) {
      Obj  row = ELM_LIST(output, i);
      UInt ii  = IMAGE(i - 1, CONST_ADDR_PERM4(q), deg_q) + 1;

      for (UInt j = 1; j <= n; j++) {
        UInt home    = INT_INTOBJ(ELM_LIST(
            ELM_LIST(table, ii), IMAGE(j - 1, CONST_ADDR_PERM4(q), deg_q) + 1));
        UInt new_val = IMAGE(home - 1, CONST_ADDR_PERM4(p), deg_p) + 1;
        ASS_LIST(row, j, INTOBJ_INT(new_val));
      }
    }
  }
  CHANGED_BAG(output);
  return 0L;
}

Obj PermuteMultiplicationTable(Obj self, Obj output, Obj table, Obj p) {
  if (!IS_LIST(output) || !IS_LIST(table)) {
    ErrorMayQuit("the arguments <output> and <table> must be lists but found "
                 "types '%s' and '%s' respectively",
                 (Int) TNAM_OBJ(output),
                 (Int) TNAM_OBJ(table));
  }

  UInt n = LEN_LIST(table);

  if (LEN_LIST(output) != n) {
    ErrorMayQuit("the arguments <output> and <table> must have the same "
                 "dimensions but found lengths %d and %d, respectively",
                 LEN_LIST(output),
                 LEN_LIST(table));
  }

  for (UInt i = 1; i <= n; i++) {
    if (!IS_LIST(ELM_LIST(output, i))) {
      ErrorMayQuit("the elements of the first argument <output> must be lists, "
                   "but found an element of type '%s' at position %d",
                   (Int) TNAM_OBJ(ELM_LIST(output, i)),
                   i);
    }
    if (!IS_LIST(ELM_LIST(table, i))) {
      ErrorMayQuit("the elements of the second argument <table> must be lists, "
                   "but found an element of type '%s' at position %d",
                   (Int) TNAM_OBJ(ELM_LIST(table, i)),
                   i);
    }
    if (LEN_LIST(ELM_LIST(output, i)) != n) {
      ErrorMayQuit("the first argument <output> must be a square table, but "
                   "found a list of length %d at position %d",
                   LEN_LIST(ELM_LIST(output, i)),
                   i);
    }
    if (LEN_LIST(ELM_LIST(table, i)) != n) {
      ErrorMayQuit("the first argument <output> must be a square table, but "
                   "found a list of length %d at position %d",
                   LEN_LIST(ELM_LIST(table, i)),
                   i);
    }
    for (UInt j = 1; j <= n; j++) {
      Obj elem = ELM_LIST(ELM_LIST(table, i), j);
      if (!IS_INTOBJ(elem) || INT_INTOBJ(elem) < 1 || INT_INTOBJ(elem) > n) {
        ErrorMayQuit("all entries in the first argument <table> must be "
                     "positive integers from 1 to "
                     "Size(<table>) = %d",
                     LEN_LIST(table),
                     0L);
      }
    }
  }
  if (!IS_PERM(p)) {
    ErrorMayQuit("the argument <p> must be a permutation", 0L, 0L);
  }

  PermuteMultiplicationTableNC(self, output, table, p);
  return 0L;
}
