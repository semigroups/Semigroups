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
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

#include "isomorph.hpp"
#include "gap_all.h"
#include "semigroups-debug.hpp"  // for SEMIGROUPS_ASSERT

Obj PermuteMultiplicationTable(Obj self, Obj temp, Obj table, Obj p) {
  if (IS_LIST(temp) == 0 || IS_LIST(table) == 0) {
    ErrorQuit("the arguments <temp> and <table> must be lists", 0L, 0L);
  }

  int n = LEN_LIST(table);

  if (LEN_LIST(temp) != n) {
    ErrorQuit("the arguments <temp> and <table> must have the same dimensions",
              0L,
              0L);
  }

  for (int i = 1; i <= n; i++) {
    if (IS_LIST(ELM_LIST(temp, i)) == 0 || IS_LIST(ELM_LIST(table, i)) == 0) {
      ErrorQuit("the elements of <temp> and <table> must be lists", 0L, 0L);
    }
    if (LEN_LIST(ELM_LIST(temp, i)) != n || LEN_LIST(ELM_LIST(table, i)) != n) {
      ErrorQuit("the arguments <temp> and <table> must be square tables of the "
                "same dimensions",
                0L,
                0L);
    }
    for (int j = 1; j <= n; j++) {
      Obj elem = ELM_LIST(ELM_LIST(table, i), j);
      if (!IS_INTOBJ(elem) || Int_ObjInt(elem) < 1 || Int_ObjInt(elem) > n) {
        ErrorQuit("all entries in <table> must be positive integers from 1 to "
                  "Size(<table>)",
                  0L,
                  0L);
      }
    }
  }

  if (IS_PERM(p) == 0) {
    ErrorQuit("the argument <p> must be a permutation", 0L, 0L);
  }

  int p_type = TNUM_OBJ(p);
  if (p_type == T_PERM2) {
    int deg_p = DEG_PERM2(p);
    Obj q     = STOREDINV_PERM(p);
    if (q == 0) {
      q = INV(p);
      SET_STOREDINV_PERM(p, q);
    }
    SEMIGROUPS_ASSERT(TNUM_OBJ(q) == T_PERM2);
    int deg_q = DEG_PERM2(q);
    for (int i = 1; i <= n; i++) {
      Obj row = ELM_LIST(temp, i);
      int ii  = IMAGE(i - 1, CONST_ADDR_PERM2(q), deg_q) + 1;

      for (int j = 1; j <= n; j++) {
        int home    = Int_ObjInt(ELM_LIST(
            ELM_LIST(table, ii), IMAGE(j - 1, CONST_ADDR_PERM2(q), deg_q) + 1));
        int new_val = IMAGE(home - 1, CONST_ADDR_PERM2(p), deg_p) + 1;
        ASS_LIST(row, j, INTOBJ_INT(new_val));
      }
    }
  } else {
    int deg_p = DEG_PERM4(p);
    Obj q     = STOREDINV_PERM(p);
    if (q == 0) {
      q = INV(p);
      SET_STOREDINV_PERM(p, q);
    }
    SEMIGROUPS_ASSERT(TNUM_OBJ(q) == T_PERM4);
    int deg_q = DEG_PERM4(q);
    for (int i = 1; i <= n; i++) {
      Obj row = ELM_LIST(temp, i);
      int ii  = IMAGE(i - 1, CONST_ADDR_PERM4(q), deg_q) + 1;

      for (int j = 1; j <= n; j++) {
        int home    = Int_ObjInt(ELM_LIST(
            ELM_LIST(table, ii), IMAGE(j - 1, CONST_ADDR_PERM4(q), deg_q) + 1));
        int new_val = IMAGE(home - 1, CONST_ADDR_PERM4(p), deg_p) + 1;
        ASS_LIST(row, j, INTOBJ_INT(new_val));
      }
    }
  }
  CHANGED_BAG(temp);
  return 0L;
}
