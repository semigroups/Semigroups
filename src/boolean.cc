/*
 * Semigroups GAP package
 *
 * This file contains some methods for Boolean matrices
 *
 */

#include "boolean.h"

Obj IS_COL_TRIM_BOOLEAN_MAT (Obj self, Obj x) {
  assert(IS_BOOL_MAT(x));
  size_t n = LEN_BLIST(ELM_PLIST(x, 1));

  for (size_t i = 1; i < n; i++) {
    for (size_t j = i + 1; j <= n; j++) {
      size_t k;
      bool contained = true;
      for (k = 1; k <= n; k++) {
        Obj row = ELM_PLIST(x, k);
        if ((ELM_BLIST(row, j) == True && ELM_BLIST(row, i) == False)) {
          contained = false;
          break;
        }
      }
      if (contained) {
        return False;
      }
    }
  }
  return True;
}

Obj HASH_FUNC_FOR_BLIST (Obj self, Obj blist, Obj data_gap) {

  size_t res  = 0;
  UInt   len  = NUMBER_BLOCKS_BLIST(blist);
  UInt*  ptr  = BLOCKS_BLIST(blist);
  UInt   data = INT_INTOBJ(data_gap);

  while (len > 0) {
    res += (*(ptr++) * 23) % data;
    len--;
  }
  return INTOBJ_INT(res + 1);
}
