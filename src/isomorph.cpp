#include "gap_all.h"
#include "isomorph.hpp"


Obj PermuteMultiplicationTable(Obj self, Obj temp, Obj M, Obj p) {
    int n = LEN_LIST(M);

    if (LEN_LIST(temp) != n) {
        ErrorQuit("the arguments <temp> and <table> must have the same dimensions", 0L, 0L);
    }
    for (int i = 1; i <= n; i++) {
        if (LEN_LIST(ELM_LIST(temp, i)) != n) {
            ErrorQuit("the arguments <temp> and <table> must have the same dimensions", 0L, 0L);
        }
    }

    if (IS_PERM(p) == 0) {
        ErrorQuit("the argument <p> must be a permutation", 0L, 0L);
    }
  
    int p_type = TNUM_OBJ(p);
    if (p_type == T_PERM2) {
        int deg_p = DEG_PERM2(p);
        Obj q = STOREDINV_PERM(p);
        if (q == 0) {
            q = INV(p);
            SET_STOREDINV_PERM(p, q);
          }
        int deg_q = DEG_PERM2(q);
        for (int i = 1; i <= n; i++) {
          Obj row = ELM_LIST(temp, i);
          int ii = IMAGE(i - 1, CONST_ADDR_PERM2(q), deg_q) + 1;
      
          for (int j = 1; j <= n; j++) {
            int home = Int_ObjInt(ELM_LIST(ELM_LIST(M, ii), IMAGE(j - 1, CONST_ADDR_PERM2(q), deg_q) + 1));
            int new_val = IMAGE(home - 1, CONST_ADDR_PERM2(p), deg_p) + 1;
            ASS_LIST(row, j, INTOBJ_INT(new_val));
          }
        }
      } else {
        int deg_p = DEG_PERM4(p);
        Obj q = STOREDINV_PERM(p);
        if (q == 0) {
            q = INV(p);
            SET_STOREDINV_PERM(p, q);
          }
        int deg_q = DEG_PERM4(q);
        for (int i = 1; i <= n; i++) {
          Obj row = ELM_LIST(temp, i);
          int ii = IMAGE(i - 1, CONST_ADDR_PERM4(q), deg_q) + 1;
      
          for (int j = 1; j <= n; j++) {
            int home = Int_ObjInt(ELM_LIST(ELM_LIST(M, ii), IMAGE(j - 1, CONST_ADDR_PERM4(q), deg_q) + 1));
            int new_val = IMAGE(home - 1, CONST_ADDR_PERM4(p), deg_p) + 1;
            ASS_LIST(row, j, INTOBJ_INT(new_val));
          }
        }
      }
    CHANGED_BAG(temp);
    return 0L;
  }