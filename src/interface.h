/*
 * Semigroups GAP package
 *
 * This file contains an interface between GAP and Semigroups++.
 *
 */

#ifndef SEMIGROUPS_GAP_INTERFACE_H
#define SEMIGROUPS_GAP_INTERFACE_H

//#define NDEBUG 
#include "semigroups.h"

extern "C" {
  #include "src/compiled.h"          /* GAP headers                */
  Obj HTValue_TreeHash_C ( Obj self, Obj ht, Obj obj );
  Obj HTAdd_TreeHash_C ( Obj self, Obj ht, Obj obj, Obj val);
}

#ifndef T_SEMI
#define T_SEMI T_SPARE2
#endif

enum SemigroupType {
  UNKNOWN = 0,
  SEMI_TRANS2 = 1,
  SEMI_TRANS4 = 2,
  SEMI_BIPART = 3
};

//#define IS_SEMI(o) (TNUM_OBJ(o) == T_SEMI);
//#define IS_TRANS_SEMI(o) (IS_SEMI(o) && (Int)(ADDR_OBJ(o)[0]) == SEMI_TRANS);
//#define IS_PPERM_SEMI(o) (IS_SEMI(o) && (Int)(ADDR_OBJ(o)[0]) == SEMI_PPERM);

// wrap one of our semigroups inside a GAP bag


#endif
