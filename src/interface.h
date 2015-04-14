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

#endif
