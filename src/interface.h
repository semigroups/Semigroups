/*
 * Semigroups GAP package
 *
 * This file contains an interface between GAP and Semigroups++.
 *
 */

#ifndef SEMIGROUPS_GAP_INTERFACE_H
#define SEMIGROUPS_GAP_INTERFACE_H

#include "semigroups.h"

extern "C" {
  #include "src/compiled.h"          /* GAP headers                */
  Obj HTValue_TreeHash_C ( Obj self, Obj ht, Obj obj );
  Obj HTAdd_TreeHash_C ( Obj self, Obj ht, Obj obj, Obj val);
}

#ifndef T_SEMI
#define T_SEMI T_SPARE2
#endif

// wrap one of our objects inside a GAP bag

template<typename T>
inline void SET_WRAP(Obj o, T* p) {
    ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(p);
}

template<typename T>
inline T* GET_WRAP(Obj o) {
    return reinterpret_cast<T*>(ADDR_OBJ(o)[0]);
}

#endif
