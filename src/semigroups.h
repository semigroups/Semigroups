/*
 * semigroups: Methods for semigroups
 */

#ifndef GAP_SEMIGROUPS_H
#define GAP_SEMIGROUPS_H

#include <vector>
#include <iostream>
extern "C" {
  #include "src/compiled.h"          /* GAP headers                */
}

#ifndef T_SEMI
#define T_SEMI T_SPARE2
#endif

template<typename T>
inline void SET_WRAP(Obj o, T* p) {
    ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(p);
}

template<typename T>
inline T* GET_WRAP(Obj o) {
    return reinterpret_cast<T*>(ADDR_OBJ(o)[0]);
}

#endif
