/*
 * semigroups: Methods for semigroups
 */

#ifndef GAP_SEMIGROUPS_H
#define GAP_SEMIGROUPS_H

#include <vector>
#include <iostream>
#include "elements.h"
#include "basics.h"

extern "C" {
  #include "src/compiled.h"          /* GAP headers                */
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

Element<u_int16_t>* NewTransformation2 (Obj o, u_int16_t n) {
  assert(TNUM_OBJ(o) == T_TRANS2);
  assert(DEG_TRANS2(o) <= n);
  std::vector<u_int16_t> image;
  UInt2* ptf = ADDR_TRANS2(o);
  for (size_t i = 0; i < DEG_TRANS2(o); i++) {
    image.push_back(ptf[i]);
  }
  for (size_t i = DEG_TRANS2(o); i < n; i++) {
    image.push_back(i);
  }
  return new Transformation<u_int16_t>(image);
}

#endif
