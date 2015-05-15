/*
 * Semigroups GAP package
 *
 * This file contains the kernel module for the Semigroup package. 
 *
 */

//#define NDEBUG 

#ifndef SEMIGROUPS_GAP_H
#define SEMIGROUPS_GAP_H 1

/*******************************************************************************
 * includes
*******************************************************************************/

#include "gap-debug.h"
#include "interface.h"

/*******************************************************************************
 * Import from GAP and Orb for hashing
*******************************************************************************/

extern "C" {
  #include "src/compiled.h"          /* GAP headers                */
  Obj HTValue_TreeHash_C ( Obj self, Obj ht, Obj obj );
  Obj HTAdd_TreeHash_C ( Obj self, Obj ht, Obj obj, Obj val);
}

/*******************************************************************************
 * GAP TNUM for wrapping C++ semigroup
*******************************************************************************/

#ifndef T_SEMI
#define T_SEMI T_SPARE2
#endif

enum SemigroupsBagType {
  INTERFACE = 0,
  UF_DATA   = 1
};

class UFData;

// put C++ object into GAP object
// TODO move this
Obj NewUFDataBag (UFData* ufdata) {
  Obj o = NewBag(T_SEMI, 2 * sizeof(Obj));
  ADDR_OBJ(o)[0] = (Obj)UF_DATA;
  ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(ufdata);
  return o;
}

Obj NewInterfaceBag (InterfaceBase* interface) {
  Obj o = NewBag(T_SEMI, 2 * sizeof(Obj));
  ADDR_OBJ(o)[0] = (Obj)INTERFACE;
  ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(interface);
  return o;
}

// get C++ Class from GAP object

template <typename Class>
inline Class* CLASS_OBJ(Obj o) {
    return reinterpret_cast<Class*>(ADDR_OBJ(o)[1]);
}

#define IS_T_SEMI(o)        (TNUM_OBJ(o) == T_SEMI)
#define IS_INTERFACE_BAG(o) (IS_T_SEMI(o) && (Int)ADDR_OBJ(o)[0] == INTERFACE)
#define IS_UF_DATA_BAG(o)   (IS_T_SEMI(o) && (Int)ADDR_OBJ(o)[0] == UF_DATA)

// free C++ semigroup inside GAP object

void SemigroupsBagFreeFunc(Obj o) { 
  if (IS_INTERFACE_BAG(o)) {
    delete CLASS_OBJ<InterfaceBase>(o);
  } else {
    delete CLASS_OBJ<UFData>(o);
  }
}

#endif
