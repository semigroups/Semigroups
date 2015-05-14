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

/*******************************************************************************
 * Import from GAP and Orb for hashing
*******************************************************************************/

extern "C" {
  #include "src/compiled.h"          /* GAP headers                */
  Obj HTValue_TreeHash_C ( Obj self, Obj ht, Obj obj );
  Obj HTAdd_TreeHash_C ( Obj self, Obj ht, Obj obj, Obj val);
}

/*******************************************************************************
 * Class for the interface from a GAP semigroup to a C++ semigroup and back 
*******************************************************************************/

class InterfaceBase {
  public:
    virtual ~InterfaceBase () {};
    virtual void enumerate (Obj limit) = 0;
    virtual bool is_done () = 0;
    virtual void find (Obj data, Obj lookfunc, Obj start, Obj end) = 0;
    virtual size_t size () = 0;
    virtual size_t current_size () = 0;
    virtual size_t nrrules () = 0;
    virtual void right_cayley_graph (Obj data) = 0;
    virtual void left_cayley_graph (Obj data) = 0;
    virtual void elements (Obj data, Obj limit) = 0;
    virtual Obj position (Obj data, Obj x) = 0;
    virtual void word (Obj data, Obj pos) = 0;
    virtual void relations (Obj data) = 0;
};

/*******************************************************************************
 * GAP TNUM for wrapping C++ semigroup
*******************************************************************************/

#ifndef T_SEMI
#define T_SEMI T_SPARE2
#endif

class UFData;

enum SemigroupsBagType {
  INTERFACE = 0,
  UF_DATA   = 1
};

// put C++ object into GAP object

Obj NewInterfaceBag (InterfaceBase* interface) {
  Obj o = NewBag(T_SEMI, 2 * sizeof(Obj));
  ADDR_OBJ(o)[0] = (Obj)INTERFACE;
  ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(interface);
  return o;
}

Obj NewUFDataBag (UFData* ufdata) {
  Obj o = NewBag(T_SEMI, 2 * sizeof(Obj));
  ADDR_OBJ(o)[0] = (Obj)UF_DATA;
  ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(ufdata);
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
