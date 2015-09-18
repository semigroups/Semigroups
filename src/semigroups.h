/*
 * Semigroups GAP package
 *
 * This file contains the kernel module for the Semigroup package. 
 *
 */

//#define NDEBUG 

#ifndef SEMIGROUPS_GAP_H
#define SEMIGROUPS_GAP_H 1

#include "gap-debug.h"

#include "data.h"
#include "interface.h"
#include "types.h"

/*******************************************************************************
 * Import from GAP and Orb for hashing
*******************************************************************************/

#include "src/compiled.h" 

extern "C" {
  Obj HTValue_TreeHash_C ( Obj self, Obj ht, Obj obj );
  Obj HTAdd_TreeHash_C ( Obj self, Obj ht, Obj obj, Obj val);
}

/*******************************************************************************
 * free a Bag of type T_SEMI - this has to go here so that InterfaceBase etc
 * are defined.
*******************************************************************************/

void SemigroupsBagFreeFunc(Obj o) { 
  if (IS_CONVERTER_BAG(o)) {
    delete CLASS_OBJ<Converter>(o);
  } else if (IS_SEMIGROUP_BAG(o)) {
    delete CLASS_OBJ<Semigroup>(o);
  }
}

Obj enumerate_semigroup (Obj self, Obj data, Obj limit, Obj lookfunc, Obj looking);

#endif
