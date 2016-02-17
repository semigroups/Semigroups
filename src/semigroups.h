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

/*extern "C" {
  Obj HTValue_TreeHash_C ( Obj self, Obj ht, Obj obj );
  Obj HTAdd_TreeHash_C ( Obj self, Obj ht, Obj obj, Obj val);
}*/

/*******************************************************************************
 * free a Bag of type T_SEMI - this has to go here so that InterfaceBase etc
 * are defined.
*******************************************************************************/

void SemigroupsBagFreeFunc (Obj o) {
  if (IS_CONVERTER_BAG(o)) {
    delete CLASS_OBJ<Converter>(o);
  } else if (IS_SEMIGROUP_BAG(o)) {
    delete CLASS_OBJ<Semigroup>(o);
  } else if (IS_GAP_BIPART_BAG(o)) {
    delete CLASS_OBJ<Bipartition>(o);
  } else if (IS_GAP_BLOCKS_BAG(o)) {
    delete CLASS_OBJ<Blocks>(o);
  }
}

void SemigroupsMarkSubBags (Obj o) {
  if ((SIZE_OBJ(o) / sizeof(Obj)) > 2) {
    for (size_t i = 2; i < (SIZE_OBJ(o) / sizeof(Obj)); i++) {
      if (ADDR_OBJ(o)[i] != NULL) {
        MARK_BAG(ADDR_OBJ(o)[i]);
      }
    }
  }
}

Obj enumerate_semigroup (Obj self, Obj data, Obj limit, Obj lookfunc, Obj looking);

#endif
