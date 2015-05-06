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
 * GAP TNUM for wrapping C++ semigroup
*******************************************************************************/

#ifndef T_SEMI
#define T_SEMI T_SPARE2
#endif

#endif
