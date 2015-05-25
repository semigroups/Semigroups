/*******************************************************************************
 * For debugging access to GAP macros
*******************************************************************************/

#ifndef SEMIGROUPS_GAP_DEBUG_H
#define SEMIGROUPS_GAP_DEBUG_H 1

#include "src/compiled.h"

//#ifdef DEBUG

Bag TYPE_COMOBJ_F (Obj x) {
  return TYPE_COMOBJ(x);
}

Bag TYPE_POSOBJ_F (Obj x) {
  return TYPE_POSOBJ(x);
}

unsigned long long TNUM_OBJ_F (Obj x) {
  return TNUM_OBJ(x);
}

unsigned long long T_PLIST_F (Obj x) {
  return T_PLIST;
}

bool IS_PLIST_F (Obj x) {
  return IS_PLIST(x);
}

unsigned long long LEN_PLIST_F (Obj x) {
  return LEN_PLIST(x);
}

bool IS_LIST_F (Obj x) {
  return IS_LIST(x);
}

bool IS_COMOBJ_F (Obj x) {
  return IS_COMOBJ(x);
}

size_t LEN_BLIST_F (Obj blist) {
  return LEN_BLIST(blist);
}

bool IS_BLIST_REP_F (Obj blist) {
  return IS_BLIST_REP(blist);
}

Obj ELM_PLIST_F (Obj plist, size_t pos) {
  return ELM_PLIST(plist, pos);
}

size_t INT_INTOBJ_F (Obj int_obj) {
  return INT_INTOBJ(int_obj);
}

Obj CALL_1ARGS_F (Obj func, Obj arg1) {
  return CALL_1ARGS(func, arg1);
}

Obj CALL_2ARGS_F (Obj func, Obj arg1, Obj arg2) {
  return CALL_2ARGS(func, arg1, arg2);
}

#endif
