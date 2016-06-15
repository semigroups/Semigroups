/*******************************************************************************
 * For debugging access to GAP macros
*******************************************************************************/

#ifndef SRC_GAP_DEBUG_H_
#define SRC_GAP_DEBUG_H_

#include "src/compiled.h"

static Bag* ADDR_OBJ_F (Obj x) {
  return ADDR_OBJ(x);
}

static UInt SIZE_OBJ_F (Obj x) {
  return SIZE_OBJ(x);
}

static Bag TYPE_COMOBJ_F (Obj x) {
  return TYPE_COMOBJ(x);
}

static Bag TYPE_POSOBJ_F (Obj x) {
  return TYPE_POSOBJ(x);
}

static unsigned long long TNUM_OBJ_F (Obj x) {
  return TNUM_OBJ(x);
}

static unsigned long long T_PLIST_F (Obj x) {
  return T_PLIST;
}

static bool IS_PLIST_F (Obj x) {
  return IS_PLIST(x);
}

static Int LEN_PLIST_F (Obj x) {
  return LEN_PLIST(x);
}

static bool IS_LIST_F (Obj x) {
  return IS_LIST(x);
}

static Int LEN_LIST_F (Obj x) {
  return LEN_LIST(x);
}

static bool IS_COMOBJ_F (Obj x) {
  return IS_COMOBJ(x);
}

static size_t LEN_BLIST_F (Obj blist) {
  return LEN_BLIST(blist);
}

static bool IS_BLIST_REP_F (Obj blist) {
  return IS_BLIST_REP(blist);
}

static Obj ELM_PLIST_F (Obj plist, size_t pos) {
  return ELM_PLIST(plist, pos);
}

static size_t INT_INTOBJ_F (Obj int_obj) {
  return INT_INTOBJ(int_obj);
}

static Obj INTOBJ_INT_F (UInt i) {
  return INTOBJ_INT(i);
}

static bool IsbPRec_F (Obj rec, UInt rnam) {
  return IsbPRec(rec, rnam);
}

static bool IS_PREC_REP_F (Obj o) {
  return IS_PREC_REP(o);
}

static size_t LEN_PREC_F (Obj o) {
  return LEN_PREC(o);
}

static Obj CALL_1ARGS_F (Obj func, Obj arg1) {
  return CALL_1ARGS(func, arg1);
}

static Obj CALL_2ARGS_F (Obj func, Obj arg1, Obj arg2) {
  return CALL_2ARGS(func, arg1, arg2);
}

static size_t DEG_TRANS_F (Obj t) {
  return DEG_TRANS(t);
}

static size_t DEG_PERM4_F (Obj t) {
  return DEG_PERM4(t);
}

#endif // SRC_GAP_DEBUG_H_
