/*******************************************************************************
 * For debugging access to GAP macros
*******************************************************************************/

#ifdef DEBUG

Bag TYPE_COMOBJ_FUNC (Obj x) {
  return TYPE_COMOBJ(x);
}

Bag TYPE_POSOBJ_FUNC (Obj x) {
  return TYPE_POSOBJ(x);
}

unsigned long long TNUM_OBJ_FUNC (Obj x) {
  return TNUM_OBJ(x);
}

bool IS_PLIST_FUNC (Obj x) {
  return IS_PLIST(x);
}

bool IS_COMOBJ_FUNC (Obj x) {
  return IS_COMOBJ(x);
}

size_t LEN_BLIST_FUNC (Obj blist) {
  return LEN_BLIST(blist);
}

bool IS_BLIST_REP_FUNC (Obj blist) {
  return IS_BLIST_REP(blist);
}

Obj ELM_PLIST_FUNC (Obj plist, size_t pos) {
  return ELM_PLIST(plist, pos);
}

#endif
