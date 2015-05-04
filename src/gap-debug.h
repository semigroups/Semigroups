#ifdef DEBUG

Bag TypeComObjFunc (Obj x) {
  return TYPE_COMOBJ(x);
}

Bag TypePosObjFunc (Obj x) {
  return TYPE_POSOBJ(x);
}

unsigned long long TNUM_OBJFunc (Obj x) {
  return TNUM_OBJ(x);
}

bool IS_PLISTFunc (Obj x) {
  return IS_PLIST(x);
}

bool IS_COMOBJFunc (Obj x) {
  return IS_COMOBJ(x);
}

#endif
