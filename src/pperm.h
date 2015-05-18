/*
 * Semigroups GAP package
 *
 * This file contains some missing macros from pperm.c in gap/src
 *
 */

/*******************************************************************************
 * PPerm macros in case they are not in pperm.h
*******************************************************************************/

#ifndef NEW_PPERM2 

#define NEW_PPERM2(deg)   NewBag(T_PPERM2, (deg+1)*sizeof(UInt2)+2*sizeof(Obj))
#define CODEG_PPERM2(f)   (*(UInt2*)((Obj*)(ADDR_OBJ(f))+2))
#define DEG_PPERM2(f)     ((UInt)(SIZE_OBJ(f)-sizeof(UInt2)-2*sizeof(Obj))/sizeof(UInt2))
#define NEW_PPERM4(deg)   NewBag(T_PPERM4, (deg+1)*sizeof(UInt4)+2*sizeof(Obj))
#define CODEG_PPERM4(f)   (*(UInt4*)((Obj*)(ADDR_OBJ(f))+2))
#define DEG_PPERM4(f)     ((UInt)(SIZE_OBJ(f)-sizeof(UInt4)-2*sizeof(Obj))/sizeof(UInt4))

#define IS_PPERM(f)       (TNUM_OBJ(f)==T_PPERM2||TNUM_OBJ(f)==T_PPERM4)
#define DEG_PPERM(f)      (TNUM_OBJ(f)==T_PPERM2?DEG_PPERM2(f):DEG_PPERM4(f))

#endif
