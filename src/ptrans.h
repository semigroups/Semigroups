
#include <stdlib.h>
#include "src/compiled.h" 

/* from permutat.c should be in permutat.h */
#define IMAGE(i,pt,dg)  (((i) < (dg)) ? (pt)[(i)] : (i))
Obj FuncTRIM_PERM(Obj self, Obj perm, Obj n);
Obj FuncLARGEST_MOVED_POINT_PERM(Obj self, Obj perm);

/* import the type from GAP */
Obj PartialTransType;
Obj PartialPermType;

/* define the type for entries in part. perm */
typedef UInt2 pttype;

/* comparison for qsort */
int cmp (const void *a, const void *b);

/*******************************************************************************
** Macros used in ptrans.c and pperm.c
*******************************************************************************/

/* retrieve entry pos of internal rep of partial trans f */ 
static inline pttype ELM_PT(Obj f, Int pos) 
{ 
    pttype *data = (pttype *) (ADDR_OBJ(f) + 1); 
    return data[pos-1]; 
} 
 
/* define entry pos of internal rep of partial trans f to be nr */ 
static inline void SET_ELM_PT(Obj f, Int pos, pttype nr) 
{ 
    pttype *data = (pttype *) (ADDR_OBJ(f) + 1); 
    data[pos-1] = nr; 
} 

/* create a new partial trans */
static inline Obj NEW_PT(Int len)
{
    Obj f;

    f = NewBag(T_DATOBJ, sizeof(pttype)*(len)+sizeof(UInt));
    TYPE_DATOBJ(f) = PartialPermType;
    return f;
}

/* create a new empty partial trans */
static inline Obj NEW_EMPTY_PT()
{ pttype i;
  Obj f;
  f = NewBag(T_DATOBJ, sizeof(pttype)*7+sizeof(UInt));
  TYPE_DATOBJ(f) = PartialPermType;
  for(i=1;i<=8;i++)
    SET_ELM_PT(f, i, (pttype) 0);
  return f;
}

/* create a new empty plist */
static inline Obj NEW_EMPTY_PLIST()
{ Obj out;
  out = NEW_PLIST(T_PLIST_EMPTY, 0);
  SET_LEN_PLIST(out, 0);
  return out;
}

/* error if 65535 points are exceeded */
static inline Int TOO_MANY_PTS_ERROR(int cond)
{
  if(cond)
  { 
    ErrorQuit("usage: can only create partial perms on at most 65535 pts,", 
      0L, 0L); 
  }
  return 0L;
}

/*******************************************************************************
** GAP function declarations
*******************************************************************************/

Obj FuncELM_LIST_PT( Obj self, Obj f, Obj i);
Obj FuncELMS_LIST_PT( Obj self, Obj f, Obj i);
Obj FuncELM_LIST_PP( Obj self, Obj f, Obj i);
Obj FuncELMS_LIST_PP(Obj self, Obj f, Obj list);
Obj FuncFullPartialPermNC( Obj self, Obj rep );
Obj FuncSparsePartialPermNC( Obj self, Obj dom, Obj ran );
Obj FuncDensePartialPermNC( Obj self, Obj img );
Obj FuncProdPP( Obj self, Obj f, Obj g );
Obj FuncDomPP (Obj self, Obj f );
Obj FuncRanPP (Obj self, Obj f );
Obj FuncRanSetPP ( Obj self, Obj f );
Obj FuncInvPP ( Obj self, Obj f );
Obj FuncOnIntegerSetsWithPP (Obj self, Obj set, Obj f);
Obj FuncOnIntegerTuplesWithPP (Obj self, Obj set, Obj f);
Obj FuncEqPP (Obj self, Obj f, Obj g);
Obj FuncLeftOne(Obj self, Obj f);
Obj FuncRightOne(Obj self, Obj f);
Obj FuncFixedPointsPP(Obj self, Obj f);
Obj FuncMovedPointsPP(Obj self, Obj f);
Obj FuncNrMovedPointsPP(Obj self, Obj f);
Obj FuncLargestMovedPointPP(Obj self, Obj f);
Obj FuncSmallestMovedPointPP(Obj self, Obj f);
Obj FuncLeqPP(Obj self, Obj f, Obj g);
Obj FuncRestrictedPP(Obj self, Obj f, Obj set);
Obj FuncNaturalLeqPP(Obj self, Obj f, Obj g);
Obj FuncQuoPP(Obj self, Obj f, Obj g);
Obj FuncProdPPPerm(Obj self, Obj f, Obj p);
Obj FuncProdPermPP(Obj self, Obj p, Obj f);
Obj FuncOnPointsPP(Obj self, Obj i, Obj f);



