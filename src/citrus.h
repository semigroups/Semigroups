
#include <stdlib.h>
#include "src/compiled.h" 

/* from permutat.c should be in permutat.h */
#define IMAGE(i,pt,dg)  (((i) < (dg)) ? (pt)[(i)] : (i))
Obj FuncTRIM_PERM(Obj self, Obj perm, Obj n);
Obj FuncLARGEST_MOVED_POINT_PERM(Obj self, Obj perm);

/* import the type from GAP */
Obj PartialPermType;
Obj TransformationType;
Obj BipartitionType;

/* define the type for entries in part. perm */
typedef UInt2 pttype;

/* comparison for qsort */
int cmp (const void *a, const void *b);

/*******************************************************************************
** Macros used in citrus.c
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

/* create a new partial perm */
static inline Obj NEW_PP(Int len)
{
    Obj f;

    f = NewBag(T_DATOBJ, sizeof(pttype)*(len)+sizeof(UInt));
    TYPE_DATOBJ(f) = PartialPermType;
    return f;
}

/* create a new transformation */
static inline Obj NEW_T(Int len)
{
    Obj f;

    f = NewBag(T_DATOBJ, sizeof(pttype)*(len)+sizeof(UInt));
    TYPE_DATOBJ(f) = TransformationType;
    return f;
}

/* create a new bipartition */
static inline Obj NEW_BP(Int len)
{
    Obj f;

    f = NewBag(T_DATOBJ, sizeof(pttype)*(len)+sizeof(UInt));
    TYPE_DATOBJ(f) = BipartitionType;
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

