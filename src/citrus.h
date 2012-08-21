
#include <stdlib.h>
#include "src/compiled.h" 
#include "src/permutat.h"

/* import the type from GAP */
Obj PartialPermType;
Obj BipartitionType;

/* define the type for entries in part. perm */
typedef UInt2 pptype;

/*******************************************************************************
** Macros used in citrus.c
*******************************************************************************/

/* create a new partial perm */
static inline Obj NEW_PP(Int len)
{
    Obj f;

    f = NewBag(T_DATOBJ, sizeof(pptype)*(len)+sizeof(UInt));
    TYPE_DATOBJ(f) = PartialPermType;
    return f;
}

/* create a new bipartition */
static inline Obj NEW_BP(Int len)
{
    Obj f;

    f = NewBag(T_DATOBJ, sizeof(pptype)*(len)+sizeof(UInt));
    TYPE_DATOBJ(f) = BipartitionType;
    return f;
}

/* create a new empty partial trans */
static inline Obj NEW_EMPTY_PP()
{ pptype i;
  Obj f;
  f = NewBag(T_DATOBJ, sizeof(pptype)*7+sizeof(UInt));
  TYPE_DATOBJ(f) = PartialPermType;
  for(i=1;i<=8;i++)
    SET_ELM_T(f, i, (pptype) 0);
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
static inline Int TOO_MANY_PTS_ERROR_PP(int cond)
{
  if(cond)
  { 
    ErrorQuit("usage: can only create partial perms on at most 65535 pts,", 
      0L, 0L); 
  }
  return 0L;
}

