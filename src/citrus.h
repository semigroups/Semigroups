
#include <stdlib.h>
#include "src/compiled.h" 

/* import the type from GAP */
Obj PartialPermType;
Obj BipartitionType;

/*******************************************************************************
** Macros used in citrus.c
*******************************************************************************/

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

