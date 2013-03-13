
const char * Revision_semigroups_c =
   "$Id: semigroups.c,v$";

#include <stdlib.h>
#include "src/compiled.h"

/* import the type from GAP */
Obj BipartitionType;

/*******************************************************************************
** Internal functions
*******************************************************************************/


/* create a new transformation */
static inline Obj NEW_UInt2(Int len, Obj type)
{
    Obj f;

    f = NewBag(T_DATOBJ, sizeof(UInt2)*(len)+sizeof(UInt));
    TYPE_DATOBJ(f) = type;
    return f;
}

/* retrieve entry pos of internal rep of partial trans f */
static inline UInt2 ELM_UInt2(Obj f, Int pos)
{
    UInt2 *data = (UInt2 *) (ADDR_OBJ(f) + 1);
    return data[pos-1];
}

/* define entry pos of internal rep of partial trans f to be nr */
static inline void SET_ELM_UInt2(Obj f, Int pos, UInt2 nr)
{
    UInt2 *data = (UInt2 *) (ADDR_OBJ(f) + 1);
    data[pos-1] = nr;
}

/* error if 65535 points are exceeded */
static inline Int TOO_MANY_PTS_ERROR_BP(int cond)
{
  if(cond)
  {
    ErrorQuit("usage: can only create bipartitions on at most 65535 pts,",
      0L, 0L);
  }
  return 0L;
}
    
/* comparison for qsort */
int cmp (const void *a, const void *b)
{ UInt2 aa, bb;

 aa = *((const UInt2 *)a);
 bb = *((const UInt2 *)b);
 return (int) (aa-bb);
}

/******************************************************************************
* A bipartition of 2n is stored in the following way:
* One plain list l, the first 2 entries are the degree (total number of
* points) and rank (number of classes) and the remaining 2n entries say, in
* which part each of the
* numbers 1..2n lies. Parts are
* numbered from 1 to 2*n and the parts (sets of numbers) are
* numbered in lexicographically ascending order.
*
* Example: If n=3, then the bipartition [[1,3,4],[2,6],[5]] of [1..6]
*          would be stored as:
*          [6,3, 1,2,1,1,3,2]
*          since part [1,3,4] is the first part, part [2,6] is the
*          second part, and part [5] is the third part, there are
*          altogether three parts.*
******************************************************************************/

/*******************************************************************************
** Macros for bipartitions specifically
*******************************************************************************/

/* length of trans internal rep */
static inline Int LEN_BP(Obj f)
{
  return ELM_UInt2(f,1)+2;
}

/* from MN's PartitionExtRep */

Obj FuncBipartitionByPartitionNC(Obj self, Obj partition)
{ UInt2 rank, deg, i, j; 
  Obj f, class;

  rank=LEN_LIST(partition);
  deg=0;

  for(i=1;i<=rank;i++) deg=deg+LEN_LIST(ELM_LIST(partition, i));
  
  f=NEW_UInt2(deg+2, BipartitionType);
  SET_ELM_UInt2(f, 1, deg);
  SET_ELM_UInt2(f, 2, rank);

  for(i=1;i<=rank;i++){
    class=ELM_LIST(partition, i);
    for(j=1;j<=LEN_LIST(class);j++){
      SET_ELM_UInt2(f, (UInt2) INT_INTOBJ(ELM_LIST(class, j))+2, i);
    }
  }

  return f;
}

Obj FuncBipartitionByIntRepNC(Obj self, Obj list)
{ UInt2 rank, deg, i, j; 
  Obj f;

  rank=0;
  deg=LEN_LIST(list);

  f=NEW_UInt2(deg+2, BipartitionType);
  SET_ELM_UInt2(f, 1, deg);

  for(i=1;i<=deg;i++){
    j=INT_INTOBJ(ELM_LIST(list, i));
    SET_ELM_UInt2(f, i+2, (UInt2) j);
    if(j>rank) rank=j;
  }

  SET_ELM_UInt2(f, 2, rank);
  return f;
}

/* method for f[i] */
Obj FuncELM_LIST_BP( Obj self, Obj f, Obj i)
{ 
  if(INT_INTOBJ(i)>LEN_BP(f)) return Fail;
  return INTOBJ_INT(ELM_UInt2(f, INT_INTOBJ(i)));
}

/* method for f{list} */
Obj FuncELMS_LIST_BP(Obj self, Obj f, Obj list)
{   Int len, i;
    Obj out;
    
    len = LEN_LIST(list);
    if(len>LEN_BP(f)) len = LEN_BP(f);
    out = NEW_PLIST(T_PLIST_CYC, len);
    SET_LEN_PLIST(out, len);
    
    for(i=1;i<=len;i++){
      SET_ELM_PLIST(out,i,  
        INTOBJ_INT(ELM_UInt2(f, INT_INTOBJ(ELM_LIST(list, i)))));
    }

    return out;
}

/* i^f */ 
Obj FuncOnPointsBP(Obj self, Obj i, Obj f)
{   UInt2 j, deg, r, k;
    Obj out;
    
    j=INT_INTOBJ(i);
    deg=ELM_UInt2(f, 1)/2;
    if(j>deg) return Fail;
    j=ELM_UInt2(f, 2+j);
    out=NEW_PLIST(T_PLIST_CYC, deg);
    r=0;

    for(k=3;k<=deg+2;k++){
      if(ELM_UInt2(f, k)==j){
        r++;
        SET_ELM_PLIST(out, r, INTOBJ_INT(k-2));
      }
    }

    SET_LEN_PLIST(out, (Int) r);
    return out;
}

/* equality test for bipartitions */
Obj FuncEqBP (Obj self, Obj f, Obj g)
{ UInt2 n=ELM_UInt2(f, 1);
  UInt2 i;

  if(n!=ELM_UInt2(g,1)) return False;
  if(n==1) return True;

  /* search for a difference */
  for(i=2;i<=n+2;i++) if(ELM_UInt2(f,i)!=ELM_UInt2(g,i)) return False;
  return True;
}

/* less than or equal */
Obj FuncLeqBP(Obj self, Obj f, Obj g)
{ UInt2 i, j, k;

  for(i=1;i<=ELM_UInt2(f,1)+2;i++){
    j=ELM_UInt2(f,i);
    k=ELM_UInt2(g,i);
    if(j!=k) return (j<k?True:False);
  }
  return False;
}

/*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * */

/******************************************************************************
*V  GVarFuncs . . . . . . . . . . . . . . . . . . list of functions to export
*/
static StructGVarFunc GVarFuncs [] = {

  /* bipartitions start here */

  { "BipartitionByPartitionNC", 1, "partition",
    FuncBipartitionByPartitionNC, 
    "pkg/semigroups/src/semigroups.c:FuncBipartitionByPartitionNC" },

  { "BipartitionByIntRepNC", 1, "list",
    FuncBipartitionByIntRepNC, 
    "pkg/semigroups/src/semigroups.c:FuncBipartitionByIntRepNC" },

  { "ELM_LIST_BP", 2, "f,i",
    FuncELM_LIST_BP,
    "pkg/semigroups/src/semigroups.c:ELM_LIST_BP" },
  
  { "ELMS_LIST_BP", 2, "f,list",
    FuncELMS_LIST_BP,
    "pkg/semigroups/src/semigroups.c:ELMS_LIST_BP" },
  
  { "OnPointsBP", 2, "i,f",
    FuncOnPointsBP,
    "pkg/semigroups/src/semigroups.c:FuncOnPointsBP" },

  { 0 }

};

/******************************************************************************
*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel ( StructInitInfo *module )
{
    /* init filters and functions                                          */
    InitHdlrFuncsFromTable( GVarFuncs );

    ImportGVarFromLibrary( "BipartitionType", &BipartitionType );

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitLibrary( <module> ) . . . . . . .  initialise library data structures
*/
static Int InitLibrary ( StructInitInfo *module )
{
    Int             i, gvar;
    Obj             tmp;

    /* init filters and functions */
    for ( i = 0;  GVarFuncs[i].name != 0;  i++ ) {
      gvar = GVarName(GVarFuncs[i].name);
      AssGVar(gvar,NewFunctionC( GVarFuncs[i].name, GVarFuncs[i].nargs,
                                 GVarFuncs[i].args, GVarFuncs[i].handler ) );
      MakeReadOnlyGVar(gvar);
    }

    tmp = NEW_PREC(0);
    gvar = GVarName("SEMIGROUPSC"); AssGVar( gvar, tmp ); MakeReadOnlyGVar(gvar);

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitInfopl()  . . . . . . . . . . . . . . . . . table of init functions
*/
static StructInitInfo module = {
#ifdef SEMIGROUPSSTATIC
 /* type        = */ MODULE_STATIC,
#else
 /* type        = */ MODULE_DYNAMIC,
#endif
 /* name        = */ "semigroups",
 /* revision_c  = */ 0,
 /* revision_h  = */ 0,
 /* version     = */ 0,
 /* crc         = */ 0,
 /* initKernel  = */ InitKernel,
 /* initLibrary = */ InitLibrary,
 /* checkInit   = */ 0,
 /* preSave     = */ 0,
 /* postSave    = */ 0,
 /* postRestore = */ 0
};

#ifndef SEMIGROUPSSTATIC
StructInitInfo * Init__Dynamic ( void )
{
  module.revision_c = Revision_semigroups_c;
  return &module;
}
#endif

StructInitInfo * Init__semigroups ( void )
{
  module.revision_c = Revision_semigroups_c;
  return &module;
}

