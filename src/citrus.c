

const char * Revision_citrus_c =
   "$Id: citrus.c,v$";

#include <stdlib.h>

#include "src/compiled.h" 

/*******************************************************************************
**
** A partial permutation is of the form:
**
** [degree, rank, min ran, max ran, min, max, img list, dom, ran, Set(ran)] 
**
** where <degree> is the length of <img list>, <rank> is the number of none zero
** entries in the <img list>,  <min ran>, <max ran> are
** self explanatory,  <min> is min(<min dom>, <min ran>), <max> is max(<max 
** dom>, <max ran>), <dom> is the domain, <ran> is the range, <Set(ran)> is the 
** range as a set (not calculated until needed), and <img list> is the list of 
** images with 0 for undefined.
*/

Obj FuncDenseCreatePartPerm_C( Obj self, Obj img )
{ 
    Obj f, j;
    Int deg, max_ran, min_ran, rank, i, jj;
    Int ran[513];

    deg = LEN_LIST(img);
    f = NEW_PLIST(T_PLIST_CYC,3*deg+6); /* the output*/
    SET_ELM_PLIST(f, 1, INTOBJ_INT(deg));
    
    max_ran=0; 
    min_ran=deg; 
    rank=0;

    /* find dom, rank, max_ran, min_ran */
    for(i=1;i<=deg;i++){
      j = ELM_LIST(img, i);
      SET_ELM_PLIST(f, i+6, j);
      jj = INT_INTOBJ(j);
      if(jj!=0){
        rank++;
        SET_ELM_PLIST(f,deg+rank+6,INTOBJ_INT(i)); /* dom*/
        ran[rank]=jj;
        if(jj>max_ran){
          max_ran=jj;
          }
        if(jj<min_ran){
          min_ran=jj;
          }
        }
      }

    SET_ELM_PLIST(f,2,INTOBJ_INT(rank));
    SET_ELM_PLIST(f,3,INTOBJ_INT(min_ran));
    SET_ELM_PLIST(f,4,INTOBJ_INT(max_ran));

    /* set range */
    for(i=1;i<=rank;i++){
      SET_ELM_PLIST(f,deg+rank+6+i,INTOBJ_INT(ran[i]));
    } 

    /* set min */
    j=ELM_PLIST(f,deg+7); /* min. dom. */
    if(min_ran<INT_INTOBJ(j)){
      SET_ELM_PLIST(f,5,INTOBJ_INT(min_ran));
    }else{
      SET_ELM_PLIST(f,5,j);
    }
    
    /* set max */
    j=ELM_PLIST(f,deg+rank+6); /* max. dom. */
    if(max_ran>INT_INTOBJ(j)){
      SET_ELM_PLIST(f,6,INTOBJ_INT(max_ran));
    }else{
      SET_ELM_PLIST(f,6,j);
    }

    SET_LEN_PLIST(f,deg+2*rank+6);
    SHRINK_PLIST(f,deg+3*rank+6);
    return f;
}

/* product of partial permutations */

Obj FuncProdPartPerm_C( Obj self, Obj f, Obj g )
{
    Obj fg,ff,gg;
    Int i,j,n,m;

    if (IS_POSOBJ(f)){ 
      ff = ELM_PLIST(f,1);
      gg = ELM_PLIST(g,1);}
    else{
      ff = f;
      gg = g;
    }

    n = LEN_LIST(ff); 
    m = LEN_LIST(gg);
    fg = NEW_PLIST(T_PLIST_CYC,n);
    SET_LEN_PLIST(fg,n);
    /* no garbage collection from here! */
    for (i = 1;i <= n;i++) {
        j = INT_INTOBJ(ELM_LIST(ff,i));
        if(j > m || j == 0){
          SET_ELM_PLIST(fg,i,INTOBJ_INT(0));
        } else {
          SET_ELM_PLIST(fg,i,ELM_LIST(gg,j));
      }
    }
    /* finished */
    return fg;
}

/* inverse of a partial permutation */

Obj FuncInvPartPerm_C ( Obj self, Obj f, Int r)
{
    Obj ff,img;
    Int n,i,j;

    if (IS_POSOBJ(f)){
      ff = ELM_PLIST(f,1);
    }
    else{
      ff = f;
    }

    n=LEN_LIST(ff);
    if(n==0) return NEW_PLIST(T_PLIST_EMPTY,INT_INTOBJ(0)); 

    img=NEW_PLIST(T_PLIST_CYC,INT_INTOBJ(r));
    SET_LEN_PLIST(img,INT_INTOBJ(r));

    for (i = 1;i <= r;i++){
      SET_ELM_PLIST(img, i, INTOBJ_INT(0));
    }

    for (i = 1; i <= n;i++){
      j = INT_INTOBJ(ELM_LIST(ff,i));
      if(j!=0){
        SET_ELM_PLIST(img, j, INTOBJ_INT(i));
      }
    }

    return img;
}

/* on sets for a partial permutation 

Obj FuncOnIntegerSetsWithPartPerm_C (Obj self, Obj f, Obj set)
{

  if (IS_POSOBJ(f)){
    ff = ELM_PLIST(f,1);
  }
  else{
    ff = f;
  }
  
  n = LEN_LIST(ff);
  

} */
    

/*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * */

/******************************************************************************
*V  GVarFuncs . . . . . . . . . . . . . . . . . . list of functions to export
*/
static StructGVarFunc GVarFuncs [] = {

  { "ProdPartPerm_C", 2, "f, g",
    FuncProdPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncProdPartPerm_C" },

  { "DenseCreatePartPerm_C", 1, "img",
    FuncDenseCreatePartPerm_C,
    "pkg/citrus/src/citrus.c:FuncDenseCreatePartPerm_C" },

  { "InvPartPerm_C", 2, "f,r",
    FuncInvPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncInvPartPerm_C" },

  { 0 }

};

/******************************************************************************
*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel ( StructInitInfo *module )
{
    /* init filters and functions                                          */
    InitHdlrFuncsFromTable( GVarFuncs );

    /* return success                                                      */
    return 0;
}

Obj FuncADD_SET(Obj self, Obj set, Obj obj);

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
    gvar = GVarName("CITRUSC"); AssGVar( gvar, tmp ); MakeReadOnlyGVar(gvar);

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitInfopl()  . . . . . . . . . . . . . . . . . table of init functions
*/
static StructInitInfo module = {
#ifdef CITRUSSTATIC
 /* type        = */ MODULE_STATIC,
#else
 /* type        = */ MODULE_DYNAMIC,
#endif
 /* name        = */ "citrus",
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

#ifndef CITRUSSTATIC
StructInitInfo * Init__Dynamic ( void )
{
  module.revision_c = Revision_citrus_c;
  return &module;
}
#endif

StructInitInfo * Init__citrus ( void )
{
  module.revision_c = Revision_citrus_c;
  return &module;
}




