

const char * Revision_citrus_c =
   "$Id: citrus.c,v$";

#include <stdlib.h>

#include "src/compiled.h" 

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

/* domain and range of a partial permutation */

Obj FuncDomRanPartPerm_C( Obj self, Obj f, Int rank)
{ 
    Obj ff,dom,ran,out;
    Int i,j,m,n;
  
    if(INT_INTOBJ(rank)==0){
      dom = NEW_PLIST(T_PLIST_EMPTY,INT_INTOBJ(rank));
      ran = NEW_PLIST(T_PLIST_EMPTY,INT_INTOBJ(rank));
      out = NEW_PLIST(T_PLIST,2);
      SET_LEN_PLIST(out,2);
      SET_ELM_PLIST(out,1,dom);
      SET_ELM_PLIST(out,2,ran);
      return out;
    }

    if (IS_POSOBJ(f)){ 
      ff = ELM_PLIST(f,1);
    }
    else{
      ff = f;
    }

    n = LEN_LIST(ff);
    dom = NEW_PLIST(T_PLIST_CYC,n);
    SET_LEN_PLIST(dom,INT_INTOBJ(rank));
    ran = NEW_PLIST(T_PLIST_CYC,n);
    SET_LEN_PLIST(ran,INT_INTOBJ(rank));
    m = 0;

    for (i = 1;i <= n;i++) {
      j = INT_INTOBJ(ELM_LIST(ff,i));
      if(j != 0){
        m=m+1;
        SET_ELM_PLIST(dom,m,INTOBJ_INT(i));
        SET_ELM_PLIST(ran,m,INTOBJ_INT(j));
      } 
    }
    out = NEW_PLIST(T_PLIST,2);
    SET_LEN_PLIST(out,2);
    SET_ELM_PLIST(out,1,dom);
    SET_ELM_PLIST(out,2,ran);
    return out;
}

/* rank of a partial permutation */

Obj FuncRankPartPerm_C( Obj self, Obj f)
{
    Obj ff;
    Int n,i,j,rank;

    if (IS_POSOBJ(f)){
      ff = ELM_PLIST(f,1);
    }
    else{
      ff = f;
    }

    n = LEN_LIST(ff);
    if (n==0) return INTOBJ_INT(n);

    rank = 0;
    for (i = 1;i <= n;i++){
      j = INT_INTOBJ(ELM_LIST(ff,i));
      if(j!=0) rank++;
    }
      
    return INTOBJ_INT(rank);
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

    img=NEW_PLIST(T_PLIST_CYC,r);
    SET_LEN_PLIST(img,r);

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

/* largest moved point partial permutation */ 

Obj FuncLargestMovedPointPartPerm_C ( Obj self, Obj f )
{
    Obj ff;
    Int n,i,j;

   if (IS_POSOBJ(f)){
      ff = ELM_PLIST(f,1);
    }
    else{
      ff = f;
    } 

    n = LEN_LIST(ff); /* degree! */
    for(i=n;1<=n;i--){
      j = INT_INTOBJ(ELM_LIST(ff,i)); 
      if (j!=0){
        break;
      }
    }
    return INTOBJ_INT(i);
}

/*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * */

/******************************************************************************
*V  GVarFuncs . . . . . . . . . . . . . . . . . . list of functions to export
*/
static StructGVarFunc GVarFuncs [] = {

  { "ProdPartPerm_C", 2, "f, g",
    FuncProdPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncProdPartPerm_C" },

  { "DomRanPartPerm_C", 2, "f, rank",
    FuncDomRanPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncDomRanPartPerm_C" },

  { "RankPartPerm_C", 1, "f",
    FuncRankPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncRankPartPerm_C" },

  { "InvPartPerm_C", 2, "f,r",
    FuncInvPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncInvPartPerm_C" },

  { "LargestMovedPointPartPerm_C", 1, "f", 
    FuncLargestMovedPointPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncLargestMovedPointPartPerm_C" },

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




