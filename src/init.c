const char * Revision_citrus_c =
   "$Id: citrus.c,v$";

#include "ptrans.h"

/*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * */

/******************************************************************************
*V  GVarFuncs . . . . . . . . . . . . . . . . . . list of functions to export
*/
static StructGVarFunc GVarFuncs [] = {

  { "ELM_LIST_PT", 2, "f,i", 
    FuncELM_LIST_PT, 
    "pkg/citrus/src/ptrans.c:ELM_LIST_PT" },

  { "ELM_LIST_PP", 2, "f,i", 
    FuncELM_LIST_PP, 
    "pkg/citrus/src/pperm.c:ELM_LIST_PP" },

  { "ELMS_LIST_PT", 2, "f,list",
    FuncELMS_LIST_PT,
    "pkg/citrus/src/ptrans.c:ELMS_LIST_PT" },

  { "ELMS_LIST_PP", 2, "f,list",
    FuncELMS_LIST_PP,
    "pkg/citrus/src/pperm.c:ELMS_LIST_PP" },

  { "FullPartialPermNC", 1, "rep",
    FuncFullPartialPermNC,
    "pkg/citrus/src/pperm.c:FuncFullPartialPermNC" },

  { "SparsePartialPermNC", 2, "dom,ran",
    FuncSparsePartialPermNC,
    "pkg/citrus/src/pperm.c:FuncSparsePartialPermNC" },

  { "DensePartialPermNC", 1, "img",
    FuncDensePartialPermNC,
    "pkg/citrus/src/pperm.c:FuncDensePartialPermNC" },
 
  { "ProdPP", 2, "f,g",
    FuncProdPP,
    "pkg/citrus/src/pperm.c:FuncProdPP" },

  { "DomPP", 1, "f",
    FuncDomPP,
    "pkg/citrus/src/pperm.c:FuncDomPP" },

  { "RanPP", 1, "f",
    FuncRanPP,
    "pkg/citrus/src/pperm.c:FuncRanPP" },

  { "RanSetPP", 1, "f",
    FuncRanSetPP,
    "pkg/citrus/src/pperm.c:FuncRanSetPP" },

  { "InvPP", 1, "f",
    FuncInvPP,
    "pkg/citrus/src/pperm.c:FuncInvPP" },

  { "OnIntegerTuplesWithPP", 2, "tup,f",
    FuncOnIntegerTuplesWithPP,
    "pkg/citrus/src/pperm.c:FuncOnIntegerTuplesWithPP" },
  
  { "OnIntegerSetsWithPP", 2, "set,f",
    FuncOnIntegerSetsWithPP,
    "pkg/citrus/src/pperm.c:FuncOnIntegerSetsWithPP" },
  
  { "EqPP", 2, "f,g",
    FuncEqPP,
    "pkg/citrus/src/pperm.c:FuncEqPP" },

  { "LeftOne", 1, "f",
    FuncLeftOne,
    "pkg/citrus/src/pperm.c:FuncLeftOne" },

  { "RightOne", 1, "f",
    FuncRightOne,
    "pkg/citrus/src/pperm.c:FuncRightOne" },

  { "FixedPointsPP", 1, "f",
    FuncFixedPointsPP,
    "pkg/citrus/src/pperm.c:FuncFixedPointsPP" },

  { "MovedPointsPP", 1, "f",
    FuncMovedPointsPP,
    "pkg/citrus/src/pperm.c:FuncMovedPointsPP" },

  { "NrMovedPointsPP", 1, "f",
    FuncNrMovedPointsPP,
    "pkg/citrus/src/pperm.c:FuncNrMovedPointsPP" },

  { "LargestMovedPointPP", 1, "f",
    FuncLargestMovedPointPP,
    "pkg/citrus/src/pperm.c:FuncLargestMovedPointPP" },

  { "SmallestMovedPointPP", 1, "f",
    FuncSmallestMovedPointPP,
    "pkg/citrus/src/pperm.c:FuncSmallestMovedPointPP" },

  { "LeqPP", 2, "f, g",
    FuncLeqPP,
    "pkg/citrus/src/pperm.c:FuncLeqPP" },

  { "RestrictedPP", 2, "f, set", 
    FuncRestrictedPP, 
    "pkg/citrus/src/pperm.c:FuncRestrictedPP" },

  { "NaturalLeqPP", 2, "f, g", 
    FuncNaturalLeqPP,
    "pkg/citrus/src/pperm.c:FuncNaturalLeqPP" },

  { "QuoPP", 2, "f, g", 
    FuncQuoPP,
    "pkg/citrus/src/pperm.c:FuncQuoPP" },

  { "ProdPPPerm", 2, "f, p",
    FuncProdPPPerm, 
    "pkg/citrus/src/pperm.c:FuncProdPPPerm" },

  { "ProdPermPP", 2, "p, f",
    FuncProdPermPP, 
    "pkg/citrus/src/pperm.c:FuncProdPermPP" },

  { "OnPointsPP", 2, "i, f",
    FuncOnPointsPP, 
    "pkg/citrus/src/pperm.c:FuncOnPointsPP" },

  { 0 }

};

/******************************************************************************
*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel ( StructInitInfo *module )
{
    /* init filters and functions                                          */
    InitHdlrFuncsFromTable( GVarFuncs );

    ImportGVarFromLibrary( "PartialPermType", &PartialPermType );
    ImportGVarFromLibrary( "PartialTransType", &PartialTransType );
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

