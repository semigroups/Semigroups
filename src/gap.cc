/*
 * Semigroups GAP package
 *
 * This file contains...
 *
 */

#include <gap.hh>

#include <assert.h>
#include <iostream>
#include <time.h>

#include "bipart.hh"
#include "converter.h"
#include "fropin.hh"
#include "ufdata.hh"

#include "semigroups++/semigroups.h"

// Imported types and functions from the library, defined at the end of the
// file

Obj HTValue;
Obj HTAdd;
Obj infinity;
Obj Ninfinity;
Obj IsBipartition;
Obj BipartTypes;
Obj BipartitionType;
Obj IsBlocks;
Obj BlocksType;
Obj IsBooleanMat;
Obj BooleanMatType;
Obj IsMatrixOverSemiring;
Obj IsMaxPlusMatrix;
Obj MaxPlusMatrixType;
Obj IsMinPlusMatrix;
Obj MinPlusMatrixType;
Obj IsTropicalMatrix;
Obj IsTropicalMinPlusMatrix;
Obj TropicalMinPlusMatrixType;
Obj IsTropicalMaxPlusMatrix;
Obj TropicalMaxPlusMatrixType;
Obj IsProjectiveMaxPlusMatrix;
Obj ProjectiveMaxPlusMatrixType;
Obj IsNTPMatrix;
Obj NTPMatrixType;
Obj IsIntegerMatrix;
Obj IntegerMatrixType;
Obj IsPBR;
Obj PBRTypes;
Obj PBRType;

/*******************************************************************************
 * GAP level print for a T_SEMI
*******************************************************************************/

void PrintSemi (Obj o) {
  Pr("<Semigroups package C++ type>", 0L, 0L);
}

/*******************************************************************************
 * free a Bag of type T_SEMI - this has to go here so that InterfaceBase etc
 * are defined.
*******************************************************************************/

void SemigroupsBagFreeFunc (Obj o) {
  //FIXME use switch
  if (IS_CONVERTER_BAG(o)) {
    delete CLASS_OBJ<Converter>(o);
  } else if (IS_SEMIGROUP_BAG(o)) {
    delete CLASS_OBJ<Semigroup>(o);
  } else if (IS_GAP_BIPART_BAG(o)) {
    delete CLASS_OBJ<Bipartition>(o);
  } else if (IS_GAP_BLOCKS_BAG(o)) {
    delete CLASS_OBJ<Blocks>(o);
  }
}

void SemigroupsMarkSubBags (Obj o) {
  if ((SIZE_OBJ(o) / sizeof(Obj)) > 2) {
    for (size_t i = 2; i < (SIZE_OBJ(o) / sizeof(Obj)); i++) {
      if (ADDR_OBJ(o)[i] != NULL) {
        MARK_BAG(ADDR_OBJ(o)[i]);
      }
    }
  }
}

/*****************************************************************************/

typedef Obj (* GVarFunc)(/*arguments*/);

#define GVAR_FUNC_TABLE_ENTRY(srcfile, name, nparam, params) \
  {#name, nparam, \
   params, \
   (GVarFunc)name, \
   srcfile ":Func" #name }

// Table of functions to export
static StructGVarFunc GVarFuncs [] = {
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_ENUMERATE, 4,
                          "data, limit, lookfunc, looking"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_RIGHT_CAYLEY_GRAPH, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_LEFT_CAYLEY_GRAPH, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_ELEMENT_NUMBER, 2,
                          "data, pos"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_ELEMENT_NUMBER_SORTED, 2,
                          "data, pos"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_NEXT_ITERATOR, 1,
                          "iter"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_NEXT_ITERATOR_SORTED, 1,
                          "iter"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_IS_DONE_ITERATOR, 1,
                          "iter"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_IS_DONE_ITERATOR_CC, 1,
                          "iter"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_AS_LIST, 1, "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_AS_SET, 1, "data"),

    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_RELATIONS, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_FACTORIZATION, 2,
                          "data, pos"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_SIZE, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_NR_IDEMPOTENTS, 1,
                            "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_CLOSURE, 3,
                          "old_data, coll, degree"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_ADD_GENERATORS, 2,
                          "data, coll"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_CURRENT_SIZE, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_CURRENT_NR_RULES, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_POSITION, 2,
                          "data, x"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_POSITION_CURRENT, 2,
                          "data, x"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_POSITION_SORTED, 2, "data, x"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_IS_DONE, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_CURRENT_MAX_WORD_LENGTH, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_LENGTH_ELEMENT, 2,
                          "data, pos"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_CAYLEY_TABLE, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SEMIGROUP_MAX_WORD_LENGTH_BY_RANK, 1,
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS, 2,
                          "scc1, scc2"),
    GVAR_FUNC_TABLE_ENTRY("interface.cc", FIND_HCLASSES, 2,
                          "left, right"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", UF_NEW, 1,
                          "size"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", UF_COPY, 1,
                          "ufdata"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", UF_SIZE, 1,
                          "ufdata"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", UF_FIND, 2,
                          "ufdata, i"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", UF_UNION, 2,
                          "ufdata, pair"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", UF_FLATTEN, 1,
                          "ufdata"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", UF_TABLE, 1,
                          "ufdata"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", UF_BLOCKS, 1,
                          "ufdata"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_NC, 1,
                          "list"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_EXT_REP, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_INT_REP, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_HASH, 2,
                          "x, data"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_DEGREE, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_RANK, 2,
                          "x, nothing"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_NR_BLOCKS, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_NR_LEFT_BLOCKS, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_PROD, 2,
                          "x, y"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_EQ, 2,
                          "x, y"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_LT, 2,
                          "x, y"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_PERM_LEFT_QUO, 2,
                          "x, y"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_LEFT_PROJ, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_RIGHT_PROJ, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_STAR, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_LAMBDA_CONJ, 2,
                          "x, y"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_STAB_ACTION, 2,
                          "x, p"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_LEFT_BLOCKS, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_RIGHT_BLOCKS, 1,
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_NC, 1,
                          "blocks"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_EXT_REP, 1,
                          "blocks"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_DEGREE, 1,
                          "blocks"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_RANK, 1,
                          "blocks"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_NR_BLOCKS, 1,
                          "blocks"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_HASH, 2,
                          "blocks, data"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_PROJ, 1,
                          "blocks"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_EQ, 2,
                          "blocks1, blocks2"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_LT, 2,
                          "blocks1, blocks2"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_E_TESTER, 2,
                          "left, right"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_E_CREATOR, 2,
                          "left, right"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_LEFT_ACT, 2,
                          "blocks, x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_RIGHT_ACT, 2,
                          "blocks, x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_INV_LEFT, 2,
                          "blocks, x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BLOCKS_INV_RIGHT, 2,
                          "blocks, x"),
    GVAR_FUNC_TABLE_ENTRY("bipart.cc", BIPART_NR_IDEMPOTENTS, 5,
                          "o, scc, lookup, nr_threads, report"),
    { 0, 0, 0, 0, 0 } /* Finish with an empty entry */
};

/******************************************************************************
*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel( StructInitInfo *module )
{
    /* init filters and functions                                          */
    InitHdlrFuncsFromTable( GVarFuncs );
    InfoBags[T_SEMI].name = "Semigroups package C++ type";
    PrintObjFuncs[T_SEMI] = PrintSemi;
    InitMarkFuncBags(T_SEMI, &SemigroupsMarkSubBags);
    InitFreeFuncBag(T_SEMI, &SemigroupsBagFreeFunc);

    ImportGVarFromLibrary("HTValue", &HTValue);
    ImportGVarFromLibrary("HTAdd", &HTAdd);

    ImportGVarFromLibrary("infinity", &infinity);
    ImportGVarFromLibrary("Ninfinity", &Ninfinity);

    ImportGVarFromLibrary("IsBipartition", &IsBipartition);
    ImportGVarFromLibrary("IsBlocks", &IsBlocks);
    ImportGVarFromLibrary("SEMIGROUPS_BipartitionTypes", &BipartTypes);
    ImportGVarFromLibrary("BipartitionType", &BipartitionType);
    ImportGVarFromLibrary("BlocksType", &BlocksType );

    ImportGVarFromLibrary("SEMIGROUPS_PBRTypes", &PBRTypes);
    ImportGVarFromLibrary("PBRType", &PBRType);

    ImportGVarFromLibrary("IsBooleanMat", &IsBooleanMat);
    ImportGVarFromLibrary("BooleanMatType", &BooleanMatType);

    ImportGVarFromLibrary("IsMatrixOverSemiring", &IsMatrixOverSemiring);

    ImportGVarFromLibrary("IsMaxPlusMatrix", &IsMaxPlusMatrix);
    ImportGVarFromLibrary("MaxPlusMatrixType", &MaxPlusMatrixType);

    ImportGVarFromLibrary("IsMinPlusMatrix", &IsMinPlusMatrix);
    ImportGVarFromLibrary("MinPlusMatrixType", &MinPlusMatrixType);

    ImportGVarFromLibrary("IsTropicalMatrix", &IsTropicalMatrix);

    ImportGVarFromLibrary("IsTropicalMaxPlusMatrix",
                          &IsTropicalMaxPlusMatrix);
    ImportGVarFromLibrary("TropicalMaxPlusMatrixType",
                          &TropicalMaxPlusMatrixType);

    ImportGVarFromLibrary("IsTropicalMinPlusMatrix",
                          &IsTropicalMinPlusMatrix);
    ImportGVarFromLibrary("TropicalMinPlusMatrixType",
                          &TropicalMinPlusMatrixType);

    ImportGVarFromLibrary("IsProjectiveMaxPlusMatrix",
                          &IsProjectiveMaxPlusMatrix);
    ImportGVarFromLibrary("ProjectiveMaxPlusMatrixType",
                          &ProjectiveMaxPlusMatrixType);

    ImportGVarFromLibrary("IsNTPMatrix", &IsNTPMatrix);
    ImportGVarFromLibrary("NTPMatrixType", &NTPMatrixType);

    ImportGVarFromLibrary("IsIntegerMatrix", &IsIntegerMatrix);
    ImportGVarFromLibrary("IntegerMatrixType", &IntegerMatrixType);

    ImportGVarFromLibrary("IsPBR", &IsPBR);
    ImportGVarFromLibrary("PBRType", &PBRType);

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitLibrary( <module> ) . . . . . . .  initialise library data structures
*/
static Int InitLibrary( StructInitInfo *module )
{
    /* init filters and functions */
    InitGVarFuncsFromTable( GVarFuncs );

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitInfopl()  . . . . . . . . . . . . . . . . . table of init functions
*/
static StructInitInfo module = {
 /* type        = */ MODULE_DYNAMIC,
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
 /* postRestore = */ 0,
 /* filename    = */ (char*) "pkg/semigroups/src/interface.cc",
 /* isGapRootR  = */ true
};

extern "C"
StructInitInfo * Init__Dynamic ( void )
{
  return &module;
}
