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

#include "bipart.h"
#include "converter.h"
#include "fropin.hh"
#include "ufdata.hh"

#include "semigroups++/semigroups.h"

Obj TheTypeTSemiObj;
Obj TheTypeTBlocksObj;

Obj TYPE_BIPART;  // function
Obj TYPES_BIPART; // plist

UInt T_SEMI = 0;
UInt T_BIPART = 0;
UInt T_BLOCKS = 0;

// Function to print a T_SEMI Obj.

void TSemiObjPrintFunc (Obj o) {
  switch (SUBTYPE_OF_T_SEMI(o)) {
    case T_SEMI_SUBTYPE_SEMIGP:
      Pr("<wrapper for instance of C++ Semigroup class>", 0L, 0L);
      break;
    case T_SEMI_SUBTYPE_CONVER:
      Pr("<wrapper for instance of C++ Converter class>", 0L, 0L);
      break;
    case T_SEMI_SUBTYPE_UFDATA:
      Pr("<wrapper for instance of C++ UFData class>", 0L, 0L);
      break;
    default:
      assert(false);
  }
}

Obj TBipartObjCopyFunc (Obj o, Int mut) {
  // Bipartition objects are mathematically immutable, so
  // we don't need to do anything,
  return o;
}

Obj TBlocksObjCopyFunc (Obj o, Int mut) {
  // Blocks objects are mathematically immutable, so
  // we don't need to do anything,
  return o;
}

void TBipartObjCleanFunc (Obj o) { }

void TBlocksObjCleanFunc (Obj o) { }

Int TBipartObjIsMutableObjFuncs (Obj o) {
  // Bipartition objects are mathematically immutable.
  return 0L;
}

Int TBlocksObjIsMutableObjFuncs (Obj o) {
  // Blocks objects are mathematically immutable.
  return 0L;
}

// Function to free a T_SEMI Obj during garbage collection.

void TSemiObjFreeFunc (Obj o) {
  switch (SUBTYPE_OF_T_SEMI(o)) {
    case T_SEMI_SUBTYPE_SEMIGP:
      delete CLASS_OBJ<Semigroup>(o);
      break;
    case T_SEMI_SUBTYPE_CONVER:
      delete CLASS_OBJ<Converter>(o);
      break;
    case T_SEMI_SUBTYPE_UFDATA:
      delete CLASS_OBJ<UFData>(o);
      break;
    default:
      assert(false);
  }
}

void TBipartObjFreeFunc (Obj o) {
  delete CLASS_OBJ<Bipartition>(o);
}

void TBlocksObjFreeFunc (Obj o) {
  delete CLASS_OBJ<Blocks>(o);
}

// Functions to return the GAP-level type of a T_SEMI Obj

Obj TSemiObjTypeFunc (Obj o) {
  return TheTypeTSemiObj;
}

Obj TBipartObjTypeFunc (Obj o) {
  return ELM_PLIST(TYPES_BIPART, CLASS_OBJ<Bipartition>(o)->degree() + 1);
}

Obj TBlocksObjTypeFunc (Obj o) {
  return TheTypeTBlocksObj;
}

// Functions to save T_SEMI, T_BIPART, T_BLOCKS

void TSemiObjSaveFunc (Obj o) {
  assert(TNUM_OBJ(o) == T_SEMI);

  SaveUInt8(SUBTYPE_OF_T_SEMI(o));

  switch (SUBTYPE_OF_T_SEMI(o)) {
    case T_SEMI_SUBTYPE_UFDATA: {
      // FIXME: what to do in this case
    }
    default: // for T_SEMI Objs of subtype T_SEMI_SUBTYPE_SEMIGP,
             // T_SEMI_SUBTYPE_CONVER do nothing further
      break;
  }
}

void TBipartObjSaveFunc (Obj o) {
  Bipartition* b = CLASS_OBJ<Bipartition>(o);
  SaveUInt8(b->degree());
  for (auto it = b->begin(); it < b->end(); it++) {
    SaveUInt4(*it);
  }
}

void TBlocksObjSaveFunc (Obj o) {
  Blocks* b = CLASS_OBJ<Blocks>(o);
  SaveUInt8(b->degree());
  if (b->degree() != 0) {
    SaveUInt8(b->nr_blocks());
    for (auto it = b->begin(); it < b->end(); it++) {
      SaveUInt4(*it);
    }
    for (auto it = b->lookup_begin(); it < b->lookup_end(); it++) {
      SaveUInt2(static_cast<UInt4>(*it));
    }
  }
}

// Functions to load T_SEMI, T_BIPART, T_BLOCKS

void TSemiObjLoadFunc (Obj o) {
  assert(TNUM_OBJ(o) == T_SEMI);

  t_semi_subtype_t type = static_cast<t_semi_subtype_t>(LoadUInt8());
  ADDR_OBJ(o)[1] = (Obj)type;

  switch (type) {
    case T_SEMI_SUBTYPE_SEMIGP: {
      ADDR_OBJ(o)[0] = static_cast<Obj>(nullptr);
      break;
    }
    case T_SEMI_SUBTYPE_CONVER: {
      ADDR_OBJ(o)[0] = static_cast<Obj>(nullptr);
      break;
    }
    case T_SEMI_SUBTYPE_UFDATA: {
      // FIXME: what to do in this case
    }
  }
}

void TBipartObjMarkSubBags (Obj o) {
  if (ADDR_OBJ(o)[1] != NULL) {
    MARK_BAG(ADDR_OBJ(o)[1]);
  }
  if (ADDR_OBJ(o)[2] != NULL) {
    MARK_BAG(ADDR_OBJ(o)[2]);
  }
}

void TBipartObjLoadFunc (Obj o) {
  UInt8 deg = LoadUInt8();
  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->reserve(2 * deg);

  for (size_t i = 0; i < 2 * deg; i++) {
    blocks->push_back(LoadUInt4());
  }
  ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(new Bipartition(blocks));
}

void TBlocksObjLoadFunc (Obj o) {

  UInt8 deg = LoadUInt8();
  if (deg == 0) {
    ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(new Blocks());
    return;
  }
  UInt8 nr_blocks = LoadUInt8();

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->reserve(deg);

  for (size_t i = 0; i < deg; i++) {
    blocks->push_back(LoadUInt4());
  }

  std::vector<bool>* lookup = new std::vector<bool>();
  lookup->reserve(nr_blocks);

  for (size_t i = 0; i < nr_blocks; i++) {
    lookup->push_back(static_cast<bool>(LoadUInt2()));
  }

  ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(new Blocks(blocks,
                                                    lookup,
                                                    nr_blocks));
}

// Filters for IS_BIPART, IS_BLOCKS

Obj IsBipartFilt;

Obj IsBipartHandler (Obj self, Obj val) {
  if (TNUM_OBJ(val) == T_BIPART) {
    return True;
  } else if (TNUM_OBJ(val) < FIRST_EXTERNAL_TNUM) {
    return False;
  } else {
    return DoFilter(self, val);
  }
}

Obj IsBlocksFilt;

Obj IsBlocksHandler (Obj self, Obj val) {
  if (TNUM_OBJ(val) == T_BLOCKS) {
    return True;
  } else if (TNUM_OBJ(val) < FIRST_EXTERNAL_TNUM) {
    return False;
  } else {
    return DoFilter(self, val);
  }
}

// Imported types and functions from the library, defined below

Obj HTValue;
Obj HTAdd;
Obj infinity;
Obj Ninfinity;
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

/*****************************************************************************
*V  GVarFilts . . . . . . . . . . . . . . . . . . . list of filters to export
*/

typedef Obj (* GVarFilt)(/*arguments*/);

static StructGVarFilt GVarFilts [] = {

    {"IS_BIPART", "obj", &IsBipartFilt,
     (GVarFilt)IsBipartHandler, "gap.cc:IS_BIPART"},

    {"IS_BLOCKS", "obj", &IsBlocksFilt,
     (GVarFilt)IsBlocksHandler, "gap.cc:IS_BLOCKS"},

    { 0, 0, 0, 0, 0 } /* Finish with an empty entry */
};

/*****************************************************************************/

typedef Obj (* GVarFunc)(/*arguments*/);

#define GVAR_FUNC_TABLE_ENTRY(srcfile, name, nparam, params) \
  {#name, nparam, \
   params, \
   (GVarFunc)name, \
   srcfile ":Func" #name }

// Table of functions to export
// FIXME the filenames are mostly wrong here
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
static Int InitKernel(StructInitInfo *module) {

    /* init filters and functions                                          */
    InitHdlrFuncsFromTable(GVarFuncs);


    T_SEMI = RegisterPackageTNUM("TSemiObj", TSemiObjTypeFunc);
    InfoBags[T_SEMI].name = "Semigroups package C++ type";
    PrintObjFuncs[T_SEMI] = TSemiObjPrintFunc;
    SaveObjFuncs[T_SEMI] = TSemiObjSaveFunc;
    LoadObjFuncs[T_SEMI] = TSemiObjLoadFunc;

    InitMarkFuncBags(T_SEMI, &MarkNoSubBags);
    InitFreeFuncBag(T_SEMI, &TSemiObjFreeFunc);

    InitCopyGVar("TheTypeTSemiObj", &TheTypeTSemiObj);

    //TODO: CopyObjFuncs, CleanObjFuncs, IsMutableObjFuncs

    T_BIPART = RegisterPackageTNUM("TBipartObj", TBipartObjTypeFunc);
    InfoBags[T_BIPART].name = "bipartition";

    ProdFuncs[T_BIPART][T_BIPART] = BIPART_PROD;
    EqFuncs[T_BIPART][T_BIPART] = BIPART_EQ;
    LtFuncs[T_BIPART][T_BIPART] = BIPART_LT;

    CopyObjFuncs[T_BIPART] = &TBipartObjCopyFunc;
    CleanObjFuncs[T_BIPART] = &TBipartObjCleanFunc;
    IsMutableObjFuncs[T_BIPART] = &TBipartObjIsMutableObjFuncs;

    InitMarkFuncBags(T_BIPART, &TBipartObjMarkSubBags);
    InitFreeFuncBag(T_BIPART, &TBipartObjFreeFunc);

    ImportGVarFromLibrary("TYPE_BIPART", &TYPE_BIPART);
    ImportGVarFromLibrary("TYPES_BIPART", &TYPES_BIPART);

    T_BLOCKS = RegisterPackageTNUM("TBlocksObj", TBlocksObjTypeFunc);
    InfoBags[T_BLOCKS].name = "blocks";

    EqFuncs[T_BLOCKS][T_BLOCKS] = BLOCKS_EQ;
    LtFuncs[T_BLOCKS][T_BLOCKS] = BLOCKS_LT;

    CopyObjFuncs[T_BLOCKS] = &TBlocksObjCopyFunc;
    CleanObjFuncs[T_BLOCKS] = &TBlocksObjCleanFunc;
    IsMutableObjFuncs[T_BLOCKS] = &TBlocksObjIsMutableObjFuncs;

    InitMarkFuncBags(T_BLOCKS, &MarkNoSubBags);
    InitFreeFuncBag(T_BLOCKS, &TBlocksObjFreeFunc);


    InitCopyGVar("TheTypeTBlocksObj", &TheTypeTBlocksObj);

    ImportGVarFromLibrary("HTValue", &HTValue);
    ImportGVarFromLibrary("HTAdd", &HTAdd);

    ImportGVarFromLibrary("infinity", &infinity);
    ImportGVarFromLibrary("Ninfinity", &Ninfinity);

    ImportGVarFromLibrary("SEMIGROUPS_PBRTypes", &PBRTypes);
    ImportGVarFromLibrary("PBRType", &PBRType);

    ImportGVarFromLibrary("IsPBR", &IsPBR);
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

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitLibrary( <module> ) . . . . . . .  initialise library data structures
*/
static Int InitLibrary( StructInitInfo *module )
{
    /* init filters and functions */
    InitGVarFiltsFromTable( GVarFilts );
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
 /* filename    = */ (char*) "pkg/semigroups/src/gap.cc",
 /* isGapRootR  = */ true
};

extern "C"
StructInitInfo * Init__Dynamic ( void )
{
  return &module;
}
