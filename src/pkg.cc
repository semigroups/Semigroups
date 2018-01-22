//
// Semigroups package for GAP
// Copyright (C) 2016 James D. Mitchell
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

// This file contains everything in the kernel module for the Semigroups
// package that involves GAP directly, i.e. importing functions/variables from
// GAP and declaring functions for GAP etc.

#include "pkg.h"

#include <iostream>

#include "bipart.h"
#include "congpairs.h"
#include "converter.h"
#include "fropin.h"
#include "semigroups-debug.h"
#include "semigrp.h"
#include "uf.h"

#include "libsemigroups/src/cong.h"
#include "libsemigroups/src/semigroups.h"
#include "libsemigroups/src/uf.h"

using libsemigroups::Congruence;
using libsemigroups::UF;

#if !defined(SIZEOF_VOID_P)
#error Something is wrong with this GAP installation: SIZEOF_VOID_P not defined
#elif SIZEOF_VOID_P == 4
#define SYSTEM_IS_32_BIT
#endif

Obj SEMIGROUPS;

Obj TheTypeTSemiObj;
Obj TheTypeTBlocksObj;

Obj TYPE_BIPART;   // function
Obj TYPES_BIPART;  // plist

UInt T_SEMI   = 0;
UInt T_BIPART = 0;
UInt T_BLOCKS = 0;

// Save/Load a UInt with length the current wordsize
inline void SaveUIntBiggest(UInt n) {
#ifdef SYSTEM_IS_32_BIT
  SaveUInt4(n);
#else
  SaveUInt8(n);
#endif
}

inline UInt LoadUIntBiggest() {
#ifdef SYSTEM_IS_32_BIT
  return LoadUInt4();
#else
  return LoadUInt8();
#endif
}

// Function to print a T_SEMI Obj.

void TSemiObjPrintFunc(Obj o) {
  switch (SUBTYPE_OF_T_SEMI(o)) {
    case T_SEMI_SUBTYPE_UF: {
      Pr("<wrapper for instance of C++ UF class>", 0L, 0L);
      break;
    }
    case T_SEMI_SUBTYPE_CONG: {
      Pr("<wrapper for instance of C++ Congruence class>", 0L, 0L);
      break;
    }
    case T_SEMI_SUBTYPE_ENSEMI: {
      Pr("<wrapper for C++ semigroup objects>", 0L, 0L);
      break;
    }
    default: { SEMIGROUPS_ASSERT(false); }
  }
}

Obj TBipartObjCopyFunc(Obj o, Int mut) {
  // Bipartition objects are mathematically immutable, so
  // we don't need to do anything,
  return o;
}

Obj TBlocksObjCopyFunc(Obj o, Int mut) {
  // Blocks objects are mathematically immutable, so
  // we don't need to do anything,
  return o;
}

void TBipartObjCleanFunc(Obj o) {}

void TBlocksObjCleanFunc(Obj o) {}

Int TBipartObjIsMutableObjFuncs(Obj o) {
  // Bipartition objects are mathematically immutable.
  return 0L;
}

Int TBlocksObjIsMutableObjFuncs(Obj o) {
  // Blocks objects are mathematically immutable.
  return 0L;
}

// Function to free a T_SEMI Obj during garbage collection.

void TSemiObjFreeFunc(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_SEMI);
  switch (SUBTYPE_OF_T_SEMI(o)) {
    case T_SEMI_SUBTYPE_UF: {
      delete CLASS_OBJ<UF*>(o);
      break;
    }
    case T_SEMI_SUBTYPE_CONG: {
      delete CLASS_OBJ<Congruence*>(o);
      break;
    }
    case T_SEMI_SUBTYPE_ENSEMI: {
      if (en_semi_get_type(o) != UNKNOWN) {
        // don't use functions to access these since they have too many
        // side effects
        delete CLASS_OBJ<Converter*>(o, 4);
        delete CLASS_OBJ<Semigroup*>(o, 5);
      }
      break;
    }
    default: { SEMIGROUPS_ASSERT(false); }
  }
}

void TBipartObjFreeFunc(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_BIPART);
  bipart_get_cpp(o)->really_delete();
  delete bipart_get_cpp(o);
}

void TBlocksObjFreeFunc(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_BLOCKS);
  delete blocks_get_cpp(o);
}

// Functions to return the GAP-level type of a T_SEMI Obj

Obj TSemiObjTypeFunc(Obj o) {
  return TheTypeTSemiObj;
}

Obj TBipartObjTypeFunc(Obj o) {
  return ELM_PLIST(TYPES_BIPART, bipart_get_cpp(o)->degree() + 1);
}

Obj TBlocksObjTypeFunc(Obj o) {
  return TheTypeTBlocksObj;
}

// Functions to save/load T_SEMI, T_BIPART, T_BLOCKS

void TSemiObjSaveFunc(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_SEMI);

  SaveUInt4(SUBTYPE_OF_T_SEMI(o));

  switch (SUBTYPE_OF_T_SEMI(o)) {
    case T_SEMI_SUBTYPE_UF: {
      UF* uf = CLASS_OBJ<UF*>(o);
      SaveUIntBiggest(uf->get_size());
      for (size_t i = 0; i < uf->get_size(); i++) {
        SaveUIntBiggest(uf->find(i));
      }
      break;
    }
    case T_SEMI_SUBTYPE_ENSEMI: {
      // [t_semi_subtype_t, en_semi_t, gap_semigroup_t, size_t degree]
      // only store gap_semigroup_t and degree if en_semi_get_semi_obj !=
      // UNKNOWN.
      SaveUInt4(en_semi_get_type(o));
      if (en_semi_get_type(o) != UNKNOWN) {
        SaveSubObj(en_semi_get_semi_obj(o));
        SaveUInt4(en_semi_get_degree(o));
      }
      break;
    }
    default: {  // for T_SEMI Objs of subtype T_SEMI_SUBTYPE_CONG
                // do nothing further
      break;
    }
  }
}

void TSemiObjLoadFunc(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_SEMI);

  t_semi_subtype_t type = static_cast<t_semi_subtype_t>(LoadUInt4());
  ADDR_OBJ(o)[0]        = reinterpret_cast<Obj>(type);

  switch (type) {
    case T_SEMI_SUBTYPE_UF: {
      size_t               size  = LoadUIntBiggest();
      std::vector<size_t>* table = new std::vector<size_t>();
      table->reserve(size);
      for (size_t i = 0; i < size; i++) {
        table->push_back(LoadUIntBiggest());
      }
      ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(new UF(*table));
      break;
    }
    case T_SEMI_SUBTYPE_CONG: {
      ADDR_OBJ(o)[1] = static_cast<Obj>(nullptr);
      break;
    }
    case T_SEMI_SUBTYPE_ENSEMI: {
      en_semi_t s_type = static_cast<en_semi_t>(LoadUInt4());
      ADDR_OBJ(o)[1]   = reinterpret_cast<Obj>(s_type);
      if (s_type != UNKNOWN) {
        SEMIGROUPS_ASSERT(SIZE_OBJ(o) == 6 * SIZEOF_VOID_P);
        ADDR_OBJ(o)[2] = LoadSubObj();                        // semigroup Obj
        ADDR_OBJ(o)[3] = reinterpret_cast<Obj>(LoadUInt4());  // degree
        ADDR_OBJ(o)[4] = static_cast<Obj>(nullptr);           // Converter*
        ADDR_OBJ(o)[5] = static_cast<Obj>(nullptr);           // Semigroup*
        CHANGED_BAG(o);
      }
      break;
    }
    default: { SEMIGROUPS_ASSERT(false); }
  }
}

void TBipartObjSaveFunc(Obj o) {
  Bipartition* b = bipart_get_cpp(o);
  SaveUInt4(b->degree());
  for (auto it = b->begin(); it < b->end(); it++) {
    SaveUInt4(*it);
  }
}

void TBipartObjLoadFunc(Obj o) {
  UInt4                   deg    = LoadUInt4();
  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->reserve(2 * deg);

  for (size_t i = 0; i < 2 * deg; i++) {
    blocks->push_back(LoadUInt4());
  }
  ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(new Bipartition(blocks));
  SEMIGROUPS_ASSERT(ADDR_OBJ(o)[1] == NULL && ADDR_OBJ(o)[2] == NULL);
}

void TBlocksObjSaveFunc(Obj o) {
  Blocks* b = blocks_get_cpp(o);
  SaveUInt4(b->degree());
  if (b->degree() != 0) {
    SaveUInt4(b->nr_blocks());
    for (auto it = b->cbegin(); it < b->cend(); it++) {
      SaveUInt4(*it);
    }
    for (auto it = b->lookup()->begin(); it < b->lookup()->end(); it++) {
      SaveUInt1(static_cast<UInt1>(*it));
    }
  }
}

void TBlocksObjLoadFunc(Obj o) {
  UInt4 deg = LoadUInt4();
  if (deg == 0) {
    ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(new Blocks());
    return;
  }
  UInt4 nr_blocks = LoadUInt4();

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->reserve(deg);

  for (size_t i = 0; i < deg; i++) {
    blocks->push_back(LoadUInt4());
  }

  std::vector<bool>* lookup = new std::vector<bool>();
  lookup->reserve(nr_blocks);

  for (size_t i = 0; i < nr_blocks; i++) {
    lookup->push_back(static_cast<bool>(LoadUInt1()));
  }

  ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(new Blocks(blocks, lookup, nr_blocks));
}

void TBipartObjMarkSubBags(Obj o) {
  if (ADDR_OBJ(o)[1] != NULL) {
    // SEMIGROUPS_ASSERT(TNUM_OBJ(ADDR_OBJ(o)[1]) == T_BLOCKS);
    MARK_BAG(ADDR_OBJ(o)[1]);
  }
  if (ADDR_OBJ(o)[2] != NULL) {
    // SEMIGROUPS_ASSERT(TNUM_OBJ(ADDR_OBJ(o)[2]) == T_BLOCKS);
    MARK_BAG(ADDR_OBJ(o)[2]);
  }
}

// Filters for IS_BIPART, IS_BLOCKS

Obj IsBipartFilt;

Obj IsBipartHandler(Obj self, Obj val) {
  if (TNUM_OBJ(val) == T_BIPART) {
    return True;
  } else if (TNUM_OBJ(val) < FIRST_EXTERNAL_TNUM) {
    return False;
  } else {
    return DoFilter(self, val);
  }
}

Obj IsBlocksFilt;

Obj IsBlocksHandler(Obj self, Obj val) {
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
Obj Pinfinity;
Obj Ninfinity;
Obj DimensionOfMatrixOverSemiring;
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
Obj TYPES_PBR;
Obj TYPE_PBR;
Obj DegreeOfPBR;
Obj FROPIN;
Obj GeneratorsOfMagma;

Obj IsSemigroup;
Obj IsSemigroupIdeal;
Obj IsActingSemigroup;

/*****************************************************************************
*V  GVarFilts . . . . . . . . . . . . . . . . . . . list of filters to export
*/

typedef Obj (*GVarFilt)(/*arguments*/);

static StructGVarFilt GVarFilts[] = {
    {"IS_BIPART",
     "obj",
     &IsBipartFilt,
     (GVarFilt) IsBipartHandler,
     "pkg.cc:IS_BIPART"},

    {"IS_BLOCKS",
     "obj",
     &IsBlocksFilt,
     (GVarFilt) IsBlocksHandler,
     "pkg.cc:IS_BLOCKS"},

    {0, 0, 0, 0, 0} /* Finish with an empty entry */
};

/*****************************************************************************/

typedef Obj (*GVarFunc)(/*arguments*/);

#define GVAR_ENTRY(srcfile, name, nparam, params) \
  { #name, nparam, params, (GVarFunc) name, srcfile ":Func" #name }

// Table of functions to export

static StructGVarFunc GVarFuncs[] = {
    GVAR_ENTRY("semigrp.cc", EN_SEMI_AS_LIST, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_AS_SET, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_CAYLEY_TABLE, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_CURRENT_MAX_WORD_LENGTH, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_CURRENT_NR_RULES, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_CURRENT_POSITION, 2, "S, x"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_CURRENT_SIZE, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_CLOSURE, 3, "new, old, coll"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_CLOSURE_DEST, 2, "S, coll"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_ELEMENT_NUMBER, 2, "S, pos"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_ELEMENT_NUMBER_SORTED, 2, "S, pos"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_ELMS_LIST, 2, "S, list"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_FACTORIZATION, 2, "S, pos"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_IDEMPOTENTS, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_IDEMS_SUBSET, 2, "S, list"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_IS_DONE, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_IS_DONE_ITERATOR, 1, "iter"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_LEFT_CAYLEY_GRAPH, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_LENGTH_ELEMENT, 2, "S, pos"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_NR_IDEMPOTENTS, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_POSITION, 2, "S, x"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_POSITION_SORTED, 2, "S, x"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_RIGHT_CAYLEY_GRAPH, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_SIZE, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_RELATIONS, 1, "S"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_ENUMERATE, 2, "S, limit"),

    GVAR_ENTRY("semigrp.cc", EN_SEMI_NEXT_ITERATOR, 1, "iter"),
    GVAR_ENTRY("semigrp.cc", EN_SEMI_NEXT_ITERATOR_SORTED, 1, "iter"),

    GVAR_ENTRY("congpairs.cc", CONG_PAIRS_NR_CLASSES, 1, "cong"),
    GVAR_ENTRY("congpairs.cc", CONG_PAIRS_IN, 3, "cong, elm1, elm2"),
    GVAR_ENTRY("congpairs.cc", CONG_PAIRS_LESS_THAN, 3, "cong, rep1, rep2"),
    GVAR_ENTRY("congpairs.cc", CONG_PAIRS_LOOKUP_PART, 1, "cong"),
    GVAR_ENTRY("congpairs.cc", CONG_PAIRS_ELM_COSET_ID, 2, "cong, elm"),
    GVAR_ENTRY("congpairs.cc", CONG_PAIRS_NONTRIVIAL_CLASSES, 1, "cong"),

    GVAR_ENTRY("fropin.cc",
               SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS,
               2,
               "scc1, scc2"),
    GVAR_ENTRY("fropin.cc", FIND_HCLASSES, 2, "left, right"),

    GVAR_ENTRY("uf.cc", UF_NEW, 1, "size"),
    GVAR_ENTRY("uf.cc", UF_COPY, 1, "uf"),
    GVAR_ENTRY("uf.cc", UF_SIZE, 1, "uf"),
    GVAR_ENTRY("uf.cc", UF_FIND, 2, "uf, i"),
    GVAR_ENTRY("uf.cc", UF_UNION, 2, "uf, pair"),
    GVAR_ENTRY("uf.cc", UF_FLATTEN, 1, "uf"),
    GVAR_ENTRY("uf.cc", UF_TABLE, 1, "uf"),
    GVAR_ENTRY("uf.cc", UF_BLOCKS, 1, "uf"),
    GVAR_ENTRY("uf.cc", UF_NR_BLOCKS, 1, "uf"),
    GVAR_ENTRY("uf.cc", UF_BLOCK_REPS, 1, "uf"),
    GVAR_ENTRY("uf.cc", UF_JOIN, 2, "uf1, uf2"),

    GVAR_ENTRY("bipart.cc", BIPART_NC, 1, "list"),
    GVAR_ENTRY("bipart.cc", BIPART_EXT_REP, 1, "x"),
    GVAR_ENTRY("bipart.cc", BIPART_INT_REP, 1, "x"),
    GVAR_ENTRY("bipart.cc", BIPART_HASH, 2, "x, data"),
    GVAR_ENTRY("bipart.cc", BIPART_DEGREE, 1, "x"),
    GVAR_ENTRY("bipart.cc", BIPART_RANK, 2, "x, nothing"),
    GVAR_ENTRY("bipart.cc", BIPART_NR_BLOCKS, 1, "x"),
    GVAR_ENTRY("bipart.cc", BIPART_NR_LEFT_BLOCKS, 1, "x"),
    GVAR_ENTRY("bipart.cc", BIPART_PERM_LEFT_QUO, 2, "x, y"),
    GVAR_ENTRY("bipart.cc", BIPART_LEFT_PROJ, 1, "x"),
    GVAR_ENTRY("bipart.cc", BIPART_RIGHT_PROJ, 1, "x"),
    GVAR_ENTRY("bipart.cc", BIPART_STAR, 1, "x"),
    GVAR_ENTRY("bipart.cc", BIPART_LAMBDA_CONJ, 2, "x, y"),
    GVAR_ENTRY("bipart.cc", BIPART_STAB_ACTION, 2, "x, p"),
    GVAR_ENTRY("bipart.cc", BIPART_LEFT_BLOCKS, 1, "x"),
    GVAR_ENTRY("bipart.cc", BIPART_RIGHT_BLOCKS, 1, "x"),
    GVAR_ENTRY("bipart.cc", BLOCKS_NC, 1, "blocks"),
    GVAR_ENTRY("bipart.cc", BLOCKS_EXT_REP, 1, "blocks"),
    GVAR_ENTRY("bipart.cc", BLOCKS_DEGREE, 1, "blocks"),
    GVAR_ENTRY("bipart.cc", BLOCKS_RANK, 1, "blocks"),
    GVAR_ENTRY("bipart.cc", BLOCKS_NR_BLOCKS, 1, "blocks"),
    GVAR_ENTRY("bipart.cc", BLOCKS_HASH, 2, "blocks, data"),
    GVAR_ENTRY("bipart.cc", BLOCKS_PROJ, 1, "blocks"),
    GVAR_ENTRY("bipart.cc", BLOCKS_EQ, 2, "blocks1, blocks2"),
    GVAR_ENTRY("bipart.cc", BLOCKS_LT, 2, "blocks1, blocks2"),
    GVAR_ENTRY("bipart.cc", BLOCKS_E_TESTER, 2, "left, right"),
    GVAR_ENTRY("bipart.cc", BLOCKS_E_CREATOR, 2, "left, right"),
    GVAR_ENTRY("bipart.cc", BLOCKS_LEFT_ACT, 2, "blocks, x"),
    GVAR_ENTRY("bipart.cc", BLOCKS_RIGHT_ACT, 2, "blocks, x"),
    GVAR_ENTRY("bipart.cc", BLOCKS_INV_LEFT, 2, "blocks, x"),
    GVAR_ENTRY("bipart.cc", BLOCKS_INV_RIGHT, 2, "blocks, x"),
    GVAR_ENTRY("bipart.cc",
               BIPART_NR_IDEMPOTENTS,
               5,
               "o, scc, lookup, nr_threads, report"),
    {0, 0, 0, 0, 0} /* Finish with an empty entry */
};

/******************************************************************************
*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel(StructInitInfo* module) {
  /* init filters and functions                                          */
  InitHdlrFiltsFromTable(GVarFilts);
  InitHdlrFuncsFromTable(GVarFuncs);

  ImportGVarFromLibrary("SEMIGROUPS", &SEMIGROUPS);

  // T_SEMI
  T_SEMI                = RegisterPackageTNUM("TSemiObj", TSemiObjTypeFunc);
  InfoBags[T_SEMI].name = "Semigroups package C++ type";
  PrintObjFuncs[T_SEMI] = TSemiObjPrintFunc;
  SaveObjFuncs[T_SEMI]  = TSemiObjSaveFunc;
  LoadObjFuncs[T_SEMI]  = TSemiObjLoadFunc;

  InitMarkFuncBags(T_SEMI, &MarkNoSubBags);
  InitFreeFuncBag(T_SEMI, &TSemiObjFreeFunc);

  InitCopyGVar("TheTypeTSemiObj", &TheTypeTSemiObj);

  // TODO(JDM): CopyObjFuncs, CleanObjFuncs, IsMutableObjFuncs for T_SEMI bags

  // T_BIPART
  T_BIPART = RegisterPackageTNUM("TBipartObj", TBipartObjTypeFunc);
  InfoBags[T_BIPART].name = "bipartition";

  CopyObjFuncs[T_BIPART]      = &TBipartObjCopyFunc;
  CleanObjFuncs[T_BIPART]     = &TBipartObjCleanFunc;
  IsMutableObjFuncs[T_BIPART] = &TBipartObjIsMutableObjFuncs;

  SaveObjFuncs[T_BIPART] = TBipartObjSaveFunc;
  LoadObjFuncs[T_BIPART] = TBipartObjLoadFunc;

  InitMarkFuncBags(T_BIPART, &TBipartObjMarkSubBags);
  InitFreeFuncBag(T_BIPART, &TBipartObjFreeFunc);

  ProdFuncs[T_BIPART][T_BIPART] = BIPART_PROD;
  EqFuncs[T_BIPART][T_BIPART]   = BIPART_EQ;
  LtFuncs[T_BIPART][T_BIPART]   = BIPART_LT;

  ImportGVarFromLibrary("TYPE_BIPART", &TYPE_BIPART);
  ImportGVarFromLibrary("TYPES_BIPART", &TYPES_BIPART);

  // T_BLOCKS
  T_BLOCKS = RegisterPackageTNUM("TBlocksObj", TBlocksObjTypeFunc);
  InfoBags[T_BLOCKS].name = "blocks";

  CopyObjFuncs[T_BLOCKS]      = &TBlocksObjCopyFunc;
  CleanObjFuncs[T_BLOCKS]     = &TBlocksObjCleanFunc;
  IsMutableObjFuncs[T_BLOCKS] = &TBlocksObjIsMutableObjFuncs;

  SaveObjFuncs[T_BLOCKS] = TBlocksObjSaveFunc;
  LoadObjFuncs[T_BLOCKS] = TBlocksObjLoadFunc;

  InitMarkFuncBags(T_BLOCKS, &MarkNoSubBags);
  InitFreeFuncBag(T_BLOCKS, &TBlocksObjFreeFunc);

  EqFuncs[T_BLOCKS][T_BLOCKS] = BLOCKS_EQ;
  LtFuncs[T_BLOCKS][T_BLOCKS] = BLOCKS_LT;

  InitCopyGVar("TheTypeTBlocksObj", &TheTypeTBlocksObj);

  // Import other stuff
  ImportGVarFromLibrary("HTValue", &HTValue);
  ImportGVarFromLibrary("HTAdd", &HTAdd);

  ImportGVarFromLibrary("infinity", &Pinfinity);
  ImportGVarFromLibrary("Ninfinity", &Ninfinity);

  ImportGVarFromLibrary("TYPES_PBR", &TYPES_PBR);
  ImportGVarFromLibrary("TYPE_PBR", &TYPE_PBR);

  ImportGVarFromLibrary("IsPBR", &IsPBR);
  ImportGVarFromLibrary("DegreeOfPBR", &DegreeOfPBR);

  ImportGVarFromLibrary("IsBooleanMat", &IsBooleanMat);
  ImportGVarFromLibrary("BooleanMatType", &BooleanMatType);

  ImportGVarFromLibrary("IsMatrixOverSemiring", &IsMatrixOverSemiring);
  ImportGVarFromLibrary("DimensionOfMatrixOverSemiring",
                        &DimensionOfMatrixOverSemiring);

  ImportGVarFromLibrary("IsMaxPlusMatrix", &IsMaxPlusMatrix);
  ImportGVarFromLibrary("MaxPlusMatrixType", &MaxPlusMatrixType);

  ImportGVarFromLibrary("IsMinPlusMatrix", &IsMinPlusMatrix);
  ImportGVarFromLibrary("MinPlusMatrixType", &MinPlusMatrixType);

  ImportGVarFromLibrary("IsTropicalMatrix", &IsTropicalMatrix);

  ImportGVarFromLibrary("IsTropicalMaxPlusMatrix", &IsTropicalMaxPlusMatrix);
  ImportGVarFromLibrary("TropicalMaxPlusMatrixType",
                        &TropicalMaxPlusMatrixType);

  ImportGVarFromLibrary("IsTropicalMinPlusMatrix", &IsTropicalMinPlusMatrix);
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

  ImportGVarFromLibrary("FROPIN", &FROPIN);

  ImportGVarFromLibrary("GeneratorsOfMagma", &GeneratorsOfMagma);

  ImportGVarFromLibrary("IsSemigroup", &IsSemigroup);
  ImportGVarFromLibrary("IsSemigroupIdeal", &IsSemigroupIdeal);
  ImportGVarFromLibrary("IsActingSemigroup", &IsActingSemigroup);

  /* return success                                                      */
  return 0;
}

/******************************************************************************
*F  InitLibrary( <module> ) . . . . . . .  initialise library data structures
*/
static Int InitLibrary(StructInitInfo* module) {
  /* init filters and functions */
  InitGVarFiltsFromTable(GVarFilts);
  InitGVarFuncsFromTable(GVarFuncs);

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
    /* postRestore = */ 0};

extern "C" StructInitInfo* Init__Dynamic(void) {
  return &module;
}
