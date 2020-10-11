//
// Semigroups package for GAP
// Copyright (C) 2016-2021 James D. Mitchell
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
#include "fropin.h"
#include "semigroups-debug.h"
#include "to_gap.hpp"

#include "libsemigroups/blocks.hpp"
#include "libsemigroups/cong.hpp"
#include "libsemigroups/fastest-bmat.hpp"
#include "libsemigroups/fpsemi.hpp"
#include "libsemigroups/froidure-pin.hpp"
#include "libsemigroups/matrix.hpp"
#include "libsemigroups/transf.hpp"

#include "gapbind14/gapbind14.hpp"

namespace {
  void set_report(bool const val) {
    libsemigroups::REPORTER.report(val);
  }
}  // namespace

////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

GAPBIND14_MODULE(libsemigroups, m);

GAPBIND14_FUNCTION(m, set_report, set_report);

////////////////////////////////////////////////////////////////////////
// FroidurePin
////////////////////////////////////////////////////////////////////////

#define BindFroidurePin(variable, element_type)                               \
  auto GAPBIND14_TOKENPASTE(variable, Copy)                                   \
      = gapbind14::init<variable, variable const&>;                           \
  GAPBIND14_CLASS(m, variable);                                               \
  GAPBIND14_CONSTRUCTOR(m, variable, create, gapbind14::init<variable>);      \
  GAPBIND14_CONSTRUCTOR(                                                      \
      m, variable, copy, GAPBIND14_TOKENPASTE(variable, Copy));               \
  GAPBIND14_MEM_FN(m, variable, add_generator, add_generator);                \
  GAPBIND14_MEM_FN(m, variable, generator, generator);                        \
  GAPBIND14_MEM_FN(m, variable, closure<std::vector<element_type>>, closure); \
  GAPBIND14_MEM_FN(m, variable, nr_generators, nr_generators);                \
  GAPBIND14_MEM_FN(m, variable, size, size);                                  \
  GAPBIND14_MEM_FN(m, variable, at, at);                                      \
  GAPBIND14_MEM_FN(m, variable, sorted_at, sorted_at);                        \
  GAPBIND14_MEM_FN(m, variable, position, position);                          \
  GAPBIND14_MEM_FN(m, variable, current_position, current_position);          \
  GAPBIND14_MEM_FN(m, variable, sorted_position, sorted_position);            \
  GAPBIND14_MEM_FN(m, variable, nr_idempotents, nr_idempotents);              \
  GAPBIND14_MEM_FN(m, variable, enumerate, enumerate);                        \
  GAPBIND14_MEM_FN(m, variable, left_cayley_graph, left_cayley_graph);        \
  GAPBIND14_MEM_FN(m, variable, right_cayley_graph, right_cayley_graph);      \
  GAPBIND14_MEM_FN(m,                                                         \
                   variable,                                                  \
                   factorisation,                                             \
                   factorisation,                                             \
                   gapbind14::overload_cast<size_t>);                         \
  GAPBIND14_ITERATOR(                                                         \
      m, variable, cbegin_idempotents(), cend_idempotents(), idempotents);    \
  GAPBIND14_ITERATOR(m, variable, cbegin_rules(), cend_rules(), rules);       \
  GAPBIND14_MEM_FN(                                                           \
      m, variable, position_to_sorted_position, position_to_sorted_position); \
  GAPBIND14_MEM_FN(m, variable, fast_product, fast_product);                  \
  GAPBIND14_MEM_FN(m, variable, is_idempotent, is_idempotent);                \
  GAPBIND14_MEM_FN(m, variable, finished, finished);

////////////////////////////////////////////////////////////////////////

// TODO must implement to_gap/to_cpp for BMat8 + HPCombi::BMat8
// using FroidurePinBMat8
//     = libsemigroups::FroidurePin<libsemigroups::FastestBMat<8>>;
// BindFroidurePin(FroidurePinBMat8);

namespace {
  using FroidurePinBMat   = libsemigroups::FroidurePin<libsemigroups::BMat<>>;
  using FroidurePinIntMat = libsemigroups::FroidurePin<libsemigroups::IntMat<>>;
  using FroidurePinMaxPlusMat
      = libsemigroups::FroidurePin<libsemigroups::MaxPlusMat<>>;
  using FroidurePinMinPlusMat
      = libsemigroups::FroidurePin<libsemigroups::MinPlusMat<>>;
  using FroidurePinMaxPlusTruncMat
      = libsemigroups::FroidurePin<libsemigroups::MaxPlusTruncMat<>>;
  using FroidurePinMinPlusTruncMat
      = libsemigroups::FroidurePin<libsemigroups::MinPlusTruncMat<>>;
  using FroidurePinNTPMat = libsemigroups::FroidurePin<libsemigroups::NTPMat<>>;
  using FroidurePinProjMaxPlusMat
      = libsemigroups::FroidurePin<libsemigroups::ProjMaxPlusMat<>>;
  using FroidurePinTransf16
      = libsemigroups::FroidurePin<libsemigroups::LeastTransf<16>>;
  using TransfUInt2            = libsemigroups::Transf<0, UInt2>;
  using FroidurePinTransfUInt2 = libsemigroups::FroidurePin<TransfUInt2>;
  using TransfUInt4            = libsemigroups::Transf<0, UInt4>;
  using FroidurePinTransfUInt4 = libsemigroups::FroidurePin<TransfUInt4>;
  using FroidurePinPPerm16
      = libsemigroups::FroidurePin<libsemigroups::LeastPPerm<16>>;
  using PPermUInt2            = libsemigroups::PPerm<0, UInt2>;
  using FroidurePinPPermUInt2 = libsemigroups::FroidurePin<PPermUInt2>;
  using PPermUInt4            = libsemigroups::PPerm<0, UInt4>;
  using FroidurePinPPermUInt4 = libsemigroups::FroidurePin<PPermUInt4>;
  using FroidurePinBipart     = libsemigroups::FroidurePin<Bipartition>;
  using FroidurePinPBR        = libsemigroups::FroidurePin<libsemigroups::PBR>;
}  // namespace

BindFroidurePin(FroidurePinBipart, Bipartition);
BindFroidurePin(FroidurePinBMat, libsemigroups::BMat<>);
// GAPBIND14_CONSTRUCTOR(m, FroidurePinBMat, copy, FroidurePinBMatCopy);
BindFroidurePin(FroidurePinIntMat, libsemigroups::IntMat<>);
BindFroidurePin(FroidurePinMaxPlusMat, libsemigroups::MaxPlusMat<>);
BindFroidurePin(FroidurePinMaxPlusTruncMat, libsemigroups::MaxPlusTruncMat<>);
BindFroidurePin(FroidurePinMinPlusMat, libsemigroups::MinPlusMat<>);
BindFroidurePin(FroidurePinMinPlusTruncMat, libsemigroups::MinPlusTruncMat<>);
BindFroidurePin(FroidurePinNTPMat, libsemigroups::NTPMat<>);
BindFroidurePin(FroidurePinProjMaxPlusMat, libsemigroups::ProjMaxPlusMat<>);
BindFroidurePin(FroidurePinPBR, libsemigroups::PBR);
BindFroidurePin(FroidurePinPPerm16, libsemigroups::LeastPPerm<16>);
BindFroidurePin(FroidurePinPPermUInt2, PPermUInt2);
BindFroidurePin(FroidurePinPPermUInt4, PPermUInt4);
BindFroidurePin(FroidurePinTransf16, libsemigroups::LeastTransf<16>);
BindFroidurePin(FroidurePinTransfUInt2, TransfUInt2);
BindFroidurePin(FroidurePinTransfUInt4, TransfUInt4);

////////////////////////////////////////////////////////////////////////
// FpSemigroup
////////////////////////////////////////////////////////////////////////

using libsemigroups::FpSemigroup;
using libsemigroups::word_type;

GAPBIND14_CLASS(m, FpSemigroup);
GAPBIND14_CONSTRUCTOR(m, FpSemigroup, create, gapbind14::init<FpSemigroup>);
GAPBIND14_MEM_FN(m,
                 FpSemigroup,
                 set_alphabet,
                 set_alphabet,
                 (gapbind14::overload_cast<size_t>) );
GAPBIND14_MEM_FN(
    m,
    FpSemigroup,
    add_rule,
    add_rule,
    (gapbind14::overload_cast<word_type const&, word_type const&>) );
GAPBIND14_MEM_FN(m,
                 FpSemigroup,
                 set_identity,
                 set_identity,
                 (gapbind14::overload_cast<libsemigroups::letter_type>) );

////////////////////////////////////////////////////////////////////////
// ToddCoxeter
////////////////////////////////////////////////////////////////////////

using libsemigroups::congruence_type;
using libsemigroups::congruence::ToddCoxeter;
using table_type = libsemigroups::congruence::ToddCoxeter::table_type;

namespace {
  auto init_todd_coxeter = gapbind14::init<ToddCoxeter, congruence_type>;
}

GAPBIND14_CLASS(m, ToddCoxeter);
GAPBIND14_CONSTRUCTOR(m, ToddCoxeter, create, init_todd_coxeter);
GAPBIND14_MEM_FN(m, ToddCoxeter, set_nr_generators, set_nr_generators);
GAPBIND14_MEM_FN(m,
                 ToddCoxeter,
                 prefill,
                 prefill,
                 (gapbind14::overload_cast<table_type const&>) );

////////////////////////////////////////////////////////////////////////
// Congruence
////////////////////////////////////////////////////////////////////////

using libsemigroups::Congruence;
using libsemigroups::FroidurePinBase;

namespace {
  auto init_congruence_bipart
      = gapbind14::init<Congruence, congruence_type, FroidurePinBipart const&>;
  auto init_congruence_bmat
      = gapbind14::init<Congruence, congruence_type, FroidurePinBMat const&>;
  auto init_congruence_pbr
      = gapbind14::init<Congruence, congruence_type, FroidurePinPBR const&>;
  auto init_congruence_pperm16
      = gapbind14::init<Congruence, congruence_type, FroidurePinPPerm16 const&>;
  auto init_congruence_ppermuint2 = gapbind14::
      init<Congruence, congruence_type, FroidurePinPPermUInt2 const&>;
  auto init_congruence_ppermuint4 = gapbind14::
      init<Congruence, congruence_type, FroidurePinPPermUInt4 const&>;
  auto init_congruence_transf16 = gapbind14::
      init<Congruence, congruence_type, FroidurePinTransf16 const&>;
  auto init_congruence_transfuint2 = gapbind14::
      init<Congruence, congruence_type, FroidurePinTransfUInt2 const&>;
  auto init_congruence_transfuint4 = gapbind14::
      init<Congruence, congruence_type, FroidurePinTransfUInt4 const&>;
  auto init_congruence_fpsemigroup
      = gapbind14::init<Congruence, congruence_type, FpSemigroup&>;
  auto init_congruence_table = gapbind14::
      init<Congruence, congruence_type, Congruence::policy::runners>;
}  // namespace

GAPBIND14_CLASS(m, Congruence);
GAPBIND14_CONSTRUCTOR(m,
                      Congruence,
                      create_fpsemigroup,
                      init_congruence_fpsemigroup);
GAPBIND14_CONSTRUCTOR(m, Congruence, create_bipart, init_congruence_bipart);
GAPBIND14_CONSTRUCTOR(m, Congruence, create_bmat, init_congruence_bmat);
GAPBIND14_CONSTRUCTOR(m, Congruence, create_pbr, init_congruence_pbr);
GAPBIND14_CONSTRUCTOR(m, Congruence, create_pperm16, init_congruence_pperm16);
GAPBIND14_CONSTRUCTOR(m,
                      Congruence,
                      create_ppermuint2,
                      init_congruence_ppermuint2);
GAPBIND14_CONSTRUCTOR(m,
                      Congruence,
                      create_ppermuint4,
                      init_congruence_ppermuint4);
GAPBIND14_CONSTRUCTOR(m, Congruence, create_transf16, init_congruence_transf16);
GAPBIND14_CONSTRUCTOR(m,
                      Congruence,
                      create_transfuint2,
                      init_congruence_transfuint2);
GAPBIND14_CONSTRUCTOR(m,
                      Congruence,
                      create_transfuint4,
                      init_congruence_transfuint4);
GAPBIND14_CONSTRUCTOR(m, Congruence, create_table, init_congruence_table);

GAPBIND14_MEM_FN(m, Congruence, set_nr_generators, set_nr_generators);
GAPBIND14_MEM_FN(m, Congruence, nr_generating_pairs, nr_generating_pairs);
GAPBIND14_MEM_FN(
    m,
    Congruence,
    add_pair,
    add_pair,
    (gapbind14::overload_cast<word_type const&, word_type const&>) );
GAPBIND14_MEM_FN(m, Congruence, nr_classes, nr_classes);
GAPBIND14_MEM_FN(m, Congruence, word_to_class_index, word_to_class_index);
GAPBIND14_MEM_FN(m, Congruence, class_index_to_word, class_index_to_word);
GAPBIND14_MEM_FN(m, Congruence, contains, contains);
GAPBIND14_MEM_FN(m, Congruence, less, less);
GAPBIND14_MEM_FN(m, Congruence, add_runner<ToddCoxeter>, add_runner);
GAPBIND14_ITERATOR(m, Congruence, cbegin_ntc(), cend_ntc(), ntc);

////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

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

// Function to print a T_SEMI Obj.

void TSemiObjPrintFunc(Obj o) {
  switch (SUBTYPE_OF_T_SEMI(o)) {
    case T_SEMI_SUBTYPE_CONG: {
      Pr("<wrapper for instance of C++ Congruence class>", 0L, 0L);
      break;
    }
    default: {
      SEMIGROUPS_ASSERT(false);
    }
  }
}

Obj TBipartObjCopyFunc(Obj o, Int mut) {
  // Bipartition objects are mathematically immutable, so
  // we don't need to do anything,
  return o;
}

Obj TBlocksObjCopyFunc(Obj o, Int mut) {
  // Blocks objects are mathematically immutable, so we don't need to do
  // anything,
  return o;
}

void TBipartObjCleanFunc(Obj o) {}

void TBlocksObjCleanFunc(Obj o) {}

// Function to free a T_SEMI Obj during garbage collection.

void TSemiObjFreeFunc(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_SEMI);
  switch (SUBTYPE_OF_T_SEMI(o)) {
    case T_SEMI_SUBTYPE_CONG: {
      delete CLASS_OBJ<Congruence*>(o);
      break;
    }
    default: {
      SEMIGROUPS_ASSERT(false);
    }
  }
}

void TBipartObjFreeFunc(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_BIPART);
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
}

void TSemiObjLoadFunc(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_SEMI);

  t_semi_subtype_t type = static_cast<t_semi_subtype_t>(LoadUInt4());
  ADDR_OBJ(o)[0]        = reinterpret_cast<Obj>(type);

  switch (type) {
    case T_SEMI_SUBTYPE_CONG: {
      ADDR_OBJ(o)[1] = static_cast<Obj>(nullptr);
      break;
    }
    default: {
      SEMIGROUPS_ASSERT(false);
    }
  }
}

Obj TSemiObjCopyFunc(Obj o, Int mut) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_SEMI);
  return o;
}

Int TSemiObjIsMutableObjFunc(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_SEMI);
  return 0L;
}

void TSemiObjCleanFunc(Obj o) {}

void TBipartObjSaveFunc(Obj o) {
  Bipartition* b = bipart_get_cpp(o);
  SaveUInt4(b->degree());
  for (auto it = b->begin(); it < b->end(); it++) {
    SaveUInt4(*it);
  }
}

void TBipartObjLoadFunc(Obj o) {
  UInt4                  deg = LoadUInt4();
  std::vector<u_int32_t> blocks;
  blocks.reserve(2 * deg);

  for (size_t i = 0; i < 2 * deg; i++) {
    blocks.push_back(LoadUInt4());
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
    MarkBag(ADDR_OBJ(o)[1]);
  }
  if (ADDR_OBJ(o)[2] != NULL) {
    MarkBag(ADDR_OBJ(o)[2]);
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
Obj IsInfinity;
Obj IsNegInfinity;
Obj IsBooleanMat;
Obj BooleanMatType;
Obj IsMaxPlusMatrix;
Obj MaxPlusMatrixType;
Obj IsMinPlusMatrix;
Obj MinPlusMatrixType;
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
Obj GeneratorsOfMagma;
Obj LARGEST_MOVED_PT_TRANS;

Obj IsSemigroup;
Obj IsSemigroupIdeal;
Obj IsActingSemigroup;

Obj PositionCanonical;

/*****************************************************************************
 *V  GVarFilts . . . . . . . . . . . . . . . . . . . list of filters to export
 */

#if defined(GAP_KERNEL_MAJOR_VERSION) && (GAP_KERNEL_MAJOR_VERSION >= 2)
typedef Obj (*GVarFilt)(Obj, Obj);
#else
typedef Obj (*GVarFilt)(/*arguments*/);
#endif

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

    GVAR_ENTRY("fropin.cc",
               SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS,
               2,
               "scc1, scc2"),

    GVAR_ENTRY("fropin.cc", FIND_HCLASSES, 2, "left, right"),
    GVAR_ENTRY("fropin.cc", RUN_FROIDURE_PIN, 2, "obj, limit"),

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
  gapbind14::init_kernel(m);

  /* init filters and functions */
  InitHdlrFiltsFromTable(GVarFilts);
  InitHdlrFuncsFromTable(GVarFuncs);

  ImportGVarFromLibrary("SEMIGROUPS", &SEMIGROUPS);

  // T_SEMI
  T_SEMI = RegisterPackageTNUM("Semigroups package C++ type", TSemiObjTypeFunc);

  PrintObjFuncs[T_SEMI]     = TSemiObjPrintFunc;
  SaveObjFuncs[T_SEMI]      = TSemiObjSaveFunc;
  LoadObjFuncs[T_SEMI]      = TSemiObjLoadFunc;
  CopyObjFuncs[T_SEMI]      = &TSemiObjCopyFunc;
  CleanObjFuncs[T_SEMI]     = &TSemiObjCleanFunc;
  IsMutableObjFuncs[T_SEMI] = &TSemiObjIsMutableObjFunc;

  InitMarkFuncBags(T_SEMI, &MarkNoSubBags);
  InitFreeFuncBag(T_SEMI, &TSemiObjFreeFunc);

  InitCopyGVar("TheTypeTSemiObj", &TheTypeTSemiObj);

  // T_BIPART
  T_BIPART = RegisterPackageTNUM("bipartition", TBipartObjTypeFunc);

  CopyObjFuncs[T_BIPART]      = &TBipartObjCopyFunc;
  CleanObjFuncs[T_BIPART]     = &TBipartObjCleanFunc;
  IsMutableObjFuncs[T_BIPART] = &AlwaysNo;

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
  T_BLOCKS = RegisterPackageTNUM("blocks", TBlocksObjTypeFunc);

  CopyObjFuncs[T_BLOCKS]      = &TBlocksObjCopyFunc;
  CleanObjFuncs[T_BLOCKS]     = &TBlocksObjCleanFunc;
  IsMutableObjFuncs[T_BLOCKS] = &AlwaysNo;

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

  ImportGVarFromLibrary("IsInfinity", &IsInfinity);
  ImportGVarFromLibrary("IsNegInfinity", &IsNegInfinity);

  ImportGVarFromLibrary("TYPES_PBR", &TYPES_PBR);
  ImportGVarFromLibrary("TYPE_PBR", &TYPE_PBR);

  ImportGVarFromLibrary("IsPBR", &IsPBR);
  ImportGVarFromLibrary("DegreeOfPBR", &DegreeOfPBR);

  ImportGVarFromLibrary("IsBooleanMat", &IsBooleanMat);
  ImportGVarFromLibrary("BooleanMatType", &BooleanMatType);

  ImportGVarFromLibrary("IsMaxPlusMatrix", &IsMaxPlusMatrix);
  ImportGVarFromLibrary("MaxPlusMatrixType", &MaxPlusMatrixType);

  ImportGVarFromLibrary("IsMinPlusMatrix", &IsMinPlusMatrix);
  ImportGVarFromLibrary("MinPlusMatrixType", &MinPlusMatrixType);

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

  ImportGVarFromLibrary("GeneratorsOfMagma", &GeneratorsOfMagma);
  ImportGVarFromLibrary("LARGEST_MOVED_PT_TRANS", &LARGEST_MOVED_PT_TRANS);

  ImportGVarFromLibrary("IsSemigroup", &IsSemigroup);
  ImportGVarFromLibrary("IsSemigroupIdeal", &IsSemigroupIdeal);
  ImportGVarFromLibrary("IsActingSemigroup", &IsActingSemigroup);

  ImportGVarFromLibrary("PositionCanonical", &PositionCanonical);

  return 0;
}

static Int InitLibrary(StructInitInfo* module) {
  gapbind14::init_library(m);
  InitGVarFiltsFromTable(GVarFilts);
  InitGVarFuncsFromTable(GVarFuncs);
  libsemigroups::REPORTER.report(false);

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
