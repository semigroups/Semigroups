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

#include "semigrp.h"

#include <algorithm>
#include <string>
#include <utility>
#include <vector>

#include "bipart.h"
#include "converter.h"
#include "fropin.h"
#include "pkg.h"
#include "src/compiled.h"

using libsemigroups::word_t;
using libsemigroups::cayley_graph_t;
using libsemigroups::MaxPlusSemiring;
using libsemigroups::MinPlusSemiring;
using libsemigroups::TropicalMaxPlusSemiring;
using libsemigroups::TropicalMinPlusSemiring;
using libsemigroups::NaturalSemiring;
using libsemigroups::Integers;
using libsemigroups::really_delete_cont;
using libsemigroups::MatrixOverSemiring;

#ifdef SEMIGROUPS_KERNEL_DEBUG
#define ERROR(obj, message)                               \
  char buf[128];                                          \
  strncpy(buf, __func__, sizeof(buf));                    \
  strncat(buf, ": ", (sizeof(buf) * 2));                  \
  strncat(buf, message, (sizeof(buf) * strlen(message))); \
  strncat(buf, " not a %s,", (sizeof(buf) * 9));          \
  ErrorQuit(buf, (Int) TNAM_OBJ(obj), 0L);

#define CHECK_SEMI_OBJ(obj)                        \
  if (CALL_1ARGS(IsSemigroup, obj) != True) {      \
    ERROR(obj, "the argument must be a semigroup") \
  }
#define CHECK_PLIST(obj)                            \
  if (!IS_PLIST(obj)) {                             \
    ERROR(obj, "the argument must be a plain list") \
  }
#define CHECK_LIST(obj)                       \
  if (!IS_LIST(obj)) {                        \
    ERROR(obj, "the argument must be a list") \
  }
#define CHECK_INTOBJ(obj)                         \
  if (!IS_INTOBJ(obj)) {                          \
    ERROR(obj, "the argument must be an integer") \
  }
#define CHECK_POS_INTOBJ(obj)                                  \
  CHECK_INTOBJ(obj)                                            \
  if (INT_INTOBJ(obj) < 0) {                                   \
    ERROR(obj, "the argument must be an non-negative integer") \
  }
#else
#define CHECK_SEMI_OBJ(so)
#define CHECK_PLIST(obj)
#define CHECK_LIST(obj)
#define CHECK_INTOBJ(obj)
#define CHECK_POS_INTOBJ(obj)
#endif

std::vector<Element const*>*
plist_to_vec(Converter* converter, gap_list_t elements, size_t degree) {
  SEMIGROUPS_ASSERT(IS_PLIST(elements));

  auto out = new std::vector<Element const*>();

  for (size_t i = 0; i < (size_t) LEN_PLIST(elements); i++) {
    out->push_back(converter->convert(ELM_LIST(elements, i + 1), degree));
  }
  return out;
}

template <typename T>
static inline gap_list_t
iterator_to_plist(Converter* converter, T first, T last) {
  gap_list_t out =
      NEW_PLIST((first == last ? T_PLIST_EMPTY : T_PLIST_HOM), last - first);
  SET_LEN_PLIST(out, last - first);
  size_t i = 1;
  for (auto it = first; it < last; ++it) {
    SET_ELM_PLIST(out, i++, converter->unconvert(*it));
  }
  CHANGED_BAG(out);
  return out;
}

gap_list_t word_t_to_plist(word_t const& word) {
  SEMIGROUPS_ASSERT(!word.empty());
  gap_list_t out = NEW_PLIST(T_PLIST_CYC + IMMUTABLE, word.size());
  // IMMUTABLE since it should not be altered on the GAP level
  SET_LEN_PLIST(out, word.size());

  for (size_t i = 0; i < word.size(); i++) {
    SET_ELM_PLIST(out, i + 1, INTOBJ_INT(word[i] + 1));
  }
  CHANGED_BAG(out);
  return out;
}

// Semigroups

gap_list_t semi_obj_get_gens(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  UInt i;
  if (FindPRec(so, RNam_GeneratorsOfMagma, &i, 1)) {
    gap_list_t gens = GET_ELM_PREC(so, i);
    PLAIN_LIST(gens);
    CHANGED_BAG(gens);
    return gens;
  } else {
#ifdef SEMIGROUPS_KERNEL_DEBUG
    // This is included since the methods for finding generating sets for
    // acting semigroup ideals may use the output of the F-P algorithm (Green's
    // relations etc), and so if we are here we are about to call
    // GeneratorsOfMagma from inside the F-P algorithm, leading to an infinite
    // loop.
    if (CALL_1ARGS(IsSemigroupIdeal, so) == True
        && CALL_1ARGS(IsActingSemigroup, so) == True) {
      ErrorQuit("the argument must not be an acting semigroup ideal,", 0L, 0L);
    }
#endif

    CALL_1ARGS(GeneratorsOfMagma, so);
    if (FindPRec(so, RNam_GeneratorsOfMagma, &i, 1)) {
      gap_list_t gens = GET_ELM_PREC(so, i);
      PLAIN_LIST(gens);
      CHANGED_BAG(gens);
      CHANGED_BAG(so);
      return gens;
    }
    ErrorQuit("cannot find generators of the semigroup,", 0L, 0L);
    return 0L;
  }
}

gap_element_t semi_obj_get_rep(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  UInt i;
  if (FindPRec(so, RNam_Representative, &i, 1)) {
    return GET_ELM_PREC(so, i);
  } else {
    gap_list_t gens = semi_obj_get_gens(so);
    if (LEN_PLIST(gens) > 0) {
      return ELM_PLIST(gens, 1);
    } else {
      ErrorQuit("cannot find a representative of the semigroup,", 0L, 0L);
      return 0L;
    }
  }
}

Obj get_default_value(Int rnam) {
  gap_rec_t opts = ElmPRec(SEMIGROUPS, RNam_DefaultOptionsRec);
  return ElmPRec(opts, rnam);
}

size_t semi_obj_get_batch_size(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  UInt i;
  if (FindPRec(so, RNam_opts, &i, 1)) {
    gap_rec_t opts = GET_ELM_PREC(so, i);
    if (FindPRec(opts, RNam_batch_size, &i, 1)) {
      return INT_INTOBJ(GET_ELM_PREC(opts, i));
    }
  }
  return INT_INTOBJ(get_default_value(RNam_batch_size));
}

bool semi_obj_get_report(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  UInt i;
  if (FindPRec(so, RNam_opts, &i, 1)) {
    gap_rec_t opts = GET_ELM_PREC(so, i);
    if (FindPRec(opts, RNam_report, &i, 1)) {
      return (GET_ELM_PREC(opts, i) == True ? true : false);
    }
  }
  return (get_default_value(RNam_report) == True ? true : false);
}

static inline size_t semi_obj_get_nr_threads(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  UInt i;
  if (FindPRec(so, RNam_opts, &i, 1)) {
    gap_rec_t opts = GET_ELM_PREC(so, i);
    if (FindPRec(opts, RNam_nr_threads, &i, 1)) {
      return INT_INTOBJ(GET_ELM_PREC(opts, i));
    }
  }
  return INT_INTOBJ(get_default_value(RNam_nr_threads));
}

static inline size_t semi_obj_get_threshold(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  gap_element_t x = semi_obj_get_rep(so);
  SEMIGROUPS_ASSERT(TNUM_OBJ(x) == T_POSOBJ);
  SEMIGROUPS_ASSERT(CALL_1ARGS(IsTropicalMatrix, x)
                    || CALL_1ARGS(IsNTPMatrix, x));
  SEMIGROUPS_ASSERT(ELM_PLIST(x, 1) != 0);
  SEMIGROUPS_ASSERT(IS_PLIST(ELM_PLIST(x, 1)));
  SEMIGROUPS_ASSERT(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
}

static inline size_t semi_obj_get_period(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  gap_element_t x = semi_obj_get_rep(so);
  SEMIGROUPS_ASSERT(TNUM_OBJ(x) == T_POSOBJ);
  SEMIGROUPS_ASSERT(CALL_1ARGS(IsNTPMatrix, x));
  SEMIGROUPS_ASSERT(ELM_PLIST(x, 1) != 0);
  SEMIGROUPS_ASSERT(IS_PLIST(ELM_PLIST(x, 1)));
  SEMIGROUPS_ASSERT(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2));
}

// The en_semi data structure which is an Obj of type T_SEMI organised as
// follows if <type> is not UNKNOWN:
//
//  0: t_semi_subtype_t,
//  1: en_semi_t         type,
//  2: gap_semigroup_t   so,
//  3: size_t            degree,
//  4: Converter*,
//  5: Semigroup*
//
// To call en_semi_init_converter positions 0 to 3 must already be set, and 4
// and 5 must be nullptrs. To call en_semi_init_semigroup,
// en_semi_init_converter must already have been called.
//
// If <type> is UNKNOWN, then the en_semi data structure looks like:
//
//  [t_semi_subtype_t, en_semi_t type]
//
//  and the whole object is only required so that the fact that
//  its en_semi_t is UNKNOWN.

Converter* en_semi_init_converter(en_semi_obj_t es) {
  SEMIGROUPS_ASSERT(en_semi_get_type(es) != UNKNOWN);
  SEMIGROUPS_ASSERT(CLASS_OBJ<Converter*>(es, 4) == nullptr);

  Converter* converter = nullptr;
  // semigroup Obj is required to get the threshold etc for matrices over a
  // semiring
  gap_semigroup_t so = en_semi_get_semi_obj(es);
  switch (en_semi_get_type(es)) {
    case TRANS2: {
      converter = new TransConverter<u_int16_t>();
      break;
    }
    case TRANS4: {
      converter = new TransConverter<u_int32_t>();
      break;
    }
    case PPERM2: {
      converter = new PPermConverter<u_int16_t>();
      break;
    }
    case PPERM4: {
      converter = new PPermConverter<u_int32_t>();
      break;
    }
    case BIPART: {
      converter = new BipartConverter();
      break;
    }
    case BOOL_MAT: {
      converter = new BoolMatConverter();
      break;
    }
    case MAX_PLUS_MAT: {
      converter = new MatrixOverSemiringConverter<MatrixOverSemiring<int64_t>>(
          new MaxPlusSemiring(), Ninfinity, MaxPlusMatrixType);
      break;
    }
    case MIN_PLUS_MAT: {
      converter = new MatrixOverSemiringConverter<MatrixOverSemiring<int64_t>>(
          new MinPlusSemiring(), Pinfinity, MinPlusMatrixType);
      break;
    }
    case TROP_MAX_PLUS_MAT: {
      converter = new MatrixOverSemiringConverter<MatrixOverSemiring<int64_t>>(
          new TropicalMaxPlusSemiring(semi_obj_get_threshold(so)),
          Ninfinity,
          TropicalMaxPlusMatrixType);
      break;
    }
    case TROP_MIN_PLUS_MAT: {
      converter = new MatrixOverSemiringConverter<MatrixOverSemiring<int64_t>>(
          new TropicalMinPlusSemiring(semi_obj_get_threshold(so)),
          Pinfinity,
          TropicalMinPlusMatrixType);
      break;
    }
    case PROJ_MAX_PLUS_MAT: {
      converter = new MatrixOverSemiringConverter<ProjectiveMaxPlusMatrix>(
          new MaxPlusSemiring(), Ninfinity, ProjectiveMaxPlusMatrixType);
      break;
    }
    case NTP_MAT: {
      converter = new MatrixOverSemiringConverter<MatrixOverSemiring<int64_t>>(
          new NaturalSemiring(semi_obj_get_threshold(so),
                              semi_obj_get_period(so)),
          INTOBJ_INT(0),
          NTPMatrixType);
      break;
    }
    case INT_MAT: {
      converter = new MatrixOverSemiringConverter<MatrixOverSemiring<int64_t>>(
          new Integers(), INTOBJ_INT(0), IntegerMatrixType);
      break;
    }
    case PBR_TYPE: {
      converter = new PBRConverter();
      break;
    }
    default: { SEMIGROUPS_ASSERT(false); }
  }
  ADDR_OBJ(es)[4] = reinterpret_cast<Obj>(converter);
  CHANGED_BAG(es);
  return converter;
}

Semigroup* en_semi_init_semigroup(en_semi_obj_t es) {
  SEMIGROUPS_ASSERT(en_semi_get_type(es) != UNKNOWN);
  SEMIGROUPS_ASSERT(CLASS_OBJ<Semigroup*>(es, 5) == nullptr);
  initRNams();

  if (en_semi_get_converter(es) == nullptr) {
    en_semi_init_converter(es);
  }

  gap_semigroup_t so        = en_semi_get_semi_obj(es);
  Converter*      converter = en_semi_get_converter(es);
  size_t          deg       = en_semi_get_degree(es);
  gap_list_t      plist     = semi_obj_get_gens(so);
  auto            gens      = plist_to_vec(converter, plist, deg);
  Semigroup*      semi_cpp  = new Semigroup(gens);
  semi_cpp->set_batch_size(semi_obj_get_batch_size(so));
  really_delete_cont(gens);
  ADDR_OBJ(es)[5] = reinterpret_cast<Obj>(semi_cpp);

  if (IsbPRec(so, RNam_Size)) {
    semi_cpp->reserve(INT_INTOBJ(ElmPRec(so, RNam_Size)));
  }

  return semi_cpp;
}

// Initialise the en_semi of the GAP semigroup <so>.

en_semi_obj_t semi_obj_init_en_semi(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  size_t    deg;
  en_semi_t type = UNKNOWN;

  gap_element_t x = semi_obj_get_rep(so);

  if (IS_TRANS(x)) {
    deg             = 0;
    gap_list_t gens = semi_obj_get_gens(so);
    for (size_t i = 1; i <= (size_t) LEN_PLIST(gens); i++) {
      size_t n = DEG_TRANS(ELM_PLIST(gens, i));
      if (n > deg) {
        deg = n;
      }
    }
    if (deg < 65536) {
      type = TRANS2;
    } else {
      type = TRANS4;
    }
  } else if (IS_PPERM(x)) {
    deg             = 0;
    gap_list_t gens = semi_obj_get_gens(so);
    for (size_t i = 1; i <= (size_t) LEN_PLIST(gens); i++) {
      size_t n = std::max(DEG_PPERM(ELM_PLIST(gens, i)),
                          CODEG_PPERM(ELM_PLIST(gens, i)));
      if (n > deg) {
        deg = n;
      }
    }
    if (deg < 65535) {
      type = PPERM2;
    } else {
      type = PPERM4;
    }
  } else if (TNUM_OBJ(x) == T_BIPART) {
    type = BIPART;
    deg  = INT_INTOBJ(BIPART_DEGREE(0L, x));
  } else if (CALL_1ARGS(IsBooleanMat, x) == True) {
    type = BOOL_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
  } else if (CALL_1ARGS(IsMaxPlusMatrix, x) == True) {
    type = MAX_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
  } else if (CALL_1ARGS(IsMinPlusMatrix, x) == True) {
    type = MIN_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
  } else if (CALL_1ARGS(IsTropicalMaxPlusMatrix, x) == True) {
    type = TROP_MAX_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
  } else if (CALL_1ARGS(IsTropicalMinPlusMatrix, x) == True) {
    type = TROP_MIN_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
  } else if (CALL_1ARGS(IsProjectiveMaxPlusMatrix, x) == True) {
    type = PROJ_MAX_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
  } else if (CALL_1ARGS(IsNTPMatrix, x) == True) {
    type = NTP_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
  } else if (CALL_1ARGS(IsIntegerMatrix, x) == True) {
    type = INT_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
  } else if (CALL_1ARGS(IsPBR, x) == True) {
    type = PBR_TYPE;
    deg  = INT_INTOBJ(CALL_1ARGS(DegreeOfPBR, x));
  }

  Obj o          = NewBag(T_SEMI, (type == UNKNOWN ? 2 : 6) * sizeof(Obj));
  ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(T_SEMI_SUBTYPE_ENSEMI);
  ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(type);

  if (type != UNKNOWN) {
    ADDR_OBJ(o)[2] = so;
    ADDR_OBJ(o)[3] = reinterpret_cast<Obj>(deg);
    ADDR_OBJ(o)[4] = static_cast<Obj>(nullptr);
    ADDR_OBJ(o)[5] = static_cast<Obj>(nullptr);
    CHANGED_BAG(o);
  }
  AssPRec(so, RNam_en_semi_cpp_semi, o);
  return o;
}

Converter* en_semi_get_converter(en_semi_obj_t es) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(es) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(es) == T_SEMI_SUBTYPE_ENSEMI);
  SEMIGROUPS_ASSERT(en_semi_get_type(es) != UNKNOWN);
  Converter* converter = CLASS_OBJ<Converter*>(es, 4);
  return (converter != nullptr ? converter : en_semi_init_converter(es));
}

Semigroup* en_semi_get_semi_cpp(en_semi_obj_t es) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(es) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(es) == T_SEMI_SUBTYPE_ENSEMI);
  SEMIGROUPS_ASSERT(en_semi_get_type(es) != UNKNOWN);
  Semigroup* semi_cpp = CLASS_OBJ<Semigroup*>(es, 5);
  return (semi_cpp != nullptr ? semi_cpp : en_semi_init_semigroup(es));
}

// Functions for semigroup Obj's i.e. gap_semigroup_t

en_semi_obj_t semi_obj_get_en_semi_no_init(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  UInt i;
  if (FindPRec(so, RNam_en_semi_cpp_semi, &i, 1)) {
    return GET_ELM_PREC(so, i);
  }
  return 0L;
}

en_semi_obj_t semi_obj_get_en_semi(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  UInt i;
  if (FindPRec(so, RNam_en_semi_cpp_semi, &i, 1)) {
    return GET_ELM_PREC(so, i);
  }
  return semi_obj_init_en_semi(so);
}

en_semi_t semi_obj_get_type(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  return en_semi_get_type(semi_obj_get_en_semi(so));
}

Semigroup* semi_obj_get_semi_cpp(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  return en_semi_get_semi_cpp(semi_obj_get_en_semi(so));
}

gap_rec_t semi_obj_get_fropin(gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  UInt i;
  if (FindPRec(so, RNam_en_semi_fropin, &i, 1)) {
    return GET_ELM_PREC(so, i);
  } else {
    if (semi_obj_get_type(so) != UNKNOWN) {  // only initialise a record
      gap_rec_t fp = NEW_PREC(0);
      SET_LEN_PREC(fp, 0);
      AssPRec(so, RNam_en_semi_fropin, fp);
      return fp;
    } else {
      CALL_1ARGS(FROPIN, so);
      if (FindPRec(so, RNam_en_semi_fropin, &i, 1)) {
        return GET_ELM_PREC(so, i);
      }
      ErrorQuit("unknown error in FROPIN,", 0L, 0L);
      return 0L;
    }
  }
}

// GAP level functions

gap_list_t EN_SEMI_AS_LIST(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi(so);

  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    semi_cpp->enumerate();
    return iterator_to_plist(
        en_semi_get_converter(es), semi_cpp->cbegin(), semi_cpp->cend());
  } else {
    return ElmPRec(fropin(so, INTOBJ_INT(-1), 0, False), RNam_elts);
  }
}

gap_list_t EN_SEMI_AS_SET(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi(so);

  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    Converter* converter = en_semi_get_converter(es);
    // The T_PLIST_HOM_SSORTED makes a huge difference to performance!!
    gap_list_t out = NEW_PLIST(T_PLIST_HOM_SSORT + IMMUTABLE, semi_cpp->size());
    SET_LEN_PLIST(out, semi_cpp->size());
    size_t i = 1;
    for (auto it = semi_cpp->cbegin_sorted(); it < semi_cpp->cend_sorted();
         ++it) {
      SET_ELM_PLIST(out, i++, converter->unconvert(*it));
      CHANGED_BAG(out);
    }
    return out;
  } else {
    gap_rec_t  fp  = fropin(so, INTOBJ_INT(-1), 0, False);
    gap_list_t out = SHALLOW_COPY_OBJ(ElmPRec(fp, RNam_elts));
    SortDensePlist(out);
    CHANGED_BAG(out);
    return out;
  }
}

gap_list_t EN_SEMI_CAYLEY_TABLE(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi(so);
  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));

    size_t n = semi_cpp->size();
    SEMIGROUPS_ASSERT(n != 0);
    gap_list_t out = NEW_PLIST(T_PLIST_TAB_RECT, n);
    // this is intentionally not IMMUTABLE

    SET_LEN_PLIST(out, n);

    for (size_t i = 0; i < n; i++) {
      gap_list_t next = NEW_PLIST(T_PLIST_CYC, n);
      // this is intentionally not IMMUTABLE
      SET_LEN_PLIST(next, n);
      SET_ELM_PLIST(out, i + 1, next);
      CHANGED_BAG(out);
    }

    for (size_t i = 0; i < n; i++) {
      gap_list_t next =
          ELM_PLIST(out, semi_cpp->position_to_sorted_position(i) + 1);
      for (size_t j = 0; j < n; j++) {
        size_t jj = semi_cpp->position_to_sorted_position(j);
        SET_ELM_PLIST(next,
                      jj + 1,
                      INTOBJ_INT(semi_cpp->position_to_sorted_position(
                                     semi_cpp->fast_product(i, j))
                                 + 1));
        CHANGED_BAG(next);
      }
      CHANGED_BAG(out);
    }
    return out;
  } else {
    return Fail;
  }
}

// This takes a newly constructed semigroup <new_so> which is generated by
// <old_so> and <plist>, and it transfers any information known about the
// <old_so>s cpp semigroup to <new_so>.
//
// The reason that <new_so> must be generated by <old_so> and <plist> (and not
// nothing, for example) is that when we call semi_obj_init_en_semi below, we
// must have the full generating set to correctly determine the degree of the
// C++ semigroup. Also if the type is UNKNOWN, then we just return new_so.

gap_semigroup_t EN_SEMI_CLOSURE(Obj             self,
                                gap_semigroup_t new_so,
                                gap_semigroup_t old_so,
                                gap_list_t      plist) {
  CHECK_SEMI_OBJ(new_so);
  CHECK_SEMI_OBJ(old_so);
  CHECK_PLIST(plist);

  en_semi_obj_t es = semi_obj_get_en_semi(old_so);

  if (en_semi_get_type(es) == UNKNOWN) {
    return new_so;  // TODO(JDM) this could be better
  }
  // Find the type, semigroup Obj, and degree. This would be unnecessary if
  // transformations and partial perms were of fixed degree, we could just copy
  // the type, and the semigroup Obj from old_so's en_semi Obj. Again note that
  // new_so must have the the generators of old_so and plist as generators for
  // this to work.
  es = semi_obj_init_en_semi(new_so);

  size_t     deg       = en_semi_get_degree(es);
  Converter* converter = en_semi_get_converter(es);
  auto       coll      = plist_to_vec(converter, plist, deg);

  Semigroup* old_semi_cpp = semi_obj_get_semi_cpp(old_so);

// CAUTION: copy_closure always copies old_semi_cpp regardless of whether or
// not every element in coll belongs to old_semi_cpp!! Only call this
// function if coll contains some elements not in old_semi_cpp. We check in
// the GAP to avoid creating new_so at all, and also to avoid sorting etc a
// collection of elements that belong to the semigroup.
#ifdef SEMIGROUPS_KERNEL_DEBUG
  bool valid = false;
  for (auto const& x : *coll) {
    if (!old_semi_cpp->test_membership(x)) {
      valid = true;
      break;
    }
  }
  SEMIGROUPS_ASSERT(valid);
#endif
  old_semi_cpp->set_report(semi_obj_get_report(new_so));
  Semigroup* new_semi_cpp = old_semi_cpp->copy_closure(coll);
  really_delete_cont(coll);

  new_semi_cpp->set_batch_size(semi_obj_get_batch_size(new_so));
  ADDR_OBJ(es)[5] = reinterpret_cast<Obj>(new_semi_cpp);
  CHANGED_BAG(es);

  // Reset the generators of the new semigroup
  gap_list_t gens = NEW_PLIST(T_PLIST_HOM, new_semi_cpp->nrgens());

  for (size_t i = 0; i < new_semi_cpp->nrgens(); i++) {
    AssPlist(gens, i + 1, converter->unconvert(new_semi_cpp->gens(i)));
  }
  AssPRec(new_so, RNam_GeneratorsOfMagma, gens);
  CHANGED_BAG(new_so);

  // Reset the fropin data since none of it is valid any longer, if any
  gap_rec_t fp = NEW_PREC(0);
  SET_LEN_PREC(fp, 0);
  AssPRec(new_so, RNam_en_semi_fropin, fp);

  return new_so;
}

// Add generators to the GAP semigroup Obj <so>. Note that this only works if
// the degree of every element in plist is less than or equal to the degree of
// the elements in <so>. If this is not the case, then this should not be
// called but ClosureSemigroup should be instead, on the GAP level.

gap_semigroup_t
EN_SEMI_CLOSURE_DEST(Obj self, gap_semigroup_t so, gap_list_t plist) {
  CHECK_SEMI_OBJ(so);
  CHECK_PLIST(plist);

  en_semi_obj_t es = semi_obj_get_en_semi(so);

  if (en_semi_get_type(es) == UNKNOWN) {
    return Fail;
  }

  SEMIGROUPS_ASSERT(IS_PLIST(plist));
  SEMIGROUPS_ASSERT(LEN_PLIST(plist) > 0);

  Semigroup*   semi_cpp  = en_semi_get_semi_cpp(es);
  size_t const deg       = semi_cpp->degree();
  Converter*   converter = en_semi_get_converter(es);

  auto coll = plist_to_vec(converter, plist, deg);
  semi_cpp->set_report(semi_obj_get_report(so));
  semi_cpp->closure(coll);
  really_delete_cont(coll);

  gap_list_t gens = ElmPRec(so, RNam_GeneratorsOfMagma);

  for (size_t i = 0; i < semi_cpp->nrgens(); i++) {
    AssPlist(gens, i + 1, converter->unconvert(semi_cpp->gens(i)));
  }
  CHANGED_BAG(so);

  // Reset the fropin data since none of it is valid any longer
  gap_rec_t fp = NEW_PREC(0);
  SET_LEN_PREC(fp, 0);
  AssPRec(so, RNam_en_semi_fropin, fp);

  return so;
}

gap_int_t EN_SEMI_CURRENT_MAX_WORD_LENGTH(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi_no_init(so);
  if (es == 0L) {
    return INTOBJ_INT(0);
  } else if (en_semi_get_type(es) != UNKNOWN) {
    return INTOBJ_INT(en_semi_get_semi_cpp(es)->current_max_word_length());
  } else {
    gap_rec_t fp = semi_obj_get_fropin(so);
    if (IsbPRec(fp, RNam_words) && LEN_PLIST(ElmPRec(fp, RNam_words)) > 0) {
      gap_list_t words = ElmPRec(fp, RNam_words);
      return INTOBJ_INT(LEN_PLIST(ELM_PLIST(words, LEN_PLIST(words))));
    } else {
      return INTOBJ_INT(0);
    }
  }
}

gap_int_t EN_SEMI_CURRENT_NR_RULES(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi_no_init(so);
  if (es == 0L) {
    return INTOBJ_INT(0);
  } else if (en_semi_get_type(es) != UNKNOWN) {
    return INTOBJ_INT(en_semi_get_semi_cpp(es)->current_nrrules());
  } else {
    gap_rec_t fp = semi_obj_get_fropin(so);
    // TODO(JDM) could write a function return_if_not_bound_prec(prec, rnam,
    // val) which returns val if rnam is not bound in prec and returns
    // prec.rnam if it is bound.
    if (IsbPRec(fp, RNam_nrrules)) {
      return ElmPRec(fp, RNam_nrrules);
    } else {
      return INTOBJ_INT(0);
    }
  }
}

gap_int_t EN_SEMI_CURRENT_SIZE(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi_no_init(so);
  if (es == 0L) {
    return INTOBJ_INT(0);
  } else if (en_semi_get_type(es) != UNKNOWN) {
    return INTOBJ_INT(en_semi_get_semi_cpp(es)->current_size());
  } else {
    gap_rec_t fp = semi_obj_get_fropin(so);
    if (IsbPRec(fp, RNam_elts)) {
      return INTOBJ_INT(LEN_PLIST(ElmPRec(fp, RNam_elts)));
    } else {
      return INTOBJ_INT(0);
    }
  }
}

// Get the <pos> element of <S> this is not cached anywhere for cpp semigroups

gap_element_t
EN_SEMI_ELEMENT_NUMBER(Obj self, gap_semigroup_t so, gap_int_t pos) {
  CHECK_SEMI_OBJ(so);
  CHECK_POS_INTOBJ(pos);

  en_semi_obj_t es = semi_obj_get_en_semi(so);
  size_t        nr = INT_INTOBJ(pos);

  if (en_semi_get_type(es) != UNKNOWN) {
    nr--;
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    Element const* x = semi_cpp->at(nr);
    return (x == nullptr ? Fail : en_semi_get_converter(es)->unconvert(x));
  } else {
    gap_rec_t fp = semi_obj_get_fropin(so);
    if (IsbPRec(fp, RNam_elts)) {
      // use the element cached in the data record if known
      gap_list_t elts = ElmPRec(fp, RNam_elts);
      if (nr <= (size_t) LEN_PLIST(elts) && ELM_PLIST(elts, nr) != 0) {
        return ELM_PLIST(elts, nr);
      }
    }
    fp              = fropin(so, pos, 0, False);
    gap_list_t elts = ElmPRec(fp, RNam_elts);
    if (nr <= (size_t) LEN_PLIST(elts) && ELM_PLIST(elts, nr) != 0) {
      return ELM_PLIST(elts, nr);
    } else {
      return Fail;
    }
  }
}

gap_element_t
EN_SEMI_ELEMENT_NUMBER_SORTED(Obj self, gap_semigroup_t so, gap_int_t pos) {
  CHECK_SEMI_OBJ(so);
  CHECK_POS_INTOBJ(pos);

  en_semi_obj_t es = semi_obj_get_en_semi(so);

  if (en_semi_get_type(es) != UNKNOWN) {
    size_t     nr       = INT_INTOBJ(pos) - 1;
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    Element const* x = semi_cpp->sorted_at(nr);

    return (x == nullptr ? Fail : en_semi_get_converter(es)->unconvert(x));
  } else {
    ErrorQuit("EN_SEMI_ELEMENT_NUMBER_SORTED: this shouldn't happen!", 0L, 0L);
    return 0L;
  }
}

gap_list_t EN_SEMI_ELMS_LIST(Obj self, gap_semigroup_t so, gap_list_t poslist) {
  CHECK_SEMI_OBJ(so);
  CHECK_LIST(poslist);

  en_semi_obj_t es = semi_obj_get_en_semi(so);

  size_t len = LEN_LIST(poslist);

  gap_list_t out = NEW_PLIST((len == 0 ? T_PLIST_EMPTY : T_PLIST_HOM), len);
  // this is intentionally not IMMUTABLE
  SET_LEN_PLIST(out, len);

  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    for (size_t i = 1; i <= len; i++) {
      gap_int_t pos = ELM_LIST(poslist, i);
      if (pos == 0 || !IS_INTOBJ(pos) || INT_INTOBJ(pos) <= 0) {
        ErrorQuit("Semigroups: ELMS_LIST: List Elements, <list>[%d] "
                  "must be a positive integer,",
                  (Int) i,
                  0L);
      }
      semi_cpp->set_report(semi_obj_get_report(so));
      Element const* x = semi_cpp->at(INT_INTOBJ(pos) - 1);
      if (x == nullptr) {
        ErrorQuit("Semigroups: ELMS_LIST: List Elements, <list>[%d] "
                  "must be at most %d,",
                  (Int) i,
                  (Int) semi_cpp->size());
      }
      SET_ELM_PLIST(out, i, en_semi_get_converter(es)->unconvert(x));
      CHANGED_BAG(out);
    }
  } else {
    for (size_t i = 1; i <= len; i++) {
      gap_int_t pos = ELM_LIST(poslist, i);
      if (pos == 0 || !IS_INTOBJ(pos) || INT_INTOBJ(pos) <= 0) {
        ErrorQuit("Semigroups: ELMS_LIST: List Elements, <list>[%d] "
                  "must be a positive integer,",
                  (Int) i,
                  0L);
      }
      gap_list_t elts = ElmPRec(fropin(so, pos, 0, False), RNam_elts);
      if (INT_INTOBJ(pos) > LEN_PLIST(elts)) {
        ErrorQuit("Semigroups: ELMS_LIST: List Elements, <list>[%d] "
                  "must be at most %d,",
                  (Int) i,
                  (Int) LEN_PLIST(elts));
      }
      SET_ELM_PLIST(out, i, ELM_PLIST(elts, INT_INTOBJ(pos)));
      CHANGED_BAG(out);
    }
  }
  return out;
}

gap_semigroup_t
EN_SEMI_ENUMERATE(Obj self, gap_semigroup_t so, gap_int_t limit) {
  CHECK_SEMI_OBJ(so);
  CHECK_INTOBJ(limit);
  size_t c_limit =
      (INT_INTOBJ(limit) < 0 ? Semigroup::LIMIT_MAX : INT_INTOBJ(limit));
  en_semi_obj_t es = semi_obj_get_en_semi(so);
  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    semi_cpp->enumerate(c_limit);
  } else {
    fropin(so, limit, 0, False);
  }
  return so;
}

gap_list_t EN_SEMI_FACTORIZATION(Obj self, gap_semigroup_t so, gap_int_t pos) {
  CHECK_SEMI_OBJ(so);
  CHECK_POS_INTOBJ(pos);

  en_semi_obj_t es    = semi_obj_get_en_semi_no_init(so);
  size_t        pos_c = INT_INTOBJ(pos);

  if (es == 0L) {
    ErrorQuit("it is not possible to factorize a not yet enumerated element,",
              0L,
              0L);
    return 0L;  // keep compiler happy
  } else if (en_semi_get_type(es) != UNKNOWN) {
    gap_list_t words;
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);

    if (pos_c > semi_cpp->current_size()) {
      ErrorQuit("the 2nd argument must be at most %d not %d",
                semi_cpp->current_size(),
                pos_c);
    }

    gap_rec_t fp = semi_obj_get_fropin(so);
    if (!IsbPRec(fp, RNam_words)) {
      // TODO(JDM) Use FindPRec instead
      word_t w;  // changed in place by the next line
      semi_cpp->set_report(semi_obj_get_report(so));
      semi_cpp->factorisation(w, pos_c - 1);
      words = NEW_PLIST(T_PLIST + IMMUTABLE, pos_c);
      // IMMUTABLE since it should not be altered on the GAP level
      SET_LEN_PLIST(words, pos_c);
      SET_ELM_PLIST(words, pos_c, word_t_to_plist(w));
      CHANGED_BAG(words);
      AssPRec(fp, RNam_words, words);
      CHANGED_BAG(so);
    } else {
      words = ElmPRec(fp, RNam_words);
      if (pos_c > (size_t) LEN_PLIST(words) || ELM_PLIST(words, pos_c) == 0) {
        // avoid retracing the Schreier tree if possible
        size_t prefix = semi_cpp->prefix(pos_c - 1) + 1;
        size_t suffix = semi_cpp->suffix(pos_c - 1) + 1;
        if (prefix != 0 && prefix <= (size_t) LEN_PLIST(words)
            && ELM_PLIST(words, prefix) != 0) {
          gap_list_t old_word = ELM_PLIST(words, prefix);
          gap_list_t new_word =
              NEW_PLIST(T_PLIST_CYC + IMMUTABLE, LEN_PLIST(old_word) + 1);
          // IMMUTABLE since it should not be altered on the GAP level
          memcpy(reinterpret_cast<void*>(
                     reinterpret_cast<char*>(ADDR_OBJ(new_word)) + sizeof(Obj)),
                 reinterpret_cast<void*>(
                     reinterpret_cast<char*>(ADDR_OBJ(old_word)) + sizeof(Obj)),
                 (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
          SET_ELM_PLIST(new_word,
                        LEN_PLIST(old_word) + 1,
                        INTOBJ_INT(semi_cpp->final_letter(pos_c - 1) + 1));
          SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);
          AssPlist(words, pos_c, new_word);
          CHANGED_BAG(fp);
          CHANGED_BAG(so);
        } else if (suffix != 0 && suffix <= (size_t) LEN_PLIST(words)
                   && ELM_PLIST(words, suffix) != 0) {
          gap_list_t old_word = ELM_PLIST(words, suffix);
          gap_list_t new_word =
              NEW_PLIST(T_PLIST_CYC + IMMUTABLE, LEN_PLIST(old_word) + 1);
          // IMMUTABLE since it should not be altered on the GAP level
          memcpy(reinterpret_cast<void*>(
                     reinterpret_cast<char*>(ADDR_OBJ(new_word))
                     + 2 * sizeof(Obj)),
                 reinterpret_cast<void*>(
                     reinterpret_cast<char*>(ADDR_OBJ(old_word)) + sizeof(Obj)),
                 (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
          SET_ELM_PLIST(
              new_word, 1, INTOBJ_INT(semi_cpp->first_letter(pos_c - 1) + 1));
          SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);
          AssPlist(words, pos_c, new_word);
          CHANGED_BAG(fp);
          CHANGED_BAG(so);
        } else {
          word_t w;  // changed in place by the next line
          semi_cpp->set_report(semi_obj_get_report(so));
          semi_cpp->factorisation(w, pos_c - 1);
          AssPlist(words, pos_c, word_t_to_plist(w));
          CHANGED_BAG(fp);
          CHANGED_BAG(so);
        }
      }
    }
    CHANGED_BAG(so);
    SEMIGROUPS_ASSERT(IsbPRec(fp, RNam_words));
    SEMIGROUPS_ASSERT(IS_PLIST(ElmPRec(fp, RNam_words)));
    SEMIGROUPS_ASSERT(pos_c <= (size_t) LEN_PLIST(ElmPRec(fp, RNam_words)));

    return ELM_PLIST(ElmPRec(fp, RNam_words), pos_c);
  } else {
    gap_rec_t fp = fropin(so, pos, 0, False);
    return ELM_PLIST(ElmPRec(fp, RNam_words), pos_c);
  }
}

gap_list_t EN_SEMI_LEFT_CAYLEY_GRAPH(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi(so);
  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    gap_list_t out = NEW_PLIST(T_PLIST_TAB_RECT, semi_cpp->size());
    // this is intentionally not IMMUTABLE
    SET_LEN_PLIST(out, semi_cpp->size());

    for (size_t i = 0; i < semi_cpp->size(); ++i) {
      gap_list_t next = NEW_PLIST(T_PLIST_CYC, semi_cpp->nrgens());
      // this is intentionally not IMMUTABLE
      SET_LEN_PLIST(next, semi_cpp->nrgens());
      for (size_t j = 0; j < semi_cpp->nrgens(); ++j) {
        SET_ELM_PLIST(next, j + 1, INTOBJ_INT(semi_cpp->left(i, j) + 1));
      }
      SET_ELM_PLIST(out, i + 1, next);
      CHANGED_BAG(out);
    }
    return out;
  } else {
    return ElmPRec(fropin(so, INTOBJ_INT(-1), 0, False), RNam_left);
  }
}

gap_int_t EN_SEMI_LENGTH_ELEMENT(Obj self, gap_semigroup_t so, gap_int_t pos) {
  CHECK_SEMI_OBJ(so);
  CHECK_POS_INTOBJ(pos);
  en_semi_obj_t es = semi_obj_get_en_semi(so);
  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    return INTOBJ_INT(semi_cpp->length_non_const(INT_INTOBJ(pos) - 1));
  } else {
    return INTOBJ_INT(LEN_PLIST(EN_SEMI_FACTORIZATION(self, so, pos)));
  }
}

gap_list_t EN_SEMI_IDEMPOTENTS(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi(so);
  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp  = en_semi_get_semi_cpp(es);
    Converter* converter = en_semi_get_converter(es);

    semi_cpp->set_report(semi_obj_get_report(so));
    semi_cpp->set_max_threads(semi_obj_get_nr_threads(so));
    return iterator_to_plist(converter,
                             semi_cpp->cbegin_idempotents(),
                             semi_cpp->cend_idempotents());
  } else {
    gap_rec_t  fp     = fropin(so, INTOBJ_INT(-1), 0, False);
    gap_list_t left   = ElmPRec(fp, RNamName("left"));
    gap_list_t last   = ElmPRec(fp, RNamName("final"));
    gap_list_t prefix = ElmPRec(fp, RNamName("prefix"));
    gap_list_t elts   = ElmPRec(fp, RNamName("elts"));

    size_t     size = LEN_PLIST(left);
    size_t     nr   = 0;
    gap_list_t out  = NEW_PLIST(T_PLIST_CYC + IMMUTABLE, 0);
    // IMMUTABLE since it should not be altered on the GAP level
    SET_LEN_PLIST(out, 0);
    for (size_t pos = 1; pos <= size; pos++) {
      size_t i = pos, j = pos;
      while (i != 0) {
        j = INT_INTOBJ(
            ELM_PLIST(ELM_PLIST(left, j), INT_INTOBJ(ELM_PLIST(last, i))));
        i = INT_INTOBJ(ELM_PLIST(prefix, i));
      }
      if (j == pos) {
        AssPlist(out, ++nr, ELM_PLIST(elts, pos));
      }
    }
    SEMIGROUPS_ASSERT(nr != 0);
    return out;
  }
}

gap_int_t EN_SEMI_IDEMS_SUBSET(Obj self, gap_semigroup_t so, gap_list_t list) {
  CHECK_SEMI_OBJ(so);
  CHECK_LIST(list);

  en_semi_obj_t es = semi_obj_get_en_semi(so);

  gap_list_t out = NEW_PLIST(T_PLIST_CYC + IMMUTABLE, 0);
  // IMMUTABLE since it should not be altered on the GAP level
  SET_LEN_PLIST(out, 0);
  size_t len = 0;

  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    semi_cpp->set_max_threads(semi_obj_get_nr_threads(so));

    for (size_t pos = 1; pos <= (size_t) LEN_LIST(list); pos++) {
      gap_int_t ent = ELM_LIST(list, pos);
      CHECK_POS_INTOBJ(ent);
      if (semi_cpp->is_idempotent(INT_INTOBJ(ent) - 1)) {
        AssPlist(out, ++len, ent);
      }
    }
  } else {
    gap_rec_t  fp     = fropin(so, INTOBJ_INT(-1), 0, False);
    gap_list_t left   = ElmPRec(fp, RNamName("left"));
    gap_list_t last   = ElmPRec(fp, RNamName("final"));
    gap_list_t prefix = ElmPRec(fp, RNamName("prefix"));

    for (size_t pos = 1; pos <= (size_t) LEN_LIST(list); pos++) {
      gap_int_t ent = ELM_LIST(list, pos);
      CHECK_POS_INTOBJ(ent);
      size_t val = INT_INTOBJ(ent);
      size_t i = val, j = val;
      while (i != 0) {
        j = INT_INTOBJ(
            ELM_PLIST(ELM_PLIST(left, j), INT_INTOBJ(ELM_PLIST(last, i))));
        i = INT_INTOBJ(ELM_PLIST(prefix, i));
      }
      if (j == val) {
        AssPlist(out, ++len, INTOBJ_INT(val));
      }
    }
  }
  SEMIGROUPS_ASSERT(len == (size_t) LEN_PLIST(out));
  if (len == 0) {
    RetypeBag(out, T_PLIST_EMPTY + IMMUTABLE);
  }
  return out;
}

gap_bool_t EN_SEMI_IS_DONE(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi_no_init(so);

  if (es == 0L) {
    return False;
  } else if (en_semi_get_type(es) != UNKNOWN) {
    return (en_semi_get_semi_cpp(es)->is_done() ? True : False);
  }

  gap_rec_t fp = semi_obj_get_fropin(so);

  size_t pos = INT_INTOBJ(ElmPRec(fp, RNam_pos));
  size_t nr  = INT_INTOBJ(ElmPRec(fp, RNam_nr));
  return (pos > nr ? True : False);
}

gap_int_t EN_SEMI_NR_IDEMPOTENTS(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi(so);
  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    semi_cpp->set_max_threads(semi_obj_get_nr_threads(so));

    return INTOBJ_INT(semi_cpp->nridempotents());
  } else {
    // This could probably be better but is also probably not worth the effort
    // of improving
    gap_rec_t  fp     = fropin(so, INTOBJ_INT(-1), 0, False);
    gap_list_t left   = ElmPRec(fp, RNamName("left"));
    gap_list_t last   = ElmPRec(fp, RNamName("final"));
    gap_list_t prefix = ElmPRec(fp, RNamName("prefix"));
    size_t     size   = LEN_PLIST(left);
    size_t     nr     = 0;
    for (size_t pos = 1; pos <= size; pos++) {
      size_t i = pos, j = pos;
      while (i != 0) {
        j = INT_INTOBJ(
            ELM_PLIST(ELM_PLIST(left, j), INT_INTOBJ(ELM_PLIST(last, i))));
        i = INT_INTOBJ(ELM_PLIST(prefix, i));
      }
      if (j == pos) {
        nr++;
      }
    }
    return INTOBJ_INT(nr);
  }
}

Obj EN_SEMI_POSITION(Obj self, gap_semigroup_t so, gap_element_t x) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi(so);

  if (en_semi_get_type(es) != UNKNOWN) {
    size_t     deg      = en_semi_get_degree(es);
    Element*   xx       = en_semi_get_converter(es)->convert(x, deg);
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    size_t pos = semi_cpp->position(xx);
    xx->really_delete();
    delete xx;
    return (pos == Semigroup::UNDEFINED ? Fail : INTOBJ_INT(pos + 1));
  } else {
    gap_rec_t data = semi_obj_get_fropin(so);
    Obj       ht   = ElmPRec(data, RNam_ht);
    size_t    pos, nr;

    do {
      Obj val = CALL_2ARGS(HTValue, ht, x);
      if (val != Fail) {
        return val;
      }
      gap_int_t limit = SumInt(ElmPRec(data, RNam_nr), INTOBJ_INT(1));
      data            = fropin(data, limit, 0, False);
      pos             = INT_INTOBJ(ElmPRec(data, RNam_pos));
      nr              = INT_INTOBJ(ElmPRec(data, RNam_nr));
    } while (pos <= nr);
    return CALL_2ARGS(HTValue, ht, x);
  }
}

// Get the position of <x> with out any further enumeration

gap_int_t
EN_SEMI_CURRENT_POSITION(Obj self, gap_semigroup_t so, gap_element_t x) {
  CHECK_SEMI_OBJ(so);

  en_semi_obj_t es = semi_obj_get_en_semi_no_init(so);
  if (es == 0L) {
    return Fail;
  } else if (en_semi_get_type(es) != UNKNOWN) {
    size_t   deg = en_semi_get_degree(es);
    Element* xx  = en_semi_get_converter(es)->convert(x, deg);
    size_t   pos = en_semi_get_semi_cpp(es)->current_position(xx);
    xx->really_delete();
    delete xx;
    return (pos == Semigroup::UNDEFINED ? Fail : INTOBJ_INT(pos + 1));
  } else {
    return CALL_2ARGS(HTValue, ElmPRec(semi_obj_get_fropin(so), RNam_ht), x);
  }
}

gap_int_t
EN_SEMI_POSITION_SORTED(Obj self, gap_semigroup_t so, gap_element_t x) {
  CHECK_SEMI_OBJ(so);

  en_semi_obj_t es = semi_obj_get_en_semi(so);

  if (en_semi_get_type(es) == UNKNOWN) {
    ErrorQuit("EN_SEMI_POSITION_SORTED: this shouldn't happen!", 0L, 0L);
    return 0L;
  } else {
    size_t     deg      = en_semi_get_degree(es);
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    Element* xx  = en_semi_get_converter(es)->convert(x, deg);
    size_t   pos = semi_cpp->sorted_position(xx);
    xx->really_delete();
    delete xx;
    return (pos == Semigroup::UNDEFINED ? Fail : INTOBJ_INT(pos + 1));
  }
}

gap_list_t EN_SEMI_RELATIONS(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  gap_list_t es = semi_obj_get_en_semi(so);
  gap_rec_t  fp = semi_obj_get_fropin(so);

  if (en_semi_get_type(es) != UNKNOWN) {
    if (!IsbPRec(fp, RNam_rules) || LEN_PLIST(ElmPRec(fp, RNam_rules)) == 0) {
      Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
      semi_cpp->set_report(semi_obj_get_report(so));
      gap_list_t rules =
          NEW_PLIST(T_PLIST_TAB_RECT + IMMUTABLE, semi_cpp->nrrules());
      // IMMUTABLE since it should not be altered on the GAP level
      SET_LEN_PLIST(rules, semi_cpp->nrrules());
      size_t nr = 0;

      semi_cpp->reset_next_relation();
      std::vector<size_t> relation;
      semi_cpp->next_relation(relation);

      while (relation.size() == 2) {
        gap_list_t next = NEW_PLIST(T_PLIST_TAB + IMMUTABLE, 2);
        // IMMUTABLE since it should not be altered on the GAP level
        SET_LEN_PLIST(next, 2);
        for (size_t i = 0; i < 2; i++) {
          gap_list_t w = NEW_PLIST(T_PLIST_CYC + IMMUTABLE, 1);
          // IMMUTABLE since it should not be altered on the GAP level
          SET_LEN_PLIST(w, 1);
          SET_ELM_PLIST(w, 1, INTOBJ_INT(relation[i] + 1));
          SET_ELM_PLIST(next, i + 1, w);
          CHANGED_BAG(next);
        }
        nr++;
        SET_ELM_PLIST(rules, nr, next);
        CHANGED_BAG(rules);
        semi_cpp->next_relation(relation);
      }

      while (!relation.empty()) {
        gap_list_t old_word =
            EN_SEMI_FACTORIZATION(self, so, INTOBJ_INT(relation[0] + 1));
        gap_list_t new_word =
            NEW_PLIST(T_PLIST_CYC + IMMUTABLE, LEN_PLIST(old_word) + 1);
        // IMMUTABLE since it should not be altered on the GAP level
        memcpy(reinterpret_cast<void*>(
                   reinterpret_cast<char*>(ADDR_OBJ(new_word)) + sizeof(Obj)),
               reinterpret_cast<void*>(
                   reinterpret_cast<char*>(ADDR_OBJ(old_word)) + sizeof(Obj)),
               (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
        SET_ELM_PLIST(
            new_word, LEN_PLIST(old_word) + 1, INTOBJ_INT(relation[1] + 1));
        SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);

        gap_list_t next = NEW_PLIST(T_PLIST_TAB + IMMUTABLE, 2);
        // IMMUTABLE since it should not be altered on the GAP level
        SET_LEN_PLIST(next, 2);
        SET_ELM_PLIST(next, 1, new_word);
        CHANGED_BAG(next);
        SET_ELM_PLIST(
            next,
            2,
            EN_SEMI_FACTORIZATION(self, so, INTOBJ_INT(relation[2] + 1)));
        CHANGED_BAG(next);
        nr++;
        SET_ELM_PLIST(rules, nr, next);
        CHANGED_BAG(rules);
        semi_cpp->next_relation(relation);
      }
      AssPRec(fp, RNam_rules, rules);
      CHANGED_BAG(so);
    }
  } else {
    fropin(so, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(fp, RNam_rules);
}

gap_list_t EN_SEMI_RIGHT_CAYLEY_GRAPH(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  en_semi_obj_t es = semi_obj_get_en_semi(so);

  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));

    gap_list_t out = NEW_PLIST(T_PLIST_TAB_RECT, semi_cpp->size());
    // this is intentionally not IMMUTABLE
    SET_LEN_PLIST(out, semi_cpp->size());

    for (size_t i = 0; i < semi_cpp->size(); ++i) {
      gap_list_t next = NEW_PLIST(T_PLIST_CYC, semi_cpp->nrgens());
      // this is intentionally not IMMUTABLE
      SET_LEN_PLIST(next, semi_cpp->nrgens());
      for (size_t j = 0; j < semi_cpp->nrgens(); ++j) {
        SET_ELM_PLIST(next, j + 1, INTOBJ_INT(semi_cpp->right(i, j) + 1));
      }
      SET_ELM_PLIST(out, i + 1, next);
      CHANGED_BAG(out);
    }
    return out;
  } else {
    return ElmPRec(fropin(so, INTOBJ_INT(-1), 0, False), RNam_right);
  }
}

gap_int_t EN_SEMI_SIZE(Obj self, gap_semigroup_t so) {
  CHECK_SEMI_OBJ(so);
  initRNams();
  en_semi_obj_t es = semi_obj_get_en_semi(so);

  if (en_semi_get_type(es) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_semi_cpp(es);
    semi_cpp->set_report(semi_obj_get_report(so));
    return INTOBJ_INT(semi_cpp->size());
  } else {
    gap_rec_t fp = fropin(so, INTOBJ_INT(-1), 0, False);
    return INTOBJ_INT(LEN_PLIST(ElmPRec(fp, RNam_elts)));
  }
}

// Iterators
// TODO(JDM) rename these

gap_bool_t EN_SEMI_IS_DONE_ITERATOR(Obj self, gap_rec_t iter) {
  initRNams();
  Int size = INT_INTOBJ(EN_SEMI_SIZE(self, ElmPRec(iter, RNam_parent)));
  return (INT_INTOBJ(ElmPRec(iter, RNam_pos)) >= size ? True : False);
}

gap_element_t EN_SEMI_NEXT_ITERATOR(Obj self, gap_rec_t iter) {
  initRNams();
  gap_int_t pos = INTOBJ_INT(INT_INTOBJ(ElmPRec(iter, RNam_pos)) + 1);
  AssPRec(iter, RNam_pos, pos);
  return EN_SEMI_ELEMENT_NUMBER(self, ElmPRec(iter, RNam_parent), pos);
}

gap_element_t EN_SEMI_NEXT_ITERATOR_SORTED(Obj self, gap_rec_t iter) {
  initRNams();
  gap_int_t pos = INTOBJ_INT(INT_INTOBJ(ElmPRec(iter, RNam_pos)) + 1);
  AssPRec(iter, RNam_pos, pos);
  return EN_SEMI_ELEMENT_NUMBER_SORTED(self, ElmPRec(iter, RNam_parent), pos);
}
