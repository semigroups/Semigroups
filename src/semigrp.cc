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

#include "bipart.h"
#include "converter.h"
#include "fropin.h"
#include "gap.h"
#include "src/compiled.h"

// Typedefs TODO these should go elsewhere

typedef Obj GapSemigroup;
typedef Obj GapPlist;
typedef Obj GapEnumerableSemigroupData;
typedef Obj GapPRec;

// RNams
static Int RNam_GeneratorsOfMagma = RNamName("GeneratorsOfMagma");
static Int RNam_Representative    = RNamName("Representative");

//static Int RNam_batch_size        = RNamName("batch_size");
static Int RNam_opts              = RNamName("opts");
//static Int RNam_report            = RNamName("report");

static Int RNam_en_semi_cpp = RNamName("__en_semi_cpp_data");
static Int RNam_en_semi_frp = RNamName("__en_semi_frp_data");

// TODO initRnams function

std::vector<Element*>*
plist_to_vec(Converter* converter, GapPlist elements, size_t degree) {
  assert(IS_PLIST(elements));

  auto out = new std::vector<Element*>();

  for (size_t i = 0; i < (size_t) LEN_PLIST(elements); i++) {
    out->push_back(converter->convert(ELM_LIST(elements, i + 1), degree));
  }
  return out;
}

Obj cayley_graph_t_to_plist(cayley_graph_t* graph) {
  assert(graph->size() != 0);
  Obj out = NEW_PLIST(T_PLIST, graph->nr_rows());
  SET_LEN_PLIST(out, graph->nr_rows());

  for (size_t i = 0; i < graph->nr_rows(); i++) {
    Obj next = NEW_PLIST(T_PLIST_CYC, graph->nr_cols());
    SET_LEN_PLIST(next, graph->nr_cols());
    typename std::vector<size_t>::const_iterator end = graph->row_cend(i);
    size_t j = 1;
    for (auto it = graph->row_cbegin(i); it != end; ++it) {
      SET_ELM_PLIST(next, j++, INTOBJ_INT(*it + 1));
    }
    SET_ELM_PLIST(out, i + 1, next);
    CHANGED_BAG(out);
  }
  return out;
}

// TODO this should go elsewhere
template <typename T> static inline void really_delete_cont(T* cont) {
  for (Element* x : *cont) {
    x->really_delete();
  }
  delete cont;
}

// Semigroups

GapPlist semi_get_gens(GapSemigroup semi_gap) {
  initRNams();
  UInt i;
  if (FindPRec(semi_gap, RNam_GeneratorsOfMagma, &i, 1)) {
    Obj gens = GET_ELM_PREC(semi_gap, i);
    PLAIN_LIST(gens);
    return gens;
  } else {
    CALL_1ARGS(GeneratorsOfMagma, semi_gap);
    if (FindPRec(semi_gap, RNam_GeneratorsOfMagma, &i, 1)) {
      Obj gens = GET_ELM_PREC(semi_gap, i);
      PLAIN_LIST(gens);
      return gens;
    }
    ErrorQuit("cannot find generators of the semigroup,", 0L, 0L);
    return 0L;
  }
}

Obj semi_get_rep(GapSemigroup S) {
  initRNams();
  UInt i;
  if (FindPRec(S, RNam_Representative, &i, 1)) {
    return GET_ELM_PREC(S, i);
  } else {
    GapPlist gens = semi_get_gens(S);
    if (LEN_PLIST(gens) > 0) {
      return ELM_PLIST(gens, 1);
    } else {
      ErrorQuit("Cannot find a representative of the semigroup!", 0L, 0L);
      return 0L;
    }
  }
}

size_t semi_get_batch_size(GapSemigroup S) {
  initRNams();
  UInt i;
  if (FindPRec(S, RNam_opts, &i, 1)) {
    GapPRec opts = GET_ELM_PREC(S, i);
    if (FindPRec(opts, RNam_batch_size, &i, 1)) {
      return INT_INTOBJ(GET_ELM_PREC(opts, i));
    } 
  }
#ifdef DEBUG
  Pr("Using default value of 8192 for reporting!\n", 0L, 0L);
#endif
  return 8192;
}

static inline bool semi_get_report(GapSemigroup S) {
  initRNams();
  UInt i;
  if (FindPRec(S, RNam_opts, &i, 1)) {
    Obj opts = GET_ELM_PREC(S, i);
    if (FindPRec(opts, RNam_report, &i, 1)) {
      return (GET_ELM_PREC(opts, i) == True ? true : false);
    }
  }
#ifdef DEBUG
  Pr("Using default value of <false> for reporting!\n", 0L, 0L);
#endif
  return false;
}

static inline size_t semi_get_threshold(GapSemigroup S) {
  initRNams();

  Obj x = semi_get_rep(S);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(CALL_1ARGS(IsTropicalMatrix, x) || CALL_1ARGS(IsNTPMatrix, x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
}

static inline size_t semi_get_period(GapSemigroup S) {
  initRNams();

  Obj x = semi_get_rep(S);

  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(CALL_1ARGS(IsNTPMatrix, x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2));
}

// Enumerable semigroups

/*enum en_semi_t {
  UNKNOWN,
  TRANS2,
  TRANS4,
  PPERM2,
  PPERM4,
  BOOL_MAT,
  BIPART,
  MAX_PLUS_MAT,
  MIN_PLUS_MAT,
  TROP_MAX_PLUS_MAT,
  TROP_MIN_PLUS_MAT,
  PROJ_MAX_PLUS_MAT,
  NTP_MAT,
  INT_MAT,
  MAT_OVER_PF,
  PBR_TYPE
};*/

Obj semi_get_en_semi(GapSemigroup S) {
  UInt i;

  if (FindPRec(S, RNam_en_semi_cpp, &i, 1)) {
    return GET_ELM_PREC(S, i);
  }

  size_t     deg;
  en_semi_t  type = UNKNOWN;
  Converter* converter;
  
  Obj x = semi_get_rep(S);

  if (IS_TRANS(x)) {
    deg           = 0;
    GapPlist gens = semi_get_gens(S);
    for (size_t i = 1; i <= (size_t) LEN_PLIST(gens); i++) {
      size_t n = DEG_TRANS(ELM_PLIST(gens, i));
      if (n > deg) {
        deg = n;
      }
    }
    if (deg < 65536) {
      type      = TRANS2;
      converter = new TransConverter<u_int16_t>();
    } else {
      type      = TRANS4;
      converter = new TransConverter<u_int32_t>();
    }
  } else if (IS_PPERM(x)) {
    deg           = 0;
    GapPlist gens = semi_get_gens(S);
    for (size_t i = 1; i <= (size_t) LEN_PLIST(gens); i++) {
      size_t n = DEG_PPERM(ELM_PLIST(gens, i));
      if (n > deg) {
        deg = n;
      }
    }
    if (deg < 65535) {
      type      = PPERM2;
      converter = new PPermConverter<u_int16_t>();
    } else {
      type      = PPERM4;
      converter = new PPermConverter<u_int32_t>();
    }
  } else if (TNUM_OBJ(x) == T_BIPART) {
    type      = BIPART;
    deg       = INT_INTOBJ(BIPART_DEGREE(0L, x));
    converter = new BipartConverter();
  } else if (CALL_1ARGS(IsBooleanMat, x) == True) {
    type      = BOOL_MAT;
    deg       = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
    converter = new BoolMatConverter();
  } else if (CALL_1ARGS(IsMaxPlusMatrix, x) == True) {
    type = MAX_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
    converter = new MatrixOverSemiringConverter(
        new semiring::MaxPlusSemiring(), Ninfinity, MaxPlusMatrixType);
  } else if (CALL_1ARGS(IsMinPlusMatrix, x) == True) {
    type = MIN_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
    converter = new MatrixOverSemiringConverter(
        new semiring::MinPlusSemiring(), infinity, MinPlusMatrixType);
  } else if (CALL_1ARGS(IsTropicalMaxPlusMatrix, x) == True) {
    type = TROP_MAX_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
    converter = new MatrixOverSemiringConverter(
        new semiring::TropicalMaxPlusSemiring(
          semi_get_threshold(S)),
        Ninfinity,
        TropicalMaxPlusMatrixType);
  } else if (CALL_1ARGS(IsTropicalMinPlusMatrix, x) == True) {
    type = TROP_MIN_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
    converter = new MatrixOverSemiringConverter(
        new semiring::TropicalMinPlusSemiring(
          semi_get_threshold(S)),
        infinity,
        TropicalMinPlusMatrixType);
  } else if (CALL_1ARGS(IsProjectiveMaxPlusMatrix, x) == True) {
    type = PROJ_MAX_PLUS_MAT;
    deg  = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
    converter = new ProjectiveMaxPlusMatrixConverter(
        new semiring::MaxPlusSemiring(),
        Ninfinity,
        ProjectiveMaxPlusMatrixType);
  } else if (CALL_1ARGS(IsNTPMatrix, x) == True) {
    type      = NTP_MAT;
    deg       = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
    converter = new MatrixOverSemiringConverter(
        new semiring::NaturalSemiring(semi_get_threshold(S),
          semi_get_period(S)),
        INTOBJ_INT(0),
        NTPMatrixType);
  } else if (CALL_1ARGS(IsIntegerMatrix, x) == True) {
    type      = INT_MAT;
    deg       = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
    converter = new MatrixOverSemiringConverter(
        new semiring::Integers(), INTOBJ_INT(0), IntegerMatrixType);
  } else if (CALL_1ARGS(IsPBR, x) == True) {
    type = PBR_TYPE;
    deg  = INT_INTOBJ(CALL_1ARGS(DegreeOfPBR, x));
    converter = new PBRConverter();
  }

  if (type != UNKNOWN) {
    GapPlist gens_gap = semi_get_gens(S);

    std::vector<Element*>* gens = plist_to_vec(converter, gens_gap, deg);
    Semigroup* semi_cpp = new Semigroup(gens);
    semi_cpp->set_batch_size(semi_get_batch_size(S));
    
    Obj o          = NewBag(T_SEMI, 5 * sizeof(Obj));
    ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(T_SEMI_SUBTYPE_ENSEMI);
    ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(type);
    ADDR_OBJ(o)[2] = reinterpret_cast<Obj>(semi_cpp);
    ADDR_OBJ(o)[3] = reinterpret_cast<Obj>(converter);
    ADDR_OBJ(o)[4] = reinterpret_cast<Obj>(deg);

    AssPRec(S, RNam_en_semi_cpp, o);
    really_delete_cont(gens);
    return o;
  }

  Obj o          = NewBag(T_SEMI, 2 * sizeof(Obj));
  ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(T_SEMI_SUBTYPE_ENSEMI);
  ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(type);
  AssPRec(S, RNam_en_semi_cpp, o);
  CHANGED_BAG(S);
  return o; 
}

Obj semi_get_en_semi_frp(GapSemigroup S) {
  UInt i;
  if (FindPRec(S, RNam_en_semi_frp, &i, 1)) {
    return GET_ELM_PREC(S, i);
  }
  CALL_1ARGS(SEMIGROUPS_InitEnSemiFrpData, S);
  if (FindPRec(S, RNam_en_semi_frp, &i, 1)) {
    return GET_ELM_PREC(S, i);
  }
  ErrorQuit("unknown error in SEMIGROUPS_InitEnSemiFrpData,", 0L, 0L);
  return 0L;
}

static inline Semigroup* semi_get_cpp(GapSemigroup S) {
  return en_semi_get_cpp(semi_get_en_semi(S));
}

static inline Converter* semi_get_converter(GapSemigroup S) {
  return en_semi_get_converter(semi_get_en_semi(S));
}

static inline size_t en_semi_get_degree(Obj en_semi) {
  return CLASS_OBJ<size_t>(en_semi, 4);
}

static inline size_t semi_get_degree(GapSemigroup S) {
  return en_semi_get_degree(semi_get_en_semi(S));
}

static inline en_semi_t semi_get_type(GapSemigroup S) {
  return en_semi_get_type(semi_get_en_semi(S));
}

/*bool en_semi_has_cpp_semi(Obj en_semi_data) {
  initRNams();
  UInt i;
  return (FindPRec(en_semi_data, RNam_en_semi_cpp_semi, &i, 1)
          && GET_ELM_PREC(en_semi_data, i) != nullptr);
}

bool en_semi_has_converter(Obj en_semi_data) {
  initRNams();
  UInt i;
  return (FindPRec(en_semi_data, RNam_en_semi_converter, &i, 1)
          && GET_ELM_PREC(en_semi_data, i) != nullptr);
}*/


// GAP level functions
 
Obj EN_SEMI_RIGHT_CAYLEY_GRAPH(Obj self, GapSemigroup S) {
  initRNams();
  Obj en_semi = semi_get_en_semi(S);

  if (en_semi_get_type(en_semi) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_cpp(en_semi);
    bool       report   = semi_get_report(S);
    return cayley_graph_t_to_plist(
        semi_cpp->right_cayley_graph(report));
  } else {
    return ElmPRec(fropin(S, INTOBJ_INT(-1), 0, False),  RNam_right);
  }
}

Obj EN_SEMI_SIZE(Obj self, GapSemigroup S) {
  initRNams();
  Obj en_semi = semi_get_en_semi(S);

  if (en_semi_get_type(en_semi) != UNKNOWN) {
    bool report = semi_get_report(S);
    return INTOBJ_INT(en_semi_get_cpp(en_semi)->size(report));
  } else {
    Obj data = fropin(S, INTOBJ_INT(-1), 0, False);
    return INTOBJ_INT(LEN_PLIST(ElmPRec(data, RNam_elts)));
  }
}
