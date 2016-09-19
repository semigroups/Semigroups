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

#include "src/compiled.h"

// Typedefs TODO these should go elsewhere

typedef Obj GapSemigroup;
typedef Obj GapPlist;
typedef Obj GapList;
typedef Obj GapElement;

// RNams
static Int RNam_GeneratorsOfMagma = RNamName("GeneratorsOfMagma");
static Int RNam_opts = RNamName("opts");

static Int RNam_en_semi_data = RNamName("__en_semi_data");
static Int RNam_en_semi_data_cpp_semi = RNamName("__en_semi_data_cpp_semi");

//TODO initRnams function

std::vector<Element*>*
plist_to_vec(Converter* converter, GapPlist elements, size_t degree) {
  assert(IS_PLIST(elements));

  auto out = new std::vector<Element*>();

  for (size_t i = 0; i < (size_t) LEN_PLIST(elements); i++) {
    out->push_back(converter->convert(ELM_LIST(elements, i + 1), degree));
  }
  return out;
}

// TODO this should go elsewhere
template <typename T>
static inline void really_delete_cont(T* cont) {
  for (Element* x : *cont) {
    x->really_delete();
  }
  delete cont;
}

// Semigroups

GapPlist semi_get_gens(GapSemigroup semi_gap) {
  UInt i;
  if (FindPRec(semi_gap, RNam_GeneratorsOfMagma, &i, 1)) {
    PLAIN_LIST(GET_ELM_PREC(semi_gap, i));
    return GET_ELM_PREC(semi_gap, i);
  } else {
    ErrorQuit("Cannot find generators of the semigroup!", 0L, 0L);
    return 0L;
  }
}

Obj semi_get_rep(GapSemigroup semi_gap) {
  UInt i;
  if (FindPRec(semi_gap, RNam_Representative, &i, 1)) {
    return GET_ELM_PREC(semi_gap, i);
  } else {
    GapPlist gens = semi_get_gens(semi_gap);
    if (LEN_PLIST(gens) > 0) {
      return ELM_PLIST(gens, 1);
    } else {
      ErrorQuit("Cannot find a representative of the semigroup!", 0L, 0L);
      return 0L;
    }
  }
}

size_t semi_get_batch_size(GapSemigroup semi_gap) {
  UInt i;
  if (FindPRec(semi_gap, RNam_opts, &i, 1)) {
    opts = GET_ELM_PREC(semi_gap, i);
    if (FindPRec(opts, RNam_batch_size, &i, 1)) {
      return INT_INTOBJ(GET_ELM_PREC(opts, i));
    } else {
      // Print warning
      return 8192;
    }
  } else {
    ErrorQuit("Cannot find generators of the semigroup options record!", 0L, 0L);
    return 0L;
  }
}

static inline bool semi_get_report(GapSemigroup semi_gap) {
  UInt i;
  if (FindPRec(S, RNam_opts, &i, 1)) {
    Obj opts = GET_ELM_PREC(S, i);
    if (FindPRec(opts, RNam_report, &i, 1)) {
      return (GET_ELM_PREC(opts, i) == True ? true : false);
    } else {
      // Print warning
      return false;
    }
  } else {
    ErrorQuit("Cannot find generators of the semigroup options record!", 0L, 0L);
    return 0L;
  }
}

// Enumerable semigroups

enum en_semi_t {
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
};

en_semi_t en_semi_type (GapSemigroup S) {
  en_semi_t type;
  en_semi_type_and_degree(S, type, 0);
  return type;
}

size_t en_semi_degree (GapSemigroup S) {
  size_t degree;
  en_semi_type_and_degree(S, 0, degree);
  return degree;
}

void en_semi_type_and_degree(GapSemigroup S, en_semi_t& type, size_t& deg) {
  Obj x = semi_get_rep(S);

  if (IS_TRANS(x)) {
    if (data_degree(data) < 65536) { //FIXME!!
      return TRANS2;
    } else {
      return TRANS4;
    }
  }

  if (IS_PPERM(x)) {
    if (data_degree(data) < 65535) {
      return PPERM2;
    } else {
      return PPERM4;
    }
  }
  if (TNUM_OBJ(x) == T_BIPART) {
    type = BIPART;
    deg = INT_INTOBJ(BIPART_DEGREE(0L, x));
  }

  switch (TNUM_OBJ(x)) {
    case T_POSOBJ:
      if (CALL_1ARGS(IsBooleanMat, x) == True) {
        type = BOOL_MAT;
        deg = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
      } else if (CALL_1ARGS(IsMaxPlusMatrix, x) == True) {
        type = MAX_PLUS_MAT;
        deg = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
      } else if (CALL_1ARGS(IsMinPlusMatrix, x) == True) {
        type = MIN_PLUS_MAT;
        deg = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
      } else if (CALL_1ARGS(IsTropicalMaxPlusMatrix, x) == True) {
        type = TROP_MAX_PLUS_MAT;
        deg = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
      } else if (CALL_1ARGS(IsTropicalMinPlusMatrix, x) == True) {
        type = TROP_MIN_PLUS_MAT;
        deg = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
      } else if (CALL_1ARGS(IsProjectiveMaxPlusMatrix, x) == True) {
        type = PROJ_MAX_PLUS_MAT;
        deg = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
      } else if (CALL_1ARGS(IsNTPMatrix, x) == True) {
        type = NTP_MAT;
        deg = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
      } else if (CALL_1ARGS(IsIntegerMatrix, x) == True) {
        type = INT_MAT;
        deg = INT_INTOBJ(CALL_1ARGS(DimensionOfMatrixOverSemiring, x));
      } else if (CALL_1ARGS(IsPBR, x) == True) {
        type = PBR_TYPE;
        deg = INT_INTOBJ(CALL_1ARGS(DegreeOfPBR, x));
      }
      return UNKNOWN;
    // intentional fall through
    default:
      return UNKNOWN;
  }
}

static inline Obj en_semi_get_data(GapSemigroup semi_gap) {
  initRNams();
  UInt i;
  if (FindPRec(semi_gap, RNam_en_semi_data, &i, 1)) {
    return GET_ELM_PREC(semi_gap, i);
  } else {
    Obj en_semi_data = NEW_PREC(0);
    SET_LEN_PREC(en_semi_data, 0);
    AssPRec(semi_gap, RNam_en_semi_data, en_semi_data);
    CHANGED_BAG(semi_gap);
    return en_semi_data;
  }
}

bool en_semi_data_has_cpp_semi(Obj en_semi_data) {
  initRNams();
  UInt i;
  return (FindPRec(en_semi_data, RNam_en_semi_data_cpp_semi, &i, 1) 
          && GET_ELM_PREC(en_semi_data, i) != nullptr);
}

void en_semi_data_init_conv () {

}

void en_semi_data_init_cpp_semi(GapSemigroup semi_gap, Semigroup* semi_cpp) {

  Obj en_semi_data = en_semi_get_data(semi_gap);

  if (semi_cpp != nullptr) {
    UInt i;
    if (en_semi_data_has_cpp_semi(en_semi_data)) {
      assert(false);
    }
    semi_cpp->set_batch_size(en_semi_get_batch_size(semi_gap));
    AssPRec(en_semi_data, 
            RNam_en_semi_data_cpp_semi, 
            OBJ_CLASS(semi_cpp, T_SEMI_SUBTYPE_SEMIGP));
    return;
  }

  if (en_semi_data_has_cpp_semi(en_semi_data)) {
    return;
  }

  Converter* converter = en_semi_data_get_conv(en_semi_data);

  GapPlist gens_gap = semi_get_gens(semi_gap);

  size_t                 degree = semi_get_degree(semi_gap);
  std::vector<Element*>* gens = plist_to_vec(converter, gens_gap, degree)

  semi_cpp = new Semigroup(gens);
  semi_cpp->set_batch_size(semi_get_batch_size(semi_gap));

  AssPRec(en_semi_data, 
          RNam_en_semi_data_cpp_semi, 
          OBJ_CLASS(semi_cpp, T_SEMI_SUBTYPE_SEMIGP));

  really_delete_cont(gens);
}

