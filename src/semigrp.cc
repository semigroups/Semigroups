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

#ifdef DEBUG
#define START_FUNC std::cout << __func__ << " called" << std::endl;
#define STOP_FUNC std::cout << __func__ << " returned" << std::endl;
#else 
#define START_FUNC
#define STOP_FUNC
#endif

// RNams
static Int RNam_GeneratorsOfMagma = RNamName("GeneratorsOfMagma");
static Int RNam_Representative    = RNamName("Representative");

//static Int RNam_batch_size        = RNamName("batch_size");
static Int RNam_ht     = RNamName("ht");
static Int RNam_nr     = RNamName("nr");
static Int RNam_opts   = RNamName("opts");
static Int RNam_parent = RNamName("parent");
//static Int RNam_report            = RNamName("report");

static Int RNam_en_semi_cpp = RNamName("__en_semi_cpp_data");
static Int RNam_en_semi_frp = RNamName("__en_semi_frp_data");

// TODO initRnams function

std::vector<Element*>*
plist_to_vec(Converter* converter, gap_plist_t elements, size_t degree) {
  START_FUNC
  assert(IS_PLIST(elements));

  auto out = new std::vector<Element*>();

  for (size_t i = 0; i < (size_t) LEN_PLIST(elements); i++) {
    out->push_back(converter->convert(ELM_LIST(elements, i + 1), degree));
  }
  return out;
  STOP_FUNC
}

gap_plist_t word_t_to_plist(word_t const& word) {
  gap_plist_t out = NEW_PLIST(T_PLIST_CYC, word.size());
  SET_LEN_PLIST(out, word.size());

  for (size_t i = 0; i < word.size(); i++) {
    SET_ELM_PLIST(out, i + 1, INTOBJ_INT(word[i] + 1));
  }
  CHANGED_BAG(out);
  return out;
}

gap_plist_t cayley_graph_t_to_plist(cayley_graph_t* graph) {
  START_FUNC
  assert(graph->size() != 0);
  gap_plist_t out = NEW_PLIST(T_PLIST, graph->nr_rows());
  SET_LEN_PLIST(out, graph->nr_rows());

  for (size_t i = 0; i < graph->nr_rows(); i++) {
    gap_plist_t next = NEW_PLIST(T_PLIST_CYC, graph->nr_cols());
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
  STOP_FUNC
}

// TODO this should go elsewhere
template <typename T> static inline void really_delete_cont(T* cont) {
  START_FUNC
  for (Element* x : *cont) {
    x->really_delete();
  }
  delete cont;
  STOP_FUNC
}

// Semigroups

gap_plist_t semi_get_gens(gap_semigroup_t semi_gap) {
  START_FUNC
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
  STOP_FUNC
}

Obj semi_get_rep(gap_semigroup_t S) {
  START_FUNC
  initRNams();
  UInt i;
  if (FindPRec(S, RNam_Representative, &i, 1)) {
    return GET_ELM_PREC(S, i);
  } else {
    gap_plist_t gens = semi_get_gens(S);
    if (LEN_PLIST(gens) > 0) {
      return ELM_PLIST(gens, 1);
    } else {
      ErrorQuit("Cannot find a representative of the semigroup!", 0L, 0L);
      return 0L;
    }
  }
  STOP_FUNC
}

size_t semi_get_batch_size(gap_semigroup_t S) {
  START_FUNC
  initRNams();
  UInt i;
  if (FindPRec(S, RNam_opts, &i, 1)) {
    gap_prec_t opts = GET_ELM_PREC(S, i);
    if (FindPRec(opts, RNam_batch_size, &i, 1)) {
      return INT_INTOBJ(GET_ELM_PREC(opts, i));
    } 
  }
#ifdef DEBUG
  Pr("Using default value of 8192 for reporting!\n", 0L, 0L);
#endif
  return 8192;
  STOP_FUNC
}

static inline bool semi_obj_get_report(gap_semigroup_t S) {
  START_FUNC
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
  STOP_FUNC
}

static inline size_t semi_obj_get_nr_threads(gap_semigroup_t S) {
  initRNams();
  UInt i;
  if (FindPRec(S, RNam_opts, &i, 1)) {
    Obj opts = GET_ELM_PREC(S, i);
    if (FindPRec(opts, RNam_nr_threads, &i, 1)) {
      return INT_INTOBJ(GET_ELM_PREC(opts, i));
    }
  }
#ifdef DEBUG
  Pr("Using default value of 1 for number of threads!\n", 0L, 0L);
#endif
  return 1;
  STOP_FUNC
}

static inline size_t semi_get_threshold(gap_semigroup_t S) {
  START_FUNC
  initRNams();

  Obj x = semi_get_rep(S);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(CALL_1ARGS(IsTropicalMatrix, x) || CALL_1ARGS(IsNTPMatrix, x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
  STOP_FUNC
}

static inline size_t semi_get_period(gap_semigroup_t S) {
  START_FUNC
  initRNams();

  Obj x = semi_get_rep(S);

  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(CALL_1ARGS(IsNTPMatrix, x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2));
  STOP_FUNC
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

Obj semi_obj_get_en_semi(gap_semigroup_t S) {
  START_FUNC
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
    gap_plist_t gens = semi_get_gens(S);
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
    gap_plist_t gens = semi_get_gens(S);
    for (size_t i = 1; i <= (size_t) LEN_PLIST(gens); i++) {
      size_t n = std::max(DEG_PPERM(ELM_PLIST(gens, i)),
                          CODEG_PPERM(ELM_PLIST(gens, i)));
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
    gap_plist_t gens_gap = semi_get_gens(S);

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
  STOP_FUNC
}

//FIXME should be inline
Semigroup* semi_obj_get_semi_cpp(gap_semigroup_t S) {
  return en_semi_get_cpp(semi_obj_get_en_semi(S));
}

gap_prec_t semi_obj_get_fropin(gap_semigroup_t S) {
  UInt i;
  if (FindPRec(S, RNam_en_semi_frp, &i, 1)) {
    return GET_ELM_PREC(S, i);
  } else {
    if (semi_obj_get_type(S) != UNKNOWN) {  // only initialise a record
      gap_prec_t fp = NEW_PREC(0);
      SET_LEN_PREC(fp, 0);
      AssPRec(S, RNam_en_semi_frp, fp);
      CHANGED_BAG(S);
      return fp;
    } else {
      CALL_1ARGS(INIT_FROPIN, S);
      if (FindPRec(S, RNam_en_semi_frp, &i, 1)) {
        return GET_ELM_PREC(S, i);
      }
      ErrorQuit("unknown error in INIT_FROPIN,", 0L, 0L);
      return 0L;
    }
  }
}

//FIXME should be inline
en_semi_t semi_obj_get_type(gap_semigroup_t S) {
  return en_semi_get_type(semi_obj_get_en_semi(S));
}

//static inline Converter* semi_get_converter(gap_semigroup_t S) {
//  return en_semi_get_converter(semi_obj_get_en_semi(S));
//}

//static inline size_t semi_get_degree(gap_semigroup_t S) {
//  return en_semi_get_degree(semi_obj_get_en_semi(S));
//}


/*bool en_semi_has_cpp_semi(Obj en_semi_data) {
  initRNams();
  UInt i;
  return (FindPRec(en_semi_data, RNam_en_semi_cpp_semi, &i, 1)
          && GET_ELM_PREC(en_semi_data, i) != nullptr);
  STOP_FUNC
}

bool en_semi_has_converter(Obj en_semi_data) {
  initRNams();
  UInt i;
  return (FindPRec(en_semi_data, RNam_en_semi_converter, &i, 1)
          && GET_ELM_PREC(en_semi_data, i) != nullptr);
}*/

static inline size_t en_semi_get_degree(Obj en_semi) {
  return CLASS_OBJ<size_t>(en_semi, 4);
  STOP_FUNC
}

// GAP level functions

Obj EN_SEMI_FACTORIZATION(Obj self, gap_semigroup_t so, gap_int_t pos) {
  Obj    es    = semi_obj_get_en_semi(so);
  size_t pos_c = INT_INTOBJ(pos);

  if (en_semi_get_type(es) != UNKNOWN) {
    gap_plist_t words;
    Semigroup*  semi_cpp = en_semi_get_cpp(es);

    if (pos_c > semi_cpp->current_size()) {
      ErrorQuit("the 2nd argument must be at most %d not %d",
                semi_cpp->current_size(),
                pos_c);
    }

    gap_prec_t fp = semi_obj_get_fropin(so);
    if (!IsbPRec(fp, RNam_words)) {
      // TODO Use FindPRec instead
      word_t w;  // changed in place by the next line
      semi_cpp->factorisation(w, pos_c - 1, semi_obj_get_report(so));
      words = NEW_PLIST(T_PLIST, pos_c);
      SET_LEN_PLIST(words, pos_c);
      SET_ELM_PLIST(words, pos_c, word_t_to_plist(w));
      CHANGED_BAG(words);
      AssPRec(fp, RNam_words, words);
    } else {
      words = ElmPRec(fp, RNam_words);
      if (pos_c > (size_t) LEN_PLIST(words) || ELM_PLIST(words, pos_c) == 0) {
        // avoid retracing the Schreier tree if possible
        size_t prefix = semi_cpp->prefix(pos_c - 1) + 1;
        size_t suffix = semi_cpp->suffix(pos_c - 1) + 1;
        if (prefix != 0 && prefix <= (size_t) LEN_PLIST(words)
            && ELM_PLIST(words, prefix) != 0) {
          Obj old_word = ELM_PLIST(words, prefix);
          Obj new_word = NEW_PLIST(T_PLIST_CYC, LEN_PLIST(old_word) + 1);
          memcpy((void*) ((char*) (ADDR_OBJ(new_word)) + sizeof(Obj)),
                 (void*) ((char*) (ADDR_OBJ(old_word)) + sizeof(Obj)),
                 (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
          SET_ELM_PLIST(new_word,
                        LEN_PLIST(old_word) + 1,
                        INTOBJ_INT(semi_cpp->final_letter(pos_c - 1) + 1));
          SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);
          AssPlist(words, pos_c, new_word);
        } else if (suffix != 0 && suffix <= (size_t) LEN_PLIST(words)
                   && ELM_PLIST(words, suffix) != 0) {
          Obj old_word = ELM_PLIST(words, suffix);
          Obj new_word = NEW_PLIST(T_PLIST_CYC, LEN_PLIST(old_word) + 1);
          memcpy((void*) ((char*) (ADDR_OBJ(new_word)) + 2 * sizeof(Obj)),
                 (void*) ((char*) (ADDR_OBJ(old_word)) + sizeof(Obj)),
                 (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
          SET_ELM_PLIST(
              new_word, 1, INTOBJ_INT(semi_cpp->first_letter(pos_c - 1) + 1));
          SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);
          AssPlist(words, pos_c, new_word);
        } else {
          word_t w;  // changed in place by the next line
          semi_cpp->factorisation(w, pos_c - 1, semi_obj_get_report(so));
          AssPlist(words, pos_c, word_t_to_plist(w));
        }
      }
    }
    CHANGED_BAG(fp);
    assert(IsbPRec(fp, RNam_words));
    assert(IS_PLIST(ElmPRec(fp, RNam_words)));
    assert(pos_c <= (size_t) LEN_PLIST(ElmPRec(fp, RNam_words)));
    return ELM_PLIST(ElmPRec(fp, RNam_words), pos_c);
  } else {
    gap_prec_t fp = fropin(so, INTOBJ_INT(pos), 0, False);
    return ELM_PLIST(ElmPRec(fp, RNam_words), pos_c);
  }
}

Obj EN_SEMI_LENGTH_ELEMENT(Obj self, gap_semigroup_t S, gap_int_t pos) {
  Obj en_semi = semi_obj_get_en_semi(S);
  if (en_semi_get_type(en_semi) != UNKNOWN) {
    return INTOBJ_INT(en_semi_get_cpp(en_semi)->length_non_const(
        INT_INTOBJ(pos) - 1, semi_obj_get_report(S)));
  } else {
    // TODO uncomment!!
    return INTOBJ_INT(LEN_PLIST(EN_SEMI_FACTORIZATION(self, S, pos)));
  }
}

Obj EN_SEMI_IS_DONE_ITERATOR(Obj self, Obj iter) {
  initRNams();
  Int size = INT_INTOBJ(EN_SEMI_SIZE(self, ElmPRec(iter, RNam_parent)));
  return (INT_INTOBJ(ElmPRec(iter, RNam_pos)) == size ? True : False);
}

Obj EN_SEMI_NR_IDEMPOTENTS(Obj self, gap_semigroup_t S) {
  Obj en_semi = semi_obj_get_en_semi(S);
  if (en_semi_get_type(en_semi) == UNKNOWN) {
    ErrorQuit("this should not have happen,", 0L, 0L);
  }
  return INTOBJ_INT(en_semi_get_cpp(en_semi)->nr_idempotents(
      semi_obj_get_report(S), semi_obj_get_nr_threads(S)));
}


Obj EN_SEMI_POSITION(Obj self, gap_semigroup_t S, Obj x) {
  Obj en_semi = semi_obj_get_en_semi(S);

  if (en_semi_get_type(en_semi) != UNKNOWN) {
    size_t   deg    = en_semi_get_degree(en_semi);
    Element* xx     = en_semi_get_converter(en_semi)->convert(x, deg);
    bool     report = semi_obj_get_report(S);
    size_t   pos    = en_semi_get_cpp(en_semi)->position(xx, report);
    delete xx;
    return (pos == Semigroup::UNDEFINED ? Fail : INTOBJ_INT(pos + 1));
  } else {
    Obj    data = semi_obj_get_fropin(S);
    Obj    ht   = ElmPRec(data, RNam_ht);
    size_t pos, nr;

    do {
      Obj val = CALL_2ARGS(HTValue, ht, x);
      if (val != Fail) {
        return val;
      }
      Obj limit = SumInt(ElmPRec(data, RNam_nr), INTOBJ_INT(1));
      fropin(data, limit, 0, False);
      pos = INT_INTOBJ(ElmPRec(data, RNam_pos));
      nr  = INT_INTOBJ(ElmPRec(data, RNam_nr));
    } while (pos <= nr);
    return CALL_2ARGS(HTValue, ht, x);
  }
}

// Get the position of <x> with out any further enumeration

Obj EN_SEMI_POSITION_CURRENT(Obj self, gap_semigroup_t S, gap_element_t x) {
  START_FUNC
  Obj en_semi = semi_obj_get_en_semi(S);

  if (en_semi_get_type(en_semi) != UNKNOWN) {
    size_t     deg       = en_semi_get_degree(en_semi);
    Element*   xx(en_semi_get_converter(en_semi)->convert(x, deg));
    size_t     pos = en_semi_get_cpp(en_semi)->position_current(xx);
    delete xx;
    return (pos == Semigroup::UNDEFINED ? Fail : INTOBJ_INT(pos + 1));
  } else {
    return CALL_2ARGS(HTValue, ElmPRec(semi_obj_get_fropin(S), RNam_ht), x);
  }
  STOP_FUNC
}

Obj EN_SEMI_LEFT_CAYLEY_GRAPH(Obj self, gap_semigroup_t S) {
  START_FUNC
  Obj en_semi = semi_obj_get_en_semi(S);

  if (en_semi_get_type(en_semi) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_cpp(en_semi);
    bool       report   = semi_obj_get_report(S);
    return cayley_graph_t_to_plist(
        semi_cpp->left_cayley_graph(report));
  } else {
    return ElmPRec(fropin(S, INTOBJ_INT(-1), 0, False),  RNam_left);
  }
  STOP_FUNC
}

Obj EN_SEMI_RELATIONS(Obj self, gap_semigroup_t so) {
  initRNams();
  gap_plist_t es = semi_obj_get_en_semi(so);
  gap_prec_t  fp = semi_obj_get_fropin(so);

  if (en_semi_get_type(es) != UNKNOWN) {
    if (!IsbPRec(fp, RNam_rules) || LEN_PLIST(ElmPRec(fp, RNam_rules)) == 0) {
      Semigroup*  semigroup = en_semi_get_cpp(es);
      bool        report    = semi_obj_get_report(so);
      gap_plist_t rules     = NEW_PLIST(T_PLIST, semigroup->nrrules(report));
      SET_LEN_PLIST(rules, semigroup->nrrules(report));
      size_t nr = 0;

      semigroup->reset_next_relation();
      std::vector<size_t> relation;
      semigroup->next_relation(relation, report);

      while (relation.size() == 2) {
        gap_plist_t next = NEW_PLIST(T_PLIST, 2);
        SET_LEN_PLIST(next, 2);
        for (size_t i = 0; i < 2; i++) {
          gap_plist_t w = NEW_PLIST(T_PLIST_CYC, 1);
          SET_LEN_PLIST(w, 1);
          SET_ELM_PLIST(w, 1, INTOBJ_INT(relation[i] + 1));
          SET_ELM_PLIST(next, i + 1, w);
          CHANGED_BAG(next);
        }
        nr++;
        SET_ELM_PLIST(rules, nr, next);
        CHANGED_BAG(rules);
        semigroup->next_relation(relation, report);
      }

      while (!relation.empty()) {
        gap_plist_t old_word =
            EN_SEMI_FACTORIZATION(self, so, INTOBJ_INT(relation[0] + 1));
        gap_plist_t new_word = NEW_PLIST(T_PLIST_CYC, LEN_PLIST(old_word) + 1);
        memcpy((void*) ((char*) (ADDR_OBJ(new_word)) + sizeof(Obj)),
               (void*) ((char*) (ADDR_OBJ(old_word)) + sizeof(Obj)),
               (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
        SET_ELM_PLIST(
            new_word, LEN_PLIST(old_word) + 1, INTOBJ_INT(relation[1] + 1));
        SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);

        gap_plist_t next = NEW_PLIST(T_PLIST, 2);
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
        semigroup->next_relation(relation, report);
      }
      AssPRec(fp, RNam_rules, rules);
      CHANGED_BAG(fp);
      CHANGED_BAG(so);
    }
  } else {
    fropin(so, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(fp, RNam_rules);
}

Obj EN_SEMI_RIGHT_CAYLEY_GRAPH(Obj self, gap_semigroup_t S) {
  START_FUNC
  Obj en_semi = semi_obj_get_en_semi(S);

  if (en_semi_get_type(en_semi) != UNKNOWN) {
    Semigroup* semi_cpp = en_semi_get_cpp(en_semi);
    bool       report   = semi_obj_get_report(S);
    return cayley_graph_t_to_plist(
        semi_cpp->right_cayley_graph(report));
  } else {
    return ElmPRec(fropin(S, INTOBJ_INT(-1), 0, False),  RNam_right);
  }
  STOP_FUNC
}

Obj EN_SEMI_SIZE(Obj self, gap_semigroup_t S) {
  START_FUNC
  initRNams();
  Obj en_semi = semi_obj_get_en_semi(S);

  if (en_semi_get_type(en_semi) != UNKNOWN) {
    bool report = semi_obj_get_report(S);
    return INTOBJ_INT(en_semi_get_cpp(en_semi)->size(report));
  } else {
    Obj data = fropin(S, INTOBJ_INT(-1), 0, False);
    return INTOBJ_INT(LEN_PLIST(ElmPRec(data, RNam_elts)));
  }
  STOP_FUNC
}
