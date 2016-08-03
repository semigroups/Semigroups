/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ congruence objects.
 *
 */

#include "data.h"
#include "fincong.h"

#include "semigroups++/tc.h"

static std::vector<size_t> __lookup;

bool fin_cong_has_cpp_cong (Obj cong) {
  initRNams();
  return IsbPRec(cong, RNam_congruence)
    && CLASS_OBJ<Congruence>(ElmPRec(cong, RNam_congruence)) != nullptr;
}

inline Semigroup* cong_range (Obj o) {
  initRNams();
  // assert(IsSemigroupCongruenceByGeneratingPairsRep(o));
  Obj data = ElmPRec(o, RNam_fin_cong_range);
  return data_semigroup(data);
}

inline bool cong_range_type (Obj o) {
  initRNams();
  // assert(IsSemigroupCongruenceByGeneratingPairsRep(o));
  Obj data = ElmPRec(o, RNam_fin_cong_range);
  return data_type(data);
}

void fin_cong_init (Obj o) {
  // assert(IsSemigroupCongruenceByGeneratingPairsRep(o));
  assert(! fin_cong_has_cpp_cong(o));

  if (cong_range_type(o) != UNKNOWN) {
    initRNams();

    Semigroup* range = cong_range(o);
    range->enumerate(-1, rec_get_report(o));

    Obj genpairs = ElmPRec(o, RNam_fin_cong_genpairs);
    Obj type =     ElmPRec(o, RNam_fin_cong_type);

    std::vector<relation_t> extra;
    for (size_t i = 1; i <= (size_t) LEN_PLIST(genpairs); i++) {
      Obj    rel1 = ELM_PLIST(ELM_PLIST(genpairs, i), 1);
      Obj    rel2 = ELM_PLIST(ELM_PLIST(genpairs, i), 2);
      word_t lhs, rhs;
      for (size_t j = 1; j <= (size_t) LEN_PLIST(rel1); j++) {
        lhs.push_back(INT_INTOBJ(ELM_PLIST(rel1, j)) - 1);
      }
      for (size_t j = 1; j <= (size_t) LEN_PLIST(rel2); j++) {
        rhs.push_back(INT_INTOBJ(ELM_PLIST(rel2, j)) - 1);
      }
      extra.push_back(make_pair(lhs, rhs));
    }

    Congruence* cong = finite_cong_enumerate(std::string(CSTR_STRING(type)),
                                             range,
                                             extra,
                                             rec_get_report(o));

    AssPRec(o, RNam_congruence, OBJ_CLASS(cong, T_SEMI_SUBTYPE_CONG));
  } else {
    ErrorQuit("fin_cong_init: not yet implemented,", 0L, 0L);
  }
}

Congruence* fin_cong_get_cpp (Obj cong) {
  initRNams();
  if (!fin_cong_has_cpp_cong(cong)) {
    fin_cong_init(cong);
  }
  Obj tsemiobj = ElmPRec(cong, RNam_congruence);
  assert(TNUM_OBJ(tsemiobj) == T_SEMI
         && SUBTYPE_OF_T_SEMI(tsemiobj) == T_SEMI_SUBTYPE_CONG);
  return CLASS_OBJ<Congruence>(tsemiobj);
}

Obj FIN_CONG_NR_CLASSES (Obj self, Obj o) {
  if (cong_range_type(o) != UNKNOWN) {
    return INTOBJ_INT(fin_cong_get_cpp(o)->nr_active_cosets() - 1);
  }
}

Obj FIN_CONG_PAIR_IN (Obj self, Obj o, Obj pair) {

  if (cong_range_type(o) != UNKNOWN) {
    Congruence* cong = fin_cong_get_cpp(o);
    Obj x = ELM_LIST(pair, 1);
    Obj y = ELM_LIST(pair, 2);

    word_t lhs, rhs;
    for (size_t j = 1; j <= (size_t) LEN_LIST(x); j++) {
      lhs.push_back(INT_INTOBJ(ELM_LIST(x, j)) - 1);
    }
    for (size_t j = 1; j <= (size_t) LEN_LIST(y); j++) {
      rhs.push_back(INT_INTOBJ(ELM_LIST(y, j)) - 1);
    }
    return (cong->word_to_coset(lhs) == cong->word_to_coset(rhs) ? True : False);
  }
}

Obj FIN_CONG_LOOKUP (Obj self, Obj o) {

  if (cong_range_type(o) != UNKNOWN) {
    Congruence* cong = fin_cong_get_cpp(o);
    Semigroup* range = cong_range(o);
    bool report = rec_get_report(o);
    word_t word;

    __lookup.clear();
    __lookup.resize(range->size());
    size_t next = 1;

    Obj out = NEW_PLIST(T_PLIST_CYC, range->size());
    SET_LEN_PLIST(out, range->size());

    for (size_t i = 0; i < range->size(); i++) {
      range->factorisation(word, i, report);
      size_t coset = cong->word_to_coset(word);
      if (__lookup[coset] == 0) {
        __lookup[coset] = next++;
      }
      SET_ELM_PLIST(out, i + 1, INTOBJ_INT(__lookup[coset]));
      word.clear();
    }
    return out;
  }
}
