/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ congruence objects.
 *
 */

#include "data.h"
#include "fincong.h"

#include "semigroups++/tc.h"

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

inline Converter* cong_range_converter (Obj o) {
  initRNams();
  // assert(IsSemigroupCongruenceByGeneratingPairsRep(o));
  Obj data = ElmPRec(o, RNam_fin_cong_range);
  return data_converter(data);
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
    cong->compress();

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

Obj FIN_CONG_LOOKUP_PART (Obj self, Obj o) {
//TODO assert o is correct type of object
  if (IsbPRec(o, RNam_fin_cong_lookup)) {
    assert(IsbPRec(o, RNam_fin_cong_partition));
    return True;
  }

  if (cong_range_type(o) != UNKNOWN) {
    Congruence* cong = fin_cong_get_cpp(o);
    Semigroup* range = cong_range(o);
    bool report = rec_get_report(o);
    word_t word;

    Obj lookup = NEW_PLIST(T_PLIST_CYC, range->size());
    SET_LEN_PLIST(lookup, range->size());

    Obj partition = NEW_PLIST(T_PLIST_HOM, cong->nr_active_cosets());
    SET_LEN_PLIST(partition, cong->nr_active_cosets() - 1);

    for (size_t i = 0; i < cong->nr_active_cosets(); i++) {
      Obj next = NEW_PLIST(T_PLIST_CYC, 0);
      SET_LEN_PLIST(next, 0);
      SET_ELM_PLIST(partition, i + 1, next);
      CHANGED_BAG(partition);
    }

    for (size_t i = 0; i < range->size(); i++) {
      range->factorisation(word, i, report);
      size_t coset = cong->word_to_coset(word);
      SET_ELM_PLIST(lookup, i + 1, INTOBJ_INT(coset));

      Obj c = ELM_PLIST(partition, coset);
      AssPlist(c, LEN_PLIST(c) + 1, INTOBJ_INT(i + 1));
      CHANGED_BAG(partition);

      word.clear();
    }
    AssPRec(o, RNam_fin_cong_partition, partition);
    AssPRec(o, RNam_fin_cong_lookup, lookup);
  }
}

Obj FIN_CONG_CLASS_COSET_ID (Obj self, Obj o) {
  //TODO assert o is correct type of object

  Obj rep      = ElmPRec(o, RNam_rep);
  Obj cong_gap = ElmPRec(o, RNam_cong);
  Obj data     = ElmPRec(cong_gap, RNam_fin_cong_range);

  if (IsbPRec(o, RNam_fin_cong_lookup)) {
    Obj lookup = ElmPRec(cong_gap, RNam_fin_cong_lookup);
    return ELM_PLIST(lookup, INT_INTOBJ(SEMIGROUP_POSITION(self, data, rep)));
  } else if (cong_range_type(cong_gap) != UNKNOWN) {
    Congruence* cong = fin_cong_get_cpp(cong_gap);
    Semigroup* range = cong_range(cong_gap);

    word_t word;
    range->factorisation(word,
                         INT_INTOBJ(SEMIGROUP_POSITION(self, data, rep)) - 1,
                         rec_get_report(o));
    return INTOBJ_INT(cong->word_to_coset(word));
  }
}
