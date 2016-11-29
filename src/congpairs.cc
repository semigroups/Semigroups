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

#include "src/congpairs.h"

#include <string>
#include <vector>

#include "fropin.h"
#include "rnams.h"
#include "semigrp.h"

#include "semigroupsplusplus/cong.h"

using semigroupsplusplus::Congruence;
using semigroupsplusplus::word_t;
using semigroupsplusplus::relation_t;
using semigroupsplusplus::RecVec;

static inline word_t plist_to_word_t(Obj plist) {
  word_t word;
  for (size_t i = 1; i <= (size_t) LEN_PLIST(plist); i++) {
    Obj j = ELM_PLIST(plist, i);
    assert(IS_INTOBJ(j));
    word.push_back(INT_INTOBJ(j) - 1);
  }
  return word;
}

static inline bool cong_obj_has_cpp_cong(Obj cong) {
  initRNams();
  return IsbPRec(cong, RNam_cong_pairs_congruence)
         && CLASS_OBJ<Congruence*>(ElmPRec(cong, RNam_cong_pairs_congruence))
                != nullptr;
}

static inline gap_semigroup_t cong_obj_get_range_obj(gap_cong_t o) {
  initRNams();
  return ElmPRec(o, RNam_range);
}

static inline Semigroup* cong_obj_get_range(gap_cong_t o) {
  return semi_obj_get_semi_cpp(cong_obj_get_range_obj(o));
}

static inline bool cong_obj_get_range_type(gap_cong_t o) {
  initRNams();
  // assert(IsSemigroupCongruenceByGeneratingPairsRep(o));
  return semi_obj_get_type(cong_obj_get_range_obj(o));
}

static void cong_obj_init_cpp_cong(Obj o) {
  // assert(IsSemigroupCongruenceByGeneratingPairsRep(o));
  assert(!cong_obj_has_cpp_cong(o));

  initRNams();

  gap_list_t      genpairs  = ElmPRec(o, RNam_genpairs);
  gap_semigroup_t range_obj = cong_obj_get_range_obj(o);
  std::string     type      = std::string(CSTR_STRING(ElmPRec(o, RNam_type)));
  Congruence*     cong;
  bool            report = semi_obj_get_report(range_obj);

  if (cong_obj_get_range_type(o) != UNKNOWN) {
    Semigroup* range = cong_obj_get_range(o);
    range->enumerate(Semigroup::LIMIT_MAX, report);

    std::vector<relation_t> extra;
    word_t                  lhs, rhs;

    for (size_t i = 1; i <= (size_t) LEN_PLIST(genpairs); i++) {
      Obj lhs_obj = ELM_PLIST(ELM_PLIST(genpairs, i), 1);
      Obj rhs_obj = ELM_PLIST(ELM_PLIST(genpairs, i), 2);

      range->factorisation(
          lhs, INT_INTOBJ(EN_SEMI_POSITION(0L, range_obj, lhs_obj)) - 1);
      range->factorisation(
          rhs, INT_INTOBJ(EN_SEMI_POSITION(0L, range_obj, rhs_obj)) - 1);

      extra.push_back(make_pair(lhs, rhs));
      lhs.clear();
      rhs.clear();
    }
    if (range->size() > 1000) {
      cong = parallel_todd_coxeter(
          new Congruence(type, range, extra, true, report, 1),
          new Congruence(type, range, extra, false, report, 2),
          report);
    } else {
      cong = new Congruence(type, range, extra, true, report, 1);
      cong->todd_coxeter(report);
    }
  } else {
    gap_rec_t               data  = fropin(range_obj, INTOBJ_INT(-1), 0, False);
    gap_list_t              rules = ElmPRec(data, RNam_rules);
    gap_list_t              words = ElmPRec(data, RNam_words);
    std::vector<relation_t> rels;
    std::vector<relation_t> extra;

    // convert the rules (i.e. relations) to relation_t's
    for (size_t i = 1; i <= (size_t) LEN_PLIST(rules); i++) {
      word_t lhs = plist_to_word_t(ELM_PLIST(ELM_PLIST(rules, i), 1));
      word_t rhs = plist_to_word_t(ELM_PLIST(ELM_PLIST(rules, i), 2));
      rels.push_back(make_pair(lhs, rhs));
    }

    // convert the generating pairs to relation_t's
    for (size_t i = 1; i <= (size_t) LEN_PLIST(genpairs); i++) {
      Obj lhs_obj = ELM_PLIST(ELM_PLIST(genpairs, i), 1);
      Obj rhs_obj = ELM_PLIST(ELM_PLIST(genpairs, i), 2);

      Obj lhs = ELM_PLIST(words,
                          INT_INTOBJ(EN_SEMI_POSITION(0L, range_obj, lhs_obj)));
      Obj rhs = ELM_PLIST(words,
                          INT_INTOBJ(EN_SEMI_POSITION(0L, range_obj, rhs_obj)));

      extra.push_back(make_pair(plist_to_word_t(lhs), plist_to_word_t(rhs)));
    }

    size_t nrgens = LEN_PLIST(semi_obj_get_gens(range_obj));

    Obj graph;

    if (type == "left") {
      // the left Cayley graph
      graph = ElmPRec(data, RNam_left);
    } else {
      assert(type == "right" || type == "twosided");
      // the right Cayley graph
      graph = ElmPRec(data, RNam_right);
    }

    RecVec<size_t> prefill(nrgens, LEN_PLIST(graph) + 1);

    Obj genslookup = ElmPRec(data, RNam_genslookup);
    for (size_t i = 0; i < nrgens; i++) {
      prefill.set(0, i, INT_INTOBJ(ELM_PLIST(genslookup, i + 1)));
    }

    for (size_t i = 1; i <= (size_t) LEN_PLIST(graph); i++) {
      Obj next = ELM_PLIST(graph, i);
      for (size_t j = 1; j <= nrgens; j++) {
        prefill.set(i, j - 1, INT_INTOBJ(ELM_PLIST(next, j)));
      }
    }
    if (LEN_PLIST(ElmPRec(data, RNam_elts)) > 1000) {
      cong =
          parallel_todd_coxeter(new Congruence(type, nrgens, extra, prefill, 1),
                                new Congruence(type, nrgens, rels, extra, 2),
                                report);
    } else {
      cong = new Congruence(
          type, nrgens, extra, prefill, 1);
      cong->todd_coxeter(report);
    }
  }
  cong->compress();
  AssPRec(o, RNam_cong_pairs_congruence, OBJ_CLASS(cong, T_SEMI_SUBTYPE_CONG));
}

static Congruence* cong_obj_get_cpp(Obj cong) {
  initRNams();
  if (!cong_obj_has_cpp_cong(cong)) {
    cong_obj_init_cpp_cong(cong);
  }
  Obj tsemiobj = ElmPRec(cong, RNam_cong_pairs_congruence);
  assert(TNUM_OBJ(tsemiobj) == T_SEMI
         && SUBTYPE_OF_T_SEMI(tsemiobj) == T_SEMI_SUBTYPE_CONG);
  return CLASS_OBJ<Congruence*>(tsemiobj);
}

Obj CONG_PAIRS_NR_CLASSES(Obj self, Obj o) {
  return INTOBJ_INT(cong_obj_get_cpp(o)->nr_classes());
}

Obj CONG_PAIRS_IN(Obj self, gap_cong_t o, gap_list_t pair) {
  initRNams();
  gap_semigroup_t S = cong_obj_get_range_obj(o);
  size_t lhs_pos    = INT_INTOBJ(EN_SEMI_POSITION(0L, S, ELM_LIST(pair, 1)));
  size_t rhs_pos    = INT_INTOBJ(EN_SEMI_POSITION(0L, S, ELM_LIST(pair, 2)));

  // FIXME check if l/rhs_pos are undefined!!
  if (IsbPRec(o, RNam_fin_cong_lookup)) {
    // TODO(JDM) use FindPRec and GET_ELM_PREC
    Obj lookup = ElmPRec(o, RNam_fin_cong_lookup);
    Obj out    = True;
    EQ_INTOBJS(out, ELM_PLIST(lookup, lhs_pos), ELM_PLIST(lookup, rhs_pos));
    return out;
  }

  word_t lhs, rhs;

  if (cong_obj_get_range_type(o) != UNKNOWN) {
    Semigroup* range = cong_obj_get_range(o);

    range->factorisation(lhs, lhs_pos - 1);
    range->factorisation(rhs, rhs_pos - 1);
  } else {
    gap_rec_t data  = fropin(S, INTOBJ_INT(-1), 0, False);
    gap_list_t words = ElmPRec(data, RNam_words);

    lhs = plist_to_word_t(ELM_PLIST(words, lhs_pos));
    rhs = plist_to_word_t(ELM_PLIST(words, rhs_pos));
  }

  Congruence* cong = cong_obj_get_cpp(o);
  return (cong->word_to_coset(lhs) == cong->word_to_coset(rhs) ? True : False);
}

Obj CONG_PAIRS_LOOKUP_PART(Obj self, gap_cong_t o) {
  initRNams();
  // TODO(JDM) assert o is correct type of object
  if (IsbPRec(o, RNam_fin_cong_lookup)) {
    assert(IsbPRec(o, RNam_fin_cong_partition));
    return True;
  }

  Congruence*     cong      = cong_obj_get_cpp(o);
  gap_semigroup_t range_obj = cong_obj_get_range_obj(o);
  bool            report    = semi_obj_get_report(range_obj);

  Obj partition = NEW_PLIST(T_PLIST_HOM, cong->nr_classes());
  SET_LEN_PLIST(partition, cong->nr_classes());

  for (size_t i = 0; i < cong->nr_classes(); i++) {
    Obj next = NEW_PLIST(T_PLIST_CYC, 0);
    SET_LEN_PLIST(next, 0);
    SET_ELM_PLIST(partition, i + 1, next);
    CHANGED_BAG(partition);
  }

  Obj lookup;

  if (cong_obj_get_range_type(o) != UNKNOWN) {
    Semigroup* range = cong_obj_get_range(o);

    lookup = NEW_PLIST(T_PLIST_CYC, range->size());
    SET_LEN_PLIST(lookup, range->size());

    word_t word;

    for (size_t i = 0; i < range->size(); i++) {
      range->factorisation(word, i, report);  // changes word in place
      size_t coset = cong->word_to_coset(word);
      SET_ELM_PLIST(lookup, i + 1, INTOBJ_INT(coset));

      Obj c = ELM_PLIST(partition, coset);
      AssPlist(c, LEN_PLIST(c) + 1, INTOBJ_INT(i + 1));
      CHANGED_BAG(partition);

      word.clear();
    }
  } else {
    gap_rec_t data =
        fropin(cong_obj_get_range_obj(o), INTOBJ_INT(-1), 0, False);
    Obj words = ElmPRec(data, RNam_words);

    lookup = NEW_PLIST(T_PLIST_CYC, LEN_PLIST(words));
    SET_LEN_PLIST(lookup, LEN_PLIST(words));

    for (size_t i = 1; i <= (size_t) LEN_PLIST(words); i++) {
      size_t coset = cong->word_to_coset(plist_to_word_t(ELM_PLIST(words, i)));
      SET_ELM_PLIST(lookup, i, INTOBJ_INT(coset));

      Obj c = ELM_PLIST(partition, coset);
      AssPlist(c, LEN_PLIST(c) + 1, INTOBJ_INT(i));
      CHANGED_BAG(partition);
    }
  }
  AssPRec(o, RNam_fin_cong_partition, partition);
  AssPRec(o, RNam_fin_cong_lookup, lookup);
  return True;
}

Obj CONG_PAIRS_CLASS_COSET_ID(Obj self, gap_cong_class_t o) {
  initRNams();
  // TODO(JDM) assert o is correct type of object

  Obj             rep       = ElmPRec(o, RNam_rep);
  Obj             cong_obj  = ElmPRec(o, RNam_cong);
  gap_semigroup_t range_obj = cong_obj_get_range_obj(cong_obj);
  bool            report    = semi_obj_get_report(range_obj);

  if (IsbPRec(o, RNam_fin_cong_lookup)) {
    // TODO(JDM) Use FindPRec and GET_ELM_PREC
    Obj lookup = ElmPRec(cong_obj, RNam_fin_cong_lookup);
    return ELM_PLIST(lookup,
                     INT_INTOBJ(EN_SEMI_POSITION(self, range_obj, rep)));
  } else if (cong_obj_get_range_type(cong_obj) != UNKNOWN) {
    Congruence* cong  = cong_obj_get_cpp(cong_obj);
    Semigroup*  range = cong_obj_get_range(cong_obj);

    word_t word;
    range->factorisation(
        word, INT_INTOBJ(EN_SEMI_POSITION(self, range_obj, rep)) - 1, report);
    return INTOBJ_INT(cong->word_to_coset(word));
  } else {
    gap_rec_t  data = fropin(range_obj, INTOBJ_INT(-1), 0, False);
    Congruence* cong = cong_obj_get_cpp(cong_obj);

    Obj word = ELM_PLIST(ElmPRec(data, RNam_words),
                         INT_INTOBJ(EN_SEMI_POSITION(self, range_obj, rep)));

    return INTOBJ_INT(cong->word_to_coset(plist_to_word_t(word)));
  }
}
