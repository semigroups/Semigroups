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
#include <unordered_map>
#include <utility>
#include <vector>

#include "fropin.h"
#include "rnams.h"
#include "semigrp.h"

#include "libsemigroups/cong.hpp"
#include "libsemigroups/fpsemi.hpp"
#include "libsemigroups/report.hpp"

using libsemigroups::FpSemigroup;
using libsemigroups::relation_type;
using libsemigroups::ReportGuard;
using libsemigroups::word_type;
using libsemigroups::detail::DynamicArray2;
using Congruence      = libsemigroups::Congruence;
using congruence_type = libsemigroups::congruence_type;

static inline congruence_type cstring_to_congruence_t(char const* type) {
  std::string stype = std::string(type);
  if (stype == "left") {
    return congruence_type::left;
  } else if (stype == "right") {
    return congruence_type::right;
  } else if (stype == "twosided") {
    return congruence_type::twosided;
  } else {
    ErrorQuit("Unrecognised type %s", (Int) type, 0L);
    return congruence_type::left;
  }
}

static inline word_type plist_to_word_type(gap_list_t plist) {
  word_type word;
  for (size_t i = 1; i <= (size_t) LEN_PLIST(plist); i++) {
    Obj j = ELM_PLIST(plist, i);
    SEMIGROUPS_ASSERT(IS_INTOBJ(j));
    word.push_back(INT_INTOBJ(j) - 1);
  }
  return word;
}

static inline gap_list_t word_type_to_plist(word_type const* w) {
  gap_list_t plist = NEW_PLIST_IMM(T_PLIST_CYC, w->size());
  SET_LEN_PLIST(plist, w->size());
  for (size_t i = 0; i < w->size(); i++) {
    SET_ELM_PLIST(plist, i + 1, INTOBJ_INT((*w)[i] + 1));
  }
  return plist;
}

static inline bool cong_obj_has_cpp_cong(gap_cong_t cong) {
  initRNams();
  return IsbPRec(cong, RNam_cong_pairs_congruence)
         && CLASS_OBJ<Congruence*>(ElmPRec(cong, RNam_cong_pairs_congruence))
                != nullptr;
}

static inline gap_semigroup_t cong_obj_get_range_obj(gap_cong_t o) {
  initRNams();
  return ElmPRec(o, RNam_range);
}

static inline FroidurePin<Element const*>* cong_obj_get_range(gap_cong_t o) {
  return semi_obj_get_semi_cpp(cong_obj_get_range_obj(o));
}

static inline bool cong_obj_get_range_type(gap_cong_t o) {
  initRNams();
  // SEMIGROUPS_ASSERT(IsSemigroup<>CongruenceByGeneratingPairsRep(o));
  return semi_obj_get_type(cong_obj_get_range_obj(o));
}

static inline bool cong_obj_is_fp_cong(gap_cong_t cong) {
  initRNams();
  return (IsbPRec(cong, RNam_fp_nrgens));
}

static void cong_obj_init_cpp_cong(gap_cong_t o) {
  // SEMIGROUPS_ASSERT(IsSemigroup<>CongruenceByGeneratingPairsRep(o));
  SEMIGROUPS_ASSERT(!cong_obj_has_cpp_cong(o));
  initRNams();
  gap_list_t      genpairs  = ElmPRec(o, RNam_genpairs);
  gap_semigroup_t range_obj = cong_obj_get_range_obj(o);
  congruence_type type
      = cstring_to_congruence_t(CSTR_STRING(ElmPRec(o, RNam_type)));
  Congruence* cong   = nullptr;
  bool        report = semi_obj_get_report(range_obj);
  auto        rg     = ReportGuard(report);

  if (cong_obj_is_fp_cong(o)) {
    size_t nrgens = INT_INTOBJ(ElmPRec(o, RNam_fp_nrgens));

    FpSemigroup S;
    S.set_alphabet(nrgens);

    // Get the fp semigroup's rels
    gap_list_t gap_rels = ElmPRec(o, RNam_fp_rels);
    for (size_t i = 1; i <= (size_t) LEN_PLIST(gap_rels); i++) {
      gap_list_t rel = ELM_PLIST(gap_rels, i);
      S.add_rule(plist_to_word_type(ELM_PLIST(rel, 1)),
                  plist_to_word_type(ELM_PLIST(rel, 2)));
    }

    cong = new Congruence(type, S);

    // Get the extra pairs
    gap_list_t gap_extra = ElmPRec(o, RNam_fp_extra);
    for (size_t i = 1; i <= (size_t) LEN_PLIST(gap_extra); i++) {
      gap_list_t pair = ELM_PLIST(gap_extra, i);
      cong->add_pair(plist_to_word_type(ELM_PLIST(pair, 1)),
                     plist_to_word_type(ELM_PLIST(pair, 2)));
    }
  } else if (cong_obj_get_range_type(o) != UNKNOWN) {
    FroidurePin<Element const*>* range = cong_obj_get_range(o);

    cong = new Congruence(type, *range);
    for (size_t i = 1; i <= (size_t) LEN_PLIST(genpairs); i++) {
      Obj lhs_obj = ELM_PLIST(ELM_PLIST(genpairs, i), 1);
      Obj rhs_obj = ELM_PLIST(ELM_PLIST(genpairs, i), 2);
      cong->add_pair(
          range->factorisation(
              INT_INTOBJ(EN_SEMI_POSITION(0L, range_obj, lhs_obj)) - 1),
          range->factorisation(
              INT_INTOBJ(EN_SEMI_POSITION(0L, range_obj, rhs_obj)) - 1));
    }
  } else {
    gap_rec_t data = fropin(range_obj, INTOBJ_INT(-1), 0, False);
    gap_list_t words = ElmPRec(data, RNam_words);

    size_t nrgens = LEN_PLIST(semi_obj_get_gens(range_obj));

    cong = new Congruence(type, Congruence::policy::runners::none);
    cong->set_nr_generators(nrgens);

    libsemigroups::congruence::ToddCoxeter tc(type);
    tc.set_nr_generators(nrgens);

    // convert the generating pairs to relation_type's
    for (size_t i = 1; i <= (size_t) LEN_PLIST(genpairs); i++) {
      Obj lhs_obj = ELM_PLIST(ELM_PLIST(genpairs, i), 1);
      Obj rhs_obj = ELM_PLIST(ELM_PLIST(genpairs, i), 2);

      Obj lhs = ELM_PLIST(words,
                          INT_INTOBJ(EN_SEMI_POSITION(0L, range_obj, lhs_obj)));
      Obj rhs = ELM_PLIST(words,
                          INT_INTOBJ(EN_SEMI_POSITION(0L, range_obj, rhs_obj)));
      tc.add_pair(plist_to_word_type(lhs), plist_to_word_type(rhs));
    }

    Obj graph;

    if (type == congruence_type::left) {
      // the left Cayley graph
      graph = ElmPRec(data, RNam_left);
    } else {
      SEMIGROUPS_ASSERT(type == congruence_type::right
                        || type == congruence_type::twosided);
      // the right Cayley graph
      graph = ElmPRec(data, RNam_right);
    }

    DynamicArray2<size_t> table(nrgens, LEN_PLIST(graph));

    for (size_t i = 1; i <= (size_t) LEN_PLIST(graph); i++) {
      Obj next = ELM_PLIST(graph, i);
      for (size_t j = 1; j <= nrgens; j++) {
        table.set(i - 1, j - 1, INT_INTOBJ(ELM_PLIST(next, j)) - 1);
      }
    }
    tc.prefill(table);
    cong->add_runner(tc);
  }
  SEMIGROUPS_ASSERT(cong != nullptr);
  AssPRec(o, RNam_cong_pairs_congruence, OBJ_CLASS(cong, T_SEMI_SUBTYPE_CONG));
}

static Congruence* cong_obj_get_cpp(gap_cong_t cong) {
  initRNams();
  if (!cong_obj_has_cpp_cong(cong)) {
    cong_obj_init_cpp_cong(cong);
  }
  Obj tsemiobj = ElmPRec(cong, RNam_cong_pairs_congruence);
  SEMIGROUPS_ASSERT(TNUM_OBJ(tsemiobj) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(tsemiobj) == T_SEMI_SUBTYPE_CONG);
  return CLASS_OBJ<Congruence*>(tsemiobj);
}

Obj CONG_PAIRS_NR_CLASSES(Obj self, gap_cong_t o) {
  return INTOBJ_INT(cong_obj_get_cpp(o)->nr_classes());
}

Obj CONG_PAIRS_IN(Obj self, gap_cong_t o, Obj elm1, Obj elm2) {
  initRNams();

  word_type lhs, rhs;

  if (cong_obj_is_fp_cong(o)) {
    lhs = plist_to_word_type(elm1);
    rhs = plist_to_word_type(elm2);
  } else {
    gap_semigroup_t S       = cong_obj_get_range_obj(o);
    size_t          lhs_pos = INT_INTOBJ(EN_SEMI_POSITION(0L, S, elm1));
    size_t          rhs_pos = INT_INTOBJ(EN_SEMI_POSITION(0L, S, elm2));
    SEMIGROUPS_ASSERT(lhs_pos != UNDEFINED);
    SEMIGROUPS_ASSERT(rhs_pos != UNDEFINED);

    if (IsbPRec(o, RNam_fin_cong_lookup)) {
      // TODO(JDM) use FindPRec and GET_ELM_PREC
      Obj lookup = ElmPRec(o, RNam_fin_cong_lookup);
      int out    = (ELM_PLIST(lookup, lhs_pos) == ELM_PLIST(lookup, rhs_pos));
      return out ? True : False;
    }

    if (cong_obj_get_range_type(o) != UNKNOWN) {
      FroidurePin<Element const*>* range = cong_obj_get_range(o);

      range->factorisation(lhs, lhs_pos - 1);
      range->factorisation(rhs, rhs_pos - 1);
    } else {
      gap_rec_t  data  = fropin(S, INTOBJ_INT(-1), 0, False);
      gap_list_t words = ElmPRec(data, RNam_words);

      lhs = plist_to_word_type(ELM_PLIST(words, lhs_pos));
      rhs = plist_to_word_type(ELM_PLIST(words, rhs_pos));
    }
  }

  Congruence* cong = cong_obj_get_cpp(o);
  return (cong->contains(lhs, rhs) ? True : False);
}

// This describes a total ordering on the classes of the congruence.
// Returns True if pair[1] is in a lower class than pair[2], False otherwise.
Obj CONG_PAIRS_LESS_THAN(Obj        self,
                         gap_cong_t o,
                         gap_list_t rep1,
                         gap_list_t rep2) {
  initRNams();

  word_type lhs, rhs;

  if (cong_obj_is_fp_cong(o)) {
    lhs = plist_to_word_type(rep1);
    rhs = plist_to_word_type(rep2);
  } else {
    gap_semigroup_t S       = cong_obj_get_range_obj(o);
    size_t          lhs_pos = INT_INTOBJ(EN_SEMI_POSITION(0L, S, rep1));
    size_t          rhs_pos = INT_INTOBJ(EN_SEMI_POSITION(0L, S, rep2));

    if (cong_obj_get_range_type(o) != UNKNOWN) {
      FroidurePin<Element const*>* range = cong_obj_get_range(o);

      range->factorisation(lhs, lhs_pos - 1);
      range->factorisation(rhs, rhs_pos - 1);
    } else {
      gap_rec_t  data  = fropin(S, INTOBJ_INT(-1), 0, False);
      gap_list_t words = ElmPRec(data, RNam_words);

      lhs = plist_to_word_type(ELM_PLIST(words, lhs_pos));
      rhs = plist_to_word_type(ELM_PLIST(words, rhs_pos));
    }
  }

  Congruence* cong = cong_obj_get_cpp(o);
  return (cong->less(lhs, rhs) ? True : False);
}

Obj CONG_PAIRS_LOOKUP_PART(Obj self, gap_cong_t o) {
  initRNams();
  // TODO(JDM) assert o is correct type of object
  if (IsbPRec(o, RNam_fin_cong_lookup)) {
    SEMIGROUPS_ASSERT(IsbPRec(o, RNam_fin_cong_partition));
    return True;
  }

  Congruence*     cong      = cong_obj_get_cpp(o);
  gap_semigroup_t range_obj = cong_obj_get_range_obj(o);
  bool            report    = semi_obj_get_report(range_obj);

  Obj partition = NEW_PLIST_IMM(T_PLIST_TAB, cong->nr_classes());
  SET_LEN_PLIST(partition, cong->nr_classes());

  // Map a class index to a new index in the range [1..nrclasses]
  std::unordered_map<size_t, size_t> class_dictionary;
  size_t                             next_unused_class_index = 1;

  for (size_t i = 0; i < cong->nr_classes(); i++) {
    Obj next = NEW_PLIST_IMM(T_PLIST_CYC, 0);
    SET_LEN_PLIST(next, 0);  // should never be 0 later on, so this should be ok
    SET_ELM_PLIST(partition, i + 1, next);
    CHANGED_BAG(partition);
  }

  Obj lookup;

  if (cong_obj_get_range_type(o) != UNKNOWN) {
    FroidurePin<Element const*>* range = cong_obj_get_range(o);
    auto                         rg    = ReportGuard(report);

    lookup = NEW_PLIST_IMM(T_PLIST_CYC, range->size());
    SET_LEN_PLIST(lookup, range->size());

    word_type word;
    for (size_t i = 0; i < range->size(); i++) {
      range->factorisation(word, i);  // changes word in place
      size_t class_index = cong->word_to_class_index(word);

      auto it = class_dictionary.find(class_index);
      if (it == class_dictionary.end()) {
        class_dictionary.emplace(class_index, next_unused_class_index);
        class_index = next_unused_class_index++;
      } else {
        class_index = it->second;
      }

      SET_ELM_PLIST(lookup, i + 1, INTOBJ_INT(class_index));

      Obj c = ELM_PLIST(partition, class_index);
      AssPlist(c, LEN_PLIST(c) + 1, INTOBJ_INT(i + 1));

      word.clear();
    }
  } else {
    gap_rec_t data
        = fropin(cong_obj_get_range_obj(o), INTOBJ_INT(-1), 0, False);
    Obj words = ElmPRec(data, RNam_words);

    lookup = NEW_PLIST_IMM(T_PLIST_CYC, LEN_PLIST(words));
    SET_LEN_PLIST(lookup, LEN_PLIST(words));

    for (size_t i = 1; i <= (size_t) LEN_PLIST(words); i++) {
      size_t class_index
          = cong->word_to_class_index(plist_to_word_type(ELM_PLIST(words, i)));

      auto it = class_dictionary.find(class_index);
      if (it == class_dictionary.end()) {
        class_dictionary.emplace(class_index, next_unused_class_index);
        class_index = next_unused_class_index++;
      } else {
        class_index = it->second;
      }

      SET_ELM_PLIST(lookup, i, INTOBJ_INT(class_index));

      Obj c = ELM_PLIST(partition, class_index);
      AssPlist(c, LEN_PLIST(c) + 1, INTOBJ_INT(i));
    }
  }
  AssPRec(o, RNam_fin_cong_partition, partition);
  AssPRec(o, RNam_fin_cong_lookup, lookup);
  return True;
}

Obj CONG_PAIRS_ELM_COSET_ID(Obj self, gap_cong_t cong_obj, Obj elm) {
  initRNams();
  // TODO(JDM) assert args are correct types
  gap_semigroup_t range_obj = cong_obj_get_range_obj(cong_obj);
  bool            report    = semi_obj_get_report(range_obj);

  if (IsbPRec(cong_obj, RNam_fin_cong_lookup)) {
    SEMIGROUPS_ASSERT(!cong_obj_is_fp_cong(cong_obj));
    // TODO(JDM) Use FindPRec and GET_ELM_PREC
    Obj lookup = ElmPRec(cong_obj, RNam_fin_cong_lookup);
    return ELM_PLIST(lookup,
                     INT_INTOBJ(EN_SEMI_POSITION(self, range_obj, elm)));
  } else if (cong_obj_is_fp_cong(cong_obj)) {
    // elm should be a gap_list_t representing a word
    Congruence* cong = cong_obj_get_cpp(cong_obj);
    return INTOBJ_INT(cong->word_to_class_index(plist_to_word_type(elm)) + 1);
  } else if (cong_obj_get_range_type(cong_obj) != UNKNOWN) {
    Congruence*                  cong  = cong_obj_get_cpp(cong_obj);
    FroidurePin<Element const*>* range = cong_obj_get_range(cong_obj);
    auto                         rg    = ReportGuard(report);
    word_type                    word;
    range->factorisation(
        word, INT_INTOBJ(EN_SEMI_POSITION(self, range_obj, elm)) - 1);
    return INTOBJ_INT(cong->word_to_class_index(word) + 1);
  } else {
    gap_rec_t   data = fropin(range_obj, INTOBJ_INT(-1), 0, False);
    Congruence* cong = cong_obj_get_cpp(cong_obj);

    Obj word = ELM_PLIST(ElmPRec(data, RNam_words),
                         INT_INTOBJ(EN_SEMI_POSITION(self, range_obj, elm)));

    return INTOBJ_INT(cong->word_to_class_index(plist_to_word_type(word)) + 1);
  }
}

gap_list_t CONG_PAIRS_NONTRIVIAL_CLASSES(Obj self, gap_cong_t o) {
  initRNams();
  Congruence* cong = cong_obj_get_cpp(o);

  // Initialise gap_lists
  gap_list_t gap_lists
      = NEW_PLIST_IMM(T_PLIST_TAB, cong->nr_non_trivial_classes());
  SET_LEN_PLIST(gap_lists, cong->nr_non_trivial_classes());

  // Convert the words to plists
  for (auto it1 = cong->cbegin_ntc(); it1 < cong->cend_ntc(); ++it1) {
    gap_list_t next_class = NEW_PLIST_IMM(T_PLIST_TAB, it1->size());
    SET_LEN_PLIST(next_class, it1->size());
    size_t pos = 0;
    for (auto it2 = it1->cbegin(); it2 < it1->cend(); ++it2) {
      SET_ELM_PLIST(next_class, ++pos, word_type_to_plist(&(*it2)));
      CHANGED_BAG(next_class);
    }
    SET_ELM_PLIST(gap_lists, (it1 - cong->cbegin_ntc()) + 1, next_class);
    CHANGED_BAG(gap_lists);
  }
  return gap_lists;
}
