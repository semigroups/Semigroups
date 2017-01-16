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

// This file contains some functions for finitely presented semigroups and
// monoids.

#include "semifp.h"

#include <vector>

#include "gap.h"
#include "rnams.h"
#include "semigroupsplusplus/cong.h"
#include "semigrp.h"

using semigroupsplusplus::word_t;
using semigroupsplusplus::Congruence;
using semigroupsplusplus::relation_t;

static inline word_t plist_to_word_t(Obj plist) {
  word_t word;
  for (size_t i = 1; i <= (size_t) LEN_PLIST(plist); i++) {
    Obj j = ELM_PLIST(plist, i);
    assert(IS_INTOBJ(j));
    word.push_back(INT_INTOBJ(j) - 1);
  }
  return word;
}

static inline word_t ext_rep_obj_to_word_t(Obj ext_rep_obj) {
  word_t word;
  for (size_t i = 1; i < (size_t) LEN_PLIST(ext_rep_obj); i += 2) {
    size_t val = INT_INTOBJ(ELM_PLIST(ext_rep_obj, i)) - 1;
    size_t pow = INT_INTOBJ(ELM_PLIST(ext_rep_obj, i + 1));
    for (size_t j = 0; j < pow; j++) {
      word.push_back(val);
    }
  }
  return word;
}

static inline bool fp_semi_has_cpp_cong(Obj S) {
  initRNams();
  return IsbPRec(S, RNam_fp_semi_cong)
         && CLASS_OBJ<Congruence*>(ElmPRec(S, RNam_fp_semi_cong)) != nullptr;
}

static Congruence* fp_semi_get_cpp_cong(Obj S) {
  initRNams();
  assert(IsbPRec(S, RNam_fp_semi_rels));

  if (!fp_semi_has_cpp_cong(S)) {
    std::vector<relation_t> rels;
    Obj                     gap_rels = ElmPRec(S, RNam_fp_semi_rels);
    for (size_t i = 1; i <= (size_t) LEN_PLIST(gap_rels); i++) {
      word_t lhs = plist_to_word_t(ELM_PLIST(ELM_PLIST(gap_rels, i), 1));
      word_t rhs = plist_to_word_t(ELM_PLIST(ELM_PLIST(gap_rels, i), 2));
      rels.push_back(make_pair(lhs, rhs));
    }

    Congruence* cong =
        new Congruence("twosided",
                       INT_INTOBJ(ElmPRec(S, RNam_fp_semi_nrgens)),
                       rels,
                       std::vector<relation_t>());
    AssPRec(S, RNam_fp_semi_cong, OBJ_CLASS(cong, T_SEMI_SUBTYPE_CONG));
  }

  return CLASS_OBJ<Congruence*>(ElmPRec(S, RNam_fp_semi_cong));
}

Obj FP_SEMI_SIZE(Obj self, gap_semigroup_t S) {
  // TODO(JDM) assert S is correct types of args
  Congruence* cong = fp_semi_get_cpp_cong(S);
  cong->set_report(semi_obj_get_report(S));
  return INTOBJ_INT(cong->nr_classes());
}

Obj FP_SEMI_EQ(Obj self, Obj S, Obj x, Obj y) {
  // TODO(JDM) assert S, x, y are correct types of args
  Congruence* cong = fp_semi_get_cpp_cong(S);
  cong->set_report(semi_obj_get_report(S));

  word_t lhs = ext_rep_obj_to_word_t(x);
  word_t rhs = ext_rep_obj_to_word_t(y);

  return ((cong->word_to_class_index(lhs) == cong->word_to_class_index(rhs))
              ? True
              : False);
}

Obj FP_SEMI_COSET_ID(Obj self, Obj S, Obj x) {
  // TODO(JDM) assert S, x are correct types of args
  Congruence* cong = fp_semi_get_cpp_cong(S);
  cong->set_report(semi_obj_get_report(S));
  return INTOBJ_INT(cong->word_to_class_index(ext_rep_obj_to_word_t(x)));
}
