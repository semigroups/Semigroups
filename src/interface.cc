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

#include <assert.h>

#include <string>
#include <utility>

#include "src/compiled.h"

#include "converter.h"
#include "data.h"
#include "fropin.h"
#include "gap.h"
#include "interface.h"
#include "semigrp.h"

#include "semigroupsplusplus/semigroups.h"

// Helper functions

template <typename T>
static inline void really_delete_cont(T* cont) {
  for (Element* x : *cont) {
    x->really_delete();
  }
  delete cont;
}

/*******************************************************************************
 * ConvertElements:
 ******************************************************************************/

std::vector<Element*>*
ConvertElements(Converter* converter, Obj elements, size_t degree) {
  assert(IS_LIST(elements));

  auto out = new std::vector<Element*>();

  for (size_t i = 0; i < (size_t) LEN_LIST(elements); i++) {
    out->push_back(converter->convert(ELM_LIST(elements, i + 1), degree));
  }
  return out;
}

Obj UnconvertElements(Converter* converter, std::vector<Element*>* elements) {

  if (elements->empty()) {
    Obj out = NEW_PLIST(T_PLIST_EMPTY, 0);
    SET_LEN_PLIST(out, 0);
    return out;
  }

  Obj out = NEW_PLIST(T_PLIST, elements->size());
  SET_LEN_PLIST(out, elements->size());

  for (size_t i = 0; i < elements->size(); i++) {
    SET_ELM_PLIST(out, i + 1, converter->unconvert(elements->at(i)));
    CHANGED_BAG(out);
  }
  return out;
}

/*******************************************************************************
 * ConvertFromCayleyGraph: helper function to convert a cayley_graph_t to a GAP
 * plist of GAP plists
 ******************************************************************************/

Obj ConvertFromCayleyGraph(cayley_graph_t* graph) {
  assert(graph->size() != 0);
  Obj out = NEW_PLIST(T_PLIST, graph->nr_rows());
  SET_LEN_PLIST(out, graph->nr_rows());

  for (size_t i = 0; i < graph->nr_rows(); i++) {
    Obj next = NEW_PLIST(T_PLIST_CYC, graph->nr_cols());
    SET_LEN_PLIST(next, graph->nr_cols());
    for (size_t j = 0; j < graph->nr_cols(); j++) { // TODO reinstate this
      SET_ELM_PLIST(next, j + 1, INTOBJ_INT(graph->get(i, j) + 1));
    }
    SET_ELM_PLIST(out, i + 1, next);
    CHANGED_BAG(out);
  }
  return out;
}

// GAP level functions


/*******************************************************************************
 * SEMIGROUP_CURRENT_NR_RULES:
 ******************************************************************************/

Obj SEMIGROUP_CURRENT_NR_RULES(Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    return INTOBJ_INT(data_semigroup(data)->current_nrrules());
  }
  return INTOBJ_INT(ElmPRec(data, RNamName("nrrules")));
}

/*******************************************************************************
 * SEMIGROUP_CURRENT_SIZE:
 ******************************************************************************/

Obj SEMIGROUP_CURRENT_SIZE(Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    return INTOBJ_INT(data_semigroup(data)->current_size());
  }

  initRNams();
  return INTOBJ_INT(LEN_PLIST(ElmPRec(data, RNam_elts)));
}

Obj SEMIGROUP_AS_SET(Obj self, Obj data) {
  // TODO make this faster by running through _pos_sorted so that we run through
  // semigroup->_elements in order, and fill in out (below) out of order
  if (data_type(data) == UNKNOWN) {
    ErrorQuit("SEMIGROUP_AS_SET: this shouldn't happen!", 0L, 0L);
    return 0L;
  }

  std::vector<std::pair<Element*, size_t>>* pairs =
      data_semigroup(data)->sorted_elements(rec_get_report(data));
  Converter* converter = data_converter(data);

  Obj out = NEW_PLIST(T_PLIST, pairs->size());
  SET_LEN_PLIST(out, pairs->size());

  size_t i = 1;
  for (auto x : *pairs) {
    SET_ELM_PLIST(out, i++, converter->unconvert(x.first));
    CHANGED_BAG(out);
  }
  return out;
}

Obj SEMIGROUP_POSITION_SORTED(Obj self, Obj data, Obj x) {

  // use the element cached in the data record if known
  if (data_type(data) == UNKNOWN) {
    ErrorQuit("SEMIGROUP_POSITION_SORTED: this shouldn't happen!", 0L, 0L);
    return 0L;
  } else {
    size_t     deg       = data_degree(data);
    Semigroup* semigroup = data_semigroup(data);
    Converter* converter = data_converter(data);
    Element* xx(converter->convert(x, deg));
    size_t     pos = semigroup->position_sorted(xx, rec_get_report(data));
    delete xx;
    return (pos == Semigroup::UNDEFINED ? Fail : INTOBJ_INT(pos + 1));
  }
}

/*******************************************************************************
 * SEMIGROUP_IS_DONE:
 ******************************************************************************/

Obj SEMIGROUP_IS_DONE(Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    return (data_semigroup(data)->is_done() ? True : False);
  }

  size_t pos = INT_INTOBJ(ElmPRec(data, RNamName("pos")));
  size_t nr  = INT_INTOBJ(ElmPRec(data, RNamName("nr")));
  return (pos > nr ? True : False);
}

