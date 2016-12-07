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

// This file defines UFData, a class used to make an equivalence relation on
// the integers {1 .. n}, using the UNION-FIND METHOD: new pairs can be added
// and the appropriate classes combined quickly.

#include "ufdata.h"
#include <assert.h>

#include "gap.h"
#include "src/compiled.h"

// GAP level functions

Obj UF_NEW(Obj self, Obj size) {
  assert(IS_INTOBJ(size) && INT_INTOBJ(size) > 0);
  return OBJ_CLASS(new UFData(INT_INTOBJ(size)), T_SEMI_SUBTYPE_UFDATA);
}

Obj UF_COPY(Obj self, Obj ufdata) {
  return OBJ_CLASS(new UFData(*CLASS_OBJ<UFData*>(ufdata)),
                   T_SEMI_SUBTYPE_UFDATA);
}

Obj UF_SIZE(Obj self, Obj ufdata) {
  return INTOBJ_INT(CLASS_OBJ<UFData*>(ufdata)->get_size());
}

Obj UF_FIND(Obj self, Obj ufdata, Obj i) {
  assert(IS_INTOBJ(i) && INT_INTOBJ(i) > 0);
  return INTOBJ_INT(CLASS_OBJ<UFData*>(ufdata)->find(INT_INTOBJ(i) - 1) + 1);
}

Obj UF_UNION(Obj self, Obj ufdata, Obj pair) {
  assert(IS_PLIST(pair) && LEN_PLIST(pair) == 2);
  assert(IS_INTOBJ(ELM_PLIST(pair, 1)) && INT_INTOBJ(ELM_PLIST(pair, 1)) > 0);
  assert(IS_INTOBJ(ELM_PLIST(pair, 2)) && INT_INTOBJ(ELM_PLIST(pair, 2)) > 0);
  CLASS_OBJ<UFData*>(ufdata)->unite(INT_INTOBJ(ELM_PLIST(pair, 1)) - 1,
                                   INT_INTOBJ(ELM_PLIST(pair, 2)) - 1);
  return 0L;
}

Obj UF_FLATTEN(Obj self, Obj ufdata) {
  CLASS_OBJ<UFData*>(ufdata)->flatten();
  return 0L;
}

Obj UF_TABLE(Obj self, Obj ufdata) {
  UFData::table_t* table     = CLASS_OBJ<UFData*>(ufdata)->get_table();
  size_t           size      = table->size();
  Obj              gap_table = NEW_PLIST(T_PLIST_CYC + IMMUTABLE, size);
  // IMMUTABLE since it should not be altered on the GAP level
  SET_LEN_PLIST(gap_table, size);
  for (size_t i = 0; i < size; i++) {
    SET_ELM_PLIST(gap_table, i + 1, INTOBJ_INT(table->at(i) + 1));
  }
  return gap_table;
}

Obj UF_BLOCKS(Obj self, Obj ufdata) {
  UFData::blocks_t const* blocks = CLASS_OBJ<UFData*>(ufdata)->get_blocks();
  size_t                  size   = blocks->size();
  size_t                  i, j;

  // Rewrite each block as a PLIST object, and put it into a PLIST.
  Obj gap_blocks = NEW_PLIST(T_PLIST, size);
  SET_LEN_PLIST(gap_blocks, size);
  for (i = 0; i < size; i++) {
    if ((*blocks)[i] != nullptr) { // nullptr represents a hole in the list
      Obj block = NEW_PLIST(T_PLIST_CYC, (*blocks)[i]->size());
      SET_LEN_PLIST(block, (*blocks)[i]->size());
      for (j = 0; j < (*blocks)[i]->size(); j++) {
        SET_ELM_PLIST(block, j + 1, INTOBJ_INT((*(*blocks)[i])[j] + 1));
      }
      SET_ELM_PLIST(gap_blocks, i + 1, block);
      CHANGED_BAG(gap_blocks);
    }
  }

  return gap_blocks;
}
