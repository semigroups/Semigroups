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

// This file uses UF, a class in libsemigroups used to make an equivalence
// relation on the integers {1 .. n}, using the UNION-FIND METHOD: new pairs can
// be added and the appropriate classes combined quickly.

#include "uf.h"

#include "pkg.h"
#include "semigroups-debug.h"
#include "src/compiled.h"

#include "libsemigroups/src/uf.h"

using libsemigroups::UF;

// GAP level functions

Obj UF_NEW(Obj self, Obj size) {
  SEMIGROUPS_ASSERT(IS_INTOBJ(size) && INT_INTOBJ(size) > 0);
  return OBJ_CLASS(new UF(INT_INTOBJ(size)), T_SEMI_SUBTYPE_UF);
}

Obj UF_COPY(Obj self, Obj uf) {
  return OBJ_CLASS(new UF(*CLASS_OBJ<UF*>(uf)), T_SEMI_SUBTYPE_UF);
}

Obj UF_SIZE(Obj self, Obj uf) {
  return INTOBJ_INT(CLASS_OBJ<UF*>(uf)->get_size());
}

Obj UF_FIND(Obj self, Obj uf, Obj i) {
  SEMIGROUPS_ASSERT(IS_INTOBJ(i) && INT_INTOBJ(i) > 0);
  return INTOBJ_INT(CLASS_OBJ<UF*>(uf)->find(INT_INTOBJ(i) - 1) + 1);
}

Obj UF_UNION(Obj self, Obj uf, Obj pair) {
  SEMIGROUPS_ASSERT(IS_PLIST(pair) && LEN_PLIST(pair) == 2);
  SEMIGROUPS_ASSERT(IS_INTOBJ(ELM_PLIST(pair, 1))
                    && INT_INTOBJ(ELM_PLIST(pair, 1)) > 0);
  SEMIGROUPS_ASSERT(IS_INTOBJ(ELM_PLIST(pair, 2))
                    && INT_INTOBJ(ELM_PLIST(pair, 2)) > 0);
  CLASS_OBJ<UF*>(uf)->unite(INT_INTOBJ(ELM_PLIST(pair, 1)) - 1,
                            INT_INTOBJ(ELM_PLIST(pair, 2)) - 1);
  return 0L;
}

Obj UF_FLATTEN(Obj self, Obj uf) {
  CLASS_OBJ<UF*>(uf)->flatten();
  return 0L;
}

Obj UF_TABLE(Obj self, Obj uf) {
  UF::table_t* table     = CLASS_OBJ<UF*>(uf)->get_table();
  size_t       size      = table->size();
  Obj          gap_table = NEW_PLIST_IMM(T_PLIST_CYC, size);
  // IMMUTABLE since it should not be altered on the GAP level
  SET_LEN_PLIST(gap_table, size);
  for (size_t i = 0; i < size; i++) {
    SET_ELM_PLIST(gap_table, i + 1, INTOBJ_INT(table->at(i) + 1));
  }
  return gap_table;
}

Obj UF_BLOCKS(Obj self, Obj uf) {
  UF::blocks_t const* blocks = CLASS_OBJ<UF*>(uf)->get_blocks();
  size_t              size   = blocks->size();
  size_t              i, j;

  // Rewrite each block as a PLIST object, and put it into a PLIST.
  Obj gap_blocks = NEW_PLIST(T_PLIST, 0);
  SET_LEN_PLIST(gap_blocks, 0);
  for (i = 0; i < size; i++) {
    if ((*blocks)[i] != nullptr) {  // nullptr represents a hole in the list
      Obj block = NEW_PLIST(T_PLIST_CYC, (*blocks)[i]->size());
      SET_LEN_PLIST(block, (*blocks)[i]->size());
      for (j = 0; j < (*blocks)[i]->size(); j++) {
        SET_ELM_PLIST(block, j + 1, INTOBJ_INT((*(*blocks)[i])[j] + 1));
      }
      AssPlist(gap_blocks, i + 1, block);
    }
  }
  if (LEN_PLIST(gap_blocks) == 0) {
    RetypeBag(gap_blocks, T_PLIST_EMPTY);
  }
  return gap_blocks;
}

gap_int_t UF_NR_BLOCKS(Obj self, Obj uf) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(uf) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(uf) == T_SEMI_SUBTYPE_UF);
  return INTOBJ_INT(CLASS_OBJ<UF*>(uf)->nr_blocks());
}

gap_list_t UF_BLOCK_REPS(Obj self, Obj uf) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(uf) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(uf) == T_SEMI_SUBTYPE_UF);
  UF* uf_cpp = CLASS_OBJ<UF*>(uf);
  uf_cpp->reset_next_rep();
  size_t next_rep = uf_cpp->next_rep();

  gap_list_t out = NEW_PLIST(T_PLIST_CYC, 0);
  SET_LEN_PLIST(out, 0);
  size_t nr = 0;

  while (next_rep < uf_cpp->get_size()) {
    AssPlist(out, ++nr, INTOBJ_INT(next_rep + 1));
    next_rep = uf_cpp->next_rep();
  }
  return out;
}

Obj UF_JOIN(Obj self, Obj uf1, Obj uf2) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(uf1) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(uf1) == T_SEMI_SUBTYPE_UF);
  SEMIGROUPS_ASSERT(TNUM_OBJ(uf2) == T_SEMI
                    && SUBTYPE_OF_T_SEMI(uf2) == T_SEMI_SUBTYPE_UF);
  SEMIGROUPS_ASSERT(CLASS_OBJ<UF*>(uf1)->get_size()
                    == CLASS_OBJ<UF*>(uf2)->get_size());
  Obj uf_join = UF_COPY(self, uf1);
  CLASS_OBJ<UF*>(uf_join)->join(CLASS_OBJ<UF*>(uf2));
  return uf_join;
}
