/*
 * Semigroups GAP package
 *
 * This file contains some methods for bipartitions
 *
 */

#include "src/bipart.h"
#include "src/semigroups++/elements.h"
#include <vector>

// Create a bipartition
// This is just a wrapper for the C++ objects.
//
// @return a GAP Obj..

Obj BIPART_NC (Obj gap_blocks) {

  assert(IS_LIST(gap_blocks));
  assert(LEN_LIST(gap_blocks) > 0);

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();

  size_t degree         = 0;
  size_t nr_left_blocks = 0;
  size_t nr_blocks;
  bool   by_ext_rep;

  if (IS_LIST(ELM_LIST(gap_blocks, 1))) { // gap_blocks is a list of lists
    by_ext_rep = true;
    nr_blocks = LEN_LIST(gap_blocks);
    for (size_t i = 1; i <= nr_blocks; i++) {
      assert(IS_LIST(ELM_LIST(gap_blocks, i)));
      degree += LEN_LIST(ELM_LIST(gap_blocks, i));
    }
    blocks->resize(degree);

    degree /= 2;

    for (size_t i = 1; i <= nr_blocks; i++) {
      Obj block = ELM_LIST(gap_blocks, i);
      for (size_t j = 1; j <= (size_t) LEN_LIST(block); j++) {
        assert(IS_INTOBJ(ELM_LIST(block, j)));
        int jj = INT_INTOBJ(ELM_LIST(block, j));
        if (jj < 0) {
          (*blocks)[- jj + degree - 1] = i - 1;
        } else {
          nr_left_blocks = i;
          (*blocks)[jj - 1] = i - 1;
        }
      }
    }
  } else { // gap_blocks is the internal rep of a bipartition
    by_ext_rep = false;
    blocks->resize(LEN_LIST(gap_blocks));
    for (size_t i = 1; i <= (size_t) LEN_LIST(gap_blocks) / 2; i++) {
      assert(IS_INTOBJ(ELM_LIST(gap_blocks, i)) &&
             INT_INTOBJ(ELM_LIST(gap_blocks, i)) > 0);
      size_t index = INT_INTOBJ(ELM_LIST(gap_blocks, i));
      blocks->push_back(index);
      nr_blocks = (index > nr_blocks ? index : nr_blocks);
    }
    nr_left_blocks = nr_blocks;
    for (size_t i = ((size_t) LEN_LIST(gap_blocks) / 2) + 1;
         i <= (size_t) LEN_LIST(gap_blocks); i++) {
      assert(IS_INTOBJ(ELM_LIST(gap_blocks, i)) &&
             INT_INTOBJ(ELM_LIST(gap_blocks, i)) > 0);
      size_t index = INT_INTOBJ(ELM_LIST(gap_blocks, i));
      blocks->push_back(index);
      nr_blocks = (index > nr_blocks ? index : nr_blocks);
    }

  }

  // construct C++ object
  Bipartition* x = new Bipartition(blocks);
  x->set_nr_left_blocks(nr_left_blocks);
  x->set_nr_blocks(nr_blocks);

  // construct GAP wrapper for C++ object
  Obj out = NewSemigroupsBag(x, GAP_BIPART, 4);
  if (by_ext_rep) {
    ADDR_OBJ(out)[2] = gap_blocks; //copy blocks here? FIXME
  } else {
    ADDR_OBJ(out)[3] = gap_blocks; //copy blocks here? FIXME
  }

  return out;
}
