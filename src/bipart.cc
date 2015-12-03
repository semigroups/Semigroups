/*
 * Semigroups GAP package
 *
 * This file contains some methods for bipartitions
 *
 */

#include "src/bipart.h"
#include "src/semigroups++/elements.h"
#include "src/permutat.h"
#include "src/precord.h"
#include <vector>

// Global variables

static std::vector<size_t> _BUFFER;
static Int                 _RNam_wrapper = RNamName("wrapper");

// Helper functions

inline Obj NEW_GAP_BIPART (Bipartition* x) {

  // construct GAP wrapper for C++ object
  Obj wrapper = NewSemigroupsBag(x, GAP_BIPART, 4);

  // put the GAP wrapper in a list and Objectify
  Obj out = NEW_PREC(1);
  AssPRec(out, _RNam_wrapper, wrapper);
  TYPE_COMOBJ(out) = BipartitionType;
  RetypeBag(out, T_COMOBJ);
  CHANGED_BAG(out);

  return out;
}

inline Obj GET_WRAPPER (Obj x) {
  //TODO check that x is a bipartition and that _RNam_wrapper is set
  return ElmPRec(x, _RNam_wrapper);
}

inline Obj GET_ELM_WRAPPER (Obj x, size_t pos) {
  //TODO check that x is a bipartition and that pos is not bigger than it
  // should be
  Obj wrapper = GET_WRAPPER(x);
  return ELM_PLIST(wrapper, pos);
}

inline void SET_ELM_WRAPPER (Obj x, size_t pos, Obj val) {
  //TODO check that x is a bipartition and that pos is not bigger than it
  // should be
  Obj wrapper = GET_WRAPPER(x);
  SET_ELM_PLIST(wrapper, pos, val);
  CHANGED_BAG(wrapper);
  CHANGED_BAG(x);
}

inline void SET_BIPART_EXT_REP (Obj x, Obj blocks) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  SET_ELM_WRAPPER(x, 2, blocks);
}

inline Obj GET_BIPART_EXT_REP (Obj x) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  return GET_ELM_WRAPPER(x, 2);
}

inline void SET_BIPART_INT_REP (Obj x, Obj blocks) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  SET_ELM_WRAPPER(x, 3, blocks);
}

inline Obj GET_BIPART_INT_REP (Obj x) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  return GET_ELM_WRAPPER(x, 3);
}

inline Bipartition* GET_CPP_BIPART (Obj x) {
  //TODO check that x is a bipartition

  return CLASS_OBJ<Bipartition>(GET_WRAPPER(x));
}

// GAP kernel functions

// Create a bipartition
// This is just a wrapper for the C++ objects.
//
// @return a GAP Obj..

Obj BIPART_NC (Obj self, Obj gap_blocks) {

  assert(IS_LIST(gap_blocks));
  assert(LEN_LIST(gap_blocks) > 0);

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();

  size_t degree         = 0;
  size_t nr_left_blocks = 0;
  size_t nr_blocks      = 0;
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
    blocks->reserve(LEN_LIST(gap_blocks));
    for (size_t i = 1; i <= (size_t) LEN_LIST(gap_blocks) / 2; i++) {
      assert(IS_INTOBJ(ELM_LIST(gap_blocks, i)) &&
             INT_INTOBJ(ELM_LIST(gap_blocks, i)) > 0);
      size_t index = INT_INTOBJ(ELM_LIST(gap_blocks, i)) - 1;
      blocks->push_back(index);
      nr_blocks = (index > nr_blocks ? index : nr_blocks);
    }
    nr_left_blocks = nr_blocks;
    for (size_t i = ((size_t) LEN_LIST(gap_blocks) / 2) + 1;
         i <= (size_t) LEN_LIST(gap_blocks); i++) {
      assert(IS_INTOBJ(ELM_LIST(gap_blocks, i)) &&
             INT_INTOBJ(ELM_LIST(gap_blocks, i)) > 0);
      size_t index = INT_INTOBJ(ELM_LIST(gap_blocks, i)) - 1;
      blocks->push_back(index);
      nr_blocks = (index > nr_blocks ? index : nr_blocks);
    }
  }

  // construct C++ object
  Bipartition* x = new Bipartition(blocks);
  x->set_nr_left_blocks(nr_left_blocks + 1);
  x->set_nr_blocks(nr_blocks + 1);

  Obj out = NEW_GAP_BIPART(x);

  if (by_ext_rep) {
    SET_BIPART_EXT_REP(out, gap_blocks); //TODO copy gap_blocks?
  } else {
    SET_BIPART_INT_REP(out, gap_blocks); //TODO copy gap_blocks?
  }

  return out;
}

Obj BIPART_EXT_REP (Obj self, Obj x) {

  if (GET_BIPART_EXT_REP(x) == NULL) {
    Bipartition* xx = GET_CPP_BIPART(x);
    size_t n = xx->degree();

    Obj ext_rep = NEW_PLIST(T_PLIST_HOM, xx->nr_blocks());
    SET_LEN_PLIST(ext_rep, (Int) xx->nr_blocks());

    for (size_t i = 0; i < 2 * n; i++) {
      Obj block;
      Obj entry = INTOBJ_INT((i < n ? i + 1 : -(i - n) - 1));
      if (ELM_PLIST(ext_rep, xx->block(i) + 1) == 0) {
        block = NEW_PLIST(T_PLIST_CYC, 1);
        SET_LEN_PLIST(block, 1);
        SET_ELM_PLIST(block, 1, entry);
        SET_ELM_PLIST(ext_rep, xx->block(i) + 1, block);
        CHANGED_BAG(ext_rep);
      } else {
        block = ELM_PLIST(ext_rep, xx->block(i) + 1);
        AssPlist(block, LEN_PLIST(block) + 1, entry);
      }
    }
    SET_BIPART_EXT_REP(x, ext_rep);
  }

  return GET_BIPART_EXT_REP(x);
}

Obj BIPART_INT_REP (Obj self, Obj x) {

  if (GET_BIPART_INT_REP(x) == NULL) {
    Bipartition* xx = GET_CPP_BIPART(x); // get C++ bipartition
    size_t n = xx->degree();

    Obj int_rep = NEW_PLIST(T_PLIST_CYC, 2 * xx->degree());
    SET_LEN_PLIST(int_rep, (Int) 2 * xx->nr_blocks());

    for (size_t i = 0; i < 2 * n; i++) {
      SET_ELM_PLIST(int_rep, i + 1, INTOBJ_INT(xx->block(i) + 1));
    }
    SET_BIPART_INT_REP(x, int_rep);
  }

  return GET_BIPART_INT_REP(x);
}

Obj BIPART_DEGREE (Obj self, Obj x) {
  return INTOBJ_INT(GET_CPP_BIPART(x)->degree());
}

Obj BIPART_NR_BLOCKS (Obj self, Obj x) {
  return INTOBJ_INT(GET_CPP_BIPART(x)->nr_blocks());
}

Obj BIPART_NR_LEFT_BLOCKS (Obj self, Obj x) {
  return INTOBJ_INT(GET_CPP_BIPART(x)->nr_left_blocks());
}

Obj BIPART_RANK (Obj self, Obj x) {
  return INTOBJ_INT(GET_CPP_BIPART(x)->rank());
}

Obj BIPART_PROD (Obj self, Obj x, Obj y) {

  Bipartition* xx = GET_CPP_BIPART(x);
  Bipartition* yy = GET_CPP_BIPART(y);

  Bipartition* z = new Bipartition(xx->degree());
  z->redefine(xx, yy);

  return NEW_GAP_BIPART(z);
}

Obj BIPART_EQ (Obj self, Obj x, Obj y) {
  return (GET_CPP_BIPART(x)->equals(GET_CPP_BIPART(y)) ? True : False);
}

Obj BIPART_LT (Obj self, Obj x, Obj y) {
  return (GET_CPP_BIPART(x) < GET_CPP_BIPART(y) ? True : False);
}

// TODO this should go into semigroups++

Obj BIPART_PERM_LEFT_QUO (Obj self, Obj x, Obj y) {
  Bipartition* xx = GET_CPP_BIPART(x);
  Bipartition* yy = GET_CPP_BIPART(y);

  size_t deg  = xx->degree();
  Obj p       = NEW_PERM4(deg);
  UInt4* ptrp = ADDR_PERM4(p);

  // find indices of right blocks of <x>
  size_t  index = 0;
  _BUFFER.resize(2 * deg, -1);

  for (size_t i = deg; i < 2 * deg; i++) {
    if (_BUFFER[xx->block(i)] == (UInt4) -1) {
      _BUFFER[xx->block(i)] = index;
      index++;
    }
    ptrp[i - deg] = i - deg;
  }

  for (size_t i = deg; i < 2 * deg; i++) {
    if (yy->block(i) < xx->nr_left_blocks()) {
      ptrp[_BUFFER[yy->block(i)]] = _BUFFER[xx->block(i)];
    }
  }
  return p;
}

Obj BIPART_LEFT_PROJ (Obj self, Obj x) {

  Bipartition* xx = GET_CPP_BIPART(x);

  size_t deg  = xx->degree();
  size_t next = xx->nr_left_blocks();
  std::vector<bool> const& lookup(xx->trans_blocks_lookup());

  _BUFFER.resize(2 * deg, -1);

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->resize(2 * deg, -1);

  for (size_t i = 0; i < deg; i++) {
    (*blocks)[i] = xx->block(i);
    if (lookup[xx->block(i)]) {
      (*blocks)[i + deg] = xx->block(i);
    } else if (_BUFFER[xx->block(i)] != (size_t) -1) {
      (*blocks)[i + deg] = _BUFFER[xx->block(i)];
    } else {
      _BUFFER[xx->block(i)] = next;
      (*blocks)[i + deg] = next;
      next++;
    }
  }

  Bipartition* out = new Bipartition(blocks);
  out->set_nr_blocks(next);
  return NEW_GAP_BIPART(out);
}

Obj BIPART_STAR (Obj self, Obj x) {

  Bipartition* xx = GET_CPP_BIPART(x);
  size_t deg  = xx->degree();

  _BUFFER.resize(2 * deg, -1);

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->resize(2 * deg, -1);

  size_t next = 0;

  for (size_t i = 0; i < deg; i++) {
    if (_BUFFER[xx->block(i + deg)] != (size_t) -1) {
      (*blocks)[i] = _BUFFER[xx->block(i + deg)];
    } else {
      _BUFFER[xx->block(i + deg)] = next;
      (*blocks)[i] = next;
      next++;
    }
  }

  size_t nr_left = next;

  for (size_t i = 0; i < deg; i++) {
    if (_BUFFER[xx->block(i)] != (size_t) -1) {
      (*blocks)[i + deg] = _BUFFER[xx->block(i)];
    } else {
      _BUFFER[xx->block(i)] = next;
      (*blocks)[i + deg] = next;
      next++;
    }
  }

  Bipartition* out = new Bipartition(blocks);
  out->set_nr_blocks(next);
  out->set_nr_left_blocks(nr_left);

  return NEW_GAP_BIPART(out);
}
