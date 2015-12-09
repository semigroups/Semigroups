/*
 * Semigroups GAP package
 *
 * This file contains some methods for bipartitions
 *
 */

//TODO if we use clear before resize maybe don't need fill

#include "src/bipart.h"
#include "src/semigroups++/elements.h"
#include "src/permutat.h"
#include "src/precord.h"
#include <vector>


// Global variables

static std::vector<size_t> _BUFFER_size_t;
static std::vector<bool>   _BUFFER_bool;
static Int                 _RNam_wrapper   = RNamName("wrapper");
static Int                 _RNam_blocks    = RNamName("blocks");
static Int                 _RNam_degree    = RNamName("degree");
static Int                 _RNam_blist     = RNamName("blist");
static Int                 _RNam_nr_blocks = RNamName("nr_blocks");


inline Obj wrapper_get (Obj x) {
  //TODO check that x is a bipartition or blocks and that _RNam_wrapper is set
  return ElmPRec(x, _RNam_wrapper);
}

inline Obj wrapper_get_elm (Obj x, size_t pos) {
  //TODO check that x is a bipartition or blocks and that pos is not bigger than it
  // should be
  Obj wrapper = wrapper_get(x);
  return ELM_PLIST(wrapper, pos);
}

inline void wrapper_set_elm (Obj x, size_t pos, Obj val) {
  //TODO check that x is a bipartition or blocks and that pos is not bigger than it
  // should be
  Obj wrapper = wrapper_get(x);
  SET_ELM_PLIST(wrapper, pos, val);
  CHANGED_BAG(wrapper);
  CHANGED_BAG(x);
}

// Helper functions

inline Obj bipart_new (Bipartition* x) {

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

inline Bipartition* bipart_get_cpp (Obj x) {
  //TODO check that x is a bipartition
  return CLASS_OBJ<Bipartition>(wrapper_get(x));
}

inline void bipart_set_ext_rep (Obj x, Obj blocks) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  wrapper_set_elm(x, 2, blocks);
}

inline Obj bipart_get_ext_rep (Obj x) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  return wrapper_get_elm(x, 2);
}

inline void bipart_set_int_rep (Obj x, Obj blocks) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  wrapper_set_elm(x, 3, blocks);
}

inline Obj bipart_get_int_rep (Obj x) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  return wrapper_get_elm(x, 3);
}

inline Obj blocks_new (Blocks* x) {

  // construct GAP wrapper for C++ object
  Obj wrapper = NewSemigroupsBag(x, GAP_BLOCKS, 3);

  // put the GAP wrapper in a list and Objectify
  Obj out = NEW_PREC(1);
  AssPRec(out, _RNam_wrapper, wrapper);
  TYPE_COMOBJ(out) = BlocksType;
  RetypeBag(out, T_COMOBJ);
  CHANGED_BAG(out);

  return out;
}

inline Blocks* blocks_get_cpp (Obj x) {
  //TODO check that x is a blocks
  return CLASS_OBJ<Blocks>(wrapper_get(x));
}

inline Obj blocks_get_ext_rep (Obj x) {
  //TODO asserts
  return wrapper_get_elm(x, 2);
}

inline void blocks_set_ext_rep (Obj x, Obj ext_rep) {
  //TODO asserts
  wrapper_set_elm(x, 2, ext_rep);
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
      u_int32_t index = INT_INTOBJ(ELM_LIST(gap_blocks, i)) - 1;
      blocks->push_back(index);
      nr_blocks = (index > nr_blocks ? index : nr_blocks);
    }
    nr_left_blocks = nr_blocks + 1;
    for (size_t i = ((size_t) LEN_LIST(gap_blocks) / 2) + 1;
         i <= (size_t) LEN_LIST(gap_blocks); i++) {
      assert(IS_INTOBJ(ELM_LIST(gap_blocks, i)) &&
             INT_INTOBJ(ELM_LIST(gap_blocks, i)) > 0);
      u_int32_t index = INT_INTOBJ(ELM_LIST(gap_blocks, i)) - 1;
      blocks->push_back(index);
      nr_blocks = (index > nr_blocks ? index : nr_blocks);
    }
    nr_blocks++;
  }

  // construct C++ object
  Bipartition* x = new Bipartition(blocks);
  x->set_nr_left_blocks(nr_left_blocks);
  x->set_nr_blocks(nr_blocks);

  Obj out = bipart_new(x);

  if (by_ext_rep) {
    bipart_set_ext_rep(out, gap_blocks); //TODO copy gap_blocks?
  } else {
    bipart_set_int_rep(out, gap_blocks); //TODO copy gap_blocks?
  }

  return out;
}

Obj BIPART_EXT_REP (Obj self, Obj x) {

  if (bipart_get_ext_rep(x) == NULL) {
    Bipartition* xx = bipart_get_cpp(x);
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
    bipart_set_ext_rep(x, ext_rep);
  }

  return bipart_get_ext_rep(x);
}

Obj BIPART_INT_REP (Obj self, Obj x) {

  if (bipart_get_int_rep(x) == NULL) {
    Bipartition* xx = bipart_get_cpp(x); // get C++ bipartition
    size_t n = xx->degree();

    Obj int_rep = NEW_PLIST(T_PLIST_CYC, 2 * xx->degree());
    SET_LEN_PLIST(int_rep, (Int) 2 * xx->degree());

    for (size_t i = 0; i < 2 * n; i++) {
      SET_ELM_PLIST(int_rep, i + 1, INTOBJ_INT(xx->block(i) + 1));
    }
    bipart_set_int_rep(x, int_rep);
  }

  return bipart_get_int_rep(x);
}

Obj BIPART_HASH (Obj self, Obj x, Obj data) {
  return INTOBJ_INT((bipart_get_cpp(x)->hash_value() % INT_INTOBJ(data)) + 1);
}

Obj BIPART_DEGREE (Obj self, Obj x) {
  return INTOBJ_INT(bipart_get_cpp(x)->degree());
}

Obj BIPART_NR_BLOCKS (Obj self, Obj x) {
  return INTOBJ_INT(bipart_get_cpp(x)->nr_blocks());
}

Obj BIPART_NR_LEFT_BLOCKS (Obj self, Obj x) {
  return INTOBJ_INT(bipart_get_cpp(x)->nr_left_blocks());
}

Obj BIPART_RANK (Obj self, Obj x, Obj dummy) {
  return INTOBJ_INT(bipart_get_cpp(x)->rank());
}

Obj BIPART_PROD (Obj self, Obj x, Obj y) {

  Bipartition* xx = bipart_get_cpp(x);
  Bipartition* yy = bipart_get_cpp(y);

  Bipartition* z = new Bipartition(xx->degree());
  z->redefine(xx, yy);

  return bipart_new(z);
}

Obj BIPART_EQ (Obj self, Obj x, Obj y) {
  return (bipart_get_cpp(x)->equals(bipart_get_cpp(y)) ? True : False);
}

Obj BIPART_LT (Obj self, Obj x, Obj y) {
  return (bipart_get_cpp(x)->lt(bipart_get_cpp(y)) ? True : False);
}

// TODO this should go into semigroups++

Obj BIPART_PERM_LEFT_QUO (Obj self, Obj x, Obj y) {
  Bipartition* xx = bipart_get_cpp(x);
  Bipartition* yy = bipart_get_cpp(y);

  size_t deg  = xx->degree();
  Obj p       = NEW_PERM4(deg);
  UInt4* ptrp = ADDR_PERM4(p);

  // find indices of right blocks of <x>
  size_t  index = 0;
  std::fill(_BUFFER_size_t.begin(),
            std::min(_BUFFER_size_t.end(), _BUFFER_size_t.begin() + 2 * deg),
            -1);
  _BUFFER_size_t.resize(2 * deg, -1);

  for (size_t i = deg; i < 2 * deg; i++) {
    if (_BUFFER_size_t[xx->block(i)] == (UInt4) -1) {
      _BUFFER_size_t[xx->block(i)] = index;
      index++;
    }
    ptrp[i - deg] = i - deg;
  }

  for (size_t i = deg; i < 2 * deg; i++) {
    if (yy->block(i) < xx->nr_left_blocks()) {
      ptrp[_BUFFER_size_t[yy->block(i)]] = _BUFFER_size_t[xx->block(i)];
    }
  }
  return p;
}

Obj BIPART_LEFT_PROJ (Obj self, Obj x) {

  Bipartition* xx = bipart_get_cpp(x);

  size_t deg  = xx->degree();
  size_t next = xx->nr_left_blocks();

  std::fill(_BUFFER_size_t.begin(),
            std::min(_BUFFER_size_t.end(), _BUFFER_size_t.begin() + 2 * deg),
            -1);
  _BUFFER_size_t.resize(2 * deg, -1);

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->resize(2 * deg, -1);

  for (size_t i = 0; i < deg; i++) {
    (*blocks)[i] = xx->block(i);
    if (xx->is_transverse_block(xx->block(i))) {
      (*blocks)[i + deg] = xx->block(i);
    } else if (_BUFFER_size_t[xx->block(i)] != (size_t) -1) {
      (*blocks)[i + deg] = _BUFFER_size_t[xx->block(i)];
    } else {
      _BUFFER_size_t[xx->block(i)] = next;
      (*blocks)[i + deg] = next;
      next++;
    }
  }

  Bipartition* out = new Bipartition(blocks);
  out->set_nr_blocks(next);
  return bipart_new(out);
}

Obj BIPART_STAR (Obj self, Obj x) {

  Bipartition* xx = bipart_get_cpp(x);
  size_t deg  = xx->degree();

  std::fill(_BUFFER_size_t.begin(),
            std::min(_BUFFER_size_t.end(), _BUFFER_size_t.begin() + 2 * deg),
            -1);
  _BUFFER_size_t.resize(2 * deg, -1);

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->resize(2 * deg, -1);

  size_t next = 0;

  for (size_t i = 0; i < deg; i++) {
    if (_BUFFER_size_t[xx->block(i + deg)] != (size_t) -1) {
      (*blocks)[i] = _BUFFER_size_t[xx->block(i + deg)];
    } else {
      _BUFFER_size_t[xx->block(i + deg)] = next;
      (*blocks)[i] = next;
      next++;
    }
  }

  size_t nr_left = next;

  for (size_t i = 0; i < deg; i++) {
    if (_BUFFER_size_t[xx->block(i)] != (size_t) -1) {
      (*blocks)[i + deg] = _BUFFER_size_t[xx->block(i)];
    } else {
      _BUFFER_size_t[xx->block(i)] = next;
      (*blocks)[i + deg] = next;
      next++;
    }
  }

  Bipartition* out = new Bipartition(blocks);
  out->set_nr_blocks(next);
  out->set_nr_left_blocks(nr_left);

  return bipart_new(out);
}

// x and y should have equal left blocks

Obj BIPART_LAMBDA_CONJ (Obj self, Obj x, Obj y) {

  Bipartition* xx = bipart_get_cpp(x);
  Bipartition* yy = bipart_get_cpp(y);

  size_t deg            = xx->degree();
  size_t nr_left_blocks = xx->nr_left_blocks();
  size_t nr_blocks      = std::max(xx->nr_blocks(), yy->nr_blocks());

  std::fill(_BUFFER_size_t.begin(),
            std::min(_BUFFER_size_t.end(),
                     _BUFFER_size_t.begin() + nr_left_blocks + 3 * nr_blocks),
            -1);
  _BUFFER_size_t.resize(nr_left_blocks + 3 * nr_blocks, -1);

  auto   seen = _BUFFER_size_t.begin() + nr_left_blocks;
  auto   src  = seen + nr_blocks;
  auto   dst  = src + nr_blocks;
  size_t next = 0;

  for (size_t i = deg; i < 2 * deg; i++) {
    if (seen[yy->block(i)] == (size_t) -1) {
      seen[yy->block(i)]++;
      if (yy->block(i) < nr_left_blocks) { // connected block
        _BUFFER_size_t[yy->block(i)] = next;
      }
      next++;
    }
  }

  Obj    p    = NEW_PERM4(nr_blocks);
  UInt4* ptrp = ADDR_PERM4(p);
  next = 0;

  for (size_t i = deg; i < 2 * deg; i++) {
    if (seen[xx->block(i)] < 1) {
      seen[xx->block(i)] += 2;
      if (xx->block(i) < nr_left_blocks) { // connected block
        ptrp[next] = _BUFFER_size_t[xx->block(i)];
        src[next]++;
        dst[_BUFFER_size_t[xx->block(i)]]++;
      }
      next++;
    }
  }

  size_t j = 0;
  for (size_t i = 0; i < nr_blocks; i++) {
    if (src[i] == (size_t) -1) {
      while (dst[j] != (size_t) -1) {
        j++;
      }
      ptrp[i] = j;
      j++;
    }
  }
  return p;
}

// x and y should have equal left blocks

Obj BIPART_STAB_ACTION (Obj self, Obj x, Obj p) {
  size_t pdeg;

  // find the degree of the permutation p
  if (TNUM_OBJ(p) == T_PERM2){
    UInt2* ptr = ADDR_PERM2(p);
    pdeg       = DEG_PERM2(p);
    while (ptr[pdeg] == pdeg) {
      pdeg--;
    }
  } else if (TNUM_OBJ(p) == T_PERM4) {
    UInt4* ptr = ADDR_PERM4(p);
    pdeg       = DEG_PERM4(p);
    while (ptr[pdeg] == pdeg) {
      pdeg--;
    }
  } else {
    ErrorQuit("usage: <p> must be a list (not a %s)", (Int) TNAM_OBJ(p), 0L);
  }

  Bipartition* xx = bipart_get_cpp(x);

  size_t deg       = xx->degree();
  size_t nr_blocks = xx->nr_blocks();

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->resize(2 * deg);

  std::fill(_BUFFER_size_t.begin(),
            std::min(_BUFFER_size_t.end(),
                     _BUFFER_size_t.begin() + 2 * nr_blocks + std::max(nr_blocks, pdeg)),
            -1);
  _BUFFER_size_t.resize(2 * nr_blocks + std::max(nr_blocks, pdeg), -1);

  auto tab1 = _BUFFER_size_t.begin();
  auto tab2 = _BUFFER_size_t.begin() + nr_blocks;
  auto q    = tab2 + nr_blocks; // the inverse of p

  if (TNUM_OBJ(p) == T_PERM2){
    UInt2* ptr = ADDR_PERM2(p);
    UInt2 i;
    for (i = 0; i < pdeg; i++) {
      q[ptr[i]] = static_cast<size_t>(i);
    }
    for (; i < deg; i++) {
      q[i] = static_cast<size_t>(i);
    }
  } else if (TNUM_OBJ(p) == T_PERM4) {
    UInt4* ptr = ADDR_PERM4(p);
    UInt4 i;
    for (i = 0; i < pdeg; i++) {
      q[ptr[i]] = static_cast<size_t>(i);
    }
    for (; i < deg; i++) {
      q[i] = static_cast<size_t>(i);
    }
  }

  size_t next = 0;

  for (size_t i = deg; i < 2 * deg; i++) {
    if (tab1[xx->block(i)] == (size_t) -1) {
      tab1[xx->block(i)] = q[next];
      tab2[next]         = xx->block(i);
      next++;
    }
  }

  for (size_t i = 0; i < deg; i++) {
    (*blocks)[i]       = xx->block(i);
    (*blocks)[i + deg] = tab2[tab1[xx->block(i + deg)]];
  }

  return bipart_new(new Bipartition(blocks));
}

Obj BIPART_LEFT_BLOCKS (Obj self, Obj x) {
  return blocks_new(bipart_get_cpp(x)->left_blocks());
}

Obj BIPART_RIGHT_BLOCKS (Obj self, Obj x) {
  return blocks_new(bipart_get_cpp(x)->right_blocks());
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Blocks
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////
// GAP-level functions
////////////////////////////////////////////////////////////////////////////////

/*gap> blocks := BlocksNC([[-1, -10], [2], [-3, -4, -6, -8], [5],
> [7, 9]]);;*/


Obj BLOCKS_NC (Obj self, Obj gap_blocks) {

  assert(IS_LIST(gap_blocks));

  if (LEN_LIST(gap_blocks) == 0) {
    Obj out = blocks_new(new Blocks());
    blocks_set_ext_rep(out, gap_blocks);
    return out;
  }

  assert(IS_LIST(ELM_LIST(gap_blocks, 1)));

  size_t degree    = 0;
  size_t nr_blocks = LEN_LIST(gap_blocks);

  for (size_t i = 1; i <= nr_blocks; i++) {
    assert(IS_LIST(ELM_LIST(gap_blocks, i)));
    degree += LEN_LIST(ELM_LIST(gap_blocks, i));
  }

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->resize(degree);

  std::vector<bool>* lookup = new std::vector<bool>();
  lookup->resize(nr_blocks);

  for (size_t i = 1; i <= nr_blocks; i++) {
    Obj block = ELM_LIST(gap_blocks, i);
    for (size_t j = 1; j <= (size_t) LEN_LIST(block); j++) {
      assert(IS_INTOBJ(ELM_LIST(block, j)));
      int jj = INT_INTOBJ(ELM_LIST(block, j));
      if (jj < 0) {
        (*blocks)[-jj - 1] = i - 1;
      } else {
        (*blocks)[jj - 1] = i - 1;
        (*lookup)[i - 1] = true;
      }
    }
  }

  Obj out = blocks_new(new Blocks(blocks, lookup, nr_blocks));
  blocks_set_ext_rep(out, gap_blocks);

  return out;
}

Obj BLOCKS_EXT_REP (Obj self, Obj x) {

  if (blocks_get_ext_rep(x) == NULL) {
    Blocks* xx = blocks_get_cpp(x);
    size_t n = xx->degree();

    Obj ext_rep = NEW_PLIST(T_PLIST_HOM, xx->nr_blocks());
    SET_LEN_PLIST(ext_rep, (Int) xx->nr_blocks());

    for (size_t i = 0; i < n; i++) {
      Obj block;
      Obj entry = (xx->is_transverse_block(xx->block(i)) ? INTOBJ_INT(i + 1) :
                   INTOBJ_INT(-i - 1));
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
    blocks_set_ext_rep(x, ext_rep);
  }

  return blocks_get_ext_rep(x);
}

Obj BLOCKS_HASH (Obj self, Obj x, Obj data) {
  return INTOBJ_INT((blocks_get_cpp(x)->hash_value() % INT_INTOBJ(data)) + 1);
}

Obj BLOCKS_DEGREE (Obj self, Obj x) {
  return INTOBJ_INT(blocks_get_cpp(x)->degree());
}

Obj BLOCKS_RANK (Obj self, Obj x) {
  return INTOBJ_INT(blocks_get_cpp(x)->rank());
}

Obj BLOCKS_NR_BLOCKS (Obj self, Obj x) {
  return INTOBJ_INT(blocks_get_cpp(x)->nr_blocks());
}

Obj BLOCKS_PROJ (Obj self, Obj blocks_gap) {

  Blocks* blocks = blocks_get_cpp(blocks_gap);

  _BUFFER_size_t.clear();
  _BUFFER_size_t.resize(blocks->nr_blocks(), -1);

  std::vector<u_int32_t>* out = new std::vector<u_int32_t>();
  out->resize(2 * blocks->degree());
  u_int32_t nr_blocks = blocks->nr_blocks();

  for (u_int32_t i = 0; i < blocks->degree(); i++) {
    u_int32_t index = blocks->block(i);
    (*out)[i] = index;
    if (blocks->is_transverse_block(index)) {
      (*out)[i + blocks->degree()] = index;
    } else {
      if (_BUFFER_size_t[index] == (size_t) -1) {
        _BUFFER_size_t[index] = nr_blocks;
        nr_blocks++;
      }
      (*out)[i + blocks->degree()] = _BUFFER_size_t[index];
    }
  }
  return bipart_new(new Bipartition(out));
}

Obj BLOCKS_EQ (Obj self, Obj x, Obj y) {
  return (*blocks_get_cpp(x) == *blocks_get_cpp(y) ? True : False);
}

Obj BLOCKS_LT (Obj self, Obj x, Obj y) {
  return (*blocks_get_cpp(x) < *blocks_get_cpp(y) ? True : False);
}

////////////////////////////////////////////////////////////////////////////////
// Non-GAP functions
////////////////////////////////////////////////////////////////////////////////

// _BUFFER_bool has to be pre-assigned with the correct values

inline size_t fuse_it (size_t i) {
  while (_BUFFER_size_t[i] < i) {
    i = _BUFFER_size_t[i];
  }
  return i;
}

void fuse (u_int32_t                                 deg,
           typename std::vector<u_int32_t>::iterator left_begin,
           u_int32_t                                 left_nr_blocks,
           typename std::vector<u_int32_t>::iterator right_begin,
           u_int32_t                                 right_nr_blocks,
           bool sign) {

  _BUFFER_size_t.clear();
  _BUFFER_size_t.reserve(left_nr_blocks + right_nr_blocks);

  for (size_t i = 0; i < left_nr_blocks + right_nr_blocks; i++) {
    _BUFFER_size_t.push_back(i);
  }

  for (auto left_it = left_begin, right_it = right_begin;
       left_it < left_begin + deg; left_it++, right_it++) {
    size_t j = fuse_it(*left_it);
    size_t k = fuse_it(*right_it + left_nr_blocks);

    if (j != k) {
      if (j < k) {
        _BUFFER_size_t[k] = j;
        if (sign && _BUFFER_bool[k]) {
          _BUFFER_bool[j] = true;
        }
      } else {
        _BUFFER_size_t[j] = k;
        if (sign && _BUFFER_bool[j]) {
          _BUFFER_bool[k] = true;
        }
      }
    }
  }
}

Obj BLOCKS_E_TESTER (Obj self, Obj left_gap, Obj right_gap) {

  Blocks* left  = blocks_get_cpp(left_gap);
  Blocks* right = blocks_get_cpp(right_gap);

  if (left->rank() != right->rank()) {
    return False;
  } else if (left->rank() == 0) {
    return True;
  }

  // prepare the _BUFFER_bool for detecting transverse fused blocks
  _BUFFER_bool.clear();
  _BUFFER_bool.resize(right->nr_blocks() + 2 * left->nr_blocks());
  std::copy(right->lookup()->begin(),
            right->lookup()->end(),
            _BUFFER_bool.begin() + left->nr_blocks());
  auto seen = _BUFFER_bool.begin() + right->nr_blocks() + left->nr_blocks();

  // after the following line:
  //
  // 1) [_BUFFER_size_t.begin() .. _BUFFER_size_t.begin() + left_nr_blocks +
  //    right_nr_blocks - 1] is the fuse table for left and right
  //
  // 2) _BUFFER_bool is a lookup for the transverse blocks of the fused left
  //     and right

  fuse(left->degree(),
       left->begin(),
       left->nr_blocks(),
       right->begin(),
       right->nr_blocks(),
       true);

  // check we are injective on transverse blocks of <left> and that the fused
  // blocks are also transverse.

  for (u_int32_t i = 0; i < left->nr_blocks(); i++) {
    if (left->is_transverse_block(i)) {
      size_t j = fuse_it(i);
      if (!_BUFFER_bool[j] || seen[j]) {
        return False;
      }
      seen[j] = true;
    }
  }
  return True;
}

Obj BLOCKS_E_CREATOR (Obj self, Obj left_gap, Obj right_gap) {

  Blocks* left  = blocks_get_cpp(left_gap);
  Blocks* right = blocks_get_cpp(right_gap);

  fuse(left->degree(),
       left->begin(),
       left->nr_blocks(),
       right->begin(),
       right->nr_blocks(),
       false);

  _BUFFER_size_t.resize(3 * (left->nr_blocks() + right->nr_blocks()), 0);
  std::fill(_BUFFER_size_t.begin() + 2 * (left->nr_blocks() + right->nr_blocks()),
            _BUFFER_size_t.begin() + 3 * (left->nr_blocks() + right->nr_blocks()),
            -1);

  auto tab1 = _BUFFER_size_t.begin() + left->nr_blocks() + right->nr_blocks();
  auto tab2 = _BUFFER_size_t.begin() + 2 * (left->nr_blocks() + right->nr_blocks());

  // find new names for the signed blocks of right
  for (size_t i = 0; i < right->nr_blocks(); i++) {
    if (right->is_transverse_block(i)) {
      tab1[fuse_it(i + left->nr_blocks())] = i;
    }
  }

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->resize(2 * left->degree());

  size_t next = right->nr_blocks();

  for (size_t i = 0; i < left->degree(); i++) {
    (*blocks)[i] = right->block(i);
    size_t j = left->block(i);
    if (left->is_transverse_block(j)) {
      (*blocks)[i + left->degree()] = tab1[fuse_it(j)];
    } else {
      if (tab2[j] == (size_t) -1) {
        tab2[j] = next;
        next++;
      }
      (*blocks)[i + left->degree()] = tab2[j];
    }
  }

  Bipartition* out = new Bipartition(blocks);
  out->set_nr_blocks(next);
  out->set_nr_left_blocks(right->nr_blocks());

  return bipart_new(out);
}

// act on <blocks> on the left with the bipartition <x>

Obj BLOCKS_LEFT_ACT (Obj self, Obj blocks_gap, Obj x_gap) {

  Bipartition* x       = bipart_get_cpp(x_gap);
  Blocks*      blocks  = blocks_get_cpp(blocks_gap);

  if (blocks->nr_blocks() == 0) {
    return blocks_new(x->left_blocks());
  }

  // prepare the _BUFFER_bool for detecting transverse fused blocks
  _BUFFER_bool.clear();
  _BUFFER_bool.resize(x->nr_blocks() + blocks->nr_blocks());
  std::copy(blocks->lookup()->begin(),
            blocks->lookup()->end(),
            _BUFFER_bool.begin() + x->nr_blocks());

  fuse(x->degree(),
       x->begin() + x->degree(),
       x->nr_blocks(),
       blocks->begin(),
       blocks->nr_blocks(),
       true);

  _BUFFER_size_t.resize(2 * (x->nr_blocks() + blocks->nr_blocks()), -1);
  auto tab = _BUFFER_size_t.begin() + x->nr_blocks() + blocks->nr_blocks();

  std::vector<u_int32_t>* out_blocks = new std::vector<u_int32_t>();
  out_blocks->reserve(x->degree());
  std::vector<bool>* out_lookup = new std::vector<bool>();
  out_lookup->resize(x->degree());

  u_int32_t next = 0;

  for (u_int32_t i = 0; i < x->degree(); i++) {
    u_int32_t j = fuse_it(x->block(i));
    if (tab[j] == (size_t) -1) {
      tab[j] = next;
      next++;
    }
    out_blocks->push_back(tab[j]);
    (*out_lookup)[tab[j]] = _BUFFER_bool[j];
  }
  out_lookup->resize(next);
  return blocks_new(new Blocks(out_blocks, out_lookup));
}

// act on <blocks> on the right with the bipartition <x>

Obj BLOCKS_RIGHT_ACT (Obj self, Obj blocks_gap, Obj x_gap) {

  Bipartition* x       = bipart_get_cpp(x_gap);
  Blocks*      blocks  = blocks_get_cpp(blocks_gap);

  if (blocks->nr_blocks() == 0) {
    return blocks_new(x->right_blocks());
  }

  // prepare the _BUFFER_bool for detecting transverse fused blocks
  _BUFFER_bool.clear();
  _BUFFER_bool.resize(x->nr_blocks() + blocks->nr_blocks());
  std::copy(blocks->lookup()->begin(),
            blocks->lookup()->end(),
            _BUFFER_bool.begin());

  fuse(x->degree(),
       blocks->begin(),
       blocks->nr_blocks(),
       x->begin(),
       x->nr_blocks(),
       true);

  _BUFFER_size_t.resize(2 * (x->nr_blocks() + blocks->nr_blocks()), -1);
  auto tab = _BUFFER_size_t.begin() + x->nr_blocks() + blocks->nr_blocks();

  std::vector<u_int32_t>* out_blocks = new std::vector<u_int32_t>();
  out_blocks->reserve(x->degree());
  std::vector<bool>* out_lookup = new std::vector<bool>();
  out_lookup->resize(x->degree());

  u_int32_t next = 0;

  for (u_int32_t i = x->degree(); i < 2 * x->degree(); i++) {
    u_int32_t j = fuse_it(x->block(i) + blocks->nr_blocks());
    if (tab[j] == (size_t) -1) {
      tab[j] = next;
      next++;
    }
    out_blocks->push_back(tab[j]);
    (*out_lookup)[tab[j]] = _BUFFER_bool[j];
  }
  out_lookup->resize(next);
  return blocks_new(new Blocks(out_blocks, out_lookup));
}

Obj BLOCKS_INV_LEFT (Obj self, Obj blocks_gap, Obj x_gap) {

  Blocks*      blocks = blocks_get_cpp(blocks_gap);
  Bipartition* x      = bipart_get_cpp(x_gap);

  fuse(x->degree(),
       blocks->begin(),
       blocks->nr_blocks(),
       x->begin(),
       x->nr_blocks(),
       false);

  std::vector<u_int32_t>* out_blocks = new std::vector<u_int32_t>();
  out_blocks->resize(2 * x -> degree());

  _BUFFER_size_t.resize(2 * blocks->nr_blocks() + x->nr_blocks(), -1);
  auto tab = _BUFFER_size_t.begin() + blocks->nr_blocks() + x->nr_blocks();

  for (u_int32_t i = 0; i < blocks->nr_blocks(); i++) {
    if (blocks->is_transverse_block(i)) {
      tab[fuse_it(i)] = i;
    }
  }

  // find the left blocks of the output
  for (u_int32_t i = 0; i < blocks->degree(); i++) {
    (*out_blocks)[i] = blocks->block(i);
    u_int32_t j = fuse_it(x->block(i) + blocks->nr_blocks());
    if (j > blocks->nr_blocks() || tab[j] == (size_t) -1) {
      (*out_blocks)[i + x->degree()] = blocks->nr_blocks(); //junk
    } else {
      (*out_blocks)[i + x->degree()] = tab[j];
    }
  }

  Bipartition* out = new Bipartition(out_blocks);
  out->set_nr_left_blocks(blocks->nr_blocks());

  return bipart_new(out);
}

// LambdaInverse - fuse <blocks> with the left blocks of <f> keeping track of
// the signs of the fused classes.
//
// The left blocks of the output are then:
//
// 1) disconnected right blocks of <f> (before fusing)
//
// 2) disconnected right blocks of <f> (after fusing)
//
// 3) connected right blocks of <f> (after fusing)
//
// both types 1+2 of the disconnected blocks are unioned into one left block of
// the output with index <junk>. The connected blocks 3 of <f> are given the next
// available index, if they have not been seen before. The table <tab1> keeps
// track of which connected right blocks of <f> have been seen before and the
// corresponding index in the output, i.e. <tab1[x]> is the index in <out> of the
// fused block with index <x>.
//
// The right blocks of the output are:
//
// 1) disconnected blocks of <blocks>; or
//
// 2) connected blocks of <blocks>.
//
// The disconnected blocks 1 are given the next available index, if they have not
// been seen before. The table <tab2> keeps track of which disconnected blocks of
// <blocks> have been seen before and the corresponding index in the output, i.e.
// <tab2[x]> is the index in <out> of the disconnected block of <blocks> with
// index <x>. The connected blocks 2 of <blocks> is given the index <tab1[x]>
// where <x> is the fused index of the block.

Obj BLOCKS_INV_RIGHT (Obj self, Obj blocks_gap, Obj x_gap) {

  Blocks*      blocks = blocks_get_cpp(blocks_gap);
  Bipartition* x      = bipart_get_cpp(x_gap);

  //prepare _BUFFER_bool for fusing

  _BUFFER_bool.clear();
  _BUFFER_bool.resize(blocks->nr_blocks() + x->nr_blocks());
  std::copy(blocks->lookup()->begin(),
            blocks->lookup()->end(),
            _BUFFER_bool.begin());

  fuse(x->degree(),
       blocks->begin(),
       blocks->nr_blocks(),
       x->begin(),
       x->nr_blocks(),
       true);

  u_int32_t junk = -1;
  u_int32_t next = 0;

  std::vector<u_int32_t>* out_blocks = new std::vector<u_int32_t>();
  out_blocks->resize(2 * x -> degree());

  _BUFFER_size_t.resize(3 * blocks->nr_blocks() + 2 * x->nr_blocks(), -1);
  auto tab1 = _BUFFER_size_t.begin() + blocks->nr_blocks() + x->nr_blocks();
  auto tab2 = _BUFFER_size_t.begin() + 2 * (blocks->nr_blocks() + x->nr_blocks());

  // find the left blocks of the output
  for (u_int32_t i = 0; i < blocks->degree(); i++) {
    if (x->block(i + x->degree()) < x->nr_left_blocks()) {
      u_int32_t j = fuse_it(x->block(i + x->degree()) + blocks->nr_blocks());
      if (_BUFFER_bool[j]) {
        if (tab1[j] == (size_t) -1) {
          tab1[j] = next;
          next++;
        }
        (*out_blocks)[i] = tab1[j];
        continue;
      }
    }
    if (junk == (u_int32_t) -1) {
      junk = next;
      next++;
    }
    (*out_blocks)[i] = junk;
  }

  u_int32_t out_nr_left_blocks = next;

  // find the right blocks of the output

  for (u_int32_t i = blocks->degree(); i < 2 * blocks->degree(); i++) {
    u_int32_t j = blocks->block(i - blocks->degree());
    if (blocks->is_transverse_block(j)) {
      (*out_blocks)[i] = tab1[fuse_it(j)];
    } else {
      if (tab2[j] == (size_t) -1) {
        tab2[j] = next;
        next++;
      }
      (*out_blocks)[i] = tab2[j];
    }
  }

  Bipartition* out = new Bipartition(out_blocks);
  out->set_nr_left_blocks(out_nr_left_blocks);
  out->set_nr_blocks(next);
  return bipart_new(out);
}
