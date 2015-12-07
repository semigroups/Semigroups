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

UInt SizeBlist (
    Obj                 blist )
{
    UInt *              ptr;            /* pointer to blist                */
    UInt                nrb;            /* number of blocks in blist       */
    UInt                m;              /* number of bits in a block       */
    UInt                n;              /* number of bits in blist         */
    UInt                i;              /* loop variable                   */

    /* get the number of blocks and a pointer                              */
    nrb = NUMBER_BLOCKS_BLIST(blist);
    ptr = BLOCKS_BLIST( blist );

    /* loop over the blocks, adding the number of bits of each one         */
    n = 0;
    for ( i = 1; i <= nrb; i++ ) {
        m = *ptr++;
        COUNT_TRUES_BLOCK(m);
        n += m;
    }

    /* return the number of bits                                           */
    return n;
}

// Global variables

static std::vector<size_t> _BUFFER_size_t;
static std::vector<bool>   _BUFFER_bool;
static Int                 _RNam_wrapper   = RNamName("wrapper");
static Int                 _RNam_blocks    = RNamName("blocks");
static Int                 _RNam_degree    = RNamName("degree");
static Int                 _RNam_blist     = RNamName("blist");
static Int                 _RNam_nr_blocks = RNamName("nr_blocks");

// Helper functions

inline Obj NEW_GAP_BIPART (Bipartition* x) {

  // construct GAP wrapper for C++ object
  Obj wrapper = NewSemigroupsBag(x, GAP_BIPART, 6);

  // put the GAP wrapper in a list and Objectify
  Obj out = NEW_PREC(1);
  AssPRec(out, _RNam_wrapper, wrapper);
  TYPE_COMOBJ(out) = BipartitionType;
  RetypeBag(out, T_COMOBJ);
  CHANGED_BAG(out);

  return out;
}

// Blocks are stored internally as a list consisting of:
//
// [ nr of blocks, internal rep of blocks, transverse blocks ]
//
// <nr of blocks> is a non-negative integer, <internal rep of blocks>[i] = j if
// <i> belongs to the <j>th block, <transverse blocks>[j] = true if block <j> is
// transverse and false if it is not.

inline Obj NEW_GAP_BLOCKS (size_t degree, Obj blocks, Obj blist) {

  size_t nr_blocks = 0;
  for (size_t i = 1; i <= (size_t) LEN_LIST(blocks); i++) {
    size_t index = INT_INTOBJ(ELM_LIST(blocks, i));
    nr_blocks = (index > nr_blocks ? index : nr_blocks);
  }

  Obj out = NEW_PREC(4);

  AssPRec(out, _RNam_degree,    INTOBJ_INT(degree));
  AssPRec(out, _RNam_blocks,    blocks);
  AssPRec(out, _RNam_blist,     blist);
  AssPRec(out, _RNam_nr_blocks, INTOBJ_INT(nr_blocks));
  CHANGED_BAG(out);

  TYPE_COMOBJ(out) = BlocksType;
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

inline void SET_LEFT_BLOCKS (Obj x, Obj blocks) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  SET_ELM_WRAPPER(x, 4, blocks);
}

inline Obj GET_LEFT_BLOCKS (Obj x) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  return GET_ELM_WRAPPER(x, 4);
}

inline void SET_RIGHT_BLOCKS (Obj x, Obj blocks) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  SET_ELM_WRAPPER(x, 5, blocks);
}

inline Obj GET_RIGHT_BLOCKS (Obj x) {
  //TODO check that x is a bipartition
  //TODO check that <pos> isn't off the end of the wrapper
  //TODO check blocks
  return GET_ELM_WRAPPER(x, 5);
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
    nr_left_blocks = nr_blocks + 1;
    for (size_t i = ((size_t) LEN_LIST(gap_blocks) / 2) + 1;
         i <= (size_t) LEN_LIST(gap_blocks); i++) {
      assert(IS_INTOBJ(ELM_LIST(gap_blocks, i)) &&
             INT_INTOBJ(ELM_LIST(gap_blocks, i)) > 0);
      size_t index = INT_INTOBJ(ELM_LIST(gap_blocks, i)) - 1;
      blocks->push_back(index);
      nr_blocks = (index > nr_blocks ? index : nr_blocks);
    }
    nr_blocks++;
  }

  // construct C++ object
  Bipartition* x = new Bipartition(blocks);
  x->set_nr_left_blocks(nr_left_blocks);
  x->set_nr_blocks(nr_blocks);

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
    SET_LEN_PLIST(int_rep, (Int) 2 * xx->degree());

    for (size_t i = 0; i < 2 * n; i++) {
      SET_ELM_PLIST(int_rep, i + 1, INTOBJ_INT(xx->block(i) + 1));
    }
    SET_BIPART_INT_REP(x, int_rep);
  }

  return GET_BIPART_INT_REP(x);
}

Obj BIPART_HASH (Obj self, Obj x) {
  return INTOBJ_INT(GET_CPP_BIPART(x)->hash_value());
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

  Bipartition* xx = GET_CPP_BIPART(x);

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
  return NEW_GAP_BIPART(out);
}

Obj BIPART_STAR (Obj self, Obj x) {

  Bipartition* xx = GET_CPP_BIPART(x);
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

  return NEW_GAP_BIPART(out);
}

// x and y should have equal left blocks

Obj BIPART_LAMBDA_CONJ (Obj self, Obj x, Obj y) {

  Bipartition* xx = GET_CPP_BIPART(x);
  Bipartition* yy = GET_CPP_BIPART(y);

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

  Bipartition* xx = GET_CPP_BIPART(x);

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

  return NEW_GAP_BIPART(new Bipartition(blocks));
}

Obj BIPART_LEFT_BLOCKS (Obj self, Obj x) {
  if (GET_LEFT_BLOCKS(x) == NULL) {
    Bipartition* xx       = GET_CPP_BIPART(x);
    size_t deg            = xx->degree();
    size_t nr_left_blocks = xx->nr_left_blocks();

    Obj blist = NewBag(T_BLIST, SIZE_PLEN_BLIST(nr_left_blocks)); // transverse blocks lookup
    SET_LEN_BLIST(blist, nr_left_blocks);

    for (size_t i = 0; i < nr_left_blocks; i++) {
      if (xx->is_transverse_block(i)) {
        SET_ELM_BLIST(blist, i + 1, True);
      } else {
        SET_ELM_BLIST(blist, i + 1, False);
      }
    }

    Obj blocks = NEW_PLIST(T_PLIST_CYC, deg);
    SET_LEN_PLIST(blocks, deg);

    for (size_t i = 0; i < deg; i++) {
      SET_ELM_PLIST(blocks, i + 1, INTOBJ_INT(xx->block(i) + 1));
    }
    SET_LEFT_BLOCKS(x, NEW_GAP_BLOCKS(deg, blocks, blist));
  }
  return GET_LEFT_BLOCKS(x);
}

Obj BIPART_RIGHT_BLOCKS (Obj self, Obj x) {
  if (GET_RIGHT_BLOCKS(x) == NULL) {
    Bipartition* xx        = GET_CPP_BIPART(x);
    size_t deg             = xx->degree();
    size_t nr_right_blocks = xx->nr_right_blocks();

    Obj blist = NewBag(T_BLIST, SIZE_PLEN_BLIST(nr_right_blocks)); // transverse blocks lookup
    SET_LEN_BLIST(blist, deg);

    for (size_t i = deg; i < 2 * deg; i++) {
      if (xx->is_transverse_block(xx->block(i))) {
        SET_ELM_BLIST(blist, xx->block(i) + 1, True);
      } else {
        SET_ELM_BLIST(blist, xx->block(i) + 1, False);
      }
    }

    Obj blocks = NEW_PLIST(T_PLIST_CYC, deg);
    SET_LEN_PLIST(blocks, deg);

    for (size_t i = 0; i < deg; i++) {
      SET_ELM_PLIST(blocks, i + 1, INTOBJ_INT(xx->block(i + deg) + 1));
    }
    SET_RIGHT_BLOCKS(x, NEW_GAP_BLOCKS(deg, blocks, blist));
  }
  return GET_RIGHT_BLOCKS(x);
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Blocks
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Obj BLOCKS_DEGREE (Obj self, Obj x) {
  return ElmPRec(x, _RNam_degree);
}

Obj BLOCKS_NR_BLOCKS (Obj self, Obj x) {
  return ElmPRec(x, _RNam_nr_blocks);
}

Obj BLOCKS_ELM_LIST (Obj self, Obj x, Obj pos) {
  return ELM_LIST(ElmPRec(x, _RNam_blocks), INT_INTOBJ(pos));
}

inline size_t fuse_it (size_t i) {
  while (_BUFFER_size_t[i] < i) {
    i = _BUFFER_size_t[i];
  }
  return i;
}

////////////////////////////////////////////////////////////////////////////////
// Non-GAP functions
////////////////////////////////////////////////////////////////////////////////

//void FUSE_BIPART_BLOCKS (Obj x, Obj blocks) {
//}

//void FUSE_BLOCKS_BIPART (Obj x, Obj blocks) {
//}

size_t blocks_rank (Obj blocks) {
  return SizeBlist(ElmPRec(blocks, _RNam_blist));
}

void fuse_blocks_blocks (Obj left, Obj right, bool sign) {

  size_t deg = INT_INTOBJ(BLOCKS_DEGREE(0L, left));

  size_t left_nr_blocks  = INT_INTOBJ(BLOCKS_NR_BLOCKS(0L, left));
  size_t right_nr_blocks = INT_INTOBJ(BLOCKS_NR_BLOCKS(0L, right));

  _BUFFER_size_t.clear();
  _BUFFER_size_t.reserve(left_nr_blocks + right_nr_blocks);

  for (size_t i = 0; i < left_nr_blocks + right_nr_blocks; i++) {
    _BUFFER_size_t.push_back(i);
  }

  if (sign) {
    _BUFFER_bool.clear();

    Obj right_blist = ElmPRec(right, _RNam_blist);

    for (size_t i = 0; i < right_nr_blocks; i++) {
      if (ELM_BLIST(right_blist, i + 1) == True) {
        _BUFFER_bool.push_back(true);
      } else {
        _BUFFER_bool.push_back(false);
      }
    }
  }

  Obj left_blocks  = ElmPRec(left, _RNam_blocks);
  Obj right_blocks = ElmPRec(right, _RNam_blocks);

  for (size_t i = 1; i <= deg; i++) {
    size_t j = fuse_it(INT_INTOBJ(ELM_PLIST(left_blocks,  i)) - 1);
    size_t k = fuse_it(INT_INTOBJ(ELM_PLIST(right_blocks, i)) - 1 + left_nr_blocks);

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

////////////////////////////////////////////////////////////////////////////////
// GAP-level functions
////////////////////////////////////////////////////////////////////////////////

Obj BLOCKS_E_TESTER (Obj self, Obj left, Obj right) {

  size_t rank = blocks_rank(left);
  if (rank != blocks_rank(right)) {
    return False;
  } else if (rank == 0) {
    return True;
  }

  size_t left_nr_blocks  = INT_INTOBJ(BLOCKS_NR_BLOCKS(0L, left));

  // after the following line:
  //
  // 1) [_BUFFER_size_t.begin() .. _BUFFER_size_t.begin() + left_nr_blocks +
  //    right_nr_blocks - 1] is the fuse table for left and right
  //
  // 2) _BUFFER_bool is a lookup for the transverse blocks of the fused left
  //     and right

  fuse_blocks_blocks(left, right, true);

  size_t n = _BUFFER_bool.size();
  _BUFFER_bool.resize(n + left_nr_blocks, false);
  std::fill(_BUFFER_bool.begin() + n,
            _BUFFER_bool.begin() + left_nr_blocks,
            false);

  Obj left_blist = ElmPRec(left, _RNam_blist);

  // check we are injective on transverse blocks of <left> and that the fused
  // blocks are also transverse.

  for (size_t i = 0; i < left_nr_blocks; i++) {
    if (ELM_BLIST(left_blist, i + 1) == True) {
      size_t j = fuse_it(i);
      if (!_BUFFER_bool[j] || _BUFFER_bool[j + n]) {
        return False;
      }
      _BUFFER_bool[n + j] = true;
    }
  }
  return True;
}

Obj BLOCKS_E_CREATOR (Obj self, Obj left, Obj right) {

  fuse_blocks_blocks(left, right, false);

  size_t deg             = INT_INTOBJ(BLOCKS_DEGREE(0L, left));
  size_t left_nr_blocks  = INT_INTOBJ(BLOCKS_NR_BLOCKS(0L, left));
  size_t right_nr_blocks = INT_INTOBJ(BLOCKS_NR_BLOCKS(0L, right));

  _BUFFER_size_t.resize(3 * (left_nr_blocks + right_nr_blocks), 0);
  std::fill(_BUFFER_size_t.begin() + 2 * (left_nr_blocks + right_nr_blocks),
            _BUFFER_size_t.begin() + 3 * (left_nr_blocks + right_nr_blocks),
            -1);

  auto tab1 = _BUFFER_size_t.begin() + left_nr_blocks + right_nr_blocks;
  auto tab2 = _BUFFER_size_t.begin() + 2 * (left_nr_blocks + right_nr_blocks);

  Obj left_blist  = ElmPRec(left, _RNam_blist);
  Obj right_blist = ElmPRec(right, _RNam_blist);

  //find new names for the signed blocks of right
  for (size_t i = 0; i < right_nr_blocks; i++) {
    if (ELM_BLIST(right_blist, i + 1) == True) {
      tab1[fuse_it(i + left_nr_blocks)] = i;
    }
  }

  std::vector<u_int32_t>* blocks = new std::vector<u_int32_t>();
  blocks->resize(2 * deg);

  Obj left_blocks  = ElmPRec(left, _RNam_blocks);
  Obj right_blocks = ElmPRec(right, _RNam_blocks);

  size_t next = INT_INTOBJ(BLOCKS_NR_BLOCKS(0L, right));

  for (size_t i = 0; i < deg; i++) {
    (*blocks)[i] = INT_INTOBJ(ELM_LIST(right_blocks, i + 1)) - 1;
    size_t j = INT_INTOBJ(ELM_LIST(left_blocks, i + 1)) - 1;
    if (ELM_BLIST(left_blist, j + 1) == True) {
      (*blocks)[i + deg] = tab1[fuse_it(j)];
    } else {
      if (tab2[j] == (size_t) -1) {
        tab2[j] = next;
        next++;
      }
      (*blocks)[i + deg] = tab2[j];
    }
  }

  Bipartition* out = new Bipartition(blocks);
  out->set_nr_blocks(next);
  out->set_nr_left_blocks(right_nr_blocks);

  return NEW_GAP_BIPART(out);
}
