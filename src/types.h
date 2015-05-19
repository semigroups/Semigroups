/*
 * Semigroups GAP package
 *
 * This file contains types of semigroups for use in the Semigroups kernel
 * module.
 *
 */

#ifndef SEMIGROUPS_GAP_TYPES_H
#define SEMIGROUPS_GAP_TYPES_H 1

#include "src/compiled.h"          /* GAP headers                */

#include <assert.h>
#include <vector>

/*******************************************************************************
 * GAP TNUM for wrapping C++ semigroup
*******************************************************************************/

#ifndef T_SEMI
#define T_SEMI T_SPARE2
#endif

enum SemigroupsBagType {
  INTERFACE = 0,
  UF_DATA   = 1
};

template <typename Class>
inline Obj NewSemigroupsBag (Class* cpp_class, SemigroupsBagType type) {
  Obj o = NewBag(T_SEMI, 2 * sizeof(Obj));
  ADDR_OBJ(o)[0] = (Obj)type;
  ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(cpp_class);
  return o;
}

// get C++ Class from GAP object

template <typename Class>
inline Class* CLASS_OBJ(Obj o) {
    return reinterpret_cast<Class*>(ADDR_OBJ(o)[1]);
}

#define IS_T_SEMI(o)        (TNUM_OBJ(o) == T_SEMI)
#define IS_INTERFACE_BAG(o) (IS_T_SEMI(o) && (Int)ADDR_OBJ(o)[0] == INTERFACE)
#define IS_UF_DATA_BAG(o)   (IS_T_SEMI(o) && (Int)ADDR_OBJ(o)[0] == UF_DATA)

/*******************************************************************************
 * Macros for checking types of objects
*******************************************************************************/

#define IS_BOOL_MAT(x)           (CALL_1ARGS(IsBooleanMat, x) == True)
#define IS_BIPART(x)             (CALL_1ARGS(IsBipartition, x) == True)
#define IS_MAT_OVER_SEMI_RING(x) (CALL_1ARGS(IsMatrixOverSemiring, x) == True)
#define IS_MAX_PLUS_MAT(x)       (CALL_1ARGS(IsMaxPlusMatrix, x) == True)
#define IS_MIN_PLUS_MAT(x)       (CALL_1ARGS(IsMinPlusMatrix, x) == True)
#define IS_TROP_MAT(x)           (CALL_1ARGS(IsTropicalMatrix, x) == True)
#define IS_TROP_MAX_PLUS_MAT(x)  (CALL_1ARGS(IsTropicalMaxPlusMatrix, x) == True)
#define IS_TROP_MIN_PLUS_MAT(x)  (CALL_1ARGS(IsTropicalMinPlusMatrix, x) == True)
#define IS_PROJ_MAX_PLUS_MAT(x)  (CALL_1ARGS(IsProjectiveMaxPlusMatrix, x) == True)
#define IS_NAT_MAT(x)            (CALL_1ARGS(IsNaturalMatrix, x) == True)
#define IS_MAT_OVER_PF(x)        (CALL_1ARGS(IsMatrixOverPrimeField, x) == True)

/*******************************************************************************
 * Imported types from the library
*******************************************************************************/

extern Obj Objectify;
extern Obj infinity;
extern Obj Ninfinity;
extern Obj IsBipartition;
extern Obj BipartitionByIntRepNC;   
extern Obj IsBooleanMat;
extern Obj BooleanMatType;   
extern Obj IsMatrixOverSemiring;
extern Obj IsMaxPlusMatrix;
extern Obj MaxPlusMatrixType;   
extern Obj IsMinPlusMatrix;
extern Obj MinPlusMatrixType;   
extern Obj IsTropicalMatrix;
extern Obj IsTropicalMinPlusMatrix;
extern Obj TropicalMinPlusMatrixType;   
extern Obj IsTropicalMaxPlusMatrix;
extern Obj TropicalMaxPlusMatrixType;
extern Obj IsProjectiveMaxPlusMatrix;
extern Obj ProjectiveMaxPlusMatrixType;
extern Obj IsNaturalMatrix;
extern Obj NaturalMatrixType;
extern Obj IsMatrixOverPrimeField;
extern Obj AsMatrixOverPrimeFieldNC;

/*******************************************************************************
 * What type of semigroup do we have?
*******************************************************************************/

enum SemigroupType {
  UNKNOWN,
  TRANS2, 
  TRANS4, 
  PPERM2, 
  PPERM4, 
  BOOL_MAT, 
  BIPART,
  MAX_PLUS_MAT,
  MIN_PLUS_MAT,
  TROP_MAX_PLUS_MAT,
  TROP_MIN_PLUS_MAT,
  PROJ_MAX_PLUS_MAT,
  NAT_MAT,
  MAT_OVER_PF
};

extern SemigroupType TypeSemigroup (Obj data);

bool inline IsCCSemigroup (Obj data) {
  return TypeSemigroup(data) != UNKNOWN;
}

/*******************************************************************************
 * Get a representative of the semigroup from the data
*******************************************************************************/

Obj inline Representative (Obj data) {
  assert(IsbPRec(data, RNamName("gens")));
  assert(LEN_LIST(ElmPRec(data, RNamName("gens"))) > 0);
  return ELM_PLIST(ElmPRec(data, RNamName("gens")), 1);
}

/*******************************************************************************
 * Union-find data structure
*******************************************************************************/
typedef std::vector<size_t>   table_t;
typedef std::vector<table_t*> blocks_t;

class UFData {
  //TODO: Use pointers for all the local fields (less copying!)
public:
  UFData (size_t size) : _size(size), _haschanged(false) {
    _table.reserve(size);
    for (size_t i=0; i<size; i++) {
      _table.push_back(i);
    }
  }
  ~UFData () {
    for (size_t i=0; i<_blocks.size(); i++) {
      delete _blocks[i];
    }
  }
  size_t   get_size () { return _size; }
  table_t  get_table () { return _table; }
  blocks_t get_blocks () {
    table_t *block;
    // Is _blocks "bound" yet?
    if (_blocks.size() == 0) {
      _blocks.reserve(_size);
      for (size_t i=0; i<_size; i++) {
        block = new table_t(1, i);
        _blocks.push_back(block);
      }
    }
    // Do we need to update the blocks?
    if (_haschanged) {
      size_t ii;
      for (size_t i=0; i<_size; i++) {
        if (_blocks[i] != nullptr) {
          ii = find(i);
          if (ii != i) {
            // Combine the two blocks
            _blocks[ii]->reserve(_blocks[ii]->size() + _blocks[i]->size());
            _blocks[ii]->insert(_blocks[ii]->end(),
                               _blocks[i]->begin(),
                               _blocks[i]->end());
            delete _blocks[i];
            _blocks[i] = nullptr;
          }
        }
      }
      _haschanged = false;
    }
    return _blocks;
  }
  size_t   find (size_t i) {
    while (i != _table[i]) {
      i = _table[i];
    }
    return i;
  }
  void     unite (size_t i, size_t j) {
    size_t ii, jj;
    ii = find(i);
    jj = find(j);
    if (ii < jj) {
      _table[jj] = ii;
    } else {
      _table[ii] = jj;
    }
    _haschanged = true;
  }
  void     flatten() {
    for (size_t i=0; i<_size; i++) {
      _table[i] = find(i);
    }
  }
private:
  size_t   _size;
  table_t  _table;
  blocks_t _blocks;
  bool     _haschanged;
};

#endif
