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
#include <iostream>
#include <vector>

/*******************************************************************************
 * GAP TNUM for wrapping C++ semigroup
*******************************************************************************/

#ifndef T_SEMI
#define T_SEMI T_SPARE2 //TODO use Register_TNUM when it's available
#endif

enum SemigroupsBagType {
  UF_DATA    = 0,
  SEMIGROUP  = 1,
  CONVERTER  = 2,
  GAP_BIPART = 3,
  GAP_BLOCKS = 4
};

template <typename Class>
inline Obj NewSemigroupsBag (Class* cpp_class, SemigroupsBagType type, size_t size) {
  Obj o = NewBag(T_SEMI, size * sizeof(Obj));
  ADDR_OBJ(o)[0] = (Obj)type;
  ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(cpp_class);
  for (size_t i = 2; i < size; i++) {
    ADDR_OBJ(o)[i] = NULL;
  }
  return o;
}

// get C++ Class from GAP object

template <typename Class>
inline Class* CLASS_OBJ(Obj o) {
    return reinterpret_cast<Class*>(ADDR_OBJ(o)[1]);
}

//TODO make these into function

#define IS_T_SEMI(o)         (TNUM_OBJ(o) == T_SEMI)
#define IS_CONVERTER_BAG(o)  (IS_T_SEMI(o) && (Int)ADDR_OBJ(o)[0] == CONVERTER)
#define IS_SEMIGROUP_BAG(o)  (IS_T_SEMI(o) && (Int)ADDR_OBJ(o)[0] == SEMIGROUP)
#define IS_UF_DATA_BAG(o)    (IS_T_SEMI(o) && (Int)ADDR_OBJ(o)[0] == UF_DATA)
#define IS_GAP_BIPART_BAG(o) (IS_T_SEMI(o) && (Int)ADDR_OBJ(o)[0] == GAP_BIPART)

inline bool IS_GAP_BLOCKS_BAG (Obj o) {
  return IS_T_SEMI(o) && (Int)ADDR_OBJ(o)[0] == GAP_BLOCKS;
}

/*******************************************************************************
 * Macros for checking types of objects
*******************************************************************************/

//FIXME remove CALL_1ARGS here
//TODO make these into function

#define IS_BOOL_MAT(x)           (CALL_1ARGS(IsBooleanMat, x) == True)
#define IS_BIPART(x)             (CALL_1ARGS(IsBipartition, x) == True)
#define IS_MAT_OVER_SEMI_RING(x) (CALL_1ARGS(IsMatrixOverSemiring, x) == True)
#define IS_MAX_PLUS_MAT(x)       (CALL_1ARGS(IsMaxPlusMatrix, x) == True)
#define IS_MIN_PLUS_MAT(x)       (CALL_1ARGS(IsMinPlusMatrix, x) == True)
#define IS_TROP_MAT(x)           (CALL_1ARGS(IsTropicalMatrix, x) == True)
#define IS_TROP_MAX_PLUS_MAT(x)  (CALL_1ARGS(IsTropicalMaxPlusMatrix, x) == True)
#define IS_TROP_MIN_PLUS_MAT(x)  (CALL_1ARGS(IsTropicalMinPlusMatrix, x) == True)
#define IS_PROJ_MAX_PLUS_MAT(x)  (CALL_1ARGS(IsProjectiveMaxPlusMatrix, x) == True)
#define IS_NTP_MAT(x)            (CALL_1ARGS(IsNTPMatrix, x) == True)
#define IS_INT_MAT(x)            (CALL_1ARGS(IsIntegerMatrix, x) == True)
#define IS_PBR(x)                (CALL_1ARGS(IsPBR, x) == True)

/*******************************************************************************
 * Imported types from the library
*******************************************************************************/

extern Obj HTValue;
extern Obj HTAdd;
extern Obj infinity;
extern Obj Ninfinity;
extern Obj IsBipartition;
extern Obj BipartTypes;
extern Obj BipartitionType;
extern Obj BlocksType;
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
extern Obj IsNTPMatrix;
extern Obj NTPMatrixType;
extern Obj IsIntegerMatrix;
extern Obj IntegerMatrixType;
extern Obj IsPBR;
extern Obj PBRTypes;
extern Obj PBRType;

#endif
