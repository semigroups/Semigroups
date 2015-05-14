/*
 * Semigroups GAP package
 *
 * This file contains types of semigroups from GAP. 
 *
 */

#ifndef SEMIGROUPS_GAP_TYPES_H
#define SEMIGROUPS_GAP_TYPES_H

extern "C" {
  #include "src/compiled.h"          /* GAP headers                */
}

#include <assert.h>

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

Obj Objectify;
Obj IsPrimeInt;

Obj infinity;
Obj Ninfinity;

Obj IsBipartition;
Obj BipartitionByIntRepNC;   

Obj IsBooleanMat;
Obj BooleanMatType;   

Obj IsMatrixOverSemiring;

Obj IsMaxPlusMatrix;
Obj MaxPlusMatrixType;   

Obj IsMinPlusMatrix;
Obj MinPlusMatrixType;   

Obj IsTropicalMatrix;

Obj IsTropicalMinPlusMatrix;
Obj TropicalMinPlusMatrixType;   

Obj IsTropicalMaxPlusMatrix;
Obj TropicalMaxPlusMatrixType;

Obj IsProjectiveMaxPlusMatrix;
Obj ProjectiveMaxPlusMatrixType;

Obj IsNaturalMatrix;
Obj NaturalMatrixType;

Obj IsMatrixOverPrimeField;
Obj AsMatrixOverPrimeFieldNC;

/*******************************************************************************
 * Get a representative of the semigroup from the data
*******************************************************************************/

Obj inline Representative (Obj data) {
  assert(IsbPRec(data, RNamName("gens")));
  assert(LEN_LIST(ElmPRec(data, RNamName("gens"))) > 0);
  return ELM_PLIST(ElmPRec(data, RNamName("gens")), 1);
}

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

SemigroupType TypeSemigroup (Obj data) {
  Obj x = Representative(data);
  switch (TNUM_OBJ(x)) {
    case T_TRANS2:
      return TRANS2;
    case T_TRANS4:
      return TRANS4;
    case T_PPERM2:
      return PPERM2;
    case T_PPERM4:
      return PPERM4;
    case T_POSOBJ:
      if (IS_BOOL_MAT(x)) {
        return BOOL_MAT;
      } else if (IS_MAX_PLUS_MAT(x)) {
        return MAX_PLUS_MAT;
      } else if (IS_MIN_PLUS_MAT(x)) {
        return MIN_PLUS_MAT;
      } else if (IS_TROP_MAX_PLUS_MAT(x)) {
        return TROP_MAX_PLUS_MAT;
      } else if (IS_TROP_MIN_PLUS_MAT(x)) {
        return TROP_MIN_PLUS_MAT;
      } else if (IS_PROJ_MAX_PLUS_MAT(x)) {
        return PROJ_MAX_PLUS_MAT;
      } else if (IS_NAT_MAT(x)) {
        return NAT_MAT;
      } else if (IS_MAT_OVER_PF(x)) {
        // TODO handle non-prime fields too!
        return MAT_OVER_PF;
      } 
      return UNKNOWN;
    case T_COMOBJ:
      if (IS_BIPART(x)) {
        return BIPART;
      }
      // intentional fall through
    default: 
      return UNKNOWN;
  }
}

bool inline IsCCSemigroup (Obj data) {
  return TypeSemigroup(data) != UNKNOWN;
}

#endif
