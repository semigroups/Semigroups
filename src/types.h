/*
 * Semigroups GAP package
 *
 * This file contains types of semigroups from GAP. 
 *
 */

#ifndef SEMIGROUPS_GAP_TYPES_H
#define SEMIGROUPS_GAP_TYPES_H 1

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
 * Get a representative of the semigroup from the data
*******************************************************************************/

Obj inline Representative (Obj data);

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

SemigroupType TypeSemigroup (Obj data);

bool inline IsCCSemigroup (Obj data) {
  return TypeSemigroup(data) != UNKNOWN;
}

#endif
