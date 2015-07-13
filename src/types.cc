/*
 * Semigroups GAP package
 *
 * This file contains types of semigroups for use in the Semigroups kernel
 * module.
 *
 */

#include "types.h"

Obj Objectify;
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
Obj IsPBR;
Obj PBRType;

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
      } else if (IS_PBR(x)) {
        return PBR_TYPE;
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
