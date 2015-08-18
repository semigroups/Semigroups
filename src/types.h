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

/*******************************************************************************
 * Record names TODO init these in a function
*******************************************************************************/

static Int RNam_elts         = RNamName("elts");
static Int RNam_left         = RNamName("left");
static Int RNam_right        = RNamName("right");
static Int RNam_rules        = RNamName("rules");
static Int RNam_words        = RNamName("words");
static Int RNam_gens         = RNamName("gens");
static Int RNam_batch_size   = RNamName("batch_size");
static Int RNam_report       = RNamName("report");
static Int RNam_Interface_CC = RNamName("Interface_CC");

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
#define IS_PBR(x)                (CALL_1ARGS(IsPBR, x) == True)

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
extern Obj IsPBR;
extern Obj PBRType;

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
  MAT_OVER_PF, 
  PBR_TYPE
};

extern SemigroupType TypeSemigroup (Obj data);

bool inline IsCCSemigroup (Obj data) {
  return TypeSemigroup(data) != UNKNOWN;
}

/*******************************************************************************
 * Get a representative of the semigroup from the data
*******************************************************************************/

//TODO put these in a separate file

Obj inline Representative (Obj data) {
  // TODO more asserts 
  assert(IsbPRec(data, RNam_gens));
  assert(LEN_LIST(ElmPRec(data, RNam_gens)) > 0);
  return ELM_PLIST(ElmPRec(data, RNam_gens), 1);
}

size_t inline BatchSize (Obj data) {
  assert(IsbPRec(data, RNam_batch_size));
  assert(IS_INTOBJ(ElmPRec(data, RNam_batch_size)));
  return INT_INTOBJ(ElmPRec(data, RNam_batch_size));
}

bool inline Report (Obj data) {
  if (IsbPRec(data, RNam_report)) {
    assert(ElmPRec(data, RNam_report) == True || ElmPRec(data, RNam_report) == False);
    return (ElmPRec(data, RNam_report) == True ? true : false);
  }
  return false;
}

#endif
