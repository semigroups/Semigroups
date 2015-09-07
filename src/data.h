/*
 * Semigroups GAP package
 *
 * This file contains . . .
 * 
 *
 */

#ifndef SEMIGROUPS_GAP_DATA_H
#define SEMIGROUPS_GAP_DATA_H 1

#include <assert.h>

#include "src/compiled.h"          // GAP headers
#include "types.h"                 // types of semigroups
#include "converter.h"

#include "semigroups++/semigroups.h"

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

static Int RNam_semigroup    = RNamName("_SEMIGROUPS_semigroup");
static Int RNam_converter    = RNamName("_SEMIGROUPS_converter");

/*******************************************************************************
 * What type of semigroup's data do we have?
*******************************************************************************/

enum DataType {
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

long        data_data_threshold (Obj);
long        data_data_period    (Obj);
long        data_size_ff        (Obj);
Obj         data_rep            (Obj);
size_t      data_batch_size     (Obj);
bool        data_report         (Obj);
size_t      data_degree         (Obj);
DataType    data_type           (Obj);
void        data_init           (Obj);
void        data_delete         (Obj);
Semigroup*  data_semigroup      (Obj);
Converter*  data_converter      (Obj);

/*bool  IsCCSemigroup (Obj data) {
  return TypeSemigroup(data) != UNKNOWN;
}*/

#endif 
