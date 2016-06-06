/*
 * Semigroups GAP package
 *
 * This file contains . . .
 *
 *
 */

#ifndef SRC_DATA_H_
#define SRC_DATA_H_

#include <assert.h>

#include "src/compiled.h"          // GAP headers
#include "converter.h"
#include "gap.hh"

#include "semigroups++/semigroups.h"

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Record names
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

static Int RNam_batch_size   = 0;
static Int RNam_converter    = 0;
static Int RNam_data         = 0;
static Int RNam_degree       = 0;
static Int RNam_elts         = 0;
static Int RNam_gens         = 0;
static Int RNam_left         = 0;
static Int RNam_pos          = 0;
static Int RNam_report       = 0;
static Int RNam_right        = 0;
static Int RNam_rules        = 0;
static Int RNam_semigroup    = 0;
static Int RNam_words        = 0;
static Int RNam_wrapper      = 0;

static inline void initRNams() {
  if (!RNam_batch_size) {
    RNam_batch_size   = RNamName("batch_size");
    RNam_converter    = RNamName("_SEMIGROUPS_converter");
    RNam_data         = RNamName("data");
    RNam_degree       = RNamName("degree");
    RNam_elts         = RNamName("elts");
    RNam_gens         = RNamName("gens");
    RNam_left         = RNamName("left");
    RNam_pos          = RNamName("pos");
    RNam_report       = RNamName("report");
    RNam_right        = RNamName("right");
    RNam_rules        = RNamName("rules");
    RNam_semigroup    = RNamName("_SEMIGROUPS_semigroup");
    RNam_words        = RNamName("words");
    RNam_wrapper      = RNamName("_SEMIGROUPS_wrapper");
  }
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// What type of semigroup's data do we have?
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

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
  NTP_MAT,
  INT_MAT,
  MAT_OVER_PF,
  PBR_TYPE
};

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Functions to safely access and set things in the semigroup's data record
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

long        data_threshold      (Obj);
long        data_period         (Obj);
long        data_size_ff        (Obj);
Obj         data_rep            (Obj);
size_t      data_batch_size     (Obj);
bool        data_report         (Obj);
size_t      data_degree         (Obj);
DataType    data_type           (Obj);
void        data_init           (Obj);
void        data_init_semigroup (Obj data,
                                 Semigroup* semigroup = nullptr);
void        data_init_converter (Obj);
void        data_delete         (Obj);
Semigroup*  data_semigroup      (Obj);
Converter*  data_converter      (Obj);

#endif // SRC_DATA_H_
