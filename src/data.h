//
// Semigroups package for GAP
// Copyright (C) 2016 James D. Mitchell
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef SRC_DATA_H_
#define SRC_DATA_H_

#include <assert.h>

#include "converter.h"
#include "gap.h"
#include "src/compiled.h" // GAP headers

#include "semigroupsplusplus/semigroups.h"

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Record names
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

static Int RNam_nr_threads         = 0;
static Int RNam_batch_size         = 0;
static Int RNam_converter          = 0;
static Int RNam_data               = 0;
static Int RNam_degree             = 0;
static Int RNam_elts               = 0;
static Int RNam_gens               = 0;
static Int RNam_genslookup         = 0;
static Int RNam_left               = 0;
static Int RNam_pos                = 0;
static Int RNam_report             = 0;
static Int RNam_right              = 0;
static Int RNam_rules              = 0;
static Int RNam_semigroup          = 0;
static Int RNam_words              = 0;
static Int RNam_wrapper            = 0;
static Int RNam_relations          = 0;
static Int RNam_nr_gens            = 0;
static Int RNam_rep                = 0;
static Int RNam_cong               = 0;
static Int RNam_genpairs           = 0;
static Int RNam_fin_cong_type      = 0;
static Int RNam_fin_cong_range     = 0;
static Int RNam_fin_cong_partition = 0;
static Int RNam_fin_cong_lookup    = 0;

static Int RNam_cong_pairs_congruence = 0;

static Int RNam_fp_semi_rels   = 0;
static Int RNam_fp_semi_nrgens = 0;
static Int RNam_fp_semi_cong   = 0;

static inline void initRNams() {
  if (!RNam_batch_size) {
    RNam_nr_threads = RNamName("nr_threads");
    RNam_batch_size = RNamName("batch_size");
    RNam_converter  = RNamName("_SEMIGROUPS_converter");
    RNam_data       = RNamName("data");
    RNam_degree     = RNamName("degree");
    RNam_elts       = RNamName("elts");
    RNam_gens       = RNamName("gens");
    RNam_genslookup = RNamName("genslookup");
    RNam_left       = RNamName("left");
    RNam_pos        = RNamName("pos");
    RNam_report     = RNamName("report");
    RNam_right      = RNamName("right");
    RNam_rules      = RNamName("rules");
    RNam_semigroup  = RNamName("_SEMIGROUPS_semigroup");
    RNam_words      = RNamName("words");
    RNam_wrapper    = RNamName("_SEMIGROUPS_wrapper");
    RNam_rep        = RNamName("rep");
    RNam_cong       = RNamName("cong");

    RNam_genpairs              = RNamName("genpairs");
    RNam_fin_cong_type         = RNamName("type");
    RNam_fin_cong_range        = RNamName("range");
    RNam_cong_pairs_congruence = RNamName("__cong_pairs_congruence");

    RNam_fin_cong_partition = RNamName("__fin_cong_partition");
    RNam_fin_cong_lookup    = RNamName("__fin_cong_lookup");

    RNam_fp_semi_rels   = RNamName("__fp_semigroup_relations");
    RNam_fp_semi_nrgens = RNamName("__fp_semigroup_nrgens");
    RNam_fp_semi_cong   = RNamName("__fp_semigroup_cong");
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

bool rec_get_report(Obj);
size_t rec_get_nr_threads(Obj);

long     data_threshold(Obj);
long     data_period(Obj);
long     data_size_ff(Obj);
Obj      data_rep(Obj);
size_t   data_batch_size(Obj);
size_t   data_degree(Obj);
DataType data_type(Obj);
void     data_init(Obj);
void data_init_semigroup(Obj data, Semigroup* semigroup = nullptr);
void       data_init_converter(Obj);
void       data_delete(Obj);
Semigroup* data_semigroup(Obj);
Converter* data_converter(Obj);

#endif // SRC_DATA_H_
