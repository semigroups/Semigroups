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

// This file contains declarations of precomputed RNamName values

#ifndef SEMIGROUPS_SRC_RNAMS_H_
#define SEMIGROUPS_SRC_RNAMS_H_

#include "compiled.h"  // GAP headers

static Int RNam_batch_size        = 0;
static Int RNam_cong              = 0;
static Int RNam_elts              = 0;
static Int RNam_DefaultOptionsRec = 0;
static Int RNam_genslookup        = 0;
static Int RNam_genpairs          = 0;
static Int RNam_GeneratorsOfMagma = 0;
static Int RNam_ht                = 0;
static Int RNam_left              = 0;
static Int RNam_nr                = 0;
static Int RNam_nrrules           = 0;
static Int RNam_nr_threads        = 0;
static Int RNam_opts              = 0;
static Int RNam_parent            = 0;
static Int RNam_pos               = 0;
static Int RNam_range             = 0;
static Int RNam_rep               = 0;
static Int RNam_report            = 0;
static Int RNam_Representative    = 0;
static Int RNam_right             = 0;
static Int RNam_rules             = 0;
static Int RNam_Size              = 0;
static Int RNam_type              = 0;
static Int RNam_words             = 0;

static Int RNam_cong_pairs_congruence = 0;

static Int RNam_fin_cong_partition = 0;
static Int RNam_fin_cong_lookup    = 0;

static Int RNam_fp_semi_rels   = 0;
static Int RNam_fp_semi_nrgens = 0;
static Int RNam_fp_semi_cong   = 0;

static Int RNam_fp_nrgens = 0;
static Int RNam_fp_rels   = 0;
static Int RNam_fp_extra  = 0;

static Int RNam_en_semi_cpp_semi = 0;
static Int RNam_en_semi_fropin   = 0;

static inline void initRNams() {
  if (!RNam_batch_size) {
    RNam_batch_size        = RNamName("batch_size");
    RNam_cong              = RNamName("cong");
    RNam_elts              = RNamName("elts");
    RNam_DefaultOptionsRec = RNamName("DefaultOptionsRec");
    RNam_genslookup        = RNamName("genslookup");
    RNam_genpairs          = RNamName("genpairs");
    RNam_GeneratorsOfMagma = RNamName("GeneratorsOfMagma");
    RNam_ht                = RNamName("ht");
    RNam_left              = RNamName("left");
    RNam_nr                = RNamName("nr");
    RNam_nrrules           = RNamName("nrrules");
    RNam_nr_threads        = RNamName("nr_threads");
    RNam_opts              = RNamName("opts");
    RNam_parent            = RNamName("parent");
    RNam_pos               = RNamName("pos");
    RNam_range             = RNamName("range");
    RNam_rep               = RNamName("rep");
    RNam_report            = RNamName("report");
    RNam_Representative    = RNamName("Representative");
    RNam_right             = RNamName("right");
    RNam_rules             = RNamName("rules");
    RNam_Size              = RNamName("Size");
    RNam_type              = RNamName("type");
    RNam_words             = RNamName("words");

    RNam_cong_pairs_congruence = RNamName("__cong_pairs_congruence");

    RNam_fin_cong_partition = RNamName("__fin_cong_partition");
    RNam_fin_cong_lookup    = RNamName("__fin_cong_lookup");

    RNam_fp_semi_rels   = RNamName("__fp_semigroup_relations");
    RNam_fp_semi_nrgens = RNamName("__fp_semigroup_nrgens");
    RNam_fp_semi_cong   = RNamName("__fp_semigroup_cong");

    RNam_fp_nrgens = RNamName("fp_nrgens");
    RNam_fp_rels   = RNamName("fp_rels");
    RNam_fp_extra  = RNamName("fp_extra");

    RNam_en_semi_cpp_semi = RNamName("__en_semi_cpp_semi");
    RNam_en_semi_fropin   = RNamName("__en_semi_fropin");
  }
}

#endif  // SEMIGROUPS_SRC_RNAMS_H_
