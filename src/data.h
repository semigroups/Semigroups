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


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// What type of semigroup's data do we have?
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

enum en_semi_t {
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
en_semi_t data_type(Obj);
void     data_init(Obj);
void data_init_semigroup(Obj data, Semigroup* semigroup = nullptr);
void       data_init_converter(Obj);
void       data_delete(Obj);
Semigroup* data_semigroup(Obj);
Converter* data_converter(Obj);

#endif // SRC_DATA_H_
