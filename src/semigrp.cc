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

#include "semigrp.h"

#include "src/compiled.h"

// Internal stuff

static Int RNam_en_semi_gap = RNamName("__en_semi_gap");

// [batch_size, gens, degree, nr_threads, report]
static Int RNam_en_semi_cpp = RNamName("__en_semi_cpp");

static inline Int en_semi_gap_get(Obj S, bool& preexists = false) {
  UInt i;
  
  if (FindPRec(S, RNam_en_semi_gap, &i, 1)) {
    exists = true;
    return GET_ELM_PREC(S, i);
  } else {
    exists = false;
    en_semi_gap = NEW_PREC(0);
    SET_LEN_PREC(en_semi_gap, 0);
    AssPRec(S, RNam_en_semi_gap, en_semi_gap);
    return en_semi_gap;
  }
}

static inline Int en_semi_cpp_get(Obj S) {
  UInt i;
  if (FindPRec(S, RNam_en_semi_cpp, &i, 1)) {
    return GET_ELM_PREC(S, i);
  } else {
    en_semi_cpp_init(S);
    return ElmPRec(S, RNam_en_semi_cpp);
  }
}

static inline Obj en_semi_cpp_get_batch_size(Obj en_semi_cpp) {
  return ELM_PLIST(en_semi_cpp, 1);
}

static inline Obj en_semi_cpp_get_gens (Obj en_semi_cpp) {
  return ELM_PLIST(en_semi_cpp, 2);
}

static inline Obj en_semi_get_gens(Obj S) {
  return en_semi_cpp_get_gens(en_semi_cpp_get(S));
}


void en_semi_cpp_init(Obj S) {
  if (IsbPRec(S, RNam_en_semi_cpp)) {
    return;
  }

  Obj en_semi_cpp = NewPlist(T_PLIST_CYC, 5);
  SET_LEN_PREC(en_semi_cpp, 5);
  AssPRec(S, RNam_en_semi_cpp, en_semi_cpp);
  CHANGED_BAG(S);
  
  UInt i;
  Obj opts;
  
  if (FindPRec(S, RNamName("opts"), &i, 1)) {
    opts = GET_ELM_PREC(S, i);
  } else {
    ErrorQuit();
    return 0L;
  }
  
  if (FindPRec(opts, RNamName("batch_size"), &i, 1)) {
    SET_ELM_PLIST(en_semi_cpp, 1, GET_ELM_PREC(opts, i));
  } else {
    ErrorQuit();
    return 0L;
  }

  if (FindPRec(S, RNamName("GeneratorsOfMagma"), &i, 1)) {
    en_semi_cpp_set_gens(en_semi_cpp, GET_ELM_PREC(S, i));
  } else {
    ErrorQuit();
    return 0L;
  }
  
}

static inline Obj en_semi_get_report(Obj S) {
  UInt i;
  if (FindPRec(S, RNamName("opts"), &i, 1)) {
    Obj opts = GET_ELM_PREC(S, i);
    if (FindPRec(S, 
  } else {
    ErrorQuit();
    return 0L;
  }
}
