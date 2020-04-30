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

// This file contains everything in the kernel module for the Semigroups
// package that involves GAP directly, i.e. importing functions/variables from
// GAP and declaring functions for GAP etc.

#ifndef SEMIGROUPS_SRC_PKG_H_
#define SEMIGROUPS_SRC_PKG_H_

#if (defined(__GNUC__) && __GNUC__ < 5 \
     && !(defined(__clang__) || defined(__INTEL_COMPILER)))
#error "GCC version 5.0 or higher is required"
#endif

// Inclusion of <cstdef> appears to be required to prevent travis from issuing
// the warning:
//
//     /usr/include/c++/5/cstddef:51:11: error: ‘::max_align_t’ has not been
//     declared
//
// according to:
//
// https://stackoverflow.com/questions/35110786/how-to-fix-the-error-max-align-t

#include <cstddef>

#include <iostream>
#include <vector>

#include "compiled.h"

#include "rnams.h"
#include "semigroups-debug.h"


#if !defined(GAP_KERNEL_MAJOR_VERSION) || GAP_KERNEL_MAJOR_VERSION < 3
// compatibility with GAP <= 4.9
static inline Obj NEW_PLIST_IMM(UInt type, Int plen) {
  return NEW_PLIST(type | IMMUTABLE, plen);
}
#endif

// The following typedefs are used in the Semigroups package kernel module code
// to increase the readability of the code.

typedef Obj gap_semigroup_t;
typedef Obj gap_element_t;
typedef Obj gap_list_t;
typedef Obj gap_rec_t;
typedef Obj gap_cong_t;
typedef Obj gap_cong_class_t;
typedef Obj gap_int_t;
typedef Obj gap_bool_t;
typedef Obj gap_func_t;

// The Semigroups package uses the type T_SEMI for GAP Objs which act as
// wrappers for various C++ objects. Such a GAP Obj can be created using
// OBJ_CLASS and the C++ object can be recovered from the GAP Obj using
// CLASS_OBJ. The GAP Obj returned by OBJ_CLASS is just a bag of type T_SEMI of
// the form:
//
// [ t_semi_subtype_t, C++ pointer ]
//

extern UInt T_SEMI;
extern UInt T_BIPART;
extern UInt T_BLOCKS;

// Subtypes of objects that can be stored in a GAP Obj of type T_SEMI

enum t_semi_subtype_t {
  T_SEMI_SUBTYPE_UF     = 0,
  T_SEMI_SUBTYPE_CONG   = 1,
  T_SEMI_SUBTYPE_ENSEMI = 2
};

// Get a new GAP Obj containing a pointer to a C++ class of type Class

template <typename Class>
inline Obj OBJ_CLASS(Class* cpp_class, t_semi_subtype_t type, size_t size = 2) {
  Obj o          = NewBag(T_SEMI, size * sizeof(Obj));
  ADDR_OBJ(o)[0] = (Obj) type;
  ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(cpp_class);
  return o;
}

// Get a pointer to a C++ object of type Class from GAP Obj of type T_SEMI

template <typename Class>
inline Class CLASS_OBJ(Obj o, size_t pos = 1) {
  return reinterpret_cast<Class>(ADDR_OBJ(o)[pos]);
}

// Get the t_semi_subtype_t out of the T_SEMI Obj

inline t_semi_subtype_t SUBTYPE_OF_T_SEMI(Obj o) {
  SEMIGROUPS_ASSERT(TNUM_OBJ(o) == T_SEMI);
  return static_cast<t_semi_subtype_t>(reinterpret_cast<UInt>(ADDR_OBJ(o)[0]));
}

// Imported types and functions from the library

extern Obj SEMIGROUPS;
extern Obj HTValue;
extern Obj HTAdd;
extern Obj Pinfinity;
extern Obj Ninfinity;
extern Obj IsBooleanMat;
extern Obj BooleanMatType;
extern Obj IsMatrixOverSemiring;
extern Obj DimensionOfMatrixOverSemiring;
extern Obj IsTropicalMatrix;
extern Obj MaxPlusMatrixType;
extern Obj IsMaxPlusMatrix;
extern Obj MinPlusMatrixType;
extern Obj IsMinPlusMatrix;
extern Obj TropicalMinPlusMatrixType;
extern Obj IsTropicalMinPlusMatrix;
extern Obj TropicalMaxPlusMatrixType;
extern Obj IsTropicalMaxPlusMatrix;
extern Obj ProjectiveMaxPlusMatrixType;
extern Obj IsProjectiveMaxPlusMatrix;
extern Obj IsNTPMatrix;
extern Obj NTPMatrixType;
extern Obj IsIntegerMatrix;
extern Obj IntegerMatrixType;
extern Obj IsPBR;
extern Obj DegreeOfPBR;
extern Obj TYPES_PBR;
extern Obj TYPE_PBR;

extern Obj TYPE_BIPART;
extern Obj TYPES_BIPART;
extern Obj FROPIN;
extern Obj GeneratorsOfMagma;

extern Obj IsSemigroup;
extern Obj IsSemigroupIdeal;
extern Obj IsActingSemigroup;

#endif  // SEMIGROUPS_SRC_PKG_H_
