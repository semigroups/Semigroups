//
// Semigroups package for GAP
// Copyright (C) 2020-21 James D. Mitchell
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

// This file contains specialisations of gapbind14::to_gap for various types
// defined in the GAP package Semigroups and the corresponding types in
// libsemigroups.

#ifndef SEMIGROUPS_SRC_TO_GAP_HPP_
#define SEMIGROUPS_SRC_TO_GAP_HPP_

// Standard library
#include <algorithm>      // for sort
#include <limits>         // for numeric_limits
#include <memory>         // for unique_ptr
#include <string>         // for string
#include <unordered_map>  // for unordered_map
#include <utility>        // for pair
#include <vector>         // for vector

// GAP headers
#include "compiled.h"

// Semigroups package headers
#include "bipart.hpp"
#include "semigroups-config.hpp"
#include "semigroups-debug.hpp"

// gapbind14 headers
#include "gapbind14/gapbind14.hpp"

// libsemigroups headers
#include "libsemigroups/bipart.hpp"
#include "libsemigroups/cong.hpp"
#include "libsemigroups/libsemigroups-config.hpp"  // for LIBSEMIGROUPS_HPCOMBI_ENABLED
#include "libsemigroups/matrix.hpp"
#include "libsemigroups/pbr.hpp"
#include "libsemigroups/transf.hpp"

using libsemigroups::IsBMat;
using libsemigroups::IsIntMat;
using libsemigroups::IsMatrix;
using libsemigroups::IsMaxPlusMat;
using libsemigroups::IsMaxPlusTruncMat;
using libsemigroups::IsMinPlusMat;
using libsemigroups::IsMinPlusTruncMat;
using libsemigroups::IsNTPMat;
using libsemigroups::IsProjMaxPlusMat;

using libsemigroups::Bipartition;
using libsemigroups::MaxPlusTruncSemiring;
using libsemigroups::MinPlusTruncSemiring;
using libsemigroups::NTPSemiring;

using libsemigroups::IsBipartition;
using libsemigroups::IsPPerm;
using libsemigroups::IsTransf;

using libsemigroups::NegativeInfinity;
using libsemigroups::PositiveInfinity;
using libsemigroups::UNDEFINED;

namespace gapbind14 {
  ////////////////////////////////////////////////////////////////////////
  // Constants
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_gap<T, std::enable_if_t<std::is_same<T, PositiveInfinity>::value>> {
    using cpp_type                          = T;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;
    Obj operator()(T const x) {
      return Pinfinity;
    }
  };

  template <typename T>
  struct to_gap<T, std::enable_if_t<std::is_same<T, NegativeInfinity>::value>> {
    using cpp_type                          = T;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;
    Obj operator()(T const x) {
      return Ninfinity;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // BMat <-> IsBooleanMat
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_gap<T const&, std::enable_if_t<IsBMat<T>>> {
    using cpp_type                          = T;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;

    Obj operator()(T const& x) {
      size_t n = x.number_of_rows();
      Obj    o = NEW_PLIST(T_PLIST_TAB_RECT, n);
      SET_LEN_PLIST(o, n);

      for (size_t i = 0; i < n; i++) {
        Obj blist = NewBag(T_BLIST, SIZE_PLEN_BLIST(n));
        SET_LEN_BLIST(blist, n);
        for (size_t j = 0; j < n; j++) {
          if (x(i, j)) {
            SET_BIT_BLIST(blist, j + 1);
          }
        }
        MakeImmutable(blist);
        SET_ELM_PLIST(o, i + 1, blist);
        CHANGED_BAG(o);
      }

      SET_TYPE_POSOBJ(o, BooleanMatType);
      RetypeBag(o, T_POSOBJ);
      CHANGED_BAG(o);
      return o;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // IntMat + MaxPlusMat + MinPlusMat
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_gap<T const&,
                std::enable_if_t<IsMatrix<T> && !IsBMat<T> && !IsNTPMat<T>>> {
    using cpp_type                          = T;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;

    Obj operator()(T const& x) {
      using scalar_type = typename T::scalar_type;
      using libsemigroups::matrix_threshold;

      Obj gap_type;

      size_t n      = x.number_of_rows();
      size_t extra  = 0;
      Obj    result = NEW_PLIST(T_PLIST, n + 1);

      if (IsIntMat<T>) {
        gap_type = IntegerMatrixType;
      } else if (IsMaxPlusMat<T>) {
        gap_type = MaxPlusMatrixType;
      } else if (IsMinPlusMat<T>) {
        gap_type = MinPlusMatrixType;
      } else if (IsMaxPlusTruncMat<T>) {
        extra    = 1;
        gap_type = TropicalMaxPlusMatrixType;
        SET_ELM_PLIST(
            result, n + 1, to_gap<scalar_type>()(matrix_threshold(x)));
      } else if (IsMinPlusTruncMat<T>) {
        extra    = 1;
        gap_type = TropicalMinPlusMatrixType;
        SET_ELM_PLIST(
            result, n + 1, to_gap<scalar_type>()(matrix_threshold(x)));
      } else if (IsProjMaxPlusMat<T>) {
        gap_type = ProjectiveMaxPlusMatrixType;
      }

      SET_LEN_PLIST(result, n + extra);

      for (size_t i = 0; i < n; i++) {
        Obj row = NEW_PLIST_IMM(T_PLIST_CYC, n);
        SET_LEN_PLIST(row, n);
        for (size_t j = 0; j < n; j++) {
          scalar_type const val = x(i, j);
          Obj               itm;
          if (val != libsemigroups::POSITIVE_INFINITY
              && val != libsemigroups::NEGATIVE_INFINITY) {
            itm = to_gap<scalar_type>()(val);
          } else if (val == libsemigroups::POSITIVE_INFINITY) {
            // TODO check if we're an integer matrix, and complain!
            itm = to_gap<PositiveInfinity>()(libsemigroups::POSITIVE_INFINITY);
          } else if (val == libsemigroups::NEGATIVE_INFINITY) {
            // TODO check if we're an integer matrix, and complain!
            itm = to_gap<NegativeInfinity>()(libsemigroups::NEGATIVE_INFINITY);
          }
          AssPlist(row, j + 1, itm);
        }
        AssPlist(result, i + 1, row);
      }

      RetypeBag(result, T_POSOBJ);
      SET_TYPE_POSOBJ(result, gap_type);
      CHANGED_BAG(result);
      return result;
    }
  };

  template <typename T>
  struct to_gap<T const&, std::enable_if_t<IsNTPMat<T>>> {
    using cpp_type                          = T;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;

    Obj operator()(T const& x) {
      using scalar_type = typename T::scalar_type;
      using libsemigroups::matrix_period;
      using libsemigroups::matrix_threshold;

      size_t n      = x.number_of_rows();
      Obj    result = NEW_PLIST(T_PLIST, n + 2);
      SET_LEN_PLIST(result, n + 2);
      SET_ELM_PLIST(result, n + 1, to_gap<scalar_type>()(matrix_threshold(x)));
      SET_ELM_PLIST(result, n + 2, to_gap<scalar_type>()(matrix_period(x)));

      for (size_t i = 0; i < n; i++) {
        Obj row = NEW_PLIST_IMM(T_PLIST_CYC, n);
        SET_LEN_PLIST(row, n);
        for (size_t j = 0; j < n; j++) {
          AssPlist(row, j + 1, to_gap<scalar_type>()(x(i, j)));
        }
        AssPlist(result, i + 1, row);
      }

      RetypeBag(result, T_POSOBJ);
      SET_TYPE_POSOBJ(result, NTPMatrixType);
      CHANGED_BAG(result);
      return result;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Transformations
  ////////////////////////////////////////////////////////////////////////

  namespace detail {
    // Initialise the GAP transformation "t" using the C++ transformation "x".
    template <typename T, typename S>
    void to_gap_transf(S const& x, Obj t) {
      static_assert(
          std::is_same<T, UInt2>::value || std::is_same<T, UInt4>::value,
          "the template parameter T must be the same as UInt2 or UInt4");
      SEMIGROUPS_ASSERT(IS_TRANS(t));
      // Check that x and t are compatible
      T* ptr = reinterpret_cast<T*>(static_cast<Obj*>(ADDR_OBJ(t) + 3));
      for (T i = 0; i < libsemigroups::Degree<S>()(x); ++i) {
        ptr[i] = x[i];
      }
    }
  }  // namespace detail

  template <typename T>
  struct to_gap<T const&,
                std::enable_if_t<IsTransf<std::decay_t<T>>
#ifdef LIBSEMIGROUPS_HPCOMBI_ENABLED
                                 || std::is_same<T, HPCombi::Transf16>::value
#endif
                                 >> {
    using cpp_type = T;

    Obj operator()(T const& x) {
      auto N      = libsemigroups::Degree<T>()(x);
      Obj  result = NEW_TRANS(N);
      if (N < 65536) {
        detail::to_gap_transf<UInt2>(x, result);
      } else if (N < std::numeric_limits<UInt4>::max()) {
        detail::to_gap_transf<UInt4>(x, result);
      } else {
        // in case of future changes to transformations in GAP
        ErrorQuit("transformation degree too high!", 0L, 0L);
      }
      return result;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Partial perms
  ////////////////////////////////////////////////////////////////////////

  namespace detail {
    // Initialise the GAP partial perm "t" using the C++ partial perm "x".
    template <typename T, typename S>
    void to_gap_pperm(S const& x, Obj t, size_t const N) {
      static_assert(
          std::is_same<T, UInt2>::value || std::is_same<T, UInt4>::value,
          "the template parameter T must be the same as UInt2 or UInt4");
      SEMIGROUPS_ASSERT(IS_PPERM(t));
      SEMIGROUPS_ASSERT(DEG_PPERM(t) == N);
      SEMIGROUPS_ASSERT(N <= libsemigroups::Degree<S>()(x));

      T* ptr = reinterpret_cast<T*>(static_cast<Obj*>(ADDR_OBJ(t)) + 2) + 1;
      T  undef;

      if (IsPPerm<S>) {
        undef = UNDEFINED;
      } else {
        undef = 0xFF;
      }

      for (T i = 0; i < N; i++) {
        if (x[i] == undef) {
          ptr[i] = 0;
        } else {
          ptr[i] = x[i] + 1;
        }
      }
    }
  }  // namespace detail

  template <typename T>
  struct to_gap<
      T,
      std::enable_if_t<IsPPerm<std::decay_t<T>>
#ifdef LIBSEMIGROUPS_HPCOMBI_ENABLED
                       || std::is_same<std::decay_t<T>, HPCombi::PPerm16>::value
#endif
                       >> {
    using cpp_type = std::decay_t<T>;

    Obj operator()(T const& x) {
      auto N = libsemigroups::Degree<cpp_type>()(x);

      // remove trailing 0s
      while (N > 0 && x[N - 1] == UNDEFINED) {
        N--;
      }

      Obj result = NEW_PPERM(N);
      if (N < 65536) {
        detail::to_gap_pperm<UInt2>(x, result, N);
      } else if (N < std::numeric_limits<UInt4>::max()) {
        detail::to_gap_pperm<UInt4>(x, result, N);
      } else {
        // in case of future changes to transformations in GAP
        ErrorQuit("partial perm degree too high!", 0L, 0L);
      }
      CHANGED_BAG(result);
      return result;
    }

    // FIXME this isn't right it depends on the codegree only and not the
    // degree
    static inline Obj NEW_PPERM(size_t deg) {
      if (deg < 65536) {
        return NEW_PPERM2(deg);
      } else {
        return NEW_PPERM4(deg);
      }
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Bipartition
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_gap<T, std::enable_if_t<IsBipartition<std::decay_t<T>>>> {
    Obj operator()(Bipartition const& x) const {
      return bipart_new_obj(new Bipartition(x));
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // PBR
  ////////////////////////////////////////////////////////////////////////

  // TODO(now) this is overcomplicated, we can just use to_gap<PBR, no?
  template <typename T>
  struct to_gap<T, std::enable_if_t<libsemigroups::IsPBR<std::decay_t<T>>>> {
    Obj operator()(libsemigroups::PBR const& x) const {
      Obj result = NEW_PLIST(T_PLIST, 2 * x.degree() + 1);
      // can't use T_PLIST_TAB/HOM here because some of the subplists might be
      // empty
      SET_LEN_PLIST(result, 2 * x.degree() + 1);
      SET_ELM_PLIST(result, 1, INTOBJ_INT(x.degree()));
      for (u_int32_t i = 0; i < 2 * x.degree(); i++) {
        Obj next = SUM(to_gap<std::vector<uint32_t>>()(x[i]), INTOBJ_INT(1));
        SET_ELM_PLIST(result, i + 2, next);
        CHANGED_BAG(result);
      }
      SET_TYPE_POSOBJ(result, get_gap_type(x.degree()));
      RetypeBag(result, T_POSOBJ);
      CHANGED_BAG(result);
      return result;
    }

    static Obj get_gap_type(size_t N) {
      N++;
      if (N > (size_t) LEN_PLIST(TYPES_PBR) || ELM_PLIST(TYPES_PBR, N) == 0) {
        CALL_1ARGS(TYPE_PBR, INTOBJ_INT(N - 1));
      }
      return ELM_PLIST(TYPES_PBR, N);
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // DynamicArray2
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_gap<T, std::enable_if_t<libsemigroups::IsDynamicArray2<T>>> {
    Obj operator()(std::decay_t<T> const& da) {
      Obj result = NEW_PLIST(T_PLIST_TAB_RECT, da.nr_rows());
      // this is intentionally not IMMUTABLE
      SET_LEN_PLIST(result, da.nr_rows());

      for (size_t i = 0; i < da.nr_rows(); ++i) {
        Obj next = NEW_PLIST(T_PLIST_CYC, da.nr_cols());
        // this is intentionally not IMMUTABLE
        SET_LEN_PLIST(next, da.nr_cols());
        for (size_t j = 0; j < da.nr_cols(); ++j) {
          SET_ELM_PLIST(
              next,
              j + 1,
              to_gap<typename std::decay_t<T>::value_type>()(da.get(i, j)));
        }
        SET_ELM_PLIST(result, i + 1, next);
        CHANGED_BAG(result);
      }
      return result;
    }
  };
}  // namespace gapbind14
#endif  // SEMIGROUPS_SRC_TO_GAP_HPP_
