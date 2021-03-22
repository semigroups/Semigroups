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

// This file contains specialisations of gapbind14::to_gap and
// gapbind14::to_cpp for various types defined in the GAP package Semigroups
// and the corresponding types in libsemigroups.

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
#include "semigroups-config.h"
#include "semigroups-debug.h"

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

using libsemigroups::MaxPlusTruncSemiring;
using libsemigroups::MinPlusTruncSemiring;
using libsemigroups::NTPSemiring;

using libsemigroups::IsBipartition;
using libsemigroups::IsPPerm;
using libsemigroups::IsTransf;

using libsemigroups::NegativeInfinity;
using libsemigroups::PositiveInfinity;
using libsemigroups::UNDEFINED;

namespace semigroups {
  NTPSemiring<> const* semiring(size_t threshold, size_t period) {
    static std::unordered_map<std::pair<size_t, size_t>,
                              std::unique_ptr<NTPSemiring<> const>,
                              libsemigroups::Hash<std::pair<size_t, size_t>>>
        cache;

    auto it = cache.find(std::make_pair(threshold, period));
    if (it == cache.end()) {
      it = cache
               .emplace(
                   std::make_pair(threshold, period),
                   std::make_unique<NTPSemiring<> const>(threshold, period))
               .first;
    }
    return it->second.get();
  }

  template <typename T>
  T const* semiring(size_t threshold) {
    static std::unordered_map<size_t, std::unique_ptr<T const>> cache;
    auto it = cache.find(threshold);
    if (it == cache.end()) {
      it = cache.emplace(threshold, std::make_unique<T const>(threshold)).first;
    }
    return it->second.get();
  }
}  // namespace semigroups

namespace gapbind14 {
  ////////////////////////////////////////////////////////////////////////
  // Pinfinity <-> libsemigroups::POSITIVE_INFINITY
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
  struct to_cpp<T, std::enable_if_t<std::is_same<T, PositiveInfinity>::value>> {
    using cpp_type                          = T;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;
    auto operator()(Obj x) {
      if (CALL_1ARGS(IsInfinity, x) != True) {
        ErrorQuit("expected object satisfying IsInfinity, found %s",
                  (Int) TNAM_OBJ(x),
                  0L);
      }
      return libsemigroups::POSITIVE_INFINITY;
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

  template <typename T>
  struct to_cpp<T, std::enable_if_t<std::is_same<T, NegativeInfinity>::value>> {
    using cpp_type                          = T;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;
    auto operator()(Obj x) {
      if (CALL_1ARGS(IsNegInfinity, x) != True) {
        ErrorQuit("expected object satisfying IsNegInfinity, found %s",
                  (Int) TNAM_OBJ(x),
                  0L);
      }
      return libsemigroups::NEGATIVE_INFINITY;
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

  template <typename T>
  struct to_cpp<T, std::enable_if_t<IsBMat<T>>> {
    using cpp_type                          = std::decay_t<T>;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;

    std::decay_t<T> operator()(Obj o) {
      if (CALL_1ARGS(IsBooleanMat, o) != True) {
        ErrorQuit("expected boolean matrix but got %s!", (Int) TNAM_OBJ(o), 0L);
      }

      SEMIGROUPS_ASSERT(LEN_PLIST(o) > 0);

      Obj row = ELM_PLIST(o, 1);
      SEMIGROUPS_ASSERT(IS_PLIST(row) || IS_BLIST_REP(row));
      size_t          m = (IS_BLIST_REP(row) ? LEN_BLIST(row) : LEN_PLIST(row));
      std::decay_t<T> x(m, m);

      for (size_t i = 0; i < m; i++) {
        row = ELM_PLIST(o, i + 1);
        if (!IS_BLIST_REP(row)) {
          ConvBlist(row);
        }
        for (size_t j = 0; j < m; j++) {
          if (ELM_BLIST(row, j + 1) == True) {
            x(i, j) = 1;
          }
        }
      }
      GAPBIND14_TRY(libsemigroups::matrix_validate(x));
      return x;
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

  namespace detail {
    template <typename T>
    void init_cpp_matrix(T& x, Obj o, size_t const m) {
      using scalar_type = typename T::scalar_type;
      for (size_t i = 0; i < m; i++) {
        Obj row = ELM_PLIST(o, i + 1);
        for (size_t j = 0; j < m; j++) {
          Obj         val = ELM_PLIST(row, j + 1);
          scalar_type itm;
          if (CALL_1ARGS(IsInfinity, val) != True
              && CALL_1ARGS(IsNegInfinity, val) != True) {
            itm = to_cpp<scalar_type>()(val);
          } else if (CALL_1ARGS(IsInfinity, val) == True) {
            itm = to_cpp<PositiveInfinity>()(val);
          } else if (CALL_1ARGS(IsNegInfinity, val) == True) {
            itm = to_cpp<NegativeInfinity>()(val);
          }
          x(i, j) = itm;
        }
      }
      GAPBIND14_TRY(libsemigroups::matrix_validate(x));
    }
  }  // namespace detail

  template <typename T>
  struct to_cpp<
      T,
      std::enable_if_t<
          IsIntMat<
              T> || IsMaxPlusMat<T> || IsMinPlusMat<T> || IsProjMaxPlusMat<T>>> {
    using cpp_type                          = std::decay_t<T>;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;

    std::decay_t<T> operator()(Obj o) {
      using scalar_type   = typename std::decay_t<T>::scalar_type;
      using semiring_type = typename std::decay_t<T>::semiring_type;

      if (IsIntMat<std::decay_t<T>>) {
        if (CALL_1ARGS(IsIntegerMatrix, o) != True) {
          ErrorQuit("expected integer matrix found %s!", (Int) TNAM_OBJ(o), 0L);
        }
      } else if (IsMaxPlusMat<std::decay_t<T>>) {
        if (CALL_1ARGS(IsMaxPlusMatrix, o) != True) {
          ErrorQuit(
              "expected max-plus matrix found %s!", (Int) TNAM_OBJ(o), 0L);
        }
      } else if (IsMinPlusMat<std::decay_t<T>>) {
        if (CALL_1ARGS(IsMinPlusMatrix, o) != True) {
          ErrorQuit(
              "expected min-plus matrix found %s!", (Int) TNAM_OBJ(o), 0L);
        }
      } else if (IsProjMaxPlusMat<std::decay_t<T>>) {
        if (CALL_1ARGS(IsProjectiveMaxPlusMatrix, o) != True) {
          ErrorQuit("expected projective max-plus matrix found %s!",
                    (Int) TNAM_OBJ(o),
                    0L);
        }
      }
      if (LEN_PLIST(o) == 0) {
        ErrorQuit("expected matrix of non-zero dimension!", 0L, 0L);
      }

      size_t          m = LEN_PLIST(ELM_PLIST(o, 1));
      std::decay_t<T> x(m, m);
      detail::init_cpp_matrix(x, o, m);
      return x;
    }
  };

  template <typename T>
  struct to_cpp<
      T,
      std::enable_if_t<IsMaxPlusTruncMat<T> || IsMinPlusTruncMat<T>>> {
    using cpp_type                          = std::decay_t<T>;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;
    std::decay_t<T> operator()(Obj o) {
      LIBSEMIGROUPS_ASSERT(
          IsMinPlusTruncMat<
              std::decay_t<T>> || IsMaxPlusTruncMat<std::decay_t<T>>);
      if (IsMinPlusTruncMat<std::decay_t<T>>) {
        if (CALL_1ARGS(IsTropicalMinPlusMatrix, o) != True) {
          ErrorQuit("expected tropical min-plus matrix, found %s!",
                    (Int) TNAM_OBJ(o),
                    0L);
        }
      } else if (IsMaxPlusTruncMat<std::decay_t<T>>) {
        if (CALL_1ARGS(IsTropicalMaxPlusMatrix, o) != True) {
          ErrorQuit("expected tropical max-plus matrix, found %s!",
                    (Int) TNAM_OBJ(o),
                    0L);
        }
      } else {
        ErrorQuit("expected tropical max/min-plus matrix, found %s!",
                  (Int) TNAM_OBJ(o),
                  0L);
      }

      // FIXME something is wrong with LEN_LIST here
      // if (LEN_LIST(o) == 0) {
      //  ErrorQuit("expected matrix of non-zero dimension!", 0L, 0L);
      //}
      size_t m            = LEN_PLIST(ELM_PLIST(o, 1));
      using semiring_type = typename std::decay_t<T>::semiring_type;
      auto const* sr      = semigroups::semiring<semiring_type>(
          static_cast<size_t>(INT_INTOBJ(ELM_PLIST(o, m + 1))));
      std::decay_t<T> x(sr, m, m);
      detail::init_cpp_matrix(x, o, m);
      return x;
    }
  };

  template <typename T>
  struct to_cpp<T, std::enable_if_t<IsNTPMat<T>>> {
    using cpp_type                          = std::decay_t<T>;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;
    std::decay_t<T> operator()(Obj o) {
      if (CALL_1ARGS(IsNTPMatrix, o) != True) {
        ErrorQuit("expected ntp matrix, found %s!", (Int) TNAM_OBJ(o), 0L);
      } else if (LEN_PLIST(o) == 0) {
        ErrorQuit("expected matrix of non-zero dimension!", 0L, 0L);
      }
      size_t      m  = LEN_PLIST(ELM_PLIST(o, 1));
      auto const* sr = semigroups::semiring(
          static_cast<size_t>(INT_INTOBJ(ELM_PLIST(o, m + 1))),
          static_cast<size_t>(INT_INTOBJ(ELM_PLIST(o, m + 2))));
      std::decay_t<T> x(sr, m, m);
      using scalar_type = typename std::decay_t<T>::scalar_type;
      for (size_t i = 0; i < m; i++) {
        Obj row = ELM_PLIST(o, i + 1);
        for (size_t j = 0; j < m; j++) {
          x(i, j) = to_cpp<scalar_type>()(ELM_PLIST(row, j + 1));
        }
      }
      GAPBIND14_TRY(libsemigroups::matrix_validate(x));
      return x;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // congruence_type
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_cpp<
      T,
      std::enable_if_t<std::is_same<std::decay_t<T>,
                                    libsemigroups::congruence_type>::value>> {
    using cpp_type                          = std::decay_t<T>;
    static gap_tnum_type constexpr gap_type = T_STRING;

    std::decay_t<T> operator()(Obj o) {
      if (TNUM_OBJ(o) != T_STRING && TNUM_OBJ(o) != T_STRING + IMMUTABLE) {
        ErrorQuit("expected string but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
      using libsemigroups::congruence_type;
      std::string stype = std::string(CSTR_STRING(o));
      if (stype == "left") {
        return congruence_type::left;
      } else if (stype == "right") {
        return congruence_type::right;
      } else if (stype == "twosided") {
        return congruence_type::twosided;
      } else {
        ErrorQuit("Unrecognised type %s", (Int) stype.c_str(), 0L);
      }
    }
  };

  template <typename T>
  struct to_cpp<T,
                std::enable_if_t<std::is_same<
                    std::decay_t<T>,
                    libsemigroups::Congruence::policy::runners>::value>> {
    using cpp_type = std::decay_t<T>;

    std::decay_t<T> operator()(Obj o) {
      using runners = libsemigroups::Congruence::policy::runners;
      if (TNUM_OBJ(o) != T_STRING && TNUM_OBJ(o) != T_STRING + IMMUTABLE) {
        ErrorQuit("expected string but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
      std::string stype = std::string(CSTR_STRING(o));
      if (stype == "none") {
        return runners::none;
      } else if (stype == "standard") {
        return runners::standard;
      } else {
        ErrorQuit("Unrecognised type %s", (Int) stype.c_str(), 0L);
      }
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

    template <typename S, typename T>
    void to_cpp_transf(S& x, T* ptr, size_t const N) {
      static_assert(
          std::is_same<T, UInt2>::value || std::is_same<T, UInt4>::value,
          "the template parameter T must be the same as UInt2 or UInt4");
      SEMIGROUPS_ASSERT(N <= libsemigroups::Degree<S>()(x));
      T i;
      for (i = 0; i < N; ++i) {
        x[i] = ptr[i];
      }
      for (; i < libsemigroups::Degree<S>()(x); ++i) {
        x[i] = i;
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

  template <typename T>
  struct to_cpp<T,
                std::enable_if_t<
                    IsTransf<std::decay_t<T>>
#ifdef LIBSEMIGROUPS_HPCOMBI_ENABLED
                    || std::is_same<std::decay_t<T>, HPCombi::Transf16>::value
#endif
                    >> {
    using cpp_type = std::decay_t<T>;

    std::decay_t<T> operator()(Obj t) {
      if (!IS_PLIST(t)) {
        ErrorQuit("expected list, got %s", (Int) TNAM_OBJ(t), 0L);
      } else if (LEN_PLIST(t) != 2) {
        ErrorQuit("expected list of length 2, but it has length %d",
                  (Int) LEN_PLIST(t),
                  0L);
      } else if (!IS_TRANS(ELM_PLIST(t, 1))) {
        ErrorQuit("expected transforamtion in position 1, got %s",
                  (Int) TNAM_OBJ(t),
                  0L);
      } else if (!IS_INTOBJ(ELM_PLIST(t, 2))) {
        ErrorQuit(
            "expected integer in position 2, got %s", (Int) TNAM_OBJ(t), 0L);
      }

      Obj    x = ELM_PLIST(t, 1);
      size_t N = INT_INTOBJ(ELM_PLIST(t, 2));

      if (INT_INTOBJ(CALL_1ARGS(LARGEST_MOVED_PT_TRANS, x)) > N) {
        ErrorQuit(
            "expected transformation with largest moved point not greater than "
            "%d, found %d",
            (Int) N,
            (Int) DEG_TRANS(x));
      }

      std::decay_t<T> result;
      if (IsTransf<std::decay_t<T>>) {
        result = std::decay_t<T>(N);
      }
      if (TNUM_OBJ(x) == T_TRANS2) {
        detail::to_cpp_transf(
            result, ADDR_TRANS2(x), std::min(DEG_TRANS(x), N));
      } else if (TNUM_OBJ(x) == T_TRANS4) {
        detail::to_cpp_transf(
            result, ADDR_TRANS4(x), std::min(DEG_TRANS(x), N));
      } else {
        // in case of future changes to transf in GAP
        ErrorQuit("transformation degree too high!", 0L, 0L);
      }
      return result;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Partial perms
  ////////////////////////////////////////////////////////////////////////

  namespace detail {
    template <typename S, typename T>
    void to_cpp_pperm(S& x, T* ptr, size_t const N) {
      static_assert(
          std::is_same<T, UInt2>::value || std::is_same<T, UInt4>::value,
          "the template parameter T must be the same as UInt2 or UInt4");
      SEMIGROUPS_ASSERT(N <= libsemigroups::Degree<S>()(x));
      T i;
      T undef;
      if (IsPPerm<S>) {
        undef = UNDEFINED;
      } else {
        undef = 0xFF;
      }
      for (i = 0; i < N; ++i) {
        if (ptr[i] == 0) {
          x[i] = undef;
        } else {
          x[i] = ptr[i] - 1;
        }
      }
      for (; i < libsemigroups::Degree<S>()(x); ++i) {
        x[i] = undef;
      }
    }

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

#ifdef LIBSEMIGROUPS_HPCOMBI_ENABLED
    template <typename T>
    auto construct_pperm(size_t n)
        -> std::enable_if_t<std::is_same<T, HPCombi::PPerm16>::value, T> {
      SEMIGROUPS_ASSERT(n <= 16);
      return T();
    }

    template <typename T>
    auto construct_pperm(size_t n)
        -> std::enable_if_t<!std::is_same<T, HPCombi::PPerm16>::value, T> {
      return T(n);
    }
#else
    template <typename T>
    T construct_pperm(size_t n) {
      return T(n);
    }
#endif

  }  // namespace detail

  template <typename T>
  struct to_cpp<
      T,
      std::enable_if_t<IsPPerm<std::decay_t<T>>
#ifdef LIBSEMIGROUPS_HPCOMBI_ENABLED
                       || std::is_same<std::decay_t<T>, HPCombi::PPerm16>::value
#endif
                       >> {
    using cpp_type = std::decay_t<T>;

    std::decay_t<T> operator()(Obj t) const {
      if (!IS_PLIST(t)) {
        ErrorQuit("expected list, got %s", (Int) TNAM_OBJ(t), 0L);
      } else if (LEN_PLIST(t) != 2) {
        ErrorQuit("expected list of length 2, but it has length %d",
                  (Int) LEN_PLIST(t),
                  0L);
      } else if (!IS_PPERM(ELM_PLIST(t, 1))) {
        ErrorQuit("expected partial perm in position 1, got %s",
                  (Int) TNAM_OBJ(t),
                  0L);
      } else if (!IS_INTOBJ(ELM_PLIST(t, 2))) {
        ErrorQuit(
            "expected integer in position 2, got %s", (Int) TNAM_OBJ(t), 0L);
      }

      Obj   x = ELM_PLIST(t, 1);
      UInt4 N = INT_INTOBJ(ELM_PLIST(t, 2));
      UInt4 M = 0;
      if (TNUM_OBJ(x) == T_PPERM2) {
        M = lmp<UInt2>(ADDR_PPERM2(x), DEG_PPERM2(x));
      } else if (TNUM_OBJ(x) == T_PPERM4) {
        M = lmp<UInt4>(ADDR_PPERM4(x), DEG_PPERM4(x));
      } else {
        ErrorQuit("partial perm degree too high!", 0L, 0L);
      }

      if (M > N) {
        ErrorQuit(
            "expected partial perm where the largest point in the domain and "
            "range is %d, found %d",
            (Int) N,
            (Int) M);
      }

      std::decay_t<T> result(detail::construct_pperm<cpp_type>(N));
      if (TNUM_OBJ(x) == T_PPERM2) {
        detail::to_cpp_pperm(result, ADDR_PPERM2(x), DEG_PPERM2(x));
      } else if (TNUM_OBJ(x) == T_PPERM4) {
        detail::to_cpp_pperm(result, ADDR_PPERM4(x), DEG_PPERM4(x));
      } else {
        // in case of future changes to partial perms in GAP
        ErrorQuit("partial perm degree too high!", 0L, 0L);
      }
      return result;
    }

    template <typename S>
    static S lmp(S* ptr, S deg) {
      S result = 0;
      for (S i = 0; i < deg; i++) {
        if (ptr[i] != 0) {
          result = std::max(result, i);
          result = std::max(result, S(ptr[i] - 1));
        }
      }
      return result;
    }
  };

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
  struct to_cpp<T, std::enable_if_t<IsBipartition<std::decay_t<T>>>> {
    Bipartition& operator()(Obj x) const {
      if (TNUM_OBJ(x) != T_BIPART) {
        ErrorQuit("expected a bipartition, got %s", (Int) TNAM_OBJ(x), 0L);
      }
      return *bipart_get_cpp(x);
    }
  };

  template <typename T>
  struct to_gap<T, std::enable_if_t<IsBipartition<std::decay_t<T>>>> {
    Obj operator()(Bipartition const& x) const {
      return bipart_new_obj(new Bipartition(x));
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // PBR
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_cpp<T, std::enable_if_t<libsemigroups::IsPBR<std::decay_t<T>>>> {
    libsemigroups::PBR operator()(Obj x) const {
      if (CALL_1ARGS(IsPBR, x) != True) {
        ErrorQuit("expected a PBR, got %s", (Int) TNAM_OBJ(x), 0L);
      } else if (LEN_PLIST(x) == 0) {
        ErrorQuit("expected a PBR of degree > 0", 0L, 0L);
      }

      size_t m = INT_INTOBJ(ELM_PLIST(x, 1));
      // The next assertion fails when it shouldn't FIXME
      // SEMIGROUPS_ASSERT(LEN_PLIST(x) == 2 * m + 1);

      libsemigroups::PBR result(m);

      for (u_int32_t i = 0; i < 2 * m; i++) {
        Obj adj = ELM_PLIST(x, i + 2);
        for (u_int32_t j = 1; j <= LEN_PLIST(adj); j++) {
          result[i].push_back(INT_INTOBJ(ELM_PLIST(adj, j)) - 1);
          // assumes that adj is duplicate-free
        }
        std::sort(result[i].begin(), result[i].end());
      }
      return result;
    }
  };

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

  template <typename T>
  struct to_cpp<T, std::enable_if_t<libsemigroups::IsDynamicArray2<T>>> {
    std::decay_t<T> operator()(Obj x) {
      if (!IS_LIST(x)) {
        ErrorQuit("expected a list, got %s", (Int) TNAM_OBJ(x), 0L);
      }
      for (size_t i = 0; i < LEN_LIST(x); ++i) {
        Obj item = ELM_LIST(x, i + 1);
        if (!IS_HOMOG_LIST(item)) {
          ErrorQuit(
              "expected a list of homog. lists, but got %s in position %d",
              (Int) TNAM_OBJ(x),
              (Int) i + 1);
        } else if (i > 0 && LEN_LIST(ELM_LIST(x, i)) != LEN_LIST(item)) {
          ErrorQuit("expected a list of lists of equal length, but found a "
                    "list of length %d in position %d",
                    (Int) LEN_LIST(item),
                    (Int) i + 1);
        }
      }
      size_t nr_cols = (LEN_LIST(x) == 0 ? 0 : LEN_LIST(ELM_LIST(x, 1)));
      std::decay_t<T> result(nr_cols, LEN_LIST(x));
      for (size_t i = 0; i < LEN_LIST(x); ++i) {
        Obj item = ELM_LIST(x, i + 1);
        for (size_t j = 0; j < nr_cols; ++j) {
          result.set(i,
                     j,
                     to_cpp<typename std::decay_t<T>::value_type>()(
                         ELM_LIST(item, j + 1)));
        }
      }
      return result;
    }
  };

  template <typename T>
  struct to_gap<T, std::enable_if_t<libsemigroups::IsDynamicArray2<T>>> {
    Obj operator()(std::decay_t<T> const& da) {
      gap_list_t result = NEW_PLIST(T_PLIST_TAB_RECT, da.nr_rows());
      // this is intentionally not IMMUTABLE
      SET_LEN_PLIST(result, da.nr_rows());

      for (size_t i = 0; i < da.nr_rows(); ++i) {
        gap_list_t next = NEW_PLIST(T_PLIST_CYC, da.nr_cols());
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
