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

// This file contains specialisations of gapbind14::to_cpp for various types
// defined in the GAP package Semigroups and the corresponding types in
// libsemigroups.

#ifndef SEMIGROUPS_SRC_TO_CPP_HPP_
#define SEMIGROUPS_SRC_TO_CPP_HPP_

// Standard library
#include <algorithm>      // for sort
#include <memory>         // for unique_ptr
#include <string>         // for string
#include <type_traits>    // for decay_t, enable_if_t, is_same
#include <unordered_map>  // for unordered_map
#include <utility>        // for pair
#include <vector>         // for vector

// GAP headers
#include "compiled.h"

// Semigroups package headers
#include "bipart.hpp"            // for bipart_get_cpp
#include "froidure-pin.hpp"      // for WBMat8
#include "pkg.hpp"               // for IsInfinity etc
#include "semigroups-debug.hpp"  // for SEMIGROUPS_ASSERT

// gapbind14 headers
#include "gapbind14/gapbind14.hpp"

// libsemigroups headers
#include "libsemigroups/adapters.hpp"   // for Degree
#include "libsemigroups/bipart.hpp"     // for Bipartition
#include "libsemigroups/bmat8.hpp"      // for BMat8
#include "libsemigroups/cong-intf.hpp"  // for congruence_kind
#include "libsemigroups/cong.hpp"       // for Congruence
#include "libsemigroups/constants.hpp"  // for constants
#include "libsemigroups/debug.hpp"      // for LIBSEMIGROUPS_ASSERT
#include "libsemigroups/matrix.hpp"     // for validate etc
#include "libsemigroups/pbr.hpp"        // for PBR
#include "libsemigroups/transf.hpp"     // for IsPPerm, IsTransf

using libsemigroups::IsBMat;
using libsemigroups::IsIntMat;
using libsemigroups::IsMatrix;
using libsemigroups::IsMaxPlusMat;
using libsemigroups::IsMaxPlusTruncMat;
using libsemigroups::IsMinPlusMat;
using libsemigroups::IsMinPlusTruncMat;
using libsemigroups::IsNTPMat;
using libsemigroups::IsProjMaxPlusMat;

using libsemigroups::BMat8;
using semigroups::WBMat8;

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

namespace semigroups {
  NTPSemiring<> const* semiring(size_t threshold, size_t period);

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
  // Constants
  ////////////////////////////////////////////////////////////////////////

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
  struct to_cpp<T, std::enable_if_t<IsBMat<std::decay_t<T>>>> {
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
      GAPBIND14_TRY(libsemigroups::validate(x));
      return x;
    }
  };

  template <typename T>
  struct to_cpp<
      T,
      std::enable_if_t<std::is_same<WBMat8, std::decay_t<T>>::value>> {
    WBMat8 operator()(Obj o) {
      if (CALL_1ARGS(IsBooleanMat, o) != True) {
        ErrorQuit("expected boolean matrix but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
      // else if (LEN_LIST(o) > 8) {
      //  ErrorQuit("expected boolean matrix of dimension at most 8 got %s!",
      //            (Int) LEN_LIST(o),
      //            0L);
      // }
      SEMIGROUPS_ASSERT(LEN_PLIST(o) > 0);

      Obj row = ELM_PLIST(o, 1);
      SEMIGROUPS_ASSERT(IS_PLIST(row) || IS_BLIST_REP(row));
      size_t m = (IS_BLIST_REP(row) ? LEN_BLIST(row) : LEN_PLIST(row));
      BMat8  x(0);

      for (size_t i = 0; i < m; i++) {
        row = ELM_PLIST(o, i + 1);
        if (!IS_BLIST_REP(row)) {
          ConvBlist(row);
        }
        for (size_t j = 0; j < m; j++) {
          if (ELM_BLIST(row, j + 1) == True) {
            x.set(i, j, 1);
          }
        }
      }
      return std::make_pair(x, m);
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // IntMat + MaxPlusMat + MinPlusMat
  ////////////////////////////////////////////////////////////////////////

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
      GAPBIND14_TRY(libsemigroups::validate(x));
    }
  }  // namespace detail

  template <typename T>
  struct to_cpp<T,
                std::
                    enable_if_t<
                        IsIntMat<std::decay_t<
                            T>> || IsMaxPlusMat<std::decay_t<T>> || IsMinPlusMat<std::decay_t<T>> || IsProjMaxPlusMat<std::decay_t<T>>>> {  // NOLINT(whitespace/line_length)
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
      std::enable_if_t<IsMaxPlusTruncMat<std::decay_t<
                           T>> || IsMinPlusTruncMat<std::decay_t<T>>>> {
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

      // TODO(later) something is wrong with LEN_LIST here
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
  struct to_cpp<T, std::enable_if_t<IsNTPMat<std::decay_t<T>>>> {
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
      GAPBIND14_TRY(libsemigroups::validate(x));
      return x;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // congruence_kind
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_cpp<
      T,
      std::enable_if_t<std::is_same<std::decay_t<T>,
                                    libsemigroups::congruence_kind>::value>> {
    using cpp_type                          = std::decay_t<T>;
    static gap_tnum_type constexpr gap_type = T_STRING;

    std::decay_t<T> operator()(Obj o) {
      if (TNUM_OBJ(o) != T_STRING && TNUM_OBJ(o) != T_STRING + IMMUTABLE) {
        ErrorQuit("expected string but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
      using libsemigroups::congruence_kind;
      std::string stype = std::string(CSTR_STRING(o));
      if (stype == "left") {
        return congruence_kind::left;
      } else if (stype == "right") {
        return congruence_kind::right;
      } else if (stype == "2-sided") {
        return congruence_kind::twosided;
      } else {
        ErrorQuit("Unrecognised type %s", (Int) stype.c_str(), 0L);
      }
    }
  };

  template <typename T>
  struct to_cpp<T,
                std::enable_if_t<std::is_same<
                    std::decay_t<T>,
                    libsemigroups::Congruence::options::runners>::value>> {
    using cpp_type = std::decay_t<T>;

    std::decay_t<T> operator()(Obj o) {
      using runners = libsemigroups::Congruence::options::runners;
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
}  // namespace gapbind14
#endif  // SEMIGROUPS_SRC_TO_CPP_HPP_
