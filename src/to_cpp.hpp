//
// Semigroups package for GAP
// Copyright (C) 2020-2022 James D. Mitchell
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
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

// This file contains specialisations of gapbind14::to_cpp for various types
// defined in the GAP package Semigroups and the corresponding types in
// libsemigroups.

#ifndef SEMIGROUPS_SRC_TO_CPP_HPP_
#define SEMIGROUPS_SRC_TO_CPP_HPP_

// Standard library
#include <algorithm>      // for max, min, sort
#include <cstddef>        // for size_t
#include <cstdint>        // for uint32_t
#include <exception>      // for exception
#include <functional>     // for __unwrap_reference<>::type
#include <memory>         // for make_unique, unique_ptr
#include <string>         // for string
#include <type_traits>    // for decay_t, is_same, conditional_t
#include <unordered_map>  // for operator==, unordered_map
#include <utility>        // for forward
#include <vector>         // for vector

// GAP headers
#include "compiled.h"

// Semigroups package headers
#include "bipart.hpp"            // for bipart_get_cpp
#include "froidure-pin.hpp"      // for WBMat8
#include "pkg.hpp"               // for IsInfinity etc
#include "semigroups-debug.hpp"  // for SEMIGROUPS_ASSERT

// gapbind14 headers
#include "gapbind14/cpp-fn.hpp"  // for GAPBIND14_TRY
#include "gapbind14/to_cpp.hpp"  // for to_cpp
#include "gapbind14/to_gap.hpp"  // for gap_tnum_type

// libsemigroups headers
#include "libsemigroups/adapters.hpp"    // for Degree
#include "libsemigroups/bmat8.hpp"       // for BMat8
#include "libsemigroups/cong.hpp"        // for Congruence
#include "libsemigroups/constants.hpp"   // for NegativeInfinity, PositiveIn...
#include "libsemigroups/containers.hpp"  // for DynamicArray2
#include "libsemigroups/matrix.hpp"      // for NTPMat, MaxPlusTruncMat, Min...
#include "libsemigroups/pbr.hpp"         // for PBR
#include "libsemigroups/transf.hpp"      // for PPerm, Transf, IsPPerm
#include "libsemigroups/types.hpp"       // for congruence_kind, congruence_...

namespace libsemigroups {
  class Bipartition;
}

using libsemigroups::BMat;
using libsemigroups::BMat8;
using semigroups::WBMat8;

using libsemigroups::IntMat;
using libsemigroups::MaxPlusMat;
using libsemigroups::MaxPlusTruncMat;
using libsemigroups::MinPlusMat;
using libsemigroups::MinPlusTruncMat;
using libsemigroups::NTPMat;
using libsemigroups::ProjMaxPlusMat;

using libsemigroups::NTPSemiring;

using libsemigroups::Bipartition;
using libsemigroups::PBR;

using libsemigroups::IsPPerm;
using libsemigroups::PPerm;
using libsemigroups::Transf;

using libsemigroups::congruence_kind;
using libsemigroups::NegativeInfinity;
using libsemigroups::PositiveInfinity;
using libsemigroups::UNDEFINED;

using libsemigroups::detail::DynamicArray2;

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

  template <>
  struct to_cpp<PositiveInfinity> {
    using cpp_type                          = PositiveInfinity;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;
    auto operator()(Obj x) const {
      if (CALL_1ARGS(IsInfinity, x) != True) {
        ErrorQuit("expected object satisfying IsInfinity, found %s",
                  (Int) TNAM_OBJ(x),
                  0L);
      }
      return libsemigroups::POSITIVE_INFINITY;
    }
  };

  template <>
  struct to_cpp<NegativeInfinity> {
    using cpp_type                          = NegativeInfinity;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;
    auto operator()(Obj x) const {
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

  template <>
  struct to_cpp<BMat<>> {
    using cpp_type                          = BMat<>;
    static gap_tnum_type constexpr gap_type = T_POSOBJ;

    BMat<> operator()(Obj o) const {
      if (CALL_1ARGS(IsBooleanMat, o) != True) {
        ErrorQuit("expected boolean matrix but got %s!", (Int) TNAM_OBJ(o), 0L);
      }

      SEMIGROUPS_ASSERT(LEN_PLIST(o) > 0);

      Obj row = ELM_PLIST(o, 1);
      SEMIGROUPS_ASSERT(IS_PLIST(row) || IS_BLIST_REP(row));
      size_t m = (IS_BLIST_REP(row) ? LEN_BLIST(row) : LEN_PLIST(row));
      BMat<> x(m, m);

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

  template <>
  struct to_cpp<BMat<> const&> : to_cpp<BMat<>> {};

  template <>
  struct to_cpp<WBMat8> {
    WBMat8 operator()(Obj o) const {
      if (CALL_1ARGS(IsBooleanMat, o) != True) {
        ErrorQuit("expected boolean matrix but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
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

  template <>
  struct to_cpp<WBMat8&> : to_cpp<WBMat8> {};

  template <>
  struct to_cpp<WBMat8 const&> : to_cpp<WBMat8> {};

  ////////////////////////////////////////////////////////////////////////
  // IntMat + MaxPlusMat + MinPlusMat
  ////////////////////////////////////////////////////////////////////////

  namespace detail {

    template <typename T, typename... TArgs>
    T make_matrix(size_t r, size_t c, TArgs&&... params) {
      return T(std::forward<TArgs>(params)..., r, c);
    }

    template <typename T, typename... TArgs>
    T init_cpp_matrix(Obj o, TArgs&&... params) {
      using scalar_type = typename T::scalar_type;
      if (LEN_PLIST(o) == 0) {
        ErrorQuit("expected matrix of non-zero dimension!", 0L, 0L);
      }

      size_t m = LEN_PLIST(ELM_PLIST(o, 1));
      T      x = make_matrix<T>(m, m, std::forward<TArgs>(params)...);
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
            itm = to_cpp<std::conditional_t<std::is_signed<scalar_type>::value,
                                            NegativeInfinity,
                                            scalar_type>>()(val);
          }
          x(i, j) = itm;
        }
      }
      GAPBIND14_TRY(libsemigroups::validate(x));
      return x;
    }
  }  // namespace detail

  template <>
  struct to_cpp<IntMat<>> {
    using cpp_type = IntMat<>;

    IntMat<> operator()(Obj o) const {
      if (CALL_1ARGS(IsIntegerMatrix, o) != True) {
        ErrorQuit("expected integer matrix, found %s", (Int) TNAM_OBJ(o), 0L);
      }
      return detail::init_cpp_matrix<IntMat<>>(o);
    }
  };

  template <>
  struct to_cpp<MaxPlusMat<>> {
    using cpp_type = MaxPlusMat<>;

    MaxPlusMat<> operator()(Obj o) const {
      if (CALL_1ARGS(IsMaxPlusMatrix, o) != True) {
        ErrorQuit("expected max-plus matrix, found %s", (Int) TNAM_OBJ(o), 0L);
      }
      return detail::init_cpp_matrix<MaxPlusMat<>>(o);
    }
  };

  template <>
  struct to_cpp<MinPlusMat<>> {
    using cpp_type = MinPlusMat<>;

    MinPlusMat<> operator()(Obj o) const {
      if (CALL_1ARGS(IsMinPlusMatrix, o) != True) {
        ErrorQuit("expected min-plus matrix, found %s", (Int) TNAM_OBJ(o), 0L);
      }
      return detail::init_cpp_matrix<MinPlusMat<>>(o);
    }
  };

  template <>
  struct to_cpp<ProjMaxPlusMat<>> {
    using cpp_type = ProjMaxPlusMat<>;

    ProjMaxPlusMat<> operator()(Obj o) const {
      if (CALL_1ARGS(IsProjectiveMaxPlusMatrix, o) != True) {
        ErrorQuit("expected min-plus matrix, found %s", (Int) TNAM_OBJ(o), 0L);
      }
      return detail::init_cpp_matrix<ProjMaxPlusMat<>>(o);
    }
  };

  template <>
  struct to_cpp<MaxPlusTruncMat<>> {
    using cpp_type = MaxPlusTruncMat<>;

    cpp_type operator()(Obj o) const {
      if (CALL_1ARGS(IsTropicalMaxPlusMatrix, o) != True) {
        ErrorQuit("expected tropical max-plus matrix, found %s",
                  (Int) TNAM_OBJ(o),
                  0L);
      }

      size_t m            = LEN_PLIST(ELM_PLIST(o, 1));
      using semiring_type = typename MaxPlusTruncMat<>::semiring_type;
      auto const* sr      = semigroups::semiring<semiring_type>(
          static_cast<size_t>(INT_INTOBJ(ELM_PLIST(o, m + 1))));
      return detail::init_cpp_matrix<MaxPlusTruncMat<>>(o, sr);
    }
  };

  template <>
  struct to_cpp<MinPlusTruncMat<>> {
    using cpp_type = MinPlusTruncMat<>;

    cpp_type operator()(Obj o) const {
      if (CALL_1ARGS(IsTropicalMinPlusMatrix, o) != True) {
        ErrorQuit("expected tropical min-plus matrix, found %s",
                  (Int) TNAM_OBJ(o),
                  0L);
      }

      size_t m            = LEN_PLIST(ELM_PLIST(o, 1));
      using semiring_type = typename MinPlusTruncMat<>::semiring_type;
      auto const* sr      = semigroups::semiring<semiring_type>(
          static_cast<size_t>(INT_INTOBJ(ELM_PLIST(o, m + 1))));
      return detail::init_cpp_matrix<MinPlusTruncMat<>>(o, sr);
    }
  };

  template <>
  struct to_cpp<NTPMat<>> {
    using cpp_type = NTPMat<>;

    NTPMat<> operator()(Obj o) const {
      if (CALL_1ARGS(IsNTPMatrix, o) != True) {
        ErrorQuit("expected ntp matrix, found %s!", (Int) TNAM_OBJ(o), 0L);
      }
      SEMIGROUPS_ASSERT(LEN_PLIST(o) != 0);
      size_t      m  = LEN_PLIST(ELM_PLIST(o, 1));
      auto const* sr = semigroups::semiring(
          static_cast<size_t>(INT_INTOBJ(ELM_PLIST(o, m + 1))),
          static_cast<size_t>(INT_INTOBJ(ELM_PLIST(o, m + 2))));
      return detail::init_cpp_matrix<NTPMat<>>(o, sr);
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // congruence_kind
  ////////////////////////////////////////////////////////////////////////

  template <>
  struct to_cpp<congruence_kind> {
    using cpp_type                          = congruence_kind;
    static gap_tnum_type constexpr gap_type = T_STRING;

    cpp_type operator()(Obj o) const {
      if (TNUM_OBJ(o) != T_STRING && TNUM_OBJ(o) != T_STRING + IMMUTABLE) {
        ErrorQuit("expected string but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
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

  template <>
  struct to_cpp<libsemigroups::Congruence::options::runners> {
    using cpp_type = libsemigroups::Congruence::options::runners;

    cpp_type operator()(Obj o) const {
      if (TNUM_OBJ(o) != T_STRING && TNUM_OBJ(o) != T_STRING + IMMUTABLE) {
        ErrorQuit("expected string but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
      std::string stype = std::string(CSTR_STRING(o));
      if (stype == "none") {
        return cpp_type::none;
      } else if (stype == "standard") {
        return cpp_type::standard;
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

  template <typename Scalar>
  struct to_cpp<Transf<0, Scalar>> {
    using cpp_type = Transf<0, Scalar>;

    cpp_type operator()(Obj t) const {
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
        ErrorQuit("expected transformation with largest moved point not "
                  "greater than %d, found %d",
                  (Int) N,
                  (Int) DEG_TRANS(x));
      }

      cpp_type result(N);
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
    // TODO: renovate
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
    // TODO: renovate
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

  template <typename Scalar>
  struct to_cpp<PPerm<0, Scalar>> {
    using cpp_type = PPerm<0, Scalar>;
    // #ifdef LIBSEMIGROUPS_HPCOMBI_ENABLED
    // FIXME
    // || std::is_same<std::decay_t<T>, HPCombi::PPerm16>::value
    //#endif

    cpp_type operator()(Obj t) const {
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

      cpp_type result(detail::construct_pperm<cpp_type>(N));
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

  template <>
  struct to_cpp<Bipartition> {
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

  template <>
  struct to_cpp<PBR> {
    PBR operator()(Obj x) const {
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
  struct to_cpp<DynamicArray2<T>> {
    using cpp_type = DynamicArray2<T>;
    cpp_type operator()(Obj x) {
      using value_type = typename DynamicArray2<T>::value_type;
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
      size_t   nr_cols = (LEN_LIST(x) == 0 ? 0 : LEN_LIST(ELM_LIST(x, 1)));
      cpp_type result(nr_cols, LEN_LIST(x));
      for (size_t i = 0; i < LEN_LIST(x); ++i) {
        Obj item = ELM_LIST(x, i + 1);
        for (size_t j = 0; j < nr_cols; ++j) {
          result.set(i, j, to_cpp<value_type>()(ELM_LIST(item, j + 1)));
        }
      }
      return result;
    }
  };
}  // namespace gapbind14
#endif  // SEMIGROUPS_SRC_TO_CPP_HPP_
