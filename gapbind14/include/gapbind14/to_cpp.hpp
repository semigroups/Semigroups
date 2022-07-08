//
// gapbind14
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
//

#ifndef INCLUDE_GAPBIND14_TO_CPP_HPP_
#define INCLUDE_GAPBIND14_TO_CPP_HPP_

#include <string>  // for string
#include <vector>  // for vector

#include "gap_include.hpp"

namespace gapbind14 {

  using gap_tnum_type = UInt;

  // For Obj -> TCppType
  template <typename TCppType, typename = void>
  struct to_cpp;

  ////////////////////////////////////////////////////////////////////////
  // void
  ////////////////////////////////////////////////////////////////////////

  template <>
  struct to_cpp<void> {
    using cpp_type                          = void;
    static gap_tnum_type constexpr gap_type = 0;

    void operator()() {}
  };

  ////////////////////////////////////////////////////////////////////////
  // Strings
  ////////////////////////////////////////////////////////////////////////

  template <>
  struct to_cpp<std::string> {
    using cpp_type                          = std::string;
    static gap_tnum_type constexpr gap_type = T_STRING;

    std::string operator()(Obj o) {
      if (TNUM_OBJ(o) != T_STRING) {
        ErrorQuit("expected string, found %s", (Int) TNAM_OBJ(o), 0L);
      }
      return std::string(CSTR_STRING(o), GET_LEN_STRING(o));
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Integers
  ////////////////////////////////////////////////////////////////////////

  template <typename TCppType>
  struct to_cpp<TCppType,
                std::enable_if_t<std::is_integral<TCppType>::value
                                 && !std::is_same<TCppType, bool>::value>> {
    using cpp_type                          = TCppType;
    static gap_tnum_type constexpr gap_type = T_INT;

    Int operator()(Obj o) {
      if (TNUM_OBJ(o) != T_INT) {
        ErrorQuit("expected int, found %s", (Int) TNAM_OBJ(o), 0L);
      }
      return INT_INTOBJ(o);
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Bools
  ////////////////////////////////////////////////////////////////////////

  template <>
  struct to_cpp<bool> {
    using cpp_type                          = bool;
    static gap_tnum_type constexpr gap_type = T_BOOL;

    bool operator()(Obj o) {
      if (TNUM_OBJ(o) != T_BOOL) {
        ErrorQuit("expected bool, found %s", (Int) TNAM_OBJ(o), 0L);
      }
      return o == True ? true : false;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Vectors
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_cpp<std::vector<T>> {
    using cpp_type                          = std::vector<T>;
    static gap_tnum_type constexpr gap_type = T_PLIST_HOM;

    std::vector<T> operator()(Obj o) {
      if (!IS_LIST(o)) {
        ErrorQuit("expected list, found %s", (Int) TNAM_OBJ(o), 0L);
      }

      size_t const N   = LEN_LIST(o);
      using value_type = typename std::vector<T>::value_type;
      std::vector<T> result;
      result.reserve(N);
      for (size_t i = 0; i < N; ++i) {
        result.push_back(to_cpp<value_type>()(ELM_LIST(o, i + 1)));
      }
      return result;
    }
  };

  template <typename T>
  struct to_cpp<std::vector<T>&> : to_cpp<std::vector<T>> {};

  template <typename T>
  struct to_cpp<std::vector<T> const&> : to_cpp<std::vector<T>> {};

  template <typename T>
  struct to_cpp<std::vector<T>&&> : to_cpp<std::vector<T>> {};

}  // namespace gapbind14
#endif  // INCLUDE_GAPBIND14_TO_CPP_HPP_
