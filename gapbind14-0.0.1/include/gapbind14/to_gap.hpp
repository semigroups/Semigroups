//
// gapbind14
// Copyright (C) 2020 James D. Mitchell
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
//

#ifndef INCLUDE_GAPBIND14_TO_GAP_HPP_
#define INCLUDE_GAPBIND14_TO_GAP_HPP_

#include <string>   // for string
#include <utility>  // for pair
#include <vector>   // for vector

#include "gap_include.hpp"
#include "is_something.hpp"  // for IsVector

namespace gapbind14 {

  using gap_tnum_type = UInt;

  // For TCppType -> Obj
  template <typename TCppType, typename = void>
  struct to_gap;

  ////////////////////////////////////////////////////////////////////////
  // Strings
  ////////////////////////////////////////////////////////////////////////

  template <typename TCppType>
  struct to_gap<TCppType,
                std::enable_if_t<
                    std::is_same<std::string, std::decay_t<TCppType>>::value>> {
    using cpp_type                          = TCppType;
    static gap_tnum_type constexpr gap_type = T_STRING;

    Obj operator()(TCppType&& str) {
      Obj ret;
      C_NEW_STRING(ret, str.length(), str.c_str());
      return ret;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Integers
  ////////////////////////////////////////////////////////////////////////

  template <typename TCppType>
  struct to_gap<TCppType,
                std::enable_if_t<std::is_integral<TCppType>::value
                                 && !std::is_same<TCppType, bool>::value>> {
    using cpp_type                          = TCppType;
    static gap_tnum_type constexpr gap_type = T_INT;

    Obj operator()(TCppType&& i) {
      return INTOBJ_INT(i);
    }

    Obj operator()(TCppType const& i) {
      return INTOBJ_INT(i);
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Bools
  ////////////////////////////////////////////////////////////////////////

  template <typename TCppType>
  struct to_gap<TCppType,
                std::enable_if_t<std::is_same<TCppType, bool>::value>> {
    using cpp_type                          = TCppType;
    static gap_tnum_type constexpr gap_type = T_BOOL;

    Obj operator()(TCppType&& val) {
      return (val ? True : False);
    }

    Obj operator()(TCppType const& val) {
      return (val ? True : False);
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Vectors
  ////////////////////////////////////////////////////////////////////////

  template <typename TCppType>
  struct to_gap<TCppType, std::enable_if_t<IsVector<TCppType>::value>> {
    using cpp_type                          = std::decay_t<TCppType>;
    static gap_tnum_type constexpr gap_type = T_PLIST_HOM;

    Obj operator()(TCppType v) const {
      Obj result = NEW_PLIST(T_PLIST_HOM, v.size());
      SET_LEN_PLIST(result, v.size());
      using value_type = typename std::decay_t<TCppType>::value_type;
      size_t index     = 1;
      for (auto const& item : v) {
        AssPlist(result, index++, to_gap<value_type>()(item));
      }
      return result;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Pairs
  ////////////////////////////////////////////////////////////////////////

  template <typename TCppType>
  struct to_gap<TCppType, std::enable_if_t<IsPair<TCppType>::value>> {
    using cpp_type                          = std::decay_t<TCppType>;
    static gap_tnum_type constexpr gap_type = T_PLIST;

    Obj operator()(cpp_type const& p) const {
      Obj result = NEW_PLIST(T_PLIST, 2);
      SET_LEN_PLIST(result, 2);
      AssPlist(result, 1, to_gap<decltype(p.first)>()(p.first));
      AssPlist(result, 2, to_gap<decltype(p.second)>()(p.second));
      return result;
    }
  };
}  // namespace gapbind14
#endif  // INCLUDE_GAPBIND14_TO_GAP_HPP_
