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
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//

#ifndef INCLUDE_GAPBIND14_TAME_MAKE_HPP_
#define INCLUDE_GAPBIND14_TAME_MAKE_HPP_

#include <utility>  // for forward
#include <vector>   // for vector

#include "cpp-fn.hpp"
#include "to_cpp.hpp"
#include "to_gap.hpp"

namespace gapbind14 {
  // Forward decl - defined in gapbind14.hpp because it uses Module.
  template <size_t N, typename TClass, typename TFunctionType>
  Obj tame_constructor(Obj self, Obj args);

  ////////////////////////////////////////////////////////////////////////
  // The actual tame constructors
  ////////////////////////////////////////////////////////////////////////

  template <typename TClass, typename... TArgs>
  TClass* make(TArgs&&... params) {
    return new TClass(std::forward<TArgs>(params)...);
  }

  ////////////////////////////////////////////////////////////////////////
  // Create a vector of tame constructors
  ////////////////////////////////////////////////////////////////////////

  template <size_t N, typename Class, typename Tame, typename Wild>
  struct static_push_back_constructors {
    void operator()(std::vector<Tame>& v) {
      v.push_back(&tame_constructor<N - 1, Class, Wild>);
      static_push_back_constructors<N - 1, Class, Tame, Wild>{}(v);
    }
  };

  template <typename Class, typename Tame, typename Wild>
  struct static_push_back_constructors<0, Class, Tame, Wild> {
    void operator()(std::vector<Tame>& v) {
      std::reverse(v.begin(), v.end());
    }
  };

  template <typename Class, typename Tame, typename Wild>
  auto init_tame_constructors() {
    std::vector<Tame> fs;
    static_push_back_constructors<MAX_FUNCTIONS, Class, Tame, Wild>{}(fs);
    return fs;
  }

  template <typename Class, typename Tame, typename Wild>
  auto& all_tame_constructors() {
    static std::vector<Tame> fs = init_tame_constructors<Class, Tame, Wild>();
    return fs;
  }

  template <typename Class, typename Tame, typename Wild>
  auto get_tame_constructor(size_t i) {
    return all_tame_constructors<Class, Tame, Wild>().at(i);
  }

}  // namespace gapbind14

#endif  // INCLUDE_GAPBIND14_TAME_MAKE_HPP_
