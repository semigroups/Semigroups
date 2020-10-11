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

#ifndef INCLUDE_GAPBIND14_IS_SOMETHING_HPP_
#define INCLUDE_GAPBIND14_IS_SOMETHING_HPP_

#include <type_traits>  // for enable_if_t
#include <utility>      // for pair
#include <vector>       // for vector

namespace gapbind14 {

  template <typename T, typename = void>
  struct IsIterator : std::false_type {};

  template <typename T>
  struct IsIterator<T,
                    std::enable_if_t<!std::is_same<
                        typename std::iterator_traits<T>::value_type,
                        void>::value>> : std::true_type {};

  template <typename... Args>
  struct IsVector : std::false_type {};

  template <typename... Args>
  struct IsVector<std::vector<Args...>> : std::true_type {};

  template <typename... Args>
  struct IsVector<std::vector<Args...>&> : std::true_type {};

  template <typename... Args>
  struct IsVector<std::vector<Args...> const&> : std::true_type {};

  template <typename... Args>
  struct IsVector<std::vector<Args...>&&> : std::true_type {};

  template <typename T>
  struct IsPair : std::false_type {};

  template <typename S, typename T>
  struct IsPair<std::pair<S, T>> : std::true_type {};

  template <typename S, typename T>
  struct IsPair<std::pair<S, T> const&> : std::true_type {};

  template <typename S, typename T>
  struct IsPair<std::pair<S, T>&> : std::true_type {};

  template <typename S, typename T>
  struct IsPair<std::pair<S, T>&&> : std::true_type {};
}  // namespace gapbind14

#endif  // INCLUDE_GAPBIND14_IS_SOMETHING_HPP_
