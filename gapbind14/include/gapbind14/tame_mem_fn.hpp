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

#ifndef INCLUDE_GAPBIND14_TAME_MEM_FN_HPP_
#define INCLUDE_GAPBIND14_TAME_MEM_FN_HPP_

#include <algorithm>    // for reverse
#include <cstddef>      // for size_t
#include <type_traits>  // for enable_if_t
#include <vector>       // for vector

#include "cpp_fn.hpp"       // for CppFunction, GAPBIND14_TRY, T_GAPBIND14_OBJ,
                            // arg_count, returns_void
#include "gap_include.hpp"  // for ErrorQuit, Obj, Int, TNAM_OBJ, TNUM_OBJ
#include "to_cpp.hpp"       // for to_cpp
#include "to_gap.hpp"       // for to_gap

namespace gapbind14 {

  namespace detail {
    // forward decl
    template <typename T>
    T *obj_cpp_ptr(Obj o);

    template <typename T>
    class Subtype;

    ////////////////////////////////////////////////////////////////////////
    // Wilds
    ////////////////////////////////////////////////////////////////////////

    template <typename Wild>
    auto &all_wild_mem_fns() {
      static std::vector<Wild> fs;
      return fs;
    }

    template <typename Wild>
    auto wild_mem_fn(size_t i) {
      return all_wild_mem_fns<Wild>().at(i);
    }

    ////////////////////////////////////////////////////////////////////////
    // Tame member functions returning void
    ////////////////////////////////////////////////////////////////////////

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0)
        -> std::enable_if_t<returns_void<Wild>::value
                                && arg_count<Wild>::value == 0,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr
      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      GAPBIND14_TRY(CppFunction<Wild>()(wild_mem_fn<Wild>(N), ptr));
      return 0L;
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0, Obj arg1)
        -> std::enable_if_t<returns_void<Wild>::value
                                && arg_count<Wild>::value == 1,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      GAPBIND14_TRY(CppFunction<Wild>()(
          wild_mem_fn<Wild>(N), ptr, to_cpp<to_cpp_1_type>()(arg1)));
      return 0L;
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0, Obj arg1, Obj arg2)
        -> std::enable_if_t<returns_void<Wild>::value
                                && arg_count<Wild>::value == 2,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      GAPBIND14_TRY(CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                                        ptr,
                                        to_cpp<to_cpp_1_type>()(arg1),
                                        to_cpp<to_cpp_2_type>()(arg2)));
      return 0L;
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0, Obj arg1, Obj arg2, Obj arg3)
        -> std::enable_if_t<returns_void<Wild>::value
                                && arg_count<Wild>::value == 3,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      GAPBIND14_TRY(CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                                        ptr,
                                        to_cpp<to_cpp_1_type>()(arg1),
                                        to_cpp<to_cpp_2_type>()(arg2),
                                        to_cpp<to_cpp_3_type>()(arg3)));
      return 0L;
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0, Obj arg1, Obj arg2, Obj arg3, Obj arg4)
        -> std::enable_if_t<returns_void<Wild>::value
                                && arg_count<Wild>::value == 4,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      using to_cpp_4_type =
          typename CppFunction<Wild>::params_type::template get<3>;
      GAPBIND14_TRY(CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                                        ptr,
                                        to_cpp<to_cpp_1_type>()(arg1),
                                        to_cpp<to_cpp_2_type>()(arg2),
                                        to_cpp<to_cpp_3_type>()(arg3),
                                        to_cpp<to_cpp_4_type>()(arg4)));
      return 0L;
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self,
                     Obj arg0,
                     Obj arg1,
                     Obj arg2,
                     Obj arg3,
                     Obj arg4,
                     Obj arg5)
        -> std::enable_if_t<returns_void<Wild>::value
                                && arg_count<Wild>::value == 5,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      using to_cpp_4_type =
          typename CppFunction<Wild>::params_type::template get<3>;
      using to_cpp_5_type =
          typename CppFunction<Wild>::params_type::template get<4>;
      GAPBIND14_TRY(CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                                        ptr,
                                        to_cpp<to_cpp_1_type>()(arg1),
                                        to_cpp<to_cpp_2_type>()(arg2),
                                        to_cpp<to_cpp_3_type>()(arg3),
                                        to_cpp<to_cpp_4_type>()(arg4),
                                        to_cpp<to_cpp_5_type>()(arg5)));
      return 0L;
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self,
                     Obj arg0,
                     Obj arg1,
                     Obj arg2,
                     Obj arg3,
                     Obj arg4,
                     Obj arg5,
                     Obj arg6)
        -> std::enable_if_t<returns_void<Wild>::value
                                && arg_count<Wild>::value == 6,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      using to_cpp_4_type =
          typename CppFunction<Wild>::params_type::template get<3>;
      using to_cpp_5_type =
          typename CppFunction<Wild>::params_type::template get<4>;
      using to_cpp_6_type =
          typename CppFunction<Wild>::params_type::template get<5>;
      GAPBIND14_TRY(CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                                        ptr,
                                        to_cpp<to_cpp_1_type>()(arg1),
                                        to_cpp<to_cpp_2_type>()(arg2),
                                        to_cpp<to_cpp_3_type>()(arg3),
                                        to_cpp<to_cpp_4_type>()(arg4),
                                        to_cpp<to_cpp_5_type>()(arg5),
                                        to_cpp<to_cpp_6_type>()(arg6)));
      return 0L;
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self,
                     Obj arg0,
                     Obj arg1,
                     Obj arg2,
                     Obj arg3,
                     Obj arg4,
                     Obj arg5,
                     Obj arg6,
                     Obj arg7)
        -> std::enable_if_t<returns_void<Wild>::value
                                && arg_count<Wild>::value == 7,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      using to_cpp_4_type =
          typename CppFunction<Wild>::params_type::template get<3>;
      using to_cpp_5_type =
          typename CppFunction<Wild>::params_type::template get<4>;
      using to_cpp_6_type =
          typename CppFunction<Wild>::params_type::template get<5>;
      using to_cpp_7_type =
          typename CppFunction<Wild>::params_type::template get<6>;
      GAPBIND14_TRY(CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                                        ptr,
                                        to_cpp<to_cpp_1_type>()(arg1),
                                        to_cpp<to_cpp_2_type>()(arg2),
                                        to_cpp<to_cpp_3_type>()(arg3),
                                        to_cpp<to_cpp_4_type>()(arg4),
                                        to_cpp<to_cpp_5_type>()(arg5),
                                        to_cpp<to_cpp_6_type>()(arg6),
                                        to_cpp<to_cpp_7_type>()(arg7)));
      return 0L;
    }

    ////////////////////////////////////////////////////////////////////////
    // Tame member functions NOT returning void
    ////////////////////////////////////////////////////////////////////////

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0)
        -> std::enable_if_t<!returns_void<Wild>::value
                                && arg_count<Wild>::value == 0,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_gap_type = to_gap<typename CppFunction<Wild>::return_type>;
      GAPBIND14_TRY(
          return to_gap_type()(CppFunction<Wild>()(wild_mem_fn<Wild>(N), ptr)));
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0, Obj arg1)
        -> std::enable_if_t<!returns_void<Wild>::value
                                && arg_count<Wild>::value == 1,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_gap_type = to_gap<typename CppFunction<Wild>::return_type>;
      GAPBIND14_TRY(return to_gap_type()(CppFunction<Wild>()(
          wild_mem_fn<Wild>(N), ptr, to_cpp<to_cpp_1_type>()(arg1))));
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0, Obj arg1, Obj arg2)
        -> std::enable_if_t<!returns_void<Wild>::value
                                && arg_count<Wild>::value == 2,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_gap_type = to_gap<typename CppFunction<Wild>::return_type>;
      GAPBIND14_TRY(return to_gap_type()(
          CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                              ptr,
                              to_cpp<to_cpp_1_type>()(arg1),
                              to_cpp<to_cpp_2_type>()(arg2))));
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0, Obj arg1, Obj arg2, Obj arg3)
        -> std::enable_if_t<!returns_void<Wild>::value
                                && arg_count<Wild>::value == 3,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      using to_gap_type = to_gap<typename CppFunction<Wild>::return_type>;
      GAPBIND14_TRY(return to_gap_type()(
          CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                              ptr,
                              to_cpp<to_cpp_1_type>()(arg1),
                              to_cpp<to_cpp_2_type>()(arg2),
                              to_cpp<to_cpp_3_type>()(arg3))));
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self, Obj arg0, Obj arg1, Obj arg2, Obj arg3, Obj arg4)
        -> std::enable_if_t<!returns_void<Wild>::value
                                && arg_count<Wild>::value == 4,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      using to_cpp_4_type =
          typename CppFunction<Wild>::params_type::template get<3>;
      using to_gap_type = to_gap<typename CppFunction<Wild>::return_type>;
      GAPBIND14_TRY(return to_gap_type()(
          CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                              ptr,
                              to_cpp<to_cpp_1_type>()(arg1),
                              to_cpp<to_cpp_2_type>()(arg2),
                              to_cpp<to_cpp_3_type>()(arg3),
                              to_cpp<to_cpp_4_type>()(arg4))));
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self,
                     Obj arg0,
                     Obj arg1,
                     Obj arg2,
                     Obj arg3,
                     Obj arg4,
                     Obj arg5)
        -> std::enable_if_t<!returns_void<Wild>::value
                                && arg_count<Wild>::value == 5,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      using to_cpp_4_type =
          typename CppFunction<Wild>::params_type::template get<3>;
      using to_cpp_5_type =
          typename CppFunction<Wild>::params_type::template get<4>;
      using to_gap_type = to_gap<typename CppFunction<Wild>::return_type>;
      GAPBIND14_TRY(return to_gap_type()(
          CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                              ptr,
                              to_cpp<to_cpp_1_type>()(arg1),
                              to_cpp<to_cpp_2_type>()(arg2),
                              to_cpp<to_cpp_3_type>()(arg3),
                              to_cpp<to_cpp_4_type>()(arg4),
                              to_cpp<to_cpp_5_type>()(arg5))));
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self,
                     Obj arg0,
                     Obj arg1,
                     Obj arg2,
                     Obj arg3,
                     Obj arg4,
                     Obj arg5,
                     Obj arg6)
        -> std::enable_if_t<!returns_void<Wild>::value
                                && arg_count<Wild>::value == 6,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      using to_cpp_4_type =
          typename CppFunction<Wild>::params_type::template get<3>;
      using to_cpp_5_type =
          typename CppFunction<Wild>::params_type::template get<4>;
      using to_cpp_6_type =
          typename CppFunction<Wild>::params_type::template get<5>;
      using to_gap_type = to_gap<typename CppFunction<Wild>::return_type>;
      GAPBIND14_TRY(return to_gap_type()(
          CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                              ptr,
                              to_cpp<to_cpp_1_type>()(arg1),
                              to_cpp<to_cpp_2_type>()(arg2),
                              to_cpp<to_cpp_3_type>()(arg3),
                              to_cpp<to_cpp_4_type>()(arg4),
                              to_cpp<to_cpp_5_type>()(arg5),
                              to_cpp<to_cpp_6_type>()(arg6))));
    }

    template <size_t N, typename Wild, typename TSFINAE = Obj>
    auto tame_mem_fn(Obj self,
                     Obj arg0,
                     Obj arg1,
                     Obj arg2,
                     Obj arg3,
                     Obj arg4,
                     Obj arg5,
                     Obj arg6,
                     Obj arg7)
        -> std::enable_if_t<!returns_void<Wild>::value
                                && arg_count<Wild>::value == 7,
                            TSFINAE> {
      // arg0 is checked in obj_cpp_ptr

      using class_type = typename CppFunction<Wild>::class_type;
      class_type *ptr  = detail::obj_cpp_ptr<class_type>(arg0);

      using to_cpp_1_type =
          typename CppFunction<Wild>::params_type::template get<0>;
      using to_cpp_2_type =
          typename CppFunction<Wild>::params_type::template get<1>;
      using to_cpp_3_type =
          typename CppFunction<Wild>::params_type::template get<2>;
      using to_cpp_4_type =
          typename CppFunction<Wild>::params_type::template get<3>;
      using to_cpp_5_type =
          typename CppFunction<Wild>::params_type::template get<4>;
      using to_cpp_6_type =
          typename CppFunction<Wild>::params_type::template get<5>;
      using to_cpp_7_type =
          typename CppFunction<Wild>::params_type::template get<6>;
      using to_gap_type = to_gap<typename CppFunction<Wild>::return_type>;
      GAPBIND14_TRY(return to_gap_type()(
          CppFunction<Wild>()(wild_mem_fn<Wild>(N),
                              ptr,
                              to_cpp<to_cpp_1_type>()(arg1),
                              to_cpp<to_cpp_2_type>()(arg2),
                              to_cpp<to_cpp_3_type>()(arg3),
                              to_cpp<to_cpp_4_type>()(arg4),
                              to_cpp<to_cpp_5_type>()(arg5),
                              to_cpp<to_cpp_6_type>()(arg6),
                              to_cpp<to_cpp_7_type>()(arg7))));
    }

    ////////////////////////////////////////////////////////////////////////
    // Create a vector of tame member functions
    ////////////////////////////////////////////////////////////////////////

    template <size_t N, typename Tame, typename Wild>
    struct static_push_back_mem_fns {
      void operator()(std::vector<Tame> &v) {
        v.push_back(&tame_mem_fn<N - 1, Wild>);
        static_push_back_mem_fns<N - 1, Tame, Wild>{}(v);
      }
    };

    template <typename Tame, typename Wild>
    struct static_push_back_mem_fns<0, Tame, Wild> {
      void operator()(std::vector<Tame> &v) {
        std::reverse(v.begin(), v.end());
      }
    };

    template <typename Tame, typename Wild>
    auto init_tame_mem_fns() {
      std::vector<Tame> fs;
      static_push_back_mem_fns<MAX_FUNCTIONS, Tame, Wild>{}(fs);
      return fs;
    }

    template <typename Tame, typename Wild>
    auto &tame_mem_fns() {
      static std::vector<Tame> fs = init_tame_mem_fns<Tame, Wild>();
      return fs;
    }

    template <typename Tame, typename Wild>
    auto get_tame_mem_fn(size_t i) {
      return tame_mem_fns<Tame, Wild>().at(i);
    }
  }  // namespace detail
}  // namespace gapbind14
#endif  // INCLUDE_GAPBIND14_TAME_MEM_FN_HPP_
