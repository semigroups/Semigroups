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

// TODO:
// 1. combine GAPBIND14_MEM_FN_OVERLOAD and GAPBIND14_MEM_FN as is
//    already done with GAPBIND14_FUNCTION_OVERLOAD and
//    GAPBIND14_MEM_FN_OVERLOAD

#ifndef INCLUDE_GAPBIND14_MACROS_HPP_
#define INCLUDE_GAPBIND14_MACROS_HPP_

#include <vector>  // for vector

////////////////////////////////////////////////////////////////////////
// Helper macros
////////////////////////////////////////////////////////////////////////

#define GAPBIND14_CONCAT(first, second) first##second
#define GAPBIND14_CONCAT3(first, second, third) first##second##third

#define GAPBIND14_STRINGIFY(x) #x
#define GAPBIND14_TO_STRING(x) GAPBIND14_STRINGIFY(x)

#define GAPBIND14_TOKENPASTE(x, y) GAPBIND14_CONCAT(x, y)
#define GAPBIND14_UNIQUE_ID \
  int GAPBIND14_TOKENPASTE(gapbind14_hack_, __COUNTER__) =

////////////////////////////////////////////////////////////////////////
// Assertions
////////////////////////////////////////////////////////////////////////

#ifdef GAPBIND14_DEBUG
#define GAPBIND14_ASSERT(x) assert(x)
#else
#define GAPBIND14_ASSERT(x)
#endif

////////////////////////////////////////////////////////////////////////
// Exceptions
////////////////////////////////////////////////////////////////////////

// TODO should be possible specify return Fail, as well as ErrorQuit, when an
// exception is thrown

#define GAPBIND14_TRY(something)      \
  try {                               \
    something;                        \
  } catch (std::exception const& e) { \
    ErrorQuit(e.what(), 0L, 0L);      \
  }

////////////////////////////////////////////////////////////////////////
// Add function to list of function pointers to be initialised
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
// Create module
////////////////////////////////////////////////////////////////////////

#define GAPBIND14_MODULE(name, module)                          \
  gapbind14::Module module(GAPBIND14_TO_STRING(name));          \
                                                                \
  using gapbind14_init_func = void (*)(gapbind14::Module&);     \
  std::vector<gapbind14_init_func> gapbind14_init_functions;    \
                                                                \
  int gapbind14_push_back(gapbind14_init_func func) {           \
    gapbind14_init_functions.push_back(func);                   \
    return 0;                                                   \
  }                                                             \
                                                                \
  void gapbind14::GAPBIND14_MODULE_IMPL(gapbind14::Module& m) { \
    try {                                                       \
      for (auto init : gapbind14_init_functions) {              \
        init(m);                                                \
      }                                                         \
    } catch (...) {                                             \
    }                                                           \
    m.finalize();                                               \
  }

#define GAPBIND14_MODULE_NEW(name, module)                                   \
  ::gapbind14::Module module(GAPBIND14_TO_STRING(name));                     \
  static void GAPBIND14_CONCAT(gapbind14_init_, name)(::gapbind14::Module&); \
                                                                             \
  void ::gapbind14::GAPBIND14_MODULE_IMPL(gapbind14::Module& m) {            \
    GAPBIND14_CONCAT(gapbind14_init_, name)(m);                              \
    m.finalize();                                                            \
  }                                                                          \
                                                                             \
  void GAPBIND14_CONCAT(gapbind14_init_, name)(::gapbind14::Module & variable)

////////////////////////////////////////////////////////////////////////
// Create function wrapper
////////////////////////////////////////////////////////////////////////

#define GAPBIND14_FUNCTION_NON_OVERLOAD(module, c_func_var, gap_func_var)   \
  template <typename TFunctionType = decltype(c_func_var),                  \
            typename TSFINAE       = Obj>                                   \
  auto GAPBIND14_CONCAT(c_func_var, _wrapper)(Obj self)                     \
      ->std::enable_if_t<gapbind14::returns_void<TFunctionType>::value      \
                             && gapbind14::arg_count<TFunctionType>::value  \
                                    == 0,                                   \
                         TSFINAE> {                                         \
    GAPBIND14_TRY(gapbind14::CppFunction<TFunctionType>()(c_func_var));     \
    return 0L;                                                              \
  }                                                                         \
                                                                            \
  template <typename TFunctionType = decltype(c_func_var),                  \
            typename TSFINAE       = Obj>                                   \
  auto GAPBIND14_CONCAT(c_func_var, _wrapper)(Obj self)                     \
      ->std::enable_if_t<!gapbind14::returns_void<TFunctionType>::value     \
                             && gapbind14::arg_count<TFunctionType>::value  \
                                    == 0,                                   \
                         TSFINAE> {                                         \
    using to_gap_type = gapbind14::to_gap<                                  \
        typename gapbind14::CppFunction<TFunctionType>::return_type>;       \
    GAPBIND14_TRY(return to_gap_type()(                                     \
        gapbind14::CppFunction<TFunctionType>()(c_func_var)));              \
  }                                                                         \
  template <typename TFunctionType = decltype(c_func_var),                  \
            typename TSFINAE       = Obj>                                   \
  auto GAPBIND14_CONCAT(c_func_var, _wrapper)(Obj self, Obj arg1)           \
      ->std::enable_if_t<gapbind14::returns_void<TFunctionType>::value      \
                             && gapbind14::arg_count<TFunctionType>::value  \
                                    == 1,                                   \
                         TSFINAE> {                                         \
    using to_cpp_0_type = typename gapbind14::CppFunction<                  \
        TFunctionType>::params_type::template get<0>;                       \
    GAPBIND14_TRY(gapbind14::CppFunction<TFunctionType>()(                  \
        c_func_var, gapbind14::to_cpp<to_cpp_0_type>()(arg1)));             \
    return 0L;                                                              \
  }                                                                         \
  template <typename TFunctionType = decltype(c_func_var),                  \
            typename TSFINAE       = Obj>                                   \
  auto GAPBIND14_CONCAT(c_func_var, _wrapper)(Obj self, Obj arg1)           \
      ->std::enable_if_t<!gapbind14::returns_void<TFunctionType>::value     \
                             && gapbind14::arg_count<TFunctionType>::value  \
                                    == 1,                                   \
                         TSFINAE> {                                         \
    using to_cpp_0_type = typename gapbind14::CppFunction<                  \
        TFunctionType>::params_type::template get<0>;                       \
    using to_gap_type = gapbind14::to_gap<                                  \
        typename gapbind14::CppFunction<TFunctionType>::return_type>;       \
    GAPBIND14_TRY(                                                          \
        return to_gap_type()(gapbind14::CppFunction<TFunctionType>()(       \
            c_func_var, gapbind14::to_cpp<to_cpp_0_type>()(arg1))));        \
  }                                                                         \
  template <typename TFunctionType = decltype(c_func_var),                  \
            typename TSFINAE       = Obj>                                   \
  auto GAPBIND14_CONCAT(c_func_var, _wrapper)(Obj self, Obj arg1, Obj arg2) \
      ->std::enable_if_t<gapbind14::returns_void<TFunctionType>::value      \
                             && gapbind14::arg_count<TFunctionType>::value  \
                                    == 2,                                   \
                         TSFINAE> {                                         \
    using to_cpp_0_type = typename gapbind14::CppFunction<                  \
        TFunctionType>::params_type::template get<0>;                       \
    using to_cpp_1_type = typename gapbind14::CppFunction<                  \
        TFunctionType>::params_type::template get<1>;                       \
    GAPBIND14_TRY(gapbind14::CppFunction<TFunctionType>()(                  \
        c_func_var,                                                         \
        gapbind14::to_cpp<to_cpp_0_type>()(arg1),                           \
        gapbind14::to_cpp<to_cpp_1_type>()(arg2)));                         \
    return 0L;                                                              \
  }                                                                         \
  template <typename TFunctionType = decltype(c_func_var),                  \
            typename TSFINAE       = Obj>                                   \
  auto GAPBIND14_CONCAT(c_func_var, _wrapper)(Obj self, Obj arg1, Obj arg2) \
      ->std::enable_if_t<!gapbind14::returns_void<TFunctionType>::value     \
                             && gapbind14::arg_count<TFunctionType>::value  \
                                    == 2,                                   \
                         TSFINAE> {                                         \
    using to_cpp_0_type = typename gapbind14::CppFunction<                  \
        TFunctionType>::params_type::template get<0>;                       \
    using to_cpp_1_type = typename gapbind14::CppFunction<                  \
        TFunctionType>::params_type::template get<1>;                       \
    using to_gap_type = gapbind14::to_gap<                                  \
        typename gapbind14::CppFunction<TFunctionType>::return_type>;       \
    GAPBIND14_TRY(                                                          \
        return to_gap_type()(gapbind14::CppFunction<TFunctionType>()(       \
            c_func_var,                                                     \
            gapbind14::to_cpp<to_cpp_0_type>()(arg1),                       \
            gapbind14::to_cpp<to_cpp_1_type>()(arg2))));                    \
  }                                                                         \
  void GAPBIND14_CONCAT3(gapbind14_init_, c_func_var, _wrapper)(            \
      gapbind14::Module & gapbind14_module) {                               \
    gapbind14_module.add_func(__FILE__,                                     \
                              GAPBIND14_TO_STRING(gap_func_var),            \
                              &GAPBIND14_CONCAT(c_func_var, _wrapper) < >); \
  }                                                                         \
  GAPBIND14_UNIQUE_ID gapbind14_push_back(                                  \
      &GAPBIND14_CONCAT3(gapbind14_init_, c_func_var, _wrapper));

#define GAPBIND14_FUNCTION_OVERLOAD(module, cfunc, gfunc, vrld) \
  auto GAPBIND14_CONCAT(gfunc, _overload) = vrld(cfunc);        \
  GAPBIND14_FUNCTION_NON_OVERLOAD(                              \
      module, GAPBIND14_CONCAT(gfunc, _overload), gfunc)

#define GAPBIND14_FUNCTION_GET(_1, _2, _3, _4, NAME, ...) NAME
#define GAPBIND14_FUNCTION(...)                           \
  GAPBIND14_FUNCTION_GET(__VA_ARGS__,                     \
                         GAPBIND14_FUNCTION_OVERLOAD,     \
                         GAPBIND14_FUNCTION_NON_OVERLOAD) \
  (__VA_ARGS__)

////////////////////////////////////////////////////////////////////////
// Create class wrapper
////////////////////////////////////////////////////////////////////////

#define GAPBIND14_CLASS(module, class_var)                            \
  GAPBIND14_UNIQUE_ID module.add_subtype<class_var>(                  \
      GAPBIND14_TO_STRING(class_var));                                \
  template <>                                                         \
  struct gapbind14::IsGapBind14Type<class_var> : std::true_type {};   \
  template <>                                                         \
  struct gapbind14::IsGapBind14Type<class_var&&> : std::true_type {}; \
  template <>                                                         \
  struct gapbind14::IsGapBind14Type<class_var&> : std::true_type {};  \
  template <>                                                         \
  struct gapbind14::IsGapBind14Type<class_var const&> : std::true_type {};

////////////////////////////////////////////////////////////////////////
// Create class constructor
////////////////////////////////////////////////////////////////////////

// TODO(now) is the gapbind14_init_ function followed by gapbind14_push_back
// really necessary, can't we just make add_mem_func return an int, and call it
// directly using GAPBIND14_UNIQUE_ID??
#define GAPBIND14_CONSTRUCTOR(module, class_var, gap_func_var, vrld)           \
  template <typename TFunctionType = decltype(vrld)>                           \
  Obj GAPBIND14_CONCAT(class_var, gap_func_var)(Obj self, Obj args) {          \
    return module.create<class_var, TFunctionType>(                            \
        GAPBIND14_TO_STRING(class_var), args);                                 \
  }                                                                            \
  void GAPBIND14_CONCAT3(gapbind14_init_, class_var, gap_func_var)(            \
      gapbind14::Module & gapbind14_module) {                                  \
    gapbind14_module.add_mem_func(GAPBIND14_TO_STRING(class_var),              \
                                  __FILE__,                                    \
                                  GAPBIND14_TO_STRING(gap_func_var),           \
                                  &GAPBIND14_CONCAT(class_var, gap_func_var) < \
                                      >);                                      \
  }                                                                            \
  GAPBIND14_UNIQUE_ID gapbind14_push_back(                                     \
      &GAPBIND14_CONCAT3(gapbind14_init_, class_var, gap_func_var));

////////////////////////////////////////////////////////////////////////
// Create class member function wrapper
////////////////////////////////////////////////////////////////////////

// Add the wrapper member function to the module.
#define GAPBIND14_ADD_INIT_MEM_FUNC(module, class_var, gap_func_var)           \
  void GAPBIND14_CONCAT3(gapbind14_init_, class_var, gap_func_var)(            \
      gapbind14::Module & gapbind14_module) {                                  \
    gapbind14_module.add_mem_func(GAPBIND14_TO_STRING(class_var),              \
                                  __FILE__,                                    \
                                  GAPBIND14_TO_STRING(gap_func_var),           \
                                  &GAPBIND14_CONCAT(class_var, gap_func_var) < \
                                      >);                                      \
  }                                                                            \
  GAPBIND14_UNIQUE_ID gapbind14_push_back(                                     \
      &GAPBIND14_CONCAT3(gapbind14_init_, class_var, gap_func_var));

// The repeated start of every member function.
#define GAPBIND14_MEM_FN_START(module, class_var)                           \
  if (TNUM_OBJ(arg1) != gapbind14::T_GAPBIND14_OBJ) {                       \
    ErrorQuit(                                                              \
        "expected gapbind14 object but got %s!", (Int) TNAM_OBJ(arg1), 0L); \
  }                                                                         \
  if (module.subtype(arg1)                                                  \
      != module.subtype(GAPBIND14_TO_STRING(class_var))) {                  \
    ErrorQuit("expected %s but got %s!",                                    \
              (Int) GAPBIND14_TO_STRING(class_var),                         \
              (Int) module.name(arg1));                                     \
  }                                                                         \
  class_var* ptr = gapbind14::SubTypeSpec<class_var>::obj_cpp_ptr(arg1);

#define GAPBIND14_MEM_FN_NON_OVERLOAD(                                         \
    module, class_var, mem_fn_var, gap_fn_var)                                 \
  template <typename TFunctionType = decltype(&class_var::mem_fn_var),         \
            typename TSFINAE       = Obj>                                      \
  auto GAPBIND14_CONCAT(class_var, gap_fn_var)(Obj self, Obj arg1)             \
      ->std::enable_if_t<gapbind14::returns_void<TFunctionType>::value         \
                             && gapbind14::arg_count<TFunctionType>::value     \
                                    == 0,                                      \
                         TSFINAE> {                                            \
    GAPBIND14_MEM_FN_START(module, class_var);                                 \
    GAPBIND14_TRY(                                                             \
        gapbind14::CppFunction<TFunctionType>()(&class_var::mem_fn_var, ptr)); \
    return 0L;                                                                 \
  }                                                                            \
  template <typename TFunctionType = decltype(&class_var::mem_fn_var),         \
            typename TSFINAE       = Obj>                                      \
  auto GAPBIND14_CONCAT(class_var, gap_fn_var)(Obj self, Obj arg1)             \
      ->std::enable_if_t<!gapbind14::returns_void<TFunctionType>::value        \
                             && gapbind14::arg_count<TFunctionType>::value     \
                                    == 0,                                      \
                         TSFINAE> {                                            \
    GAPBIND14_MEM_FN_START(module, class_var);                                 \
    using to_gap_type = gapbind14::to_gap<                                     \
        typename gapbind14::CppFunction<TFunctionType>::return_type>;          \
    GAPBIND14_TRY(                                                             \
        return to_gap_type()(gapbind14::CppFunction<TFunctionType>()(          \
            &class_var::mem_fn_var, ptr)));                                    \
  }                                                                            \
  template <typename TFunctionType = decltype(&class_var::mem_fn_var),         \
            typename TSFINAE       = Obj>                                      \
  auto GAPBIND14_CONCAT(class_var, gap_fn_var)(Obj self, Obj arg1, Obj arg2)   \
      ->std::enable_if_t<gapbind14::returns_void<TFunctionType>::value         \
                             && gapbind14::arg_count<TFunctionType>::value     \
                                    == 1,                                      \
                         TSFINAE> {                                            \
    GAPBIND14_MEM_FN_START(module, class_var);                                 \
    using to_cpp_0_type = typename gapbind14::CppFunction<                     \
        TFunctionType>::params_type::template get<0>;                          \
    GAPBIND14_TRY(gapbind14::CppFunction<TFunctionType>()(                     \
        &class_var::mem_fn_var,                                                \
        ptr,                                                                   \
        gapbind14::to_cpp<to_cpp_0_type>()(arg2)));                            \
    return 0L;                                                                 \
  }                                                                            \
  template <typename TFunctionType = decltype(&class_var::mem_fn_var),         \
            typename TSFINAE       = Obj>                                      \
  auto GAPBIND14_CONCAT(class_var, gap_fn_var)(Obj self, Obj arg1, Obj arg2)   \
      ->std::enable_if_t<!gapbind14::returns_void<TFunctionType>::value        \
                             && gapbind14::arg_count<TFunctionType>::value     \
                                    == 1,                                      \
                         TSFINAE> {                                            \
    GAPBIND14_MEM_FN_START(module, class_var);                                 \
    using to_cpp_0_type = typename gapbind14::CppFunction<                     \
        TFunctionType>::params_type::template get<0>;                          \
    using to_gap_type = gapbind14::to_gap<                                     \
        typename gapbind14::CppFunction<TFunctionType>::return_type>;          \
    GAPBIND14_TRY(                                                             \
        return to_gap_type()(gapbind14::CppFunction<TFunctionType>()(          \
            &class_var::mem_fn_var,                                            \
            ptr,                                                               \
            gapbind14::to_cpp<to_cpp_0_type>()(arg2))));                       \
    return 0L;                                                                 \
  }                                                                            \
  template <typename TFunctionType = decltype(&class_var::mem_fn_var),         \
            typename TSFINAE       = Obj>                                      \
  auto GAPBIND14_CONCAT(class_var,                                             \
                        gap_fn_var)(Obj self, Obj arg1, Obj arg2, Obj arg3)    \
      ->std::enable_if_t<!gapbind14::returns_void<TFunctionType>::value        \
                             && gapbind14::arg_count<TFunctionType>::value     \
                                    == 2,                                      \
                         TSFINAE> {                                            \
    GAPBIND14_MEM_FN_START(module, class_var);                                 \
    using to_cpp_0_type = typename gapbind14::CppFunction<                     \
        TFunctionType>::params_type::template get<0>;                          \
    using to_cpp_1_type = typename gapbind14::CppFunction<                     \
        TFunctionType>::params_type::template get<1>;                          \
    using to_gap_type = gapbind14::to_gap<                                     \
        typename gapbind14::CppFunction<TFunctionType>::return_type>;          \
    GAPBIND14_TRY(                                                             \
        return to_gap_type()(gapbind14::CppFunction<TFunctionType>()(          \
            &class_var::mem_fn_var,                                            \
            ptr,                                                               \
            gapbind14::to_cpp<to_cpp_0_type>()(arg2),                          \
            gapbind14::to_cpp<to_cpp_1_type>()(arg3))));                       \
    return 0L;                                                                 \
  }                                                                            \
  GAPBIND14_ADD_INIT_MEM_FUNC(module, class_var, gap_fn_var)

#define GAPBIND14_MEM_FN_OVERLOAD(                                           \
    module, class_var, mem_fn_var, gap_fn_var, vrld)                         \
  auto GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload)                   \
      = vrld(&class_var::mem_fn_var);                                        \
  template <typename TFunctionType                                           \
            = decltype(GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload)), \
            typename TSFINAE = Obj>                                          \
  auto GAPBIND14_CONCAT(class_var, gap_fn_var)(Obj self, Obj arg1)           \
      ->std::enable_if_t<!gapbind14::returns_void<TFunctionType>::value      \
                             && gapbind14::arg_count<TFunctionType>::value   \
                                    == 0,                                    \
                         TSFINAE> {                                          \
    GAPBIND14_MEM_FN_START(module, class_var);                               \
    using to_gap_type = gapbind14::to_gap<                                   \
        typename gapbind14::CppFunction<TFunctionType>::return_type>;        \
    GAPBIND14_TRY(                                                           \
        return to_gap_type()(gapbind14::CppFunction<TFunctionType>()(        \
            GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload), ptr)));     \
  }                                                                          \
  template <typename TFunctionType                                           \
            = decltype(GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload)), \
            typename TSFINAE = Obj>                                          \
  auto GAPBIND14_CONCAT(class_var, gap_fn_var)(Obj self, Obj arg1, Obj arg2) \
      ->std::enable_if_t<!gapbind14::returns_void<TFunctionType>::value      \
                             && gapbind14::arg_count<TFunctionType>::value   \
                                    == 1,                                    \
                         TSFINAE> {                                          \
    GAPBIND14_MEM_FN_START(module, class_var);                               \
    using to_cpp_0_type = typename gapbind14::CppFunction<                   \
        TFunctionType>::params_type::template get<0>;                        \
    using to_gap_type = gapbind14::to_gap<                                   \
        typename gapbind14::CppFunction<TFunctionType>::return_type>;        \
    GAPBIND14_TRY(                                                           \
        return to_gap_type()(gapbind14::CppFunction<TFunctionType>()(        \
            GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload),             \
            ptr,                                                             \
            gapbind14::to_cpp<to_cpp_0_type>()(arg2))));                     \
  }                                                                          \
  template <typename TFunctionType                                           \
            = decltype(GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload)), \
            typename TSFINAE = Obj>                                          \
  auto GAPBIND14_CONCAT(class_var,                                           \
                        gap_fn_var)(Obj self, Obj arg1, Obj arg2, Obj arg3)  \
      ->std::enable_if_t<!gapbind14::returns_void<TFunctionType>::value      \
                             && gapbind14::arg_count<TFunctionType>::value   \
                                    == 2,                                    \
                         TSFINAE> {                                          \
    GAPBIND14_MEM_FN_START(module, class_var);                               \
    using to_cpp_0_type = typename gapbind14::CppFunction<                   \
        TFunctionType>::params_type::template get<0>;                        \
    using to_cpp_1_type = typename gapbind14::CppFunction<                   \
        TFunctionType>::params_type::template get<1>;                        \
    using to_gap_type = gapbind14::to_gap<                                   \
        typename gapbind14::CppFunction<TFunctionType>::return_type>;        \
    GAPBIND14_TRY(                                                           \
        return to_gap_type()(gapbind14::CppFunction<TFunctionType>()(        \
            GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload),             \
            ptr,                                                             \
            gapbind14::to_cpp<to_cpp_0_type>()(arg2),                        \
            gapbind14::to_cpp<to_cpp_1_type>()(arg3))));                     \
  }                                                                          \
  template <typename TFunctionType                                           \
            = decltype(GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload)), \
            typename TSFINAE = Obj>                                          \
  auto GAPBIND14_CONCAT(class_var, gap_fn_var)(Obj self, Obj arg1, Obj arg2) \
      ->std::enable_if_t<gapbind14::returns_void<TFunctionType>::value       \
                             && gapbind14::arg_count<TFunctionType>::value   \
                                    == 1,                                    \
                         TSFINAE> {                                          \
    GAPBIND14_MEM_FN_START(module, class_var);                               \
    using to_cpp_0_type = typename gapbind14::CppFunction<                   \
        TFunctionType>::params_type::template get<0>;                        \
    GAPBIND14_TRY(gapbind14::CppFunction<TFunctionType>()(                   \
        GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload),                 \
        ptr,                                                                 \
        gapbind14::to_cpp<to_cpp_0_type>()(arg2)));                          \
    return 0L;                                                               \
  }                                                                          \
  template <typename TFunctionType                                           \
            = decltype(GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload)), \
            typename TSFINAE = Obj>                                          \
  auto GAPBIND14_CONCAT(class_var,                                           \
                        gap_fn_var)(Obj self, Obj arg1, Obj arg2, Obj arg3)  \
      ->std::enable_if_t<gapbind14::returns_void<TFunctionType>::value       \
                             && gapbind14::arg_count<TFunctionType>::value   \
                                    == 2,                                    \
                         TSFINAE> {                                          \
    GAPBIND14_MEM_FN_START(module, class_var);                               \
    using to_cpp_0_type = typename gapbind14::CppFunction<                   \
        TFunctionType>::params_type::template get<0>;                        \
    using to_cpp_1_type = typename gapbind14::CppFunction<                   \
        TFunctionType>::params_type::template get<1>;                        \
    GAPBIND14_TRY(gapbind14::CppFunction<TFunctionType>()(                   \
        GAPBIND14_CONCAT3(class_var, gap_fn_var, _overload),                 \
        ptr,                                                                 \
        gapbind14::to_cpp<to_cpp_0_type>()(arg2),                            \
        gapbind14::to_cpp<to_cpp_1_type>()(arg3)));                          \
    return 0L;                                                               \
  }                                                                          \
  void GAPBIND14_CONCAT3(gapbind14_init_, class_var, gap_fn_var)(            \
      gapbind14::Module & gapbind14_module) {                                \
    gapbind14_module.add_mem_func(GAPBIND14_TO_STRING(class_var),            \
                                  __FILE__,                                  \
                                  GAPBIND14_TO_STRING(gap_fn_var),           \
                                  &GAPBIND14_CONCAT(class_var, gap_fn_var) < \
                                      >);                                    \
  }                                                                          \
  GAPBIND14_UNIQUE_ID gapbind14_push_back(                                   \
      &GAPBIND14_CONCAT3(gapbind14_init_, class_var, gap_fn_var));

#define GAPBIND14_MEM_FN_GET(_1, _2, _3, _4, _5, NAME, ...) NAME
#define GAPBIND14_MEM_FN(...)                                                \
  GAPBIND14_MEM_FN_GET(                                                      \
      __VA_ARGS__, GAPBIND14_MEM_FN_OVERLOAD, GAPBIND14_MEM_FN_NON_OVERLOAD) \
  (__VA_ARGS__)

#define GAPBIND14_ITERATOR(module, class_var, first, last, gap_fn_var)     \
  template <typename Dummy = void>                                         \
  Obj GAPBIND14_CONCAT(class_var, gap_fn_var)(Obj self, Obj arg1) {        \
    class_var* ptr = gapbind14::SubTypeSpec<class_var>::obj_cpp_ptr(arg1); \
    GAPBIND14_TRY(return gapbind14::range_to_plist(ptr->first, ptr->last)) \
  }                                                                        \
  GAPBIND14_ADD_INIT_MEM_FUNC(module, class_var, gap_fn_var)

#endif  // INCLUDE_GAPBIND14_MACROS_HPP_
