//
// gapbind14
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
//

#ifndef INCLUDE_GAPBIND14_CPP_FN_HPP_
#define INCLUDE_GAPBIND14_CPP_FN_HPP_

namespace gapbind14 {

  constexpr size_t MAX_FUNCTIONS = 32;
  extern UInt      T_GAPBIND14_OBJ;

  ////////////////////////////////////////////////////////////////////////
  // Overloading
  ////////////////////////////////////////////////////////////////////////

  template <typename... TArgs>
  struct overload_cast_impl {
    constexpr overload_cast_impl() {}

    template <typename TReturn>
    constexpr auto operator()(TReturn (*pf)(TArgs...)) const noexcept
        -> decltype(pf) {
      return pf;
    }

    template <typename TReturn, typename TClass>
    constexpr auto operator()(TReturn (TClass::*pmf)(TArgs...),
                              std::false_type = {}) const noexcept
        -> decltype(pmf) {
      return pmf;
    }

    template <typename TReturn, typename TClass>
    constexpr auto operator()(TReturn (TClass::*pmf)(TArgs...)
                                  const) const noexcept -> decltype(pmf) {
      return pmf;
    }
  };

  static constexpr auto const_ = std::true_type{};

  template <typename... TArgs>
  static constexpr overload_cast_impl<TArgs...> overload_cast = {};

  ////////////////////////////////////////////////////////////////////////
  // Function return type and parameter type info
  ////////////////////////////////////////////////////////////////////////

  // For parameter packs . . .
  template <typename... TArgs>
  struct Pack {
    template <size_t i>
    using get = std::tuple_element_t<i, std::tuple<TArgs...>>;
  };

  template <typename TReturnType, typename... TArgs>
  struct CppFunctionBase {
    using arg_count   = std::integral_constant<unsigned, sizeof...(TArgs)>;
    using return_type = TReturnType;
    using params_type = Pack<TArgs...>;

    // Function pointers . . .
    template <typename SFINAE = TReturnType>
    auto operator()(TReturnType (*f)(), TArgs... args)
        -> std::enable_if_t<sizeof...(TArgs) == 0, SFINAE> {
      return f();
    }

    template <typename SFINAE = TReturnType>
    auto operator()(TReturnType (*f)(TArgs...), TArgs... args)
        -> std::enable_if_t<sizeof...(TArgs) != 0, SFINAE> {
      return f(args...);
    }

    template <typename SFINAE = TReturnType>
    auto operator()(std::function<TReturnType(TArgs...)> f, TArgs... args)
        -> std::enable_if_t<sizeof...(TArgs) == 0, SFINAE> {
      return f();
    }
  };

  // Member functions . . .
  template <typename TClass, typename TReturnType, typename... TArgs>
  struct CppMemFnBase {
    using arg_count   = std::integral_constant<unsigned, sizeof...(TArgs)>;
    using return_type = TReturnType;
    using params_type = Pack<TArgs...>;
    using class_type  = TClass;

    template <typename SFINAE = TReturnType>
    auto operator()(TReturnType (TClass::*mem_fn)(TArgs...),
                    TClass* ptr,
                    TArgs... args)
        -> std::enable_if_t<sizeof...(TArgs) != 0, SFINAE> {
      return (ptr->*mem_fn)(args...);
    }

    template <typename SFINAE = TReturnType>
    auto operator()(TReturnType (TClass::*mem_fn)(TArgs...),
                    TClass* ptr,
                    TArgs... args)
        -> std::enable_if_t<sizeof...(TArgs) == 0, SFINAE> {
      return (ptr->*mem_fn)();
    }

    template <typename SFINAE = TReturnType>
    auto operator()(TReturnType (TClass::*mem_fn)(TArgs...) const,
                    TClass* ptr,
                    TArgs... args)
        -> std::enable_if_t<sizeof...(TArgs) != 0, SFINAE> {
      return (ptr->*mem_fn)(args...);
    }

    template <typename SFINAE = TReturnType>
    auto operator()(TReturnType (TClass::*mem_fn)(TArgs...) const,
                    TClass* ptr,
                    TArgs... args)
        -> std::enable_if_t<sizeof...(TArgs) == 0, SFINAE> {
      return (ptr->*mem_fn)();
    }
  };

  // Base declaration . . .
  template <typename TSignature>
  struct CppFunction {};

  // Free functions . . .
  template <typename TReturnType, typename... TArgs>
  struct CppFunction<TReturnType(TArgs...)>
      : CppFunctionBase<TReturnType, TArgs...> {};

  // Function pointers . . .
  template <typename TReturnType, typename... TArgs>
  struct CppFunction<TReturnType (*)(TArgs...)>
      : CppFunctionBase<TReturnType, TArgs...> {};

  // Member functions . . .
  template <typename TClass, typename TReturnType, typename... TArgs>
  struct CppFunction<TReturnType (TClass::*)(TArgs...)>
      : CppMemFnBase<TClass, TReturnType, TArgs...> {};

  // Const member functions
  template <typename TClass, typename TReturnType, typename... TArgs>
  struct CppFunction<TReturnType (TClass::*)(TArgs...) const>
      : CppMemFnBase<TClass, TReturnType, TArgs...> {};

  // std::function objects
  template <typename TReturnType, typename... TArgs>
  struct CppFunction<std::function<TReturnType(TArgs...)>>
      : CppFunctionBase<TReturnType, TArgs...> {};

  // TODO Lambdas?

  // For convenience . . .
  template <typename TFunctionType>
  using returns_void
      = std::is_void<typename CppFunction<TFunctionType>::return_type>;

  template <typename TFunctionType>
  using arg_count = typename CppFunction<TFunctionType>::arg_count;
}  // namespace gapbind14
#endif  // INCLUDE_GAPBIND14_CPP_FN_HPP_
