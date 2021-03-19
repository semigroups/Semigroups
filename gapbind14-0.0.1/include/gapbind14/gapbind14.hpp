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

// Current limitations:
// 1. Only permits single constructor

// TODO
// 1. Rename SubTypeSpec -> Subtype
// 2. Rename Subtype -> SubtypeBase
// 3. should be possible to use shared_ptr instead of raw ptrs

#ifndef INCLUDE_GAPBIND14_GAPBIND14_HPP_
#define INCLUDE_GAPBIND14_GAPBIND14_HPP_

#include <array>          // for array
#include <functional>     // for function
#include <sstream>        // for ostringstream
#include <string>         // for string
#include <tuple>          // for tuple
#include <type_traits>    // for enable_if_t
#include <unordered_map>  // for unordered_map
#include <unordered_set>  // for unordered_set
#include <utility>        // for make_pair
#include <vector>         // for vector

#include "gap_include.hpp"  // for gmp
#include "macros.hpp"       // for the macros
#include "to_cpp.hpp"       // for to_cpp
#include "to_gap.hpp"       // for to_gap

////////////////////////////////////////////////////////////////////////
// Typdefs for GAP
////////////////////////////////////////////////////////////////////////

typedef Obj (*GVarFunc)(/*arguments*/);

namespace gapbind14 {
  ////////////////////////////////////////////////////////////////////////
  // Typdefs
  ////////////////////////////////////////////////////////////////////////

  using gapbind14_sub_type = size_t;

  ////////////////////////////////////////////////////////////////////////
  // Constants
  ////////////////////////////////////////////////////////////////////////

  extern UInt T_GAPBIND14_OBJ;
  extern Obj  IsObject;
  extern Obj  DeclareOperation;
  extern Obj  InstallMethod;

  template <typename T>
  struct IsGapBind14Type : std::false_type {};

  ////////////////////////////////////////////////////////////////////////
  // Function templates
  ////////////////////////////////////////////////////////////////////////

  // Convert object to string
  template <typename T>
  std::string to_string(T const& n) {
    std::ostringstream stm;
    stm << n;
    return stm.str();
  }

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

  ////////////////////////////////////////////////////////////////////////
  // init - for constructors
  ////////////////////////////////////////////////////////////////////////

  template <typename TClass, typename... TArgs>
  TClass* init(TArgs&&... params) {
    return new TClass(std::forward<TArgs>(params)...);
  }

  void check_args(Obj args, size_t n);

  ////////////////////////////////////////////////////////////////////////
  // Subtype class - for polymorphism
  ////////////////////////////////////////////////////////////////////////

  class Subtype {
   public:
    Subtype(std::string nm, gapbind14_sub_type sbtyp);

    virtual ~Subtype() {}

    std::string const& name() const {
      return _name;
    }

    static gapbind14_sub_type obj_subtype(Obj o) {
      if (TNUM_OBJ(o) != T_GAPBIND14_OBJ) {
        ErrorQuit(
            "expected gapbind14 object but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
      GAPBIND14_ASSERT(SIZE_OBJ(o) == 2);
      return reinterpret_cast<gapbind14_sub_type>(ADDR_OBJ(o)[0]);
    }

    gapbind14_sub_type obj_subtype() {
      return _subtype;
    }

    static void save(Obj o) {
      // The order is not the same as the order that things are stored in the
      // Obj, because we require the subtype first
      SaveUInt(obj_subtype(o));
    }

    virtual void load(Obj o) = 0;
    virtual void free(Obj o) = 0;

   private:
    std::string        _name;
    gapbind14_sub_type _subtype;
  };

  ////////////////////////////////////////////////////////////////////////
  // SubtypeSpec class
  ////////////////////////////////////////////////////////////////////////

  template <typename TClass>
  class SubTypeSpec : public Subtype {
   public:
    SubTypeSpec(std::string nm, gapbind14_sub_type sbtyp)
        : Subtype(nm, sbtyp) {}

    template <typename TFunctionType>
    Obj create_obj(Obj args) {
      return new_bag(new_tclass_ptr<TFunctionType>(args));
    }

    static TClass* obj_cpp_ptr(Obj o) {
      if (TNUM_OBJ(o) != T_GAPBIND14_OBJ) {
        ErrorQuit(
            "expected gapbind14 object but got %s!", (Int) TNAM_OBJ(o), 0L);
      } else if (ADDR_OBJ(o)[1] == nullptr) {
        ErrorQuit("found nullptr expected pointer to C++ class! ", 0L, 0L);
      }

      GAPBIND14_ASSERT(SIZE_OBJ(o) == 2);
      return reinterpret_cast<TClass*>(ADDR_OBJ(o)[1]);
    }

    void free(Obj o) override {
      if (TNUM_OBJ(o) != T_GAPBIND14_OBJ) {
        ErrorQuit(
            "expected gapbind14 object but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
      GAPBIND14_ASSERT(SIZE_OBJ(o) == 2);
      delete reinterpret_cast<TClass*>(ADDR_OBJ(o)[1]);
    }

    void load(Obj o) override {
      GAPBIND14_ASSERT(obj_subtype(o) == obj_subtype());
      // ADDR_OBJ(o)[0] holds the subtype, and is loaded in the Module::load
      ADDR_OBJ(o)[1] = static_cast<Obj>(nullptr);
    }

   private:
    Obj new_bag(Obj cpp_obj) {
      Obj o          = NewBag(T_GAPBIND14_OBJ, 2 * sizeof(Obj));
      ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(obj_subtype());
      ADDR_OBJ(o)[1] = cpp_obj;
      CHANGED_BAG(o);
      return o;
    }

    template <typename TFunctionType, typename TSFINAE = Obj>
    auto new_tclass_ptr(Obj args)
        -> std::enable_if_t<arg_count<TFunctionType>::value == 0, TSFINAE> {
      check_args(args, 0);
      return reinterpret_cast<Obj>(new TClass());
    }

    template <typename TFunctionType, typename TSFINAE = Obj>
    auto new_tclass_ptr(Obj args)
        -> std::enable_if_t<arg_count<TFunctionType>::value == 1, TSFINAE> {
      check_args(args, 1);
      using to_cpp_0_type = typename gapbind14::CppFunction<
          TFunctionType>::params_type::template get<0>;
      Obj arg1 = ELM_LIST(args, 1);
      return reinterpret_cast<Obj>(new TClass(to_cpp<to_cpp_0_type>()(arg1)));
    }

    template <typename TFunctionType, typename TSFINAE = Obj>
    auto new_tclass_ptr(Obj args)
        -> std::enable_if_t<arg_count<TFunctionType>::value == 2, TSFINAE> {
      check_args(args, 2);
      using to_cpp_0_type = typename gapbind14::CppFunction<
          TFunctionType>::params_type::template get<0>;
      using to_cpp_1_type = typename gapbind14::CppFunction<
          TFunctionType>::params_type::template get<1>;
      Obj arg1 = ELM_LIST(args, 1);
      Obj arg2 = ELM_LIST(args, 2);
      return reinterpret_cast<Obj>(new TClass(to_cpp<to_cpp_0_type>()(arg1),
                                              to_cpp<to_cpp_1_type>()(arg2)));
    }

    void check_args(Obj args, size_t n) {
      if (!IS_LIST(args)) {
        ErrorQuit("expected the argument to be a list, found %s",
                  (Int) TNAM_OBJ(args),
                  0L);
      } else if (LEN_LIST(args) != n) {
        ErrorQuit("expected the argument to be a list of length %d, found %d",
                  (Int) n,
                  (Int) LEN_LIST(args));
      }
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Module class
  ////////////////////////////////////////////////////////////////////////

  class Module {
   public:
    Module() : _funcs(), _mem_funcs(), _module_name(), _next_subtype(0) {}

    explicit Module(std::string module_name) : Module() {
      _module_name = module_name;
    }

    Module(Module const&) = delete;
    Module(Module&&)      = delete;
    Module& operator=(Module const&) = default;
    Module& operator=(Module&&) = delete;

    ~Module() = default;

    void print(Obj o) {
      Pr("<class %s at %s>",
         (Int)(_subtypes.at(Subtype::obj_subtype(o))->name().c_str()),
         (Int) to_string(o).c_str());
    }

    void save(Obj o) {
      Subtype::save(o);
    }

    gapbind14_sub_type subtype(std::string const& subtype_name) const {
      auto it = _subtype_names.find(subtype_name);
      if (it == _subtype_names.end()) {
        throw std::runtime_error("No subtype named " + subtype_name);
      }
      return _subtype_names.find(subtype_name)->second;
    }

    const char* name(Obj o) {
      gapbind14_sub_type sbtyp = Subtype::obj_subtype(o);
      return _subtypes.at(sbtyp)->name().c_str();
    }

    gapbind14_sub_type subtype(Obj o) {
      return Subtype::obj_subtype(o);
    }

    void load(Obj o) {
      gapbind14_sub_type sbtyp = LoadUInt();
      ADDR_OBJ(o)[0]           = reinterpret_cast<Obj>(sbtyp);
      _subtypes.at(sbtyp)->load(o);
    }

    void free(Obj o) {
      gapbind14_sub_type sbtyp = Subtype::obj_subtype(o);
      _subtypes.at(sbtyp)->free(o);
    }

    template <typename TClass, typename TFunctionType>
    Obj create(std::string const& subtype_name, Obj args) {
      return static_cast<SubTypeSpec<TClass>*>(
                 _subtypes.at(subtype(subtype_name)))
          ->template create_obj<TFunctionType>(args);
    }

    const char* module_name() const {
      return _module_name.c_str();
    }

    StructGVarFunc const* funcs() const {
      return &_funcs[0];
    }

    StructGVarFunc const* mem_funcs(std::string const& nm) const {
      return &_mem_funcs[subtype(nm)][0];
    }

    template <typename TClass>
    gapbind14_sub_type add_subtype(std::string nm) {
      _subtype_names.insert(std::make_pair(nm, _next_subtype));
      _subtypes.push_back(new SubTypeSpec<TClass>(nm, _next_subtype));
      _next_subtype++;
      _mem_funcs.push_back(std::vector<StructGVarFunc>());
      return _subtypes.back()->obj_subtype();
    }

    template <typename... TArgs>
    void add_func(std::string        fnm,
                  std::string const& nm,
                  Obj (*func)(TArgs...)) {
      static_assert(sizeof...(TArgs) > 0,
                    "there must be at least 1 parameter: Obj self");
      // TODO(now) -> GAPBIND14_ASSERT -> static_assert
      GAPBIND14_ASSERT(sizeof...(TArgs) <= 7);
      _funcs.push_back({copy_c_str(nm),
                        sizeof...(TArgs) - 1,
                        params(sizeof...(TArgs) - 1),
                        (GVarFunc) func,
                        copy_c_str(fnm + ":Func" + nm)});
    }

    template <typename... TArgs>
    void add_mem_func(std::string const& sbtyp,
                      std::string        flnm,
                      std::string const& nm,
                      Obj (*func)(TArgs...)) {
      static_assert(sizeof...(TArgs) > 1,
                    "there must be at least 1 parameter: Obj self, Obj arg1");
      // TODO(now) -> GAPBIND14_ASSERT -> static_assert
      GAPBIND14_ASSERT(sizeof...(TArgs) <= 7);
      _mem_funcs.at(subtype(sbtyp))
          .push_back({copy_c_str(nm),
                      sizeof...(TArgs) - 1,
                      params(sizeof...(TArgs) - 1),
                      (GVarFunc) func,
                      copy_c_str(flnm + ":Func" + sbtyp + "::" + nm)});
    }

    void finalize() {
      for (auto& x : _mem_funcs) {
        x.push_back(StructGVarFunc({0, 0, 0, 0, 0}));
      }
      _funcs.push_back(StructGVarFunc({0, 0, 0, 0, 0}));
    }

    std::vector<Subtype*>::const_iterator begin() const {
      return _subtypes.cbegin();
    }

    std::vector<Subtype*>::const_iterator end() const {
      return _subtypes.cend();
    }

   private:
    static char const* copy_c_str(std::string const& str) {
      char* out = new char[str.size() + 1];  // we need extra char for NUL
      memcpy(out, str.c_str(), str.size() + 1);
      return out;
    }

    char const* params(size_t nr) {
      GAPBIND14_ASSERT(nr <= 6);
      if (nr == 0) {
        return "";
      }
      static std::string params = "arg1, arg2, arg3, arg4, arg5, arg6";
      std::string source(params.cbegin(), params.cbegin() + (nr - 1) * 6);
      source += std::string(params.cbegin() + (nr - 1) * 6,
                            params.cbegin() + (nr - 1) * 6 + 4);
      return copy_c_str(source);
    }

    std::vector<StructGVarFunc>                         _funcs;
    std::vector<std::vector<StructGVarFunc>>            _mem_funcs;
    std::string                                         _module_name;
    gapbind14_sub_type                                  _next_subtype;
    std::unordered_map<std::string, gapbind14_sub_type> _subtype_names;
    std::vector<Subtype*>                               _subtypes;
  };

  ////////////////////////////////////////////////////////////////////////
  // Forward declaration
  ////////////////////////////////////////////////////////////////////////

  void GAPBIND14_MODULE_IMPL(Module&);

  void init_library(Module& m);
  void init_kernel(Module& m);

  template <typename T>
  Obj range_to_plist(T first, T last) {
    size_t N      = std::distance(first, last);
    Obj    result = NEW_PLIST((N == 0 ? T_PLIST_EMPTY : T_PLIST_HOM), N);
    SET_LEN_PLIST(result, N);
    size_t i = 1;
    for (auto it = first; it != last; ++it) {
      AssPlist(result,
               i++,
               to_gap<typename std::iterator_traits<T>::reference>()(*it));
    }
    return result;
  }

  ////////////////////////////////////////////////////////////////////////
  // to_cpp - for gapbind14 gap objects
  ////////////////////////////////////////////////////////////////////////

  template <typename TCppType>
  struct to_cpp<TCppType, std::enable_if_t<IsGapBind14Type<TCppType>::value>> {
    using cpp_type = TCppType;
    static gap_tnum_type const gap_type;

    std::decay_t<TCppType>& operator()(Obj o) {
      if (TNUM_OBJ(o) != T_GAPBIND14_OBJ) {
        ErrorQuit(
            "expected gapbind14 object but got %s!", (Int) TNAM_OBJ(o), 0L);
      }
      return *SubTypeSpec<std::decay_t<TCppType>>::obj_cpp_ptr(o);
    }
  };

  /*template <typename TCppType>
  struct to_gap<TCppType*, std::enable_if_t<IsGapBind14Type<TCppType>::value>> {
    using cpp_type = TCppType;
    static gap_tnum_type const gap_type;

    Obj operator()(TCppType* x) {
      return SubTypeSpec<std::decay_t<TCppType>>::new_bag(
          reinterpret_cast<Obj>(x));
    }
  };*/

  template <typename TCppType>
  gap_tnum_type const
      to_cpp<TCppType,
             std::enable_if_t<IsGapBind14Type<TCppType>::value>>::gap_type
      = T_GAPBIND14_OBJ;

  ////////////////////////////////////////////////////////////////////////
  // New stuff
  ////////////////////////////////////////////////////////////////////////

  template <size_t N>
  struct GapFuncSignature;

  template <>
  struct GapFuncSignature<0> {
    using type = Obj (*)(Obj);
  };

  template <>
  struct GapFuncSignature<1> {
    using type = Obj (*)(Obj, Obj);
  };

  template <typename Wild>
  auto& wilds() {
    static std::vector<Wild> fs;
    return fs;
  }

  template <typename Wild>
  auto wild(size_t i) {
    return wilds<Wild>().at(i);
  }

  template <size_t N, typename Wild, typename TSFINAE = Obj>
  auto tame(Obj self) -> std::enable_if_t<returns_void<Wild>::value
                                              && arg_count<Wild>::value == 0,
                                          TSFINAE> {
    GAPBIND14_TRY(wild<Wild>(N)());
    return 0L;
  }

  template <size_t N, typename Wild, typename TSFINAE = Obj>
  auto tame(Obj self, Obj arg1)
      -> std::enable_if_t<returns_void<Wild>::value
                              && arg_count<Wild>::value == 1,
                          TSFINAE> {
    using to_cpp_0_type =
        typename CppFunction<Wild>::params_type::template get<0>;
    GAPBIND14_TRY(wild<Wild>(N)(to_cpp<to_cpp_0_type>()(arg1)));
    return 0L;
  }

  template <size_t N, typename Tame, typename Wild>
  struct static_push_back {
    void operator()(std::vector<Tame>& v) {
      v.push_back(&tame<N - 1, Wild>);
      static_push_back<N - 1, Tame, Wild>{}(v);
    }
  };

  template <typename Tame, typename Wild>
  struct static_push_back<0, Tame, Wild> {
    void operator()(std::vector<Tame>& v) {
      std::reverse(v.begin(), v.end());
    }
  };

  template <typename Tame, typename Wild>
  auto& tames() {
    static std::vector<Tame> fs;
    // Should only do the following one time
    static_push_back<32, Tame, Wild>{}(fs);
    return fs;
  }

  template <typename Tame, typename Wild>
  auto get_tame(size_t i) {
    return tames<Tame, Wild>().at(i);
  }

  template <typename Wild>
  static void InstallGlobalFunction(Module& m, char const* name, Wild f) {
    size_t const n = wilds<Wild>().size();
    wilds<Wild>().push_back(f);
    m.add_func(__FILE__, name, get_tame<decltype(&tame<0, Wild>), Wild>(n));
  }

  // static void InstallGlobalFunction(Module& m, char const* name, void (*f)())
  // {
  //   size_t const n = wild_function_pointers<void>().size();
  //   wild_function_pointers<void>().push_back(f);
  //   m.add_func(__FILE__, name, tame<void>(n));
  // }

}  // namespace gapbind14

#endif  // INCLUDE_GAPBIND14_GAPBIND14_HPP_
