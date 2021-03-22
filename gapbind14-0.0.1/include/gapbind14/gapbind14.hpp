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
// 3. should be possible to use shared_ptr instead of raw ptrs inside the
//    objects

#ifndef INCLUDE_GAPBIND14_GAPBIND14_HPP_
#define INCLUDE_GAPBIND14_GAPBIND14_HPP_

#include <array>          // for array
#include <functional>     // for function
#include <sstream>        // for ostringstream
#include <string>         // for string
#include <tuple>          // for tuple
#include <type_traits>    // for enable_if_t
#include <typeinfo>       // for typeid
#include <unordered_map>  // for unordered_map
#include <unordered_set>  // for unordered_set
#include <utility>        // for make_pair
#include <vector>         // for vector

#include "gap_include.hpp"   // for gmp
#include "tame-free-fn.hpp"  // for tame free functions
#include "tame-make.hpp"     // for tame member functions
#include "tame-mem-fn.hpp"   // for tame constructors
#include "to_cpp.hpp"        // for to_cpp
#include "to_gap.hpp"        // for to_gap

////////////////////////////////////////////////////////////////////////
// Assertions
////////////////////////////////////////////////////////////////////////

#ifdef GAPBIND14_DEBUG
#define GAPBIND14_ASSERT(x) assert(x)
#else
#define GAPBIND14_ASSERT(x)
#endif

////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////

#define GAPBIND14_STRINGIFY(x) #x
#define GAPBIND14_TO_STRING(x) GAPBIND14_STRINGIFY(x)

#define GAPBIND14_MODULE(name, module)                             \
  ::gapbind14::Module module(GAPBIND14_TO_STRING(name));           \
  static void         gapbind14_init_##name(::gapbind14::Module&); \
                                                                   \
  void ::gapbind14::GAPBIND14_MODULE_IMPL(gapbind14::Module& m) {  \
    gapbind14_init_##name(m);                                      \
    m.finalize();                                                  \
  }                                                                \
                                                                   \
  void gapbind14_init_##name(::gapbind14::Module& variable)

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

    // FIXME
    template <typename S, typename T>
    bool is_same_subtype() {
      return std::is_same<S, T>::value;
    }

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
    using class_type = TClass;

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
      // TODO move this into Subtype
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
      return it->second;
    }

    const char* name(Obj o) {
      gapbind14_sub_type sbtyp = Subtype::obj_subtype(o);
      return _subtypes.at(sbtyp)->name().c_str();
    }

    gapbind14_sub_type subtype(Obj o) {
      return Subtype::obj_subtype(o);
    }

    template <typename Class>
    gapbind14_sub_type subtype() {
      auto it = _type_to_subtype.find(typeid(Class).hash_code());
      if (it == _type_to_subtype.end()) {
        throw std::runtime_error(std::string("No subtype for ")
                                 + typeid(Class).name());
      }
      return it->second;
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
    Obj create(gapbind14_sub_type st, Obj args) {
      return static_cast<SubTypeSpec<TClass>*>(_subtypes.at(st))
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
      _type_to_subtype.emplace(typeid(TClass).hash_code(), _next_subtype);
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

    std::vector<StructGVarFunc>              _funcs;
    std::vector<std::vector<StructGVarFunc>> _mem_funcs;
    std::string                              _module_name;
    // TODO remove _next_subtype, it's just _subtypes.size()
    gapbind14_sub_type                                  _next_subtype;
    std::unordered_map<std::string, gapbind14_sub_type> _subtype_names;
    std::vector<Subtype*>                               _subtypes;
    std::unordered_map<size_t, gapbind14_sub_type>      _type_to_subtype;
  };

  Module& get_module();

  ////////////////////////////////////////////////////////////////////////
  // Forward declaration
  ////////////////////////////////////////////////////////////////////////

  void GAPBIND14_MODULE_IMPL(Module&);

  void init_library(Module& m);
  void init_kernel(Module& m);

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

  template <typename TCppType>
  gap_tnum_type const
      to_cpp<TCppType,
             std::enable_if_t<IsGapBind14Type<TCppType>::value>>::gap_type
      = T_GAPBIND14_OBJ;

  ////////////////////////////////////////////////////////////////////////
  // Free functions
  ////////////////////////////////////////////////////////////////////////

  template <typename Wild>
  static void InstallGlobalFunction(Module& m, char const* name, Wild f) {
    size_t const n = all_wilds<Wild>().size();
    all_wilds<Wild>().push_back(f);
    m.add_func(__FILE__, name, get_tame<decltype(&tame<0, Wild>), Wild>(n));
  }

  ////////////////////////////////////////////////////////////////////////
  // Tame constructor
  ////////////////////////////////////////////////////////////////////////

  template <size_t N, typename Class, typename Wild>
  Obj tame_constructor(Obj self, Obj args) {
    return get_module().create<Class, Wild>(get_module().subtype<Class>(),
                                            args);
  }

  ////////////////////////////////////////////////////////////////////////
  // Classes
  ////////////////////////////////////////////////////////////////////////

  template <typename... Args>
  struct init {};

  template <typename Class>
  class class_ {
   public:
    class_(Module& m, std::string name)
        : _module(m), _name(name), _subtype(_module.add_subtype<Class>(name)) {}

    template <typename... Args>
    class_& def(init<Args...> x, std::string name = "make") {
      using Wild = decltype(&make<Class, Args...>);
      _module.add_mem_func(
          _name,
          __FILE__,
          name,
          get_tame_constructor<Class,
                               decltype(&tame_constructor<0, Class, Wild>),
                               Wild>(_subtype));
      return *this;
    }

    template <typename Wild>
    auto def(char const* mem_fn_name, Wild f)
        -> std::enable_if_t<std::is_member_function_pointer<Wild>::value,
                            class_&> {
      size_t const n = all_wild_mem_fns<Wild>().size();
      all_wild_mem_fns<Wild>().push_back(f);
      _module.add_mem_func(
          _name,
          __FILE__,
          mem_fn_name,
          get_tame_mem_fn<decltype(&tame_mem_fn<0, Wild>), Wild>(n));
      return *this;
    }

    template <typename Wild>
    auto def(char const* mem_fn_name, Wild f)
        -> std::enable_if_t<!std::is_member_function_pointer<Wild>::value,
                            class_&> {
      size_t const n = all_wilds<Wild>().size();
      all_wilds<Wild>().push_back(f);
      _module.add_mem_func(_name,
                           __FILE__,
                           mem_fn_name,
                           get_tame<decltype(&tame<0, Wild>), Wild>(n));
      return *this;
    }

   private:
    Module&            _module;
    std::string        _name;
    gapbind14_sub_type _subtype;
  };

  template <typename T>
  Obj make_iterator(T first, T last) {
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
}  // namespace gapbind14

#endif  // INCLUDE_GAPBIND14_GAPBIND14_HPP_
