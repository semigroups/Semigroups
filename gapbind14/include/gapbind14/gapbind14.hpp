//
// gapbind14
// Copyright (C) 2021-2022 James D. Mitchell
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

// TODO:
// * Allow return Fail instead of ErrorQuit in GAPBIND14_TRY
// * Allow custom printing
// * Support enums
// * Support operators
// * tests for gapbind14/demo
// * update gapbind14/README.md
// * gapbind: iwyu in all files
// * gapbind: const/noexcept in all files

#ifndef INCLUDE_GAPBIND14_GAPBIND14_HPP_
#define INCLUDE_GAPBIND14_GAPBIND14_HPP_

#include <cstddef>        // for size_t
#include <iterator>       // for distance, iterator_traits
#include <memory>         // for shared_ptr
#include <sstream>        // for ostringstream
#include <stdexcept>      // for runtime_error
#include <string>         // for string
#include <type_traits>    // for enable_if_t
#include <typeinfo>       // for typeid
#include <unordered_map>  // for unordered_map
#include <utility>        // for make_pair
#include <vector>         // for vector

#include "gap_include.hpp"   // for Obj etc
#include "tame_free_fn.hpp"  // for tame free functions
#include "tame_mem_fn.hpp"   // for tame member functions
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

#define GAPBIND14_MODULE(name)                                                 \
  static void gapbind14_init_##name();                                         \
  int         gapbind14_dummy_var_##name                                       \
      = ::gapbind14::detail::emplace_init_func(#name, &gapbind14_init_##name); \
  void gapbind14_init_##name()

////////////////////////////////////////////////////////////////////////
// Typdefs for GAP
////////////////////////////////////////////////////////////////////////

namespace gapbind14 {

  // Forward decl
  template <typename T, typename>
  struct to_cpp;

  ////////////////////////////////////////////////////////////////////////
  // Aliases
  ////////////////////////////////////////////////////////////////////////

  using gapbind14_subtype = size_t;

  ////////////////////////////////////////////////////////////////////////
  // Constants
  ////////////////////////////////////////////////////////////////////////

  extern UInt T_GAPBIND14_OBJ;

  template <typename T>
  struct IsGapBind14Type : std::false_type {};

  template <typename T>
  struct IsGapBind14Type<T&> : IsGapBind14Type<T> {};

  template <typename T>
  struct IsGapBind14Type<T&&> : IsGapBind14Type<T> {};

  template <typename T>
  struct IsGapBind14Type<T const&> : IsGapBind14Type<T> {};

  template <typename T>
  struct IsGapBind14Type<std::shared_ptr<T>> : IsGapBind14Type<T> {};

  void init_library(char const*);
  void init_kernel(char const*);

  class Module;  // Forward decl
  Module& module();

  ////////////////////////////////////////////////////////////////////////
  // Helper functions
  ////////////////////////////////////////////////////////////////////////

  namespace detail {

    std::unordered_map<std::string, void (*)()>& init_funcs();

    int emplace_init_func(char const* module_name, void (*func)());

    // Convert object to string
    template <typename T>
    std::string to_string(T const& n) {
      std::ostringstream stm;
      stm << n;
      return stm.str();
    }

    void require_gapbind14_obj(Obj o);

    // Get the gapbind14 subtype of the object o.
    gapbind14_subtype obj_subtype(Obj o);

    template <typename T>
    T* obj_cpp_ptr(Obj o) {
      // Couldn't add static_assert that IsGapBind14Type<T>::value is true
      // because sometimes we call this function when T is a base class of a
      // class where IsGapBind14Type<T>::value is true.
      require_gapbind14_obj(o);
      // Also cannot check that subtype of o corresponds to T for the same
      // reason as above T might be a base class of a GapBind14Type.
      return reinterpret_cast<T*>(ADDR_OBJ(o)[1]);
    }

    char const* copy_c_str(std::string const& str);

    char const* params_c_str(size_t nr);

    ////////////////////////////////////////////////////////////////////////
    // SubtypeBase class - for polymorphism
    ////////////////////////////////////////////////////////////////////////

    class SubtypeBase {
     private:
      std::string       _name;
      gapbind14_subtype _subtype;

     public:
      SubtypeBase(std::string nm, gapbind14_subtype sbtyp);

      virtual ~SubtypeBase() {}

      std::string const& name() const {
        return _name;
      }

      gapbind14_subtype subtype() const noexcept {
        return _subtype;
      }

      virtual void free(Obj o) = 0;
    };

    ////////////////////////////////////////////////////////////////////////
    // Subtype class
    ////////////////////////////////////////////////////////////////////////

    template <typename T>
    class Subtype : public SubtypeBase {
     public:
      using class_type = T;

      Subtype(std::string nm, gapbind14_subtype sbtyp)
          : SubtypeBase(nm, sbtyp) {}

      void free(Obj o) override {
        static_assert(IsGapBind14Type<T>::value,
                      "template parameter T must satisfy IsGapbind14Type<T>");
        GAPBIND14_ASSERT(obj_subtype(o) == module().subtype<T>());
        delete detail::obj_cpp_ptr<T>(o);
      }
    };
  }  // namespace detail

  ////////////////////////////////////////////////////////////////////////
  // Module class
  ////////////////////////////////////////////////////////////////////////

  class Module {
   private:
    std::vector<StructGVarFunc>                        _funcs;
    std::vector<std::vector<StructGVarFunc>>           _mem_funcs;
    std::unordered_map<std::string, gapbind14_subtype> _subtype_names;
    std::vector<detail::SubtypeBase*>                  _subtypes;
    std::unordered_map<size_t, gapbind14_subtype>      _type_to_subtype;

    static size_t _next_subtype;

   public:
    Module()
        : _funcs(),
          _mem_funcs(),
          _subtype_names(),
          _subtypes(),
          _type_to_subtype() {}

    Module(Module const&)            = delete;
    Module(Module&&)                 = delete;
    Module& operator=(Module const&) = delete;
    Module& operator=(Module&&)      = delete;

    ~Module();

    void clear();

    gapbind14_subtype subtype(std::string const& subtype_name) const;

    template <typename T>
    gapbind14_subtype subtype() const {
      auto it = _type_to_subtype.find(typeid(T).hash_code());
      if (it == _type_to_subtype.end()) {
        throw std::runtime_error(std::string("No subtype for ")
                                 + typeid(T).name());
      }
      return it->second;
    }

    const char* subtype_name(Obj o) const {
      return _subtypes.at(detail::obj_subtype(o))->name().c_str();
    }

    void print(Obj o) {
      Pr("<class %s at %s>",
         (Int) subtype_name(o),
         (Int) detail::to_string(o).c_str());
    }

#ifdef GAP_ENABLE_SAVELOAD
    void save(Obj o) {
      SaveUInt(detail::obj_subtype(o));
    }

    void load(Obj o) const;
#endif

    void free(Obj o) const {
      _subtypes.at(detail::obj_subtype(o))->free(o);
    }

    StructGVarFunc const* funcs() const {
      return &_funcs[0];
    }

    StructGVarFunc const* mem_funcs(std::string const& nm) const {
      return &_mem_funcs[subtype(nm)][0];
    }

    template <typename T>
    gapbind14_subtype add_subtype(std::string const& nm) {
      bool inserted
          = _subtype_names.insert(std::make_pair(nm, _subtypes.size())).second;
      if (!inserted) {
        throw std::runtime_error("Subtype named " + nm + " already registered");
      }
      _type_to_subtype.emplace(typeid(T).hash_code(), _subtypes.size());
      _subtypes.push_back(new detail::Subtype<T>(nm, _subtypes.size()));
      _mem_funcs.push_back(std::vector<StructGVarFunc>());
      return _subtypes.back()->subtype();
    }

    template <typename... Args>
    void add_func(std::string        fnm,
                  std::string const& nm,
                  Obj (*func)(Args...)) {
      static_assert(sizeof...(Args) > 0,
                    "there must be at least 1 parameter: Obj self");
      static_assert(sizeof...(Args) <= 7, "Args must be at most 7");
      _funcs.push_back({detail::copy_c_str(nm),
                        sizeof...(Args) - 1,
                        detail::params_c_str(sizeof...(Args) - 1),
                        (ObjFunc) func,
                        detail::copy_c_str(fnm + ":Func" + nm)});
    }

    template <typename... Args>
    void add_mem_func(std::string const& sbtyp,
                      std::string        flnm,
                      std::string const& nm,
                      Obj (*func)(Args...)) {
      static_assert(sizeof...(Args) <= 7, "Args must be at most 7");
      _mem_funcs.at(subtype(sbtyp))
          .push_back({detail::copy_c_str(nm),
                      sizeof...(Args) - 1,
                      detail::params_c_str(sizeof...(Args) - 1),
                      (ObjFunc) func,
                      detail::copy_c_str(flnm + ":Func" + sbtyp + "::" + nm)});
    }

    void finalize();

    std::vector<detail::SubtypeBase*>::const_iterator begin() const noexcept {
      return _subtypes.cbegin();
    }

    std::vector<detail::SubtypeBase*>::const_iterator end() const noexcept {
      return _subtypes.cend();
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // to_cpp/gap - for gapbind14 gap objects
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_cpp<T, std::enable_if_t<IsGapBind14Type<T>::value>> {
    using cpp_type = std::decay_t<T>;

    std::decay_t<T>& operator()(Obj o) const {
      detail::require_gapbind14_obj(o);
      return *detail::obj_cpp_ptr<std::decay_t<T>>(o);
    }
  };

  template <typename T>
  struct to_gap<T, std::enable_if_t<IsGapBind14Type<T>::value>> {
    using cpp_type = T;

    Obj operator()(T const& obj) const {
      return to_gap<T*>()(new T(obj));
    }
  };

  template <typename T>
  struct to_gap<T*, std::enable_if_t<IsGapBind14Type<T>::value>> {
    using cpp_type = T;

    Obj operator()(T* ptr) const {
      Obj o          = NewBag(T_GAPBIND14_OBJ, 2 * sizeof(Obj));
      ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(module().subtype<T>());
      ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(ptr);
      CHANGED_BAG(o);
      return o;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Free functions
  ////////////////////////////////////////////////////////////////////////

  template <typename Wild>
  static void InstallGlobalFunction(char const* name, Wild f) {
    size_t const n = detail::all_wilds<Wild>().size();
    detail::all_wilds<Wild>().push_back(f);
    module().add_func(
        __FILE__,
        name,
        detail::get_tame<decltype(&detail::tame<0, Wild>), Wild>(n));
  }

  ////////////////////////////////////////////////////////////////////////
  // Tes
  ////////////////////////////////////////////////////////////////////////

  template <typename... Args>
  struct init {};

  namespace detail {
    template <typename T, typename... Args>
    T* make(Args... params) {
      return new T(std::forward<Args>(params)...);
    }
  }  // namespace detail

  template <typename T>
  class class_ {
   public:
    class_(std::string name)
        : _name(name), _subtype(module().add_subtype<T>(name)) {}

    template <typename... Args>
    class_& def(init<Args...> x, std::string name = "make") {
      return def(name.c_str(), &detail::make<T, Args...>);
    }

    template <typename Wild>
    auto def(char const* mem_fn_name, Wild f)
        -> std::enable_if_t<std::is_member_function_pointer<Wild>::value,
                            class_&> {
      size_t const n = detail::all_wild_mem_fns<Wild>().size();
      detail::all_wild_mem_fns<Wild>().push_back(f);
      module().add_mem_func(
          _name,
          __FILE__,
          mem_fn_name,
          detail::get_tame_mem_fn<decltype(&detail::tame_mem_fn<0, Wild>),
                                  Wild>(n));
      return *this;
    }

    template <typename Wild>
    auto def(char const* mem_fn_name, Wild f)
        -> std::enable_if_t<!std::is_member_function_pointer<Wild>::value,
                            class_&> {
      size_t const n = detail::all_wilds<Wild>().size();
      detail::all_wilds<Wild>().push_back(f);
      module().add_mem_func(
          _name,
          __FILE__,
          mem_fn_name,
          detail::get_tame<decltype(&detail::tame<0, Wild>), Wild>(n));
      return *this;
    }

   private:
    std::string       _name;
    gapbind14_subtype _subtype;
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
               to_gap<typename std::iterator_traits<T>::value_type>()(*it));
    }
    return result;
  }
}  // namespace gapbind14

#endif  // INCLUDE_GAPBIND14_GAPBIND14_HPP_
