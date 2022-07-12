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

// TODO(now)
// * Allow return Fail instead of ErrorQuit in GAPBIND14_TRY
// * Allow custom printing
// * Support enums

#ifndef INCLUDE_GAPBIND14_GAPBIND14_HPP_
#define INCLUDE_GAPBIND14_GAPBIND14_HPP_

#include <sstream>        // for ostringstream
#include <string>         // for string
#include <type_traits>    // for enable_if_t
#include <typeinfo>       // for typeid
#include <unordered_map>  // for unordered_map
#include <utility>        // for make_pair
#include <vector>         // for vector

#include "gap_include.hpp"   // for Obj etc
#include "tame-free-fn.hpp"  // for tame free functions
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

#define GAPBIND14_MODULE(name)                                            \
  static void gapbind14_init_##name(::gapbind14::Module &);               \
                                                                          \
  void ::gapbind14::GAPBIND14_MODULE_IMPL(gapbind14::Module &m) {         \
    ::gapbind14::get_module().set_module_name(GAPBIND14_STRINGIFY(name)); \
    gapbind14_init_##name(m);                                             \
    m.finalize();                                                         \
  }                                                                       \
                                                                          \
  void gapbind14_init_##name(::gapbind14::Module &variable)

////////////////////////////////////////////////////////////////////////
// Typdefs for GAP
////////////////////////////////////////////////////////////////////////

typedef Obj (*GVarFunc)(/*arguments*/);

namespace gapbind14 {

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
  struct IsGapBind14Type<T &> : IsGapBind14Type<T> {};

  template <typename T>
  struct IsGapBind14Type<T &&> : IsGapBind14Type<T> {};

  template <typename T>
  struct IsGapBind14Type<T const &> : IsGapBind14Type<T> {};

  template <typename T>
  struct IsGapBind14Type<std::shared_ptr<T>> : IsGapBind14Type<T> {};

  ////////////////////////////////////////////////////////////////////////
  // Helper functions
  ////////////////////////////////////////////////////////////////////////

  // Convert object to string
  template <typename T>
  std::string to_string(T const &n) {
    std::ostringstream stm;
    stm << n;
    return stm.str();
  }

  void require_gapbind14_obj(Obj o);

  // Get the gapbind14 subtype of the object o.
  gapbind14_subtype obj_subtype(Obj o);

  template <typename T>
  T *obj_cpp_ptr(Obj o) {
    // Couldn't add static_assert that IsGapBind14Type<T>::value is true
    // because sometimes we call this function when T is a base class of a
    // class where IsGapBind14Type<T>::value is true.
    require_gapbind14_obj(o);
    // Also cannot check that subtype of o corresponds to T for the same
    // reason as above T might be a base class of a GapBind14Type.
    return reinterpret_cast<T *>(ADDR_OBJ(o)[1]);
  }

  char const *copy_c_str(std::string const &str);

  char const *params_c_str(size_t nr);

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

    std::string const &name() const {
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

    Subtype(std::string nm, gapbind14_subtype sbtyp) : SubtypeBase(nm, sbtyp) {}

    void free(Obj o) override {
      delete obj_cpp_ptr<T>(o);
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Module class
  ////////////////////////////////////////////////////////////////////////

  class Module {
   private:
    std::vector<StructGVarFunc>                        _funcs;
    std::vector<std::vector<StructGVarFunc>>           _mem_funcs;
    std::string                                        _module_name;
    std::unordered_map<std::string, gapbind14_subtype> _subtype_names;
    std::vector<SubtypeBase *>                         _subtypes;
    std::unordered_map<size_t, gapbind14_subtype>      _type_to_subtype;

   public:
    Module()
        : _funcs(),
          _mem_funcs(),
          _module_name(),
          _subtype_names(),
          _subtypes(),
          _type_to_subtype() {}

    Module(Module const &) = delete;
    Module(Module &&)      = delete;
    Module &operator=(Module const &) = delete;
    Module &operator=(Module &&) = delete;

    ~Module() = default;

    void set_module_name(char const *nm) {
      _module_name = nm;
    }

    gapbind14_subtype subtype(std::string const &subtype_name) const {
      auto it = _subtype_names.find(subtype_name);
      if (it == _subtype_names.end()) {
        throw std::runtime_error("No subtype named " + subtype_name);
      }
      return it->second;
    }

    template <typename Class>
    gapbind14_subtype subtype() const {
      auto it = _type_to_subtype.find(typeid(Class).hash_code());
      if (it == _type_to_subtype.end()) {
        throw std::runtime_error(std::string("No subtype for ")
                                 + typeid(Class).name());
      }
      return it->second;
    }

    const char *name(Obj o) const {
      gapbind14_subtype sbtyp = obj_subtype(o);
      return _subtypes.at(sbtyp)->name().c_str();
    }

    void print(Obj o) {
      Pr("<class %s at %s>", (Int) name(o), (Int) to_string(o).c_str());
    }

    void save(Obj o) {
      SaveUInt(obj_subtype(o));
    }

    void load(Obj o) const;

    void free(Obj o) const {
      gapbind14_subtype sbtyp = obj_subtype(o);
      _subtypes.at(sbtyp)->free(o);
    }

    const char *module_name() const {
      return _module_name.c_str();
    }

    StructGVarFunc const *funcs() const {
      return &_funcs[0];
    }

    StructGVarFunc const *mem_funcs(std::string const &nm) const {
      return &_mem_funcs[subtype(nm)][0];
    }

    template <typename T>
    gapbind14_subtype add_subtype(std::string const &nm) {
      bool inserted
          = _subtype_names.insert(std::make_pair(nm, _subtypes.size())).second;
      if (!inserted) {
        throw std::runtime_error("Subtype named " + nm + " already registered");
      }
      _type_to_subtype.emplace(typeid(T).hash_code(), _subtypes.size());
      _subtypes.push_back(new Subtype<T>(nm, _subtypes.size()));
      _mem_funcs.push_back(std::vector<StructGVarFunc>());
      return _subtypes.back()->subtype();
    }

    template <typename... Args>
    void add_func(std::string        fnm,
                  std::string const &nm,
                  Obj (*func)(Args...)) {
      static_assert(sizeof...(Args) > 0,
                    "there must be at least 1 parameter: Obj self");
      static_assert(sizeof...(Args) <= 7, "Args must be at most 7");
      _funcs.push_back({copy_c_str(nm),
                        sizeof...(Args) - 1,
                        params_c_str(sizeof...(Args) - 1),
                        (GVarFunc) func,
                        copy_c_str(fnm + ":Func" + nm)});
    }

    template <typename... Args>
    void add_mem_func(std::string const &sbtyp,
                      std::string        flnm,
                      std::string const &nm,
                      Obj (*func)(Args...)) {
      // static_assert(sizeof...(Args) > 1,
      //              "there must be at least 1 parameter: Obj self, Obj arg1");
      static_assert(sizeof...(Args) <= 7, "Args must be at most 7");
      _mem_funcs.at(subtype(sbtyp))
          .push_back({copy_c_str(nm),
                      sizeof...(Args) - 1,
                      params_c_str(sizeof...(Args) - 1),
                      (GVarFunc) func,
                      copy_c_str(flnm + ":Func" + sbtyp + "::" + nm)});
    }

    void finalize();

    std::vector<SubtypeBase *>::const_iterator begin() const {
      return _subtypes.cbegin();
    }

    std::vector<SubtypeBase *>::const_iterator end() const {
      return _subtypes.cend();
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Forward declaration
  ////////////////////////////////////////////////////////////////////////

  Module &get_module();
  void    GAPBIND14_MODULE_IMPL(Module &);
  void    init_library();
  void    init_kernel();

  ////////////////////////////////////////////////////////////////////////
  // to_cpp - for gapbind14 gap objects
  ////////////////////////////////////////////////////////////////////////

  template <typename T>
  struct to_cpp<T, std::enable_if_t<IsGapBind14Type<T>::value>> {
    using cpp_type = std::decay_t<T>;

    std::decay_t<T> &operator()(Obj o) const {
      require_gapbind14_obj(o);
      return *obj_cpp_ptr<std::decay_t<T>>(o);
    }
  };

  template <typename T>
  struct to_gap<T, std::enable_if_t<IsGapBind14Type<T>::value>> {
    using cpp_type = T;

    Obj operator()(T const &obj) const {
      return to_gap<T *>()(new T(obj));
    }
  };

  template <typename T>
  struct to_gap<T *> {
    // TODO: add IsGapBind14Type
    using cpp_type = T;

    Obj operator()(T *ptr) const {
      Obj o          = NewBag(T_GAPBIND14_OBJ, 2 * sizeof(Obj));
      ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(get_module().subtype<T>());
      ADDR_OBJ(o)[1] = reinterpret_cast<Obj>(ptr);
      CHANGED_BAG(o);
      return o;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  // Free functions
  ////////////////////////////////////////////////////////////////////////

  template <typename Wild>
  static void InstallGlobalFunction(char const *name, Wild f) {
    auto &       m = get_module();
    size_t const n = all_wilds<Wild>().size();
    all_wilds<Wild>().push_back(f);
    m.add_func(__FILE__, name, get_tame<decltype(&tame<0, Wild>), Wild>(n));
  }

  ////////////////////////////////////////////////////////////////////////
  // Classes
  ////////////////////////////////////////////////////////////////////////

  template <typename... Args>
  struct init {};

  template <typename T, typename... Args>
  T *make(Args... params) {
    return new T(std::forward<Args>(params)...);
  }

  template <typename Class>
  class class_ {
   public:
    class_(std::string name)
        : _module(get_module()),
          _name(name),
          _subtype(_module.add_subtype<Class>(name)) {}

    template <typename... Args>
    class_ &def(init<Args...> x, std::string name = "make") {
      return def(name.c_str(), &make<Class, Args...>);
    }

    template <typename Wild>
    auto def(char const *mem_fn_name, Wild f)
        -> std::enable_if_t<std::is_member_function_pointer<Wild>::value,
                            class_ &> {
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
    auto def(char const *mem_fn_name, Wild f)
        -> std::enable_if_t<!std::is_member_function_pointer<Wild>::value,
                            class_ &> {
      size_t const n = all_wilds<Wild>().size();
      all_wilds<Wild>().push_back(f);
      _module.add_mem_func(_name,
                           __FILE__,
                           mem_fn_name,
                           get_tame<decltype(&tame<0, Wild>), Wild>(n));
      return *this;
    }

   private:
    Module &          _module;
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
               to_gap<typename std::iterator_traits<T>::reference>()(*it));
    }
    return result;
  }
}  // namespace gapbind14

#endif  // INCLUDE_GAPBIND14_GAPBIND14_HPP_
