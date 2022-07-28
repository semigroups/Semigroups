//
// gapbind14
// Copyright (C) 2022 James D. Mitchell
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

// A minimal demo of how to use gapbind14

#include <iostream>  // for cout
#include <string>    // for string

extern "C" {
#include <gap_all.h>  // GAP headers
}

// GAPBIND14: include this header
#include "gapbind14/gapbind14.hpp"

namespace gapbind_demo {
  void testing1() {
    std::ios_base::Init();
    std::cout << "it works\n";
  }

  std::string testing2() {
    return "it works\n";
  }

  void testing3(std::string str) {
    Pr("it works %s", (Int) str.c_str(), 0L);
  }

  std::string testing4(std::string str) {
    return "it works " + str + "\n";
  }

  std::string testing4(int i) {
    return "it works like it's over " + std::to_string(i);
  }

  int testing6(std::string str, size_t i) {
    if (i == 0) {
      throw std::runtime_error("can't take mod of 0!");
    }
    return std::hash<std::string>()(str) % i;
  }

  size_t testing7(size_t i) {
    return i;
  }

  struct Pet {
    explicit Pet(std::string name) : name(name) {}
    void setName(const std::string& name_) {
      name = name_;
    }
    const std::string& getName() const {
      return name;
    }

    std::string getName(const std::string& name_) {
      return name + name_;
    }

    void terminate() {
      name = "dead";
    }

    std::string name;
  };
}  // namespace gapbind_demo

namespace gapbind14 {
  template <>
  struct IsGapBind14Type<gapbind_demo::Pet> : std::true_type {};
};  // namespace gapbind14

GAPBIND14_MODULE(gapbind_demo) {
  gapbind14::InstallGlobalFunction("testing1", &gapbind_demo::testing1);
  gapbind14::InstallGlobalFunction("testing2", &gapbind_demo::testing2);
  gapbind14::InstallGlobalFunction("testing3", &gapbind_demo::testing3);
  gapbind14::InstallGlobalFunction(
      "testing4_string",
      gapbind14::overload_cast<std::string>(&gapbind_demo::testing4));
  gapbind14::InstallGlobalFunction(
      "testing4_int", gapbind14::overload_cast<int>(&gapbind_demo::testing4));
  gapbind14::InstallGlobalFunction("testing6", &gapbind_demo::testing6);
  gapbind14::InstallGlobalFunction("testing7", &gapbind_demo::testing7);
  gapbind14::class_<gapbind_demo::Pet>("Pet")
      .def(gapbind14::init<std::string>{}, "make")
      .def("setName", &gapbind_demo::Pet::setName)
      .def("getName", gapbind14::overload_cast<>(&gapbind_demo::Pet::getName))
      .def("getNamestring",
           gapbind14::overload_cast<std::string const&>(
               &gapbind_demo::Pet::getName))
      .def("terminate", &gapbind_demo::Pet::terminate);
}

static Int InitKernel(StructInitInfo* module) {
  gapbind14::init_kernel("gapbind_demo");
  return 0;
}

static Int InitLibrary(StructInitInfo* module) {
  gapbind14::init_library("gapbind_demo");
  return 0;
}

static StructInitInfo module = {
    /* type        = */ MODULE_DYNAMIC,
    /* name        = */ "gapbind_demo",
    /* revision_c  = */ 0,
    /* revision_h  = */ 0,
    /* version     = */ 0,
    /* crc         = */ 0,
    /* initKernel  = */ InitKernel,
    /* initLibrary = */ InitLibrary,
    /* checkInit   = */ 0,
    /* preSave     = */ 0,
    /* postSave    = */ 0,
    /* postRestore = */ 0,
    /* moduleStateSize      = */ 0,
    /* moduleStateOffsetPtr = */ 0,
    /* initModuleState      = */ 0,
    /* destroyModuleState   = */ 0,
};

extern "C" StructInitInfo* Init__Dynamic(void) {
  return &module;
}
