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
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

#include <iostream>

// GAPBIND14: include this header
#include "gapbind14/gapbind14.hpp"

// GAPBIND14: existing C++ code
void testing1() {
  std::cout << "it works\n";
}

std::string testing2() {
  return "it works\n";
}

void testing3(std::string str) {
  std::cout << "it works " << str << "\n";
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

  std::string getName2(const std::string& name_) {
    return name + name_;
  }

  void terminate() {
    name = "dead";
  }

  std::string name;
};

GAPBIND14_MODULE(demo) {
  gapbind14::InstallGlobalFunction("testing1", &testing1);

  // GAPBIND14_FUNCTION(m, testing1, testing1);
  // GAPBIND14_FUNCTION(m, testing2, testing2);
  // GAPBIND14_FUNCTION(m, testing3, testing3);
  // GAPBIND14_FUNCTION(
  //     m, testing4, testing4, gapbind14::overload_cast<std::string>);
  // GAPBIND14_FUNCTION(m, testing4, testing5, gapbind14::overload_cast<int>);
  // GAPBIND14_FUNCTION(m, testing6, testing6);
  // GAPBIND14_FUNCTION(m, testing7, testing7);

  // GAPBIND14_CLASS(m, Pet);
  // GAPBIND14_CLASS_CONSTRUCTOR(m, Pet, create,
  // gapbind14::to_cpp<std::string>); GAPBIND14_CLASS_MEM_FN(m, Pet, terminate,
  // terminate); GAPBIND14_CLASS_MEM_FN(m, Pet, getName, getName);
}

static Int InitKernel(StructInitInfo* module) {
  gapbind14::init_kernel();
  return 0;
}

static Int InitLibrary(StructInitInfo* module) {
  gapbind14::init_library();
  return 0;
}

/******************************************************************************
 *F  InitInfopl()  . . . . . . . . . . . . . . . . . table of init functions
 */
static StructInitInfo module = {/* type        = */ MODULE_DYNAMIC,
                                /* name        = */ "semigroups",
                                /* revision_c  = */ 0,
                                /* revision_h  = */ 0,
                                /* version     = */ 0,
                                /* crc         = */ 0,
                                /* initKernel  = */ InitKernel,
                                /* initLibrary = */ InitLibrary,
                                /* checkInit   = */ 0,
                                /* preSave     = */ 0,
                                /* postSave    = */ 0,
                                /* postRestore = */ PostRestore};

extern "C" StructInitInfo* Init__Dynamic(void) {
  return &module;
}
