//
// Semigroups package for GAP
// Copyright (C) 2021 James D. Mitchell
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

// Semigroups GAP package headers
#include "froidure-pin.hpp"  // for bind_froidure_pin
#include "to_cpp.hpp"        // for to_cpp
#include "to_gap.hpp"        // for to_gap

// libsemigroups headers
#include "libsemigroups/bipart.hpp"        // for Bipartition
#include "libsemigroups/froidure-pin.hpp"  // for FroidurePin

// Forward decl
namespace gapbind14 {
  class Module;
}

void init_froidure_pin_bipart(gapbind14::Module& m) {
  using libsemigroups::Bipartition;
  bind_froidure_pin<Bipartition>(m, "FroidurePinBipart");
}

void init_froidure_pin_base(gapbind14::Module& m) {
  using FroidurePin_ = std::shared_ptr<libsemigroups::FroidurePinBase>;
  gapbind14::class_<FroidurePin_>(m, "FroidurePinBase")
      .def("size", [](FroidurePin_ S) { return S->size(); });
  // .def("enumerate", &FroidurePin_::enumerate)
  // .def("left_cayley_graph", &FroidurePin_::left_cayley_graph)
  // .def("right_cayley_graph", &FroidurePin_::right_cayley_graph)
  // .def("factorisation",
  //      gapbind14::overload_cast<size_t>(&FroidurePin_::factorisation))
  // .def("position_to_sorted_position",
  //      &FroidurePin_::position_to_sorted_position)
  // .def("fast_product", &FroidurePin_::fast_product)
  // .def("finished", &FroidurePin_::finished)
  // .def("rules", [](FroidurePin_ S) {
  //   return gapbind14::make_iterator(S->cbegin_rules(), S->cend_rules());
  // });
}
