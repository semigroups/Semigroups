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
#include "froidure-pin.hpp"
#include "pkg.h"
#include "to_gap.hpp"

// gapbind14 headers
#include "gapbind14/gapbind14.hpp"

// libsemigroups headers
#include "libsemigroups/bipart.hpp"
#include "libsemigroups/froidure-pin.hpp"
#include "libsemigroups/matrix.hpp"
#include "libsemigroups/pbr.hpp"
#include "libsemigroups/transf.hpp"

using libsemigroups::FroidurePin;

namespace {
  template <typename element_type>
  void froidure_pin(gapbind14::Module& m, std::string name) {
    using FroidurePin_ = FroidurePin<element_type>;
    gapbind14::class_<FroidurePin_>(m, name)
        .def(gapbind14::init<>{}, "make")
        .def(gapbind14::init<FroidurePin_ const&>{}, "copy")
        .def("add_generator", &FroidurePin_::add_generator)
        .def("generator", &FroidurePin_::generator)
        .def("closure",
             &FroidurePin_::template closure<std::vector<element_type>>)
        .def("nr_generators", &FroidurePin_::nr_generators)
        .def("size", &FroidurePin_::size)
        .def("at", &FroidurePin_::at)
        .def("sorted_at", &FroidurePin_::sorted_at)
        .def("current_position", &FroidurePin_::current_position)
        .def("sorted_position", &FroidurePin_::sorted_position)
        .def("nr_idempotents", &FroidurePin_::nr_idempotents)
        .def("enumerate", &FroidurePin_::enumerate)
        .def("left_cayley_graph", &FroidurePin_::left_cayley_graph)
        .def("right_cayley_graph", &FroidurePin_::right_cayley_graph)
        .def("factorisation",
             gapbind14::overload_cast<size_t>(&FroidurePin_::factorisation))
        .def("position_to_sorted_position",
             &FroidurePin_::position_to_sorted_position)
        .def("fast_product", &FroidurePin_::fast_product)
        .def("is_idempotent", &FroidurePin_::is_idempotent)
        .def("finished", &FroidurePin_::finished)
        .def("position", &FroidurePin_::position)
        .def("rules",
             [](FroidurePin_& S) {
               return gapbind14::make_iterator(S.cbegin_rules(),
                                               S.cend_rules());
             })
        .def("idempotents", [](FroidurePin_& S) {
          return gapbind14::make_iterator(S.cbegin_idempotents(),
                                          S.cend_idempotents());
        });
  }
}  // namespace

void init_froidure_pin(gapbind14::Module& m) {
  using libsemigroups::Bipartition;
  using libsemigroups::BMat;
  using libsemigroups::IntMat;
  using libsemigroups::LeastPPerm;
  using libsemigroups::LeastTransf;
  using libsemigroups::MaxPlusMat;
  using libsemigroups::MaxPlusTruncMat;
  using libsemigroups::MinPlusMat;
  using libsemigroups::MinPlusTruncMat;
  using libsemigroups::NTPMat;
  using libsemigroups::PBR;
  using libsemigroups::PPerm;
  using libsemigroups::ProjMaxPlusMat;
  using libsemigroups::Transf;

  froidure_pin<Bipartition>(m, "FroidurePinBipart");
  froidure_pin<BMat<>>(m, "FroidurePinBMat");
  // TODO must implement to_gap/to_cpp for BMat8 + HPCombi::BMat8
  froidure_pin<IntMat<>>(m, "FroidurePinIntMat");
#ifdef LIBSEMIGROUPS_HPCOMBI_ENABLED
  froidure_pin<LeastPPerm<16>>(m, "FroidurePinPPerm16");
  froidure_pin<LeastTransf<16>>(m, "FroidurePinTransf16");
#endif
  froidure_pin<MaxPlusMat<>>(m, "FroidurePinMaxPlusMat");
  froidure_pin<MaxPlusTruncMat<>>(m, "FroidurePinMaxPlusTruncMat");
  froidure_pin<MinPlusMat<>>(m, "FroidurePinMinPlusMat");
  froidure_pin<MinPlusTruncMat<>>(m, "FroidurePinMinPlusTruncMat");
  froidure_pin<NTPMat<>>(m, "FroidurePinNTPMat");
  froidure_pin<PBR>(m, "FroidurePinPBR");
  froidure_pin<PPerm<0, UInt2>>(m, "FroidurePinPPermUInt2");
  froidure_pin<PPerm<0, UInt4>>(m, "FroidurePinPPermUInt4");
  froidure_pin<ProjMaxPlusMat<>>(m, "FroidurePinProjMaxPlusMat");
  froidure_pin<Transf<0, UInt2>>(m, "FroidurePinTransfUInt2");
  froidure_pin<Transf<0, UInt4>>(m, "FroidurePinTransfUInt4");
}
