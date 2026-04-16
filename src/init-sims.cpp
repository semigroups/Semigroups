//
// Semigroups package for GAP
// Copyright (C) 2026 James D. Mitchell
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

#include "init-sims.hpp"

// Semigroups GAP package headers
#include "pkg.hpp"     // for IsGapBind14Type
#include "to-cpp.hpp"  // for to_cpp
#include "to-gap.hpp"  // for to_gap

// GAP headers
#include "gap_all.h"  // for UInt2, UInt4

// GapBind14 headers
#include "gapbind14/gapbind14.hpp"  // for class_ etc

// libsemigroups headers
#include "libsemigroups/sims.hpp"  // for Sims1, RepOrc

using Sims1  = libsemigroups::Sims1;
using RepOrc = libsemigroups::RepOrc;

template <typename Word>
using Presentation = libsemigroups::Presentation<Word>;

using word_type = libsemigroups::word_type;

void init_sims(gapbind14::Module& m) {
  gapbind14::class_<typename Sims1::iterator>("Sims1Iterator")
      .def("increment", [](typename Sims1::iterator& it) { ++it; })
      .def("deref", [](typename Sims1::iterator const& it) { return *it; });

  gapbind14::class_<Sims1>("Sims1")
      .def(gapbind14::init<Presentation<word_type>>{}, "make")
      .def("number_of_threads",
           [](Sims1& s, size_t val) { s.number_of_threads(val); })
      .def("number_of_congruences", &Sims1::number_of_congruences)
      .def("cbegin", &Sims1::cbegin)
      .def("cbegin_long_rules",
           [](Sims1& s, size_t pos) { s.cbegin_long_rules(pos); });

  gapbind14::InstallGlobalFunction(
      "sims1_add_included_pair",
      [](Sims1& sims1, word_type const& u, word_type const& v) {
        libsemigroups::sims::add_included_pair(sims1, u, v);
      });

  gapbind14::class_<RepOrc>("RepOrc")
      .def(gapbind14::init<>{}, "make")
      .def("number_of_threads",
           [](RepOrc& ro, size_t val) { ro.number_of_threads(val); })
      .def("presentation",
           [](RepOrc& ro, Presentation<word_type> const& p) {
             ro.presentation(p);
           })
      .def("max_nodes", [](RepOrc& ro, size_t val) { ro.max_nodes(val); })
      .def("min_nodes", [](RepOrc& ro, size_t val) { ro.min_nodes(val); })
      .def("target_size", [](RepOrc& ro, size_t val) { ro.target_size(val); })
      .def("word_graph", &RepOrc::word_graph);
}
