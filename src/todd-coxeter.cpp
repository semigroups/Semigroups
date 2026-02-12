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

#include "todd-coxeter.hpp"

#include <cstdint>  // for uint32_t
#include <numeric>  // for iota
#include <string>   // for basic_string

// GAP headers
#include "gap_all.h"

// Semigroups pkg headers
#include "pkg.hpp"     // for IsGapBind14Type
#include "to_cpp.hpp"  // for to_cpp
#include "to_gap.hpp"  // for to_gap

// GapBind14 headers
#include "gapbind14/gapbind14.hpp"  // for class_ etc

// libsemigroups headers

#include "libsemigroups/detail/fmt.hpp"    // for format
#include "libsemigroups/detail/print.hpp"  // for to_printable

#include "libsemigroups/presentation.hpp"        // for Presentation
#include "libsemigroups/todd-coxeter-class.hpp"  // for ToddCoxeter
#include "libsemigroups/types.hpp"               // for congruence_kin...
#include "libsemigroups/word-graph.hpp"          // for WordGraph
#include "libsemigroups/word-range.hpp"          // for human_readable...

using libsemigroups::congruence_kind;
using libsemigroups::Presentation;
using libsemigroups::ToddCoxeter;
using libsemigroups::word_type;
using libsemigroups::WordGraph;

void init_todd_coxeter(gapbind14::Module& m) {
  gapbind14::class_<ToddCoxeter<word_type>>("ToddCoxeter")
      .def(gapbind14::init<congruence_kind, Presentation<word_type>>{},
           "make_from_presentation")
      .def(gapbind14::init<congruence_kind, WordGraph<uint32_t>>{},
           "make_from_wordgraph");
}
