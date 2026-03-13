//
// Semigroups package for GAP
// Copyright (C) 2021-2026 James D. Mitchell
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

#include "init-to-froidure-pin.hpp"

#include <algorithm>      // for find, fill
#include <iterator>       // for begin, end
#include <list>           // for operator!=
#include <memory>         // for static_pointer...
#include <unordered_map>  // for operator==
#include <utility>        // for declval, move

// Semigroups GAP package headers
#include "pkg.hpp"     // for IsGapBind14Type
#include "to-cpp.hpp"  // for to_cpp
#include "to-gap.hpp"  // for to_gap

// GAP headers
#include "gap_all.h"  // for UInt2, UInt4

// GapBind14 headers
#include "gapbind14/gapbind14.hpp"  // for class_ etc

// libsemigroups headers
#include "libsemigroups/detail/cong-common-class.hpp"  // for CongruenceComm...
#include "libsemigroups/detail/fmt.hpp"                // for print, format
#include "libsemigroups/detail/kbe.hpp"                // for KBE::KBE<Knuth...
#include "libsemigroups/detail/knuth-bendix-impl.hpp"  // for KnuthBendixImp...
#include "libsemigroups/detail/print.hpp"              // for to_printable
#include "libsemigroups/detail/report.hpp"             // for report_default
#include "libsemigroups/detail/string.hpp"             // for group_digits
#include "libsemigroups/detail/timer.hpp"              // for string_time

#include "libsemigroups/cong-class.hpp"          // for Congruence::get
#include "libsemigroups/constants.hpp"           // for operator!=
#include "libsemigroups/froidure-pin-base.hpp"   // for FroidurePinBase
#include "libsemigroups/froidure-pin.hpp"        // for FroidurePin::~...
#include "libsemigroups/kambites-class.hpp"      // for Kambites::~Kam...
#include "libsemigroups/knuth-bendix-class.hpp"  // for KnuthBendix::~...
#include "libsemigroups/order.hpp"               // for lexicographica...
#include "libsemigroups/presentation.hpp"        // for Presentation::...
#include "libsemigroups/runner.hpp"              // for delta
#include "libsemigroups/to-froidure-pin.hpp"     // for to
#include "libsemigroups/todd-coxeter-class.hpp"  // for ToddCoxeter::a...
#include "libsemigroups/types.hpp"               // for word_type
#include "libsemigroups/ukkonen.hpp"             // for maximal_piece_...
#include "libsemigroups/word-range.hpp"          // for human_readable...

using libsemigroups::Congruence;
using libsemigroups::FroidurePin;
using libsemigroups::FroidurePinBase;
using libsemigroups::word_type;

void init_to_froidure_pin(gapbind14::Module& m) {
  gapbind14::InstallGlobalFunction(
      "congruence_to_froidure_pin", [](Congruence<word_type>& c) {
        // to<FroidurePin> for a Congruence returns a std::unique_ptr,
        // which we have trouble dealing with in gapbind14, so we instead
        // just get the raw pointer, and get the std::unique_ptr to release
        // its ownership.
        return std::shared_ptr<FroidurePinBase>(
            libsemigroups::to<FroidurePin>(c).release());
      });
}
