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
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

#include "cong.hpp"

#include <exception>    // for exception
#include <memory>       // for shared_ptr
#include <type_traits>  // for true_type
#include <vector>       // for vector

// Semigroups GAP package headers
#include "froidure-pin.hpp"  // for to_cpp<FroidurePin<Bipartition>
#include "pkg.hpp"           // for IsGapBind14Type
#include "to_cpp.hpp"        // for to_cpp
#include "to_gap.hpp"        // for to_gap

// GAP headers
#include "compiled.h"  // for UInt2, UInt4

// GapBind14 headers
#include "gapbind14/gapbind14.hpp"  // for class_ etc

// libsemigroups headers
#include "libsemigroups/cong.hpp"          // for Congruence
#include "libsemigroups/presentation.hpp"  // for Presentation
#include "libsemigroups/types.hpp"         // for word_type

namespace gapbind14 {
  template <>
  struct IsGapBind14Type<libsemigroups::Congruence<libsemigroups::word_type>>
      : std::true_type {
    static constexpr std::string_view name = "Congruence";
  };

}  // namespace gapbind14

// TODO rm this file
////////////////////////////////////////////////////////////////////////
// Congruence
////////////////////////////////////////////////////////////////////////

using gapbind14::overload_cast;

void init_cong(gapbind14::Module& m) {}
