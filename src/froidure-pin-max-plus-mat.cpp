//
// Semigroups package for GAP
// Copyright (C) 2021-2025 James D. Mitchell
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

// Semigroups GAP package headers
#include "froidure-pin.hpp"  // for bind_froidure_pin
#include "to_cpp.hpp"        // for to_cpp
#include "to_gap.hpp"        // for to_gap

// libsemigroups headers
#include "libsemigroups/froidure-pin.hpp"  // for FroidurePin
#include "libsemigroups/matrix.hpp"        // for MaxPlusMat etc

// Forward decl
namespace gapbind14 {
  class Module;
}

void init_froidure_pin_max_plus_mat(gapbind14::Module& m) {
  using libsemigroups::MaxPlusMat;
  using libsemigroups::MaxPlusTruncMat;
  using libsemigroups::ProjMaxPlusMat;

  bind_froidure_pin<MaxPlusMat<>>(m, "FroidurePinMaxPlusMat");
  bind_froidure_pin<MaxPlusTruncMat<>>(m, "FroidurePinMaxPlusTruncMat");
  bind_froidure_pin<ProjMaxPlusMat<>>(m, "FroidurePinProjMaxPlusMat");
}
