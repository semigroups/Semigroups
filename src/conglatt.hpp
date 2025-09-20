//
// Semigroups package for GAP
// Copyright (C) 20222-2025 James D. Mitchell
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

// This file contains declarations of a function for computing the lattice of
// congruences of a semigroup from a set of generating congruences.

#ifndef SEMIGROUPS_SRC_CONGLATT_HPP_
#define SEMIGROUPS_SRC_CONGLATT_HPP_

#include "gap_all.h"  // for Obj, UInt

namespace semigroups {
  Obj LATTICE_OF_CONGRUENCES(Obj list);
}

#endif  // SEMIGROUPS_SRC_CONGLATT_HPP_
