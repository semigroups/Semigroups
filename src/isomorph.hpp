//
// Semigroups package for GAP
// Copyright (C) 2025 Pramoth Ragavan
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

#ifndef SEMIGROUPS_SRC_ISOMORPH_HPP_
#define SEMIGROUPS_SRC_ISOMORPH_HPP_

#include "gap_all.h"
#include "semigroups-debug.hpp"  // for SEMIGROUPS_ASSERT

#ifdef __cplusplus
extern "C" {
#endif

Obj PermuteMultiplicationTableNC(Obj self, Obj temp, Obj M, Obj p);
Obj PermuteMultiplicationTable(Obj self, Obj temp, Obj M, Obj p);

#ifdef __cplusplus
}
#endif

#endif  // SEMIGROUPS_SRC_ISOMORPH_HPP_
