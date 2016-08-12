//
// Semigroups package for GAP
// Copyright (C) 2016 James D. Mitchell
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

// This file contains some functions for finitely presented semigroups and
// monoids.

#ifndef SRC_SEMIFP_H_
#define SRC_SEMIFP_H_

#include "compiled.h"

Obj FP_SEMI_SIZE(Obj, Obj);
Obj FP_SEMI_EQ(Obj, Obj, Obj, Obj);
Obj FP_SEMI_COSET_ID(Obj, Obj, Obj);

#endif // SRC_SEMIFP_H_
