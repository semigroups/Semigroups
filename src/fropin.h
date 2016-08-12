//
// Copyright 2016 James D. Mitchell
//
// This file is part of the Semigroups package for GAP.  The Semigroups package
// for GAP is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// The Semigroups package for GAP is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
// Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// The Semigroup package for GAP.  If not, see <http://www.gnu.org/licenses/>.

// This file contains a implementation of the Froidure-Pin Algorithm which can
// be applied to arbitrary semigroups in GAP (although it won't work well
// unless there is a hash function defined too).

#ifndef SRC_FROPIN_H_
#define SRC_FROPIN_H_

#include "src/compiled.h"

Obj fropin(Obj data, Obj limit, Obj lookfunc, Obj looking);
Obj SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS (Obj, Obj, Obj);
Obj FIND_HCLASSES (Obj, Obj, Obj);

#endif // SRC_FROPIN_H_
