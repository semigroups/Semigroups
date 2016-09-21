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

// Macros for partial perms in case they are not in gap/src/pperm.h

#ifndef NEW_PPERM2

#define NEW_PPERM2(deg) \
  NewBag(T_PPERM2, (deg + 1) * sizeof(UInt2) + 2 * sizeof(Obj))

#define DEG_PPERM2(f) \
  ((UInt)(SIZE_OBJ(f) - sizeof(UInt2) - 2 * sizeof(Obj)) / sizeof(UInt2))

#define NEW_PPERM4(deg) \
  NewBag(T_PPERM4, (deg + 1) * sizeof(UInt4) + 2 * sizeof(Obj))

#define DEG_PPERM4(f) \
  ((UInt)(SIZE_OBJ(f) - sizeof(UInt4) - 2 * sizeof(Obj)) / sizeof(UInt4))

#define CODEG_PPERM2(f) (*(UInt2*) ((Obj*) (ADDR_OBJ(f)) + 2))
#define CODEG_PPERM4(f) (*(UInt4*) ((Obj*) (ADDR_OBJ(f)) + 2))

#define IS_PPERM(f) (TNUM_OBJ(f) == T_PPERM2 || TNUM_OBJ(f) == T_PPERM4)
#define DEG_PPERM(f) (TNUM_OBJ(f) == T_PPERM2 ? DEG_PPERM2(f) : DEG_PPERM4(f))
#define CODEG_PPERM(f) \
  (UInt)(TNUM_OBJ(f) == T_PPERM2 ? CODEG_PPERM2(f) : CODEG_PPERM4(f))

#endif
