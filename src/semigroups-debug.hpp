//
// Semigroups package for GAP
// Copyright (C) 2017 James D. Mitchell
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

// This file declares kernel debugging functionality.

#ifndef SEMIGROUPS_SRC_SEMIGROUPS_DEBUG_HPP_
#define SEMIGROUPS_SRC_SEMIGROUPS_DEBUG_HPP_

#include <assert.h>

#include "semigroups-config.hpp"

// SEMIGROUPS_ASSERT is a version of 'assert' which is enabled by the
// configure option --enable-debug

#ifdef SEMIGROUPS_KERNEL_DEBUG
#define SEMIGROUPS_ASSERT(x) assert(x)
#else
#define SEMIGROUPS_ASSERT(x)
#endif

#endif  // SEMIGROUPS_SRC_SEMIGROUPS_DEBUG_HPP_
