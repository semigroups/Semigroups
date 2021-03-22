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
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#ifndef SEMIGROUPS_SRC_FROIDURE_PIN_H_
#define SEMIGROUPS_SRC_FROIDURE_PIN_H_

#include "gapbind14/gapbind14.hpp"

#include "libsemigroups/froidure-pin.hpp"

namespace gapbind14 {

  template <typename element_type>
  struct IsGapBind14Type<libsemigroups::FroidurePin<element_type> const&>
      : std::true_type {};

  template <typename element_type>
  struct IsGapBind14Type<libsemigroups::FroidurePin<element_type>&>
      : std::true_type {};
}  // namespace gapbind14

void init_froidure_pin(gapbind14::Module&);

#endif  // SEMIGROUPS_SRC_FROIDURE_PIN_H_
