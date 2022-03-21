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

#include "to_gap.hpp"

#include <memory>         // for unique_ptr, make_unique
#include <unordered_map>  // for unordered_map
#include <utility>        // for pair, make_pair

namespace semigroups {

  NTPSemiring<> const* semiring(size_t threshold, size_t period) {
    static std::unordered_map<std::pair<size_t, size_t>,
                              std::unique_ptr<NTPSemiring<> const>,
                              libsemigroups::Hash<std::pair<size_t, size_t>>>
        cache;

    auto it = cache.find(std::make_pair(threshold, period));
    if (it == cache.end()) {
      it = cache
               .emplace(
                   std::make_pair(threshold, period),
                   std::make_unique<NTPSemiring<> const>(threshold, period))
               .first;
    }
    return it->second.get();
  }
}  // namespace semigroups
