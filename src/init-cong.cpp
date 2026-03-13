//
// Semigroups package for GAP
// Copyright (C) 2021-2026 James D. Mitchell
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

#include "init-cong.hpp"

#include <algorithm>  // for max
#include <chrono>
#include <complex>
#include <iterator>
#include <list>
#include <memory>
#include <numeric>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

// Semigroups GAP package headers
#include "pkg.hpp"     // for IsGapBind14Type
#include "to-cpp.hpp"  // for to_cpp
#include "to-gap.hpp"  // for to_gap

// GAP headers
#include "gap_all.h"  // for UInt2, UInt4

// GapBind14 headers
#include "gapbind14/gapbind14.hpp"  // for class_ etc

// libsemigroups headers
#include "libsemigroups/detail/eigen.hpp"  // for Eigen
#include "libsemigroups/detail/fmt.hpp"    // for fmt
#include "libsemigroups/detail/iterator.hpp"
#include "libsemigroups/detail/knuth-bendix-nf.hpp"
#include "libsemigroups/detail/path-iterators.hpp"
#include "libsemigroups/detail/print.hpp"
#include "libsemigroups/detail/report.hpp"
#include "libsemigroups/detail/rewriters.hpp"
#include "libsemigroups/detail/string.hpp"
#include "libsemigroups/detail/timer.hpp"
#include "libsemigroups/detail/todd-coxeter-impl.hpp"

#include "libsemigroups/cong-class.hpp"
#include "libsemigroups/cong-common-helpers.hpp"
#include "libsemigroups/cong-helpers.hpp"
#include "libsemigroups/constants.hpp"
#include "libsemigroups/forest.hpp"
#include "libsemigroups/kambites-class.hpp"
#include "libsemigroups/knuth-bendix-class.hpp"
#include "libsemigroups/knuth-bendix-helpers.hpp"
#include "libsemigroups/obvinf.hpp"
#include "libsemigroups/order.hpp"
#include "libsemigroups/paths-count.hpp"
#include "libsemigroups/paths.hpp"
#include "libsemigroups/presentation.hpp"
#include "libsemigroups/ranges.hpp"
#include "libsemigroups/runner.hpp"
#include "libsemigroups/todd-coxeter-class.hpp"
#include "libsemigroups/todd-coxeter-helpers.hpp"
#include "libsemigroups/types.hpp"
#include "libsemigroups/ukkonen.hpp"
#include "libsemigroups/word-graph-helpers.hpp"
#include "libsemigroups/word-graph-view.hpp"
#include "libsemigroups/word-graph.hpp"
#include "libsemigroups/word-range.hpp"

using libsemigroups::Congruence;
using libsemigroups::Presentation;
using libsemigroups::word_type;

void init_cong(gapbind14::Module& m) {
  gapbind14::class_<Congruence<word_type>>("Congruence")
      .def(gapbind14::init<congruence_kind, Presentation<word_type>>{}, "make")
      .def("number_of_generating_pairs",
           &Congruence<word_type>::number_of_generating_pairs)
      .def("add_generating_pair",
           [](Congruence<word_type>& self,
              word_type const&       u,
              word_type const&       v) {
             return libsemigroups::congruence::add_generating_pair(self, u, v);
           })
      .def("number_of_classes", &Congruence<word_type>::number_of_classes)
      .def("contains",
           [](Congruence<word_type>& self,
              word_type const&       u,
              word_type const&       v) {
             // FIXME the following is a hack to make one test file work
             self.run_for(std::chrono::milliseconds(10));
             return libsemigroups::congruence::contains(self, u, v);
           })
      .def("reduce", [](Congruence<word_type>& self, word_type const& u) {
        return libsemigroups::congruence::reduce(self, u);
      });

  gapbind14::InstallGlobalFunction(
      "congruence_normal_forms", [](Congruence<word_type>& c) {
        using ToddCoxeter = libsemigroups::ToddCoxeter<word_type>;
        using KnuthBendix = libsemigroups::KnuthBendix<word_type>;

        c.run();
        if (c.has<ToddCoxeter>() && c.get<ToddCoxeter>()->finished()) {
          auto nf = libsemigroups::todd_coxeter::normal_forms(
              *c.get<ToddCoxeter>());
          return gapbind14::make_iterator(nf);
        } else if (c.has<KnuthBendix>() && c.get<KnuthBendix>()->finished()) {
          auto nf = libsemigroups::knuth_bendix::normal_forms(
              *c.get<KnuthBendix>());
          return gapbind14::make_iterator(nf);
        }
        throw std::runtime_error("Cannot compute normal forms!");
      });

  gapbind14::InstallGlobalFunction(
      "congruence_non_trivial_classes",
      [](Congruence<word_type>& c, std::vector<word_type> const& words) {
        auto ntc = libsemigroups::congruence::non_trivial_classes(
            c, words.begin(), words.end());
        return gapbind14::make_iterator(ntc.begin(), ntc.end());
      });

  gapbind14::InstallGlobalFunction(
      "infinite_congruence_non_trivial_classes",
      [](Congruence<word_type>& super, Congruence<word_type>& sub) {
        auto ntc = libsemigroups::knuth_bendix::non_trivial_classes(
            *super.get<libsemigroups::KnuthBendix<word_type>>(),
            *sub.get<libsemigroups::KnuthBendix<word_type>>());
        return gapbind14::make_iterator(ntc.begin(), ntc.end());
      });
}
