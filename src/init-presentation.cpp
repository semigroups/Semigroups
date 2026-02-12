//
// Semigroups package for GAP
// Copyright (C) 2026 James D. Mitchell
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

#include "init-presentation.hpp"

// Semigroups GAP package headers
#include "pkg.hpp"     // for IsGapBind14Type
#include "to-cpp.hpp"  // for to_cpp
#include "to-gap.hpp"  // for to_gap

// GAP headers
#include "gap_all.h"  // for UInt2, UInt4

// GapBind14 headers
#include "gapbind14/gapbind14.hpp"  // for class_ etc

// libsemigroups headers
#include "libsemigroups/presentation.hpp"  // for Presentation

using libsemigroups::Presentation;
using libsemigroups::RepOrc;
using libsemigroups::Sims1;
using libsemigroups::word_type;

void init_presentation(gapbind14::Module& m) {
  gapbind14::class_<Presentation<word_type>>("Presentation")
      .def(gapbind14::init<>{}, "make")
      .def("copy",
           [](Presentation<word_type>& thing) { return Presentation(thing); })
      .def("alphabet",
           [](Presentation<word_type>& thing) { return thing.alphabet(); })
      .def("set_alphabet",
           [](Presentation<word_type>& thing, word_type val) -> void {
             thing.alphabet(val);
           })
      .def("set_alphabet_size",
           [](Presentation<word_type>& thing, size_t size) -> void {
             thing.alphabet(size);
           })
      .def("alphabet_from_rules",
           [](Presentation<word_type>& thing) -> void {
             thing.alphabet_from_rules();
           })
      .def("contains_empty_word",
           [](Presentation<word_type>& thing, bool val) -> void {
             thing.contains_empty_word(val);
           })
      .def("throw_if_bad_alphabet_or_rules",
           &Presentation<word_type>::throw_if_bad_alphabet_or_rules)
      .def("number_of_rules",
           [](Presentation<word_type> const& thing) -> size_t {
             return thing.rules.size();
           });

  gapbind14::InstallGlobalFunction(
      "presentation_add_rule_no_checks",
      gapbind14::overload_cast<Presentation<word_type>&,
                               word_type const&,
                               word_type const&>(
          &libsemigroups::presentation::add_rule_no_checks<word_type>));

  gapbind14::InstallGlobalFunction(
      "presentation_add_rule",
      gapbind14::overload_cast<Presentation<word_type>&,
                               word_type const&,
                               word_type const&>(
          &libsemigroups::presentation::add_rule<word_type>));

  gapbind14::InstallGlobalFunction(
      "presentation_add_identity_rules",
      gapbind14::overload_cast<Presentation<word_type>&, size_t>(
          &libsemigroups::presentation::add_identity_rules<word_type>));

  // Because reverse has two overloads (one for rvalue reference, and one for
  // lvalue reference) we use a lambda here instead of overload_cast.
  gapbind14::InstallGlobalFunction(
      "presentation_reverse", [](Presentation<word_type>& thing) -> void {
        libsemigroups::presentation::reverse(thing);
      });

  gapbind14::InstallGlobalFunction(
      "presentation_normalize_alphabet",
      gapbind14::overload_cast<Presentation<word_type>&>(
          &libsemigroups::presentation::normalize_alphabet<word_type>));
}
