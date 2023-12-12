//
// Semigroups package for GAP
// Copyright (C) 2023 James D. Mitchell
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

// This file contains everything in the kernel module related to the file
// libsemigroups/include/words.hpp.

#include "words.hpp"

#include "gapbind14/gapbind14.hpp"

#include "libsemigroups/words.hpp"

namespace gapbind14 {
  template <>
  struct IsGapBind14Type<libsemigroups::Words> : std::true_type {
    static constexpr std::string_view name = "Words";
  };
}  // namespace gapbind14

void init_words(gapbind14::Module& m) {
  // TODO combine class_<libsemigroups::Words> and IsGapBind14Type into a macro
  gapbind14::class_<libsemigroups::Words>("Words");

  gapbind14::DeclareCategory("IsWords", "IsObject");
  gapbind14::DeclareOperation("Words", {});
  gapbind14::DeclareOperation("Count", {"IsWords"});
  gapbind14::DeclareOperation("FirstWord", {"IsWords"});
  // TODO IsObject -> IsHomogeneousList
  gapbind14::DeclareOperation("FirstWord", {"IsWords", "IsObject"});
  gapbind14::DeclareOperation("LastWord", {"IsWords"});
  // TODO IsObject -> IsHomogeneousList
  gapbind14::DeclareOperation("LastWord", {"IsWords", "IsObject"});
  // TODO IsObject -> IsPosInt
  gapbind14::DeclareOperation("NumberOfLetters", {"IsWords", "IsObject"});
  gapbind14::DeclareOperation("Get", {"IsWords"});
  gapbind14::DeclareOperation("Next", {"IsWords"});
  gapbind14::DeclareOperation("AtEnd", {"IsWords"});

  gapbind14::InstallMethod(
      "Words", "for no arguments", {}, gapbind14::init<libsemigroups::Words>());

  gapbind14::InstallMethod("Count",
                           "for an IsWords object",
                           {"IsWords"},
                           &libsemigroups::Words::count);

  gapbind14::InstallMethod(
      "FirstWord",
      "for an IsWords object",
      {"IsWords"},
      gapbind14::overload_cast<>(&libsemigroups::Words::first));

  gapbind14::InstallMethod(
      "FirstWord",
      "for an IsWords object and word",
      {"IsWords", "IsObject"},
      // gapbind14 currently doesn't handle the reference returned by
      // words.first(w) properly so we wrap it in a lambda
      [](libsemigroups::Words& words, libsemigroups::word_type const& w) {
        words.first(w);
      });

  gapbind14::InstallMethod(
      "LastWord",
      "for an IsWords object",
      {"IsWords"},
      gapbind14::overload_cast<>(&libsemigroups::Words::last));

  gapbind14::InstallMethod(
      "LastWord",
      "for an IsWords object and word",
      {"IsWords", "IsObject"},
      // gapbind14 currently doesn't handle the reference returned by
      // words.first(w) properly so we wrap it in a lambda
      [](libsemigroups::Words& words, libsemigroups::word_type const& w) {
        words.last(w);
      });

  gapbind14::InstallMethod("NumberOfLetters",
                           "for an IsWords object and a pos. int.",
                           {"IsWords", "IsObject"},
                           [](libsemigroups::Words& words, size_t n) {
                             words.number_of_letters(n);
                           });

  gapbind14::InstallMethod(
      "Get", "for an IsWords object", {"IsWords"}, &libsemigroups::Words::get);

  gapbind14::InstallMethod("Next",
                           "for an IsWords object",
                           {"IsWords"},
                           &libsemigroups::Words::next);

  gapbind14::InstallMethod("AtEnd",
                           "for an IsWords object",
                           {"IsWords"},
                           &libsemigroups::Words::at_end);
  // Only works if PrintObjFuncs isn't installed later in init_kernel
  // gapbind14::InstallMethod("PrintObj",
  //                          "for an IsWords object XXX",
  //                          {"IsWords"},
  //                          [](Obj o) { Pr("bananas", 0L, 0L); });
}
