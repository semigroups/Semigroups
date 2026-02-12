//
// Semigroups package for GAP
// Copyright (C) 2021-26 James D. Mitchell
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

#include "to-congruence.hpp"

// Semigroups GAP package headers
#include "pkg.hpp"     // for IsGapBind14Type
#include "to_cpp.hpp"  // for to_cpp
#include "to_gap.hpp"  // for to_gap

// GAP headers
#include "gap_all.h"  // for UInt2, UInt4

// GapBind14 headers
#include "gapbind14/gapbind14.hpp"  // for class_ etc

// libsemigroups headers
#include "libsemigroups/to-cong.hpp"

using libsemigroups::Congruence;
using libsemigroups::FroidurePinBase;
using libsemigroups::Presentation;
using libsemigroups::word_type;

void init_to_congruence(gapbind14::Module& m) {
  gapbind14::InstallGlobalFunction(
      "froidure_pin_to_left_congruence", [](FroidurePinBase& fpb) {
        return libsemigroups::to<Congruence<word_type>>(
            congruence_kind::onesided, fpb, fpb.left_cayley_graph());
      });

  gapbind14::InstallGlobalFunction(
      "froidure_pin_to_right_congruence", [](FroidurePinBase& fpb) {
        return libsemigroups::to<Congruence<word_type>>(
            congruence_kind::onesided, fpb, fpb.right_cayley_graph());
      });

  gapbind14::InstallGlobalFunction(
      "froidure_pin_to_2_sided_congruence", [](FroidurePinBase& fpb) {
        return libsemigroups::to<Congruence<word_type>>(
            congruence_kind::twosided, fpb, fpb.right_cayley_graph());
      });

  gapbind14::InstallGlobalFunction(
      "shared_ptr_froidure_pin_to_2_sided_congruence",
      [](std::shared_ptr<FroidurePinBase>& fpb) {
        return libsemigroups::to<Congruence<word_type>>(
            congruence_kind::twosided, *fpb, fpb->right_cayley_graph());
      });

  gapbind14::InstallGlobalFunction(
      "shared_ptr_froidure_pin_to_left_congruence",
      [](std::shared_ptr<FroidurePinBase>& fpb) {
        return libsemigroups::to<Congruence<word_type>>(
            congruence_kind::onesided, *fpb, fpb->left_cayley_graph());
      });

  gapbind14::InstallGlobalFunction(
      "shared_ptr_froidure_pin_to_right_congruence",
      [](std::shared_ptr<FroidurePinBase>& fpb) {
        return libsemigroups::to<Congruence<word_type>>(
            congruence_kind::onesided, *fpb, fpb->right_cayley_graph());
      });

  gapbind14::InstallGlobalFunction(
      "gap_froidure_pin_to_congruence", [](Obj kind_str_obj, Obj gap_fp) {
        std::string kind_str(CSTR_STRING(kind_str_obj));
        Obj         gap_wg;
        if (kind_str == "left") {
          gap_wg = ElmPRec(gap_fp, RNamName("left"));
        } else {
          gap_wg = ElmPRec(gap_fp, RNamName("right"));
        }

        SEMIGROUPS_ASSERT(IS_PLIST(gap_wg));
        SEMIGROUPS_ASSERT(LEN_PLIST(gap_wg) > 0);

        WordGraph<uint32_t> wg(LEN_PLIST(gap_wg) + 1,
                               LEN_PLIST(ELM_PLIST(gap_wg, 1)));

        Obj genslookup = ElmPRec(gap_fp, RNamName("genslookup"));
        SEMIGROUPS_ASSERT(IS_PLIST(genslookup));
        SEMIGROUPS_ASSERT(LEN_PLIST(genslookup) == wg.out_degree());

        for (uint32_t a = 0; a < wg.out_degree(); ++a) {
          wg.target_no_checks(0, a, INT_INTOBJ(ELM_PLIST(genslookup, a + 1)));
        }

        for (uint32_t n = 0; n < wg.number_of_nodes() - 1; ++n) {
          SEMIGROUPS_ASSERT(IS_PLIST(ELM_PLIST(wg, n)));
          SEMIGROUPS_ASSERT(LEN_PLIST(ELM_PLIST(wg, n)) == wg.out_degree());
          for (uint32_t a = 0; a < wg.out_degree(); ++a) {
            wg.target_no_checks(
                n + 1,
                a,
                INT_INTOBJ(ELM_PLIST(ELM_PLIST(gap_wg, n + 1), a + 1)));
          }
        }
        // TODO std::move wg
        return libsemigroups::to<Congruence<word_type>>(
            gapbind14::to_cpp<congruence_kind>{}(kind_str_obj), wg);
      });
}
