//
// Semigroups package for GAP
// Copyright (C) 2016 James D. Mitchell and Wilf A. Wilson
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

#include "ideals.h"

#include <vector>

#include "data.h"

#include "semigroupsplusplus/ideals.h"

Obj IDEAL_SIZE(Obj self, Obj prec) {
  assert(IsbPRec(prec, RNamName("data")));
  assert(IsbPRec(prec, RNamName("gen")));
  assert(IsbPRec(prec, RNamName("degree")));

  Obj data = ElmPRec(prec, RNamName("data"));
  Semigroup* semigroup = data_semigroup(data);
  Element*   x         = data_converter(data)->convert(
      ElmPRec(prec, RNamName("gen")),
      INT_INTOBJ(ElmPRec(prec, RNamName("degree"))));

  std::vector<Element*>* gens = new std::vector<Element*>();
  gens->push_back(x);
  RightIdeal ideal = RightIdeal(semigroup, gens);
  delete gens;
  return INTOBJ_INT(ideal.size());
}
