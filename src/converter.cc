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

#include "converter.h"

#include <vector>

#include "bipart.h"
#include "pkg.h"

using libsemigroups::SemiringWithThreshold;
using libsemigroups::NaturalSemiring;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Boolean matrices
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

BooleanMat* BoolMatConverter::convert(Obj o, size_t n) const {
  assert(CALL_1ARGS(IsBooleanMat, o));
  assert(IS_BLIST_REP(ELM_PLIST(o, 1)));

  size_t             m = LEN_BLIST(ELM_PLIST(o, 1));
  std::vector<bool>* x(new std::vector<bool>());
  x->resize(m * m, false);

  for (size_t i = 0; i < m; i++) {
    Obj row = ELM_PLIST(o, i + 1);
    assert(IS_BLIST_REP(row));
    for (size_t j = 0; j < m; j++) {
      if (ELM_BLIST(row, j + 1) == True) {
        x->at(i * m + j) = true;
      }
    }
  }
  return new BooleanMat(x);
}

Obj BoolMatConverter::unconvert(Element const* x) const {
  size_t            n = x->degree();
  BooleanMat const* xx(static_cast<BooleanMat const*>(x));

  Obj o = NEW_PLIST(T_PLIST_TAB_RECT + IMMUTABLE, n);
  SET_LEN_PLIST(o, n);

  for (size_t i = 0; i < n; i++) {
    Obj blist = NewBag(T_BLIST + IMMUTABLE, SIZE_PLEN_BLIST(n));
    SET_LEN_BLIST(blist, n);
    for (size_t j = 0; j < n; j++) {
      if ((*xx)[i * n + j]) {
        SET_ELM_BLIST(blist, j + 1, True);
      } else {
        SET_ELM_BLIST(blist, j + 1, False);
      }
    }
    SET_ELM_PLIST(o, i + 1, blist);
    CHANGED_BAG(o);
  }

  TYPE_POSOBJ(o) = BooleanMatType;
  RetypeBag(o, T_POSOBJ);
  CHANGED_BAG(o);
  return o;
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Bipartitions
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Bipartition* BipartConverter::convert(Obj o, size_t n) const {
  assert(TNUM_OBJ(o) == T_BIPART);
  return static_cast<Bipartition*>(
      static_cast<Element*>(bipart_get_cpp(o))->really_copy());
}

Obj BipartConverter::unconvert(Element const* x) const {
  return bipart_new_obj(static_cast<Bipartition*>(x->really_copy()));
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Matrices over semirings
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

MatrixOverSemiring* MatrixOverSemiringConverter::convert(Obj    o,
                                                         size_t n) const {
  assert(CALL_1ARGS(IsMatrixOverSemiring, o) == True);
  assert(IS_PLIST(ELM_PLIST(o, 1)));

  size_t m = LEN_PLIST(ELM_PLIST(o, 1));

  std::vector<int64_t>* matrix(new std::vector<int64_t>());
  matrix->reserve(m);

  for (size_t i = 0; i < m; i++) {
    Obj row = ELM_PLIST(o, i + 1);
    for (size_t j = 0; j < m; j++) {
      Obj entry = ELM_PLIST(row, j + 1);
      if (EQ(_gap_zero, entry)) {
        matrix->push_back(_semiring->zero());
      } else {
        matrix->push_back(INT_INTOBJ(entry));
      }
    }
  }
  return new MatrixOverSemiring(matrix, _semiring);
}

Obj MatrixOverSemiringConverter::unconvert(Element const* x) const {
  MatrixOverSemiring const* xx(static_cast<MatrixOverSemiring const*>(x));
  size_t                    n = xx->degree();

  Obj plist = NEW_PLIST(T_PLIST_HOM + IMMUTABLE, n + 2);

  if (_gap_type == NTPMatrixType) {
    NaturalSemiring* semiring = static_cast<NaturalSemiring*>(_semiring);
    SET_LEN_PLIST(plist, n + 2);
    SET_ELM_PLIST(plist, n + 1, INTOBJ_INT(semiring->threshold()));
    SET_ELM_PLIST(plist, n + 2, INTOBJ_INT(semiring->period()));
  } else if (_gap_type == MaxPlusMatrixType || _gap_type == MinPlusMatrixType
             || _gap_type == TropicalMaxPlusMatrixType
             || _gap_type == TropicalMinPlusMatrixType
             || _gap_type == ProjectiveMaxPlusMatrixType) {
    SemiringWithThreshold* semiring =
        static_cast<SemiringWithThreshold*>(_semiring);
    SET_LEN_PLIST(plist, n + 1);
    SET_ELM_PLIST(plist, n + 1, INTOBJ_INT(semiring->threshold()));
  } else {
    SET_LEN_PLIST(plist, n);
    RetypeBag(plist, T_PLIST_TAB_RECT + IMMUTABLE);
  }

  for (size_t i = 0; i < n; i++) {
    Obj row = NEW_PLIST(T_PLIST_CYC + IMMUTABLE, n);
    SET_LEN_PLIST(row, n);
    for (size_t j = 0; j < n; j++) {
      int64_t entry = xx->at(i * n + j);
      if (entry == _semiring->zero()) {
        SET_ELM_PLIST(row, j + 1, _gap_zero);
      } else {
        SET_ELM_PLIST(row, j + 1, INTOBJ_INT(entry));
      }
    }
    SET_ELM_PLIST(plist, i + 1, row);
    CHANGED_BAG(plist);
  }
  TYPE_POSOBJ(plist) = _gap_type;
  RetypeBag(plist, T_POSOBJ);
  CHANGED_BAG(plist);
  return plist;
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Partitioned binary relations (PBRs)
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Obj PBRConverter::get_gap_type(size_t deg) const {
  deg++;
  if (deg > (size_t) LEN_PLIST(TYPES_PBR) || ELM_PLIST(TYPES_PBR, deg) == 0) {
    CALL_1ARGS(TYPE_PBR, INTOBJ_INT(deg - 1));
  }
  return ELM_PLIST(TYPES_PBR, deg);
}

// TODO(JDM) add some more asserts here

PBR* PBRConverter::convert(Obj o, size_t n) const {
  assert(CALL_1ARGS(IsPBR, o));
  size_t                               m = INT_INTOBJ(ELM_PLIST(o, 1));
  std::vector<std::vector<u_int32_t>>* pbr(
      new std::vector<std::vector<u_int32_t>>());
  pbr->reserve(m);

  for (u_int32_t i = 0; i < 2 * m; i++) {
    Obj                    adj = ELM_PLIST(o, i + 2);
    std::vector<u_int32_t> next;
    for (u_int32_t j = 1; j <= LEN_PLIST(adj); j++) {
      next.push_back(INT_INTOBJ(ELM_PLIST(adj, j)) - 1);
      // assumes that adj is duplicate-free
    }
    std::sort(next.begin(), next.end());
    pbr->push_back(next);
  }
  return new PBR(pbr);
}

Obj PBRConverter::unconvert(Element const* xx) const {
  PBR const* x(static_cast<PBR const*>(xx));
  Obj        plist = NEW_PLIST(T_PLIST + IMMUTABLE, 2 * x->degree() + 1);
  // can't use T_PLIST_TAB/HOM here because some of the subplists might be empty
  SET_LEN_PLIST(plist, 2 * x->degree() + 1);
  SET_ELM_PLIST(plist, 1, INTOBJ_INT(x->degree()));
  for (u_int32_t i = 0; i < 2 * x->degree(); i++) {
    size_t m = x->at(i).size();
    Obj    adj;
    if (m == 0) {
      adj = NEW_PLIST(T_PLIST_EMPTY + IMMUTABLE, 0);
    } else {
      adj = NEW_PLIST(T_PLIST_CYC + IMMUTABLE, m);
      for (size_t j = 0; j < x->at(i).size(); j++) {
        SET_ELM_PLIST(adj, j + 1, INTOBJ_INT(x->at(i).at(j) + 1));
      }
    }
    SET_LEN_PLIST(adj, m);
    SET_ELM_PLIST(plist, i + 2, adj);
    CHANGED_BAG(plist);
  }
  TYPE_POSOBJ(plist) = get_gap_type(x->degree());
  RetypeBag(plist, T_POSOBJ);
  CHANGED_BAG(plist);
  return plist;
}
