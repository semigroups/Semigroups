/*******************************************************************************
 * Semigroups GAP package
 *
 * This file contains converters from GAP to C++ elements and back.
 *
*******************************************************************************/

#include "converter.h"
#include "types.h"
#include "bipart.h"

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Boolean matrices
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

BooleanMat* BoolMatConverter::convert (Obj o, size_t n) {
  assert(IS_BOOL_MAT(o));
  assert(LEN_PLIST(o) > 0);
  assert(IS_BLIST_REP(ELM_PLIST(o, 1)));

  size_t m = LEN_BLIST(ELM_PLIST(o, 1));
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

Obj BoolMatConverter::unconvert (Element* x) {
  size_t n = x->degree();
  BooleanMat* xx(static_cast<BooleanMat*>(x));

  Obj o = NEW_PLIST(T_PLIST, n);
  SET_LEN_PLIST(o, n);

  for (size_t i = 0; i < n; i++) {
    Obj blist = NewBag(T_BLIST, SIZE_PLEN_BLIST(n));
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

Bipartition* BipartConverter::convert (Obj o, size_t n) {
  assert(IS_BIPART(o));
  return static_cast<Bipartition*>(static_cast<Element*>(bipart_get_cpp(o))->really_copy());
}

Obj BipartConverter::unconvert (Element* x) {
  return bipart_new(static_cast<Bipartition*>(x->really_copy()));
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Matrices over semirings
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

MatrixOverSemiring* MatrixOverSemiringConverter::convert (Obj o, size_t n) {
  assert(IS_MAT_OVER_SEMI_RING(o));
  assert(LEN_PLIST(o) > 0);
  assert(IS_PLIST(ELM_PLIST(o, 1)));

  size_t m = LEN_PLIST(ELM_PLIST(o, 1));

  std::vector<long>* matrix(new std::vector<long>());
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

Obj MatrixOverSemiringConverter::unconvert (Element* x) {
  MatrixOverSemiring* xx(static_cast<MatrixOverSemiring*>(x));
  size_t n = xx->degree();

  Obj plist = NEW_PLIST(T_PLIST, n + 2);
  if (_semiring->period() != -1) {
    SET_LEN_PLIST(plist, n + 2);
    SET_ELM_PLIST(plist, n + 1, INTOBJ_INT(_semiring->threshold()));
    SET_ELM_PLIST(plist, n + 2, INTOBJ_INT(_semiring->period()));
  } else if (_semiring->threshold() != -1) {
    SET_LEN_PLIST(plist, n + 1);
    SET_ELM_PLIST(plist, n + 1, INTOBJ_INT(_semiring->threshold()));
  } else {
    SET_LEN_PLIST(plist, n);
  }

  for (size_t i = 0; i < n; i++) {
    Obj row = NEW_PLIST(T_PLIST_CYC, n);
    SET_LEN_PLIST(row, n);
    for (size_t j = 0; j < n; j++) {
      long entry = xx->at(i * n + j);
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

// TODO add some more asserts here

PBR* PBRConverter::convert (Obj o, size_t n) {
  assert(IS_PBR(o));
  size_t m = INT_INTOBJ(ELM_PLIST(o, 1));
  std::vector<std::vector<u_int32_t> >* pbr(new std::vector<std::vector<u_int32_t> >());
  pbr->reserve(m);

  for (u_int32_t i = 0; i < 2 * m; i++) {
    Obj adj = ELM_PLIST(o, i + 2);
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

Obj PBRConverter::unconvert (Element* xx) {
  PBR* x(static_cast<PBR*>(xx));
  Obj plist = NEW_PLIST(T_PLIST_TAB, 2 * x->degree() + 1);
  SET_LEN_PLIST(plist, 2 * x->degree() + 1);
  SET_ELM_PLIST(plist, 1, INTOBJ_INT(x->degree()));
  for (u_int32_t i = 0; i < 2 * x->degree(); i++) {
    size_t m = x->at(i).size();
    Obj adj;
    if (m == 0) {
      adj = NEW_PLIST(T_PLIST_EMPTY, 0);
    } else {
      adj = NEW_PLIST(T_PLIST_CYC, m);
      for (size_t j = 0; j < x->at(i).size(); j++) {
        SET_ELM_PLIST(adj, j + 1, INTOBJ_INT(x->at(i).at(j) + 1));
      }
    }
    SET_LEN_PLIST(adj, m);
    SET_ELM_PLIST(plist, i + 2, adj);
    CHANGED_BAG(plist);
  }
  TYPE_POSOBJ(plist) = PBRType;
  RetypeBag(plist, T_POSOBJ);
  CHANGED_BAG(plist);
  return plist;
  //return CALL_2ARGS(Objectify, PBRType, plist);
}
