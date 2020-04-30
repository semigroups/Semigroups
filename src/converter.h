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

#ifndef SEMIGROUPS_SRC_CONVERTER_H_
#define SEMIGROUPS_SRC_CONVERTER_H_

// Inclusion of <cstdef> appears to be required to prevent travis from issuing
// the warning:
//
//     /usr/include/c++/5/cstddef:51:11: error: ‘::max_align_t’ has not been
//     declared
//
// according to:
//
// https://stackoverflow.com/questions/35110786/how-to-fix-the-error-max-align-t

#include <cstddef>

#include <algorithm>
#include <vector>

#include "compiled.h"

#include "pkg.h"
#include "semigroups-debug.h"

#include "libsemigroups/element.hpp"
#include "libsemigroups/semiring.hpp"

using libsemigroups::Bipartition;
using libsemigroups::BooleanMat;
using libsemigroups::Element;
using libsemigroups::NaturalSemiring;
using libsemigroups::PartialPerm;
using libsemigroups::PBR;
using libsemigroups::ProjectiveMaxPlusMatrix;
using libsemigroups::Semiring;
using libsemigroups::SemiringWithThreshold;
using libsemigroups::Transformation;
using libsemigroups::UNDEFINED;
using libsemigroups::detail::MatrixOverSemiringBase;

////////////////////////////////////////////////////////////////////////////////
// Abstract base class
////////////////////////////////////////////////////////////////////////////////

class Converter {
 public:
  virtual ~Converter() {}
  virtual Element* convert(Obj, size_t) const      = 0;
  virtual Obj      unconvert(Element const*) const = 0;
};

////////////////////////////////////////////////////////////////////////////////
// Transformations
////////////////////////////////////////////////////////////////////////////////

template <typename T>
class TransConverter : public Converter {
 public:
  Transformation<T>* convert(Obj o, size_t n) const override {
    SEMIGROUPS_ASSERT(IS_TRANS(o));

    std::vector<T> x;
    x.reserve(n);

    size_t i = 0;
    if (TNUM_OBJ(o) == T_TRANS2) {
      UInt2* pto2 = ADDR_TRANS2(o);
      for (i = 0; i < std::min((size_t) DEG_TRANS2(o), n); i++) {
        x.push_back(pto2[i]);
      }
    } else if (TNUM_OBJ(o) == T_TRANS4) {
      UInt4* pto4 = ADDR_TRANS4(o);
      for (i = 0; i < std::min((size_t) DEG_TRANS4(o), n); i++) {
        x.push_back(pto4[i]);
      }
    } else {
      // in case of future changes to transformations in GAP
      SEMIGROUPS_ASSERT(false);
    }

    for (; i < n; i++) {
      x.push_back(i);
    }
    return new Transformation<T>(x);
  }

  Obj unconvert(Element const* x) const override {
    auto xx = static_cast<Transformation<T> const*>(x);
    Obj  o  = NEW_TRANS(xx->degree());

    T* pto = reinterpret_cast<T*>(static_cast<Obj*>(ADDR_OBJ(o) + 3));
    for (T i = 0; i < xx->degree(); i++) {
      pto[i] = (*xx)[i];
    }
    return o;
  }
};

////////////////////////////////////////////////////////////////////////////////
// Partial perms
////////////////////////////////////////////////////////////////////////////////

template <typename T>
class PPermConverter : public Converter {
 public:
  PartialPerm<T>* convert(Obj o, size_t n) const override {
    SEMIGROUPS_ASSERT(IS_PPERM(o));

    std::vector<T> x;
    x.reserve(n);

    size_t i = 0;
    if (TNUM_OBJ(o) == T_PPERM2) {
      UInt2* pto2 = ADDR_PPERM<UInt2>(o);
      for (i = 0; i < std::min((size_t) DEG_PPERM2(o), n); i++) {
        if (pto2[i] == 0) {
          x.push_back(UNDEFINED);
        } else {
          x.push_back(pto2[i] - 1);
        }
      }
    } else if (TNUM_OBJ(o) == T_PPERM4) {
      UInt4* pto4 = ADDR_PPERM<UInt4>(o);
      for (i = 0; i < std::min((size_t) DEG_PPERM4(o), n); i++) {
        if (pto4[i] == 0) {
          x.push_back(UNDEFINED);
        } else {
          x.push_back(pto4[i] - 1);
        }
      }
    } else {
      // in case of future changes to partial perms in GAP
      SEMIGROUPS_ASSERT(false);
    }

    for (; i < n; i++) {
      x.push_back(UNDEFINED);
    }
    return new PartialPerm<T>(x);
  }

  // similar to FuncDensePartialPermNC in gap/src/pperm.c
  Obj unconvert(Element const* x) const override {
    auto xx  = static_cast<PartialPerm<T> const*>(x);
    T    deg = xx->degree();

    // remove trailing 0s
    while (deg > 0 && (*xx)[deg - 1] == UNDEFINED) {
      deg--;
    }

    Obj o   = NEW_PPERM(deg);
    T*  pto = reinterpret_cast<T*>(static_cast<Obj*>(ADDR_OBJ(o)) + 2) + 1;

    for (T i = 0; i < deg; i++) {
      if ((*xx)[i] == UNDEFINED) {
        pto[i] = 0;
      } else {
        pto[i] = (*xx)[i] + 1;
      }
    }
    return o;
  }

 private:
  // FIXME this isn't right it depends on the codegree only and not the degree
  inline Obj NEW_PPERM(size_t deg) const {
    if (deg < 65536) {
      return NEW_PPERM2(deg);
    } else {
      return NEW_PPERM4(deg);
    }
  }

  template <typename UIntT>
  inline UIntT* ADDR_PPERM(Obj x) const {
    return reinterpret_cast<UIntT*>(static_cast<Obj*>(ADDR_OBJ(x)) + 2) + 1;
  }
};

////////////////////////////////////////////////////////////////////////////////
// Bipartitions
////////////////////////////////////////////////////////////////////////////////

class BipartConverter : public Converter {
 public:
  Bipartition* convert(Obj o, size_t n) const override;
  Obj          unconvert(Element const* x) const override;
};

////////////////////////////////////////////////////////////////////////////////
// Matrices over semirings
////////////////////////////////////////////////////////////////////////////////

template <class TSubclass>
class MatrixOverSemiringConverter : public Converter {
 public:
  ~MatrixOverSemiringConverter() {
    delete _semiring;
  }

  MatrixOverSemiringConverter(Semiring<int64_t>* semiring,
                              Obj                gap_zero,
                              Obj                gap_type)
      : _semiring(semiring), _gap_zero(gap_zero), _gap_type(gap_type) {}

  MatrixOverSemiringBase<int64_t, TSubclass>* convert(Obj    o,
                                                      size_t n) const override {
    assert(CALL_1ARGS(IsMatrixOverSemiring, o) == True);
    assert(IS_PLIST(ELM_PLIST(o, 1)));

    size_t m = LEN_PLIST(ELM_PLIST(o, 1));

    std::vector<int64_t> matrix;
    matrix.reserve(m);

    for (size_t i = 0; i < m; i++) {
      Obj row = ELM_PLIST(o, i + 1);
      for (size_t j = 0; j < m; j++) {
        Obj entry = ELM_PLIST(row, j + 1);
        if (EQ(_gap_zero, entry)) {
          matrix.push_back(_semiring->zero());
        } else {
          matrix.push_back(INT_INTOBJ(entry));
        }
      }
    }
    return new TSubclass(matrix, _semiring);
  }

  Obj unconvert(Element const* x) const override {
    TSubclass const* xx(static_cast<TSubclass const*>(x));
    size_t           n = xx->degree();

    Obj plist = NEW_PLIST(T_PLIST, n + 2);

    if (_gap_type == NTPMatrixType) {
      NaturalSemiring* semiring = static_cast<NaturalSemiring*>(_semiring);
      SET_LEN_PLIST(plist, n + 2);
      SET_ELM_PLIST(plist, n + 1, INTOBJ_INT(semiring->threshold()));
      SET_ELM_PLIST(plist, n + 2, INTOBJ_INT(semiring->period()));
    } else if (_gap_type == TropicalMaxPlusMatrixType
               || _gap_type == TropicalMinPlusMatrixType) {
      SemiringWithThreshold* semiring
          = static_cast<SemiringWithThreshold*>(_semiring);
      SET_LEN_PLIST(plist, n + 1);
      SET_ELM_PLIST(plist, n + 1, INTOBJ_INT(semiring->threshold()));
    } else {
      SET_LEN_PLIST(plist, n);
    }

    for (size_t i = 0; i < n; i++) {
      Obj row = NEW_PLIST_IMM(T_PLIST_CYC, n);
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
    SET_TYPE_POSOBJ(plist, _gap_type);
    RetypeBag(plist, T_POSOBJ);
    CHANGED_BAG(plist);
    return plist;
  }

 protected:
  Semiring<int64_t>* _semiring;
  Obj                _gap_zero;
  Obj                _gap_type;
};

////////////////////////////////////////////////////////////////////////////////
// Boolean matrices
////////////////////////////////////////////////////////////////////////////////

class BoolMatConverter : public Converter {
 public:
  BooleanMat* convert(Obj o, size_t n) const override;
  Obj         unconvert(Element const* x) const override;
};

////////////////////////////////////////////////////////////////////////////////
// Partitioned binary relations (PBRs)
////////////////////////////////////////////////////////////////////////////////

class PBRConverter : public Converter {
 public:
  PBR* convert(Obj o, size_t n) const override;
  Obj  unconvert(Element const* x) const override;

 private:
  Obj get_gap_type(size_t deg) const;
};

#endif  // SEMIGROUPS_SRC_CONVERTER_H_
