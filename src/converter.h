/*
 * Semigroups GAP package
 *
 * This file contains converters from GAP to C++ elements and back.
 *
 */

#ifndef SRC_CONVERTER_H_

#define SRC_CONVERTER_H_

#include <algorithm>

#include "src/compiled.h"          /* GAP headers                */
#include "src/pperm.h"

#include "semigroups++/elements.h"

////////////////////////////////////////////////////////////////////////////////
// Abstract base class
////////////////////////////////////////////////////////////////////////////////

class Converter {
 public:
    virtual ~Converter () {}
    virtual Element* convert   (Obj, size_t) = 0;
    virtual Obj      unconvert (Element*)    = 0;
};

////////////////////////////////////////////////////////////////////////////////
// Transformations
////////////////////////////////////////////////////////////////////////////////

template <typename T>
class TransConverter : public Converter {

 public:
    Transformation<T>* convert (Obj o, size_t n) {
      assert(IS_TRANS(o));

      auto x = new std::vector<T>();
      x->reserve(n);

      size_t i;
      if (TNUM_OBJ(o) == T_TRANS2) {
        UInt2* pto2 = ADDR_TRANS2(o);
        for (i = 0; i < std::min((size_t) DEG_TRANS2(o), n); i++) {
          x->push_back(pto2[i]);
        }
      } else if (TNUM_OBJ(o) == T_TRANS4) {
        UInt4* pto4 = ADDR_TRANS4(o);
        for (i = 0; i < std::min((size_t) DEG_TRANS4(o), n); i++) {
          x->push_back(pto4[i]);
        }
      } else {
        // in case of future changes to transformations in GAP
        assert(false);
      }

      for (; i < n; i++) {
        x->push_back(i);
      }
      return new Transformation<T>(x);
    }

    Obj unconvert (Element* x) {
      auto xx = static_cast<Transformation<T>*>(x);
      Obj o = NEW_TRANS(xx->degree());

      T* pto = ((T*)((Obj*)(ADDR_OBJ(o)) + 3));
      for (T i = 0; i < xx->degree(); i++) {
        pto[i] = (*xx)[i];
      }
      return o;
    }

 private:
    inline Obj NEW_TRANS (size_t deg) {
      if (deg < 65536) {
        return NEW_TRANS2(deg);
      } else {
        return NEW_TRANS4(deg);
      }
    }
};

////////////////////////////////////////////////////////////////////////////////
// Partial perms
////////////////////////////////////////////////////////////////////////////////

template <typename T>
class PPermConverter : public Converter {

  public:

    PartialPerm<T>* convert (Obj o, size_t n) {
      assert(IS_PPERM(o));

      auto x = new std::vector<T>();
      x->reserve(n);

      size_t i;
      if (TNUM_OBJ(o) == T_PPERM2) {
        UInt2* pto2 = ADDR_PPERM2(o);
        for (i = 0; i < std::min((size_t) DEG_PPERM2(o), n); i++) {
          if (pto2[i] == 0) {
            x->push_back(UNDEFINED);
          } else {
            x->push_back(pto2[i] - 1);
          }
        }
      } else if (TNUM_OBJ(o) == T_PPERM4) {
        UInt4* pto4 = ADDR_PPERM4(o);
        for (i = 0; i < std::min((size_t) DEG_PPERM4(o), n); i++) {
          if (pto4[i] == 0) {
            x->push_back(UNDEFINED);
          } else {
            x->push_back(pto4[i] - 1);
          }
        }
      } else {
        // in case of future changes to partial perms in GAP
        assert(false);
      }

      for (; i < n; i++) {
        x->push_back(UNDEFINED);
      }
      return new PartialPerm<T>(x);
    }

    // similar to FuncDensePartialPermNC in gap/src/pperm.c
    Obj unconvert (Element* x) {
      auto xx  = static_cast<PartialPerm<T>*>(x);
      T    deg = xx->degree();

      // remove trailing 0s
      while (deg > 0 && (*xx)[deg - 1] == UNDEFINED) {
        deg--;
      }

      Obj o     = NEW_PPERM(deg);
      T*  pto   = ((T*)((Obj*)(ADDR_OBJ(o)) + 2) + 1);
      T   codeg = 0;

      for (T i = 0; i < deg; i++) {
        if ((*xx)[i] == UNDEFINED) {
          pto[i] = 0;
        } else {
          pto[i] = (*xx)[i] + 1;
          if (pto[i] > codeg) {
            codeg = pto[i];
          }
        }
      }
      set_codeg(o, deg, codeg);
      return o;
    }

 private:

    void set_codeg (Obj o, T deg, T codeg) {
      if (deg < 65536) {
        CODEG_PPERM2(o) = codeg;
      } else {
        CODEG_PPERM4(o) = codeg;
      }
    }

    inline Obj NEW_PPERM (size_t deg) {
      if (deg < 65536) {
        return NEW_PPERM2(deg);
      } else {
        return NEW_PPERM4(deg);
      }
    }

    // FIXME expose these in pperm.h
    inline UInt2* ADDR_PPERM2 (Obj x) {
      return ((UInt2*)((Obj*)(ADDR_OBJ(x)) + 2) + 1);
    }
    inline UInt4* ADDR_PPERM4 (Obj x) {
      return ((UInt4*)((Obj*)(ADDR_OBJ(x)) + 2) + 1);
    }

    T UNDEFINED = (T) -1;
};

////////////////////////////////////////////////////////////////////////////////
// Boolean matrices
////////////////////////////////////////////////////////////////////////////////

class BoolMatConverter : public Converter {

  public:

    BooleanMat* convert   (Obj o, size_t n);
    Obj         unconvert (Element* x);
};

////////////////////////////////////////////////////////////////////////////////
// Bipartitions
////////////////////////////////////////////////////////////////////////////////

class BipartConverter : public Converter {

  public:

    Bipartition* convert   (Obj o, size_t n);
    Obj          unconvert (Element* x);
};

////////////////////////////////////////////////////////////////////////////////
// Matrices over semirings
////////////////////////////////////////////////////////////////////////////////

class MatrixOverSemiringConverter : public Converter {

  public:

    ~MatrixOverSemiringConverter () {
      delete _semiring;
    }

    MatrixOverSemiringConverter (Semiring* semiring,
                                 Obj       gap_zero,
                                 Obj       gap_type)
      : _semiring(semiring),
        _gap_zero(gap_zero),
        _gap_type(gap_type) {}

    virtual MatrixOverSemiring* convert   (Obj      o, size_t n);
    virtual Obj                 unconvert (Element* x          );

  protected:

    Semiring* _semiring;
    Obj       _gap_zero;
    Obj       _gap_type;
};

////////////////////////////////////////////////////////////////////////////////
// Projective max-plus matrices
////////////////////////////////////////////////////////////////////////////////

class ProjectiveMaxPlusMatrixConverter : public MatrixOverSemiringConverter {

  public:

    ProjectiveMaxPlusMatrixConverter(Semiring* semiring,
                                     Obj       gap_zero,
                                     Obj       gap_type)
      : MatrixOverSemiringConverter(semiring, gap_zero, gap_type) {}

    ProjectiveMaxPlusMatrix* convert (Obj o, size_t n) {
      return
        static_cast<ProjectiveMaxPlusMatrix*>(
            MatrixOverSemiringConverter::convert(o, n));
    }

    Obj unconvert (Element* x) {
      return MatrixOverSemiringConverter::unconvert(x);
    }
};

////////////////////////////////////////////////////////////////////////////////
// Partitioned binary relations (PBRs)
////////////////////////////////////////////////////////////////////////////////

class PBRConverter : public Converter {

  public:

    PBR* convert   (Obj      o, size_t n);
    Obj  unconvert (Element* x          );

  private: 
    Obj get_gap_type (size_t deg);
};

#endif
