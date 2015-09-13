/*
 * Semigroups GAP package
 *
 * This file contains converters from GAP to C++ elements and back.
 *
 */

#ifndef SEMIGROUPS_GAP_CONVERTER_H
#define SEMIGROUPS_GAP_CONVERTER_H 1

#include "src/compiled.h"          /* GAP headers                */
#include "pperm.h"

#include "semigroups++/elements.h"

/*******************************************************************************
 * Abstract base class
*******************************************************************************/

class Converter {
  public:
    virtual ~Converter () {};
    virtual Element* convert   (Obj, size_t) = 0;
    virtual Obj      unconvert (Element*)    = 0;
};

/*******************************************************************************
 * Transformations
*******************************************************************************/

template <typename T>
class TransConverter : public Converter {
  
  public: 

    Transformation<T>* convert (Obj o, size_t n) {
      assert(IS_TRANS(o));
      //assert(DEG_TRANS(o) <= n);

      auto x = new std::vector<T>();
      x->reserve(DEG_TRANS(o));
      
      T* pto = ADDR_TRANS(o);
      T i;
      for (i = 0; i < DEG_TRANS(o); i++) {
        x->push_back(pto[i]);
      }
      for (; i < n; i++) {
        x->push_back(i);
      }
      return new Transformation<T>(x);
    }

    Obj unconvert (Element* x) {
      auto xx = static_cast<Transformation<T>*>(x);
      Obj o = NEW_TRANS(x->degree());
      T* pto = ADDR_TRANS(o);
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

    // helper for getting ADDR_TRANS2/4
    inline T* ADDR_TRANS (Obj x) {
      return ((T*)((Obj*)(ADDR_OBJ(x))+3));
    }
};

/*******************************************************************************
 * Partial perms
*******************************************************************************/

template <typename T>
class PPermConverter : public Converter {

  public: 

    PartialPerm<T>* convert (Obj o, size_t n) {
      assert(IS_PPERM(o));

      auto x = new std::vector<T>();
      T*   pto = ADDR_PPERM(o);
      T    i;

      for (i = 0; i < DEG_PPERM(o); i++) {
        if (pto[i] == 0) {
          x->push_back(UNDEFINED);
        } else {
          x->push_back(pto[i] - 1);
        }
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

      //remove trailing 0s
      while (deg > 0 && (*xx)[deg - 1] == UNDEFINED) {
        deg--;
      }

      Obj o     = NEW_PPERM(deg);
      T*  pto   = ADDR_PPERM(o);
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

    // helper for getting ADDR_PPERM2/4
    inline T* ADDR_PPERM (Obj x) {
      return ((T*)((Obj*)(ADDR_OBJ(x))+2)+1);
    }

    T UNDEFINED = (T) -1;
};

/*******************************************************************************
 * Boolean matrices
*******************************************************************************/

class BoolMatConverter : public Converter {

  public: 

    BooleanMat* convert   (Obj o, size_t n);
    Obj         unconvert (Element* x  );
};

/*******************************************************************************
 * Bipartitions
*******************************************************************************/

class BipartConverter : public Converter {

  public: 

    Bipartition* convert   (Obj o, size_t n);
    Obj          unconvert (Element* x);
};

/*******************************************************************************
 * Matrices over semirings 
*******************************************************************************/

/*class MatrixOverSemiringConverter : public Converter<MatrixOverSemiring> {

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

    MatrixOverSemiring* convert (Obj o, size_t n);

    virtual Obj unconvert (MatrixOverSemiring* x);

  protected: 
    
    Semiring* _semiring;
    Obj       _gap_zero;
    Obj       _gap_type;
};*/

/*******************************************************************************
 * Projective max-plus matrices
*******************************************************************************/

/*class ProjectiveMaxPlusMatrixConverter : public Converter<ProjectiveMaxPlusMatrix>, 
                                         public MatrixOverSemiringConverter {

  public:
    ProjectiveMaxPlusMatrixConverter(Semiring* semiring, 
                                     Obj       gap_zero, 
                                     Obj       gap_type)
      : MatrixOverSemiringConverter(semiring, gap_zero, gap_type) {}

    ProjectiveMaxPlusMatrix* convert (Obj o, size_t n) {
      return
        static_cast<ProjectiveMaxPlusMatrix*>(MatrixOverSemiringConverter::convert(o, n));
    }

    Obj unconvert (ProjectiveMaxPlusMatrix* x) {
      return MatrixOverSemiringConverter::unconvert(x);
    }
};*/

/*******************************************************************************
 * Matrices over prime field
*******************************************************************************/

/*class MatrixOverPrimeFieldConverter : public Converter<MatrixOverSemiring> {

  public:

    ~MatrixOverPrimeFieldConverter () {
      delete _field;
    }

    MatrixOverPrimeFieldConverter (PrimeField* field) 
      : _field(field) {}

    MatrixOverSemiring* convert (Obj o, size_t n);

    Obj unconvert (MatrixOverSemiring* x);

  protected: 
    
    PrimeField* _field;
};*/

/*******************************************************************************
 * Partitioned binary relations (PBRs)
*******************************************************************************/

/*class PBRConverter : public Converter<PBR> {

  public:

    PBR* convert (Obj o, size_t n);

    Obj unconvert (PBR* x);

};*/
#endif
