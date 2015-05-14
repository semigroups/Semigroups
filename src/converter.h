/*
 * Semigroups GAP package
 *
 * This file contains converters from GAP to C++ elements and back.
 *
 */

#include "semigroups++/elements.h"

/*******************************************************************************
 * Abstract base class
*******************************************************************************/

template <typename T>
class Converter {
  public:
    virtual ~Converter () {};
    virtual T* convert (Obj, size_t) = 0;
    virtual Obj unconvert (T*) = 0;
};

/*******************************************************************************
 * Transformations
*******************************************************************************/

template <typename T>
class TransConverter : public Converter<Transformation<T> > {
  
  public: 

    Transformation<T>* convert (Obj o, size_t n);
    Obj unconvert (Transformation<T>* x); 

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
class PPermConverter : public Converter<PartialPerm<T> > {

  public: 

    PartialPerm<T>* convert (Obj o, size_t n);

    Obj unconvert (PartialPerm<T>* x);
  
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
};

/*******************************************************************************
 * Bipartitions
*******************************************************************************/

class BipartConverter : public Converter<Bipartition> {

  public: 

    Bipartition* convert (Obj o, size_t n);

    Obj unconvert (Bipartition* x);
};

/*******************************************************************************
 * Boolean matrices
*******************************************************************************/

class BoolMatConverter : public Converter<BooleanMat> {

  public: 

    BooleanMat* convert (Obj o, size_t n);

    Obj unconvert (BooleanMat* x);
};

/*******************************************************************************
 * Matrices over semirings 
*******************************************************************************/

class MatrixOverSemiringConverter : public Converter<MatrixOverSemiring> {

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
};

/*******************************************************************************
 * Projective max-plus matrices
*******************************************************************************/

class ProjectiveMaxPlusMatrixConverter : public Converter<ProjectiveMaxPlusMatrix>, 
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
};

/*******************************************************************************
 * Matrices over prime field
*******************************************************************************/

class MatrixOverPrimeFieldConverter : public Converter<MatrixOverSemiring> {

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
};
