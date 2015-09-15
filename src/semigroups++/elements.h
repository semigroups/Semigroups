/*
 * Semigroups++
 *
 * This file contains classes for creating elements of a semigroup.
 *
 */

#ifndef SEMIGROUPS_ELEMENTS_H
#define SEMIGROUPS_ELEMENTS_H
//#define NDEBUG

#include "semiring.h"

#include <assert.h>
#include <functional>
#include <iostream>
#include <math.h>
#include <vector>
#include <unordered_set>

using namespace semiring;
using namespace std;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Abstract base class for elements of a semigroup
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

class Element {

  public: 
    virtual ~Element () {}

    bool     operator == (const Element& that) const {
      return this->equals(&that);
    };

    virtual size_t   complexity    ()                               const = 0;
    virtual size_t   degree        ()                               const = 0;
    virtual bool     equals        (const Element*)                 const = 0;
    virtual size_t   hash_value    ()                               const = 0;
    virtual Element* identity      ()                               const = 0;
    virtual Element* really_copy   (size_t = 0)                     const = 0;
    virtual void     really_delete ()                                     = 0;
    virtual void     redefine      (Element const*, Element const*)       = 0;
};

class myequal {
public:
   size_t operator()(const Element* x, const Element* y) const{
     return x->equals(y);
   }
};

namespace std {
  template<>
    struct hash<const Element* > {
    size_t operator() (const Element* x) const {
      return x->hash_value();
    }
  };
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Abstract base class for elements using a vector to store the data.
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

template <typename T, typename Constructor> 
class ElementWithVectorData : public Element {

  public: 
    
    ElementWithVectorData (std::vector<T>* vector) : _vector(vector) {}

    inline T operator [] (size_t pos) const {
      return _vector[pos];
    }
    
    inline T at (size_t pos) const {
      return _vector->at(pos);
    }
    
    bool equals (const Element* that) const {
      return *(static_cast<const Constructor*>(that)->_vector) 
        == *(this->_vector);
    }

    virtual Element* really_copy (size_t increase_deg_by) const {
      assert(increase_deg_by == 0);
      std::vector<T>* vector(new std::vector<T>(*_vector));
      return new Constructor(vector);
    }

    virtual void really_delete () {
      delete _vector;
    };

  protected:

    std::vector<T>* _vector;
};

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Template for transformations
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

template <typename T>
class PartialTransformation : 
  public ElementWithVectorData<T, PartialTransformation<T> > {

  public:
    
    PartialTransformation (std::vector<T>* vector) : 
      ElementWithVectorData<T, PartialTransformation<T> >(vector) {}
    
    size_t complexity () const {
      return this->_vector->size();
    }

    size_t degree () const {
      return this->_vector->size();
    }

    size_t hash_value () const {
      size_t seed = 0;
      T deg = this->degree();
      for (T i = 0; i < deg; i++) {
        seed = ((seed * deg) + this->_vector->at(i));
      }
      return seed;
    }
    
    Element* identity () const {
      auto vector = new std::vector<T>();
      vector->reserve(this->degree());
      for (T i = 0; i < this->degree(); i++) {
        vector->push_back(i);
      }
      return new PartialTransformation<T>(vector);
    }
};

template <typename T>
class Transformation : public Element {

  public:
   
    Transformation (std::vector<T>* image) : _image(image) {}
   
    inline T operator [] (size_t pos) const {
      return _image->at(pos);
    }

    size_t complexity () const {
      return _image->size();
    }

    size_t degree () const {
      return _image->size();
    }
    
    bool equals (const Element* that) const {
      return *(static_cast<const Transformation<T>*>(that)->_image) == *(this->_image);
    }
    
    size_t hash_value () const {
      size_t seed = 0;
      T deg = this->degree();
      for (T i = 0; i < deg; i++) {
        seed = ((seed * deg) + this->_image->at(i));
      }
      return seed;
    }
    
    // the identity of this
    Element* identity () const {
      auto image = new std::vector<T>();
      image->reserve(this->degree());
      for (T i = 0; i < this->degree(); i++) {
        image->push_back(i);
      }
      return new Transformation<T>(image);
    }

    Element* really_copy (size_t increase_deg_by = 0) const override {
      auto out = new std::vector<T>(*_image);
      for (size_t i = _image->size(); i < _image->size() + increase_deg_by; i++) {
        out->push_back(i);
      }
      return new Transformation<T>(out);
    }

    void really_delete () {
      delete _image;
    }

    // multiply x and y into this
    void redefine (Element const* x, Element const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      auto xx = *static_cast<Transformation<T> const*>(x);
      auto yy = *static_cast<Transformation<T> const*>(y);

      for (T i = 0; i < this->degree(); i++) {
        _image->at(i) = yy[xx[i]];
      }
    }

  private:

    std::vector<T>* _image;
};

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Template for partial perms
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

template <typename T>
class PartialPerm : public Element {

  public:

    PartialPerm (std::vector<T>* image) : _image(image) {}
    
    inline T operator [] (size_t pos) const {
      return _image->at(pos);
    }

    size_t complexity () const {
      return _image->size();
    }
    
    size_t degree () const {
      return _image->size();
    }

    bool equals (const Element* that) const {
      return *(static_cast<const PartialPerm<T>*>(that)->_image) == *(this->_image);
    }
    
    size_t hash_value () const {
      size_t seed = 0;
      T deg = this->degree();
      for (T i = 0; i < deg; i++) {
        seed = ((seed * deg) + this->_image->at(i));
      }
      return seed;
    }
    
    // the identity of this
    Element* identity () const {
      auto image = new std::vector<T>();
      image->reserve(this->degree());
      for (T i = 0; i < this->degree(); i++) {
        image->push_back(i);
      }
      return new PartialPerm<T>(image);
    }

    Element* really_copy (size_t increase_deg_by = 0) const override {
      auto out = new std::vector<T>(*_image);
      for (size_t i = 0; i < increase_deg_by; i++) {
        out->push_back(UNDEFINED);
      }
      return new PartialPerm<T>(out);
    }
    
    void really_delete () {
      delete _image;
    }

    // multiply x and y into this
    void redefine (Element const* x, Element const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      auto xx = *static_cast<PartialPerm<T> const*>(x);
      auto yy = *static_cast<PartialPerm<T> const*>(y);

      for (T i = 0; i < this->degree(); i++) {
        _image->at(i) = (xx[i] == UNDEFINED ? UNDEFINED : yy[xx[i]]);
      }
    }

  private:

    std::vector<T>* _image;
    T               UNDEFINED = (T) -1;
};

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Boolean matrices
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

class BooleanMat: public Element {

  public:

    BooleanMat             (std::vector<bool>* matrix) : _matrix(matrix) {}
    bool     at            (size_t pos)                     const;

    size_t   complexity    ()                               const;
    size_t   degree        ()                               const;
    bool     equals        (const Element*)                 const;
    size_t   hash_value    ()                               const;
    Element* identity      ()                               const;
    Element* really_copy   (size_t = 0)                     const;
    void     really_delete ();
    void     redefine      (Element const*, Element const*);
  
  private:
    

    std::vector<bool>* _matrix;

};

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Bipartitions
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

class Bipartition : public Element {

  public:

    Bipartition            (std::vector<u_int32_t>* blocks) : _blocks(blocks) {}
    u_int32_t block        (size_t pos)                     const;

    size_t   complexity    ()                               const;
    size_t   degree        ()                               const;
    bool     equals        (const Element*)                 const;
    size_t   hash_value    ()                               const;
    Element* identity      ()                               const;
    Element* really_copy   (size_t = 0)                     const;
    void     really_delete ()                                    ;
    void     redefine      (Element const*, Element const*)      ;
  
  private:

    u_int32_t fuseit   (std::vector<u_int32_t>const&, u_int32_t);
    u_int32_t nrblocks () const;

    std::vector<u_int32_t>* _blocks;
};

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Matrices over semirings
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

class MatrixOverSemiring: public Element {

  public:

    MatrixOverSemiring     (std::vector<long>* matrix, 
                            Semiring*          semiring) :
                           _matrix(matrix),
                           _semiring(semiring) {}

             long      at            (size_t pos)                     const;
             Semiring* semiring      ()                               const;

             size_t    complexity    ()                               const;
             size_t    degree        ()                               const;
             bool      equals        (const Element*)                 const;
    virtual  size_t    hash_value    ()                               const;
             Element*  identity      ()                               const;
             Element*  really_copy   (size_t = 0)                     const;
             void      really_delete ();
             void      redefine      (Element const*, 
                                      Element const*);

  protected:

    std::vector<long>* _matrix;

  private: 

    // a function applied after redefinition 
    virtual void after () {}

    Semiring*          _semiring;
    
}; 

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Projective max-plus matrices
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

class ProjectiveMaxPlusMatrix: public MatrixOverSemiring {

  public:

    ProjectiveMaxPlusMatrix(std::vector<long>* matrix, 
                            Semiring*          semiring) :
      MatrixOverSemiring(matrix, semiring) {};
    
    size_t    hash_value    ()                               const;
  
  private: 

    // a function applied after redefinition 
    void after ();

};

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Partitioned binary relations
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

class PBR: public ElementWithVectorData<std::vector<u_int32_t>, PBR> {

  public: 
    
    PBR (std::vector<std::vector<u_int32_t> >* vector) : 
      ElementWithVectorData<std::vector<u_int32_t>, PBR>(vector) {}

    size_t   complexity    ()                               const;
    size_t   degree        ()                               const;
    size_t   hash_value    ()                               const;
    Element* identity      ()                               const;
    void     redefine      (Element const*, Element const*)      ;

  private:
    
    void add_adjacency (size_t vertex1, size_t vertex2);
    
    void x_dfs (u_int32_t          n,
                u_int32_t          i, 
                u_int32_t          v,        // the vertex we're currently doing
                std::vector<bool>& x_seen,
                std::vector<bool>& y_seen,
                PBR const*         x, 
                PBR const*         y      );
    
    void y_dfs (u_int32_t          n,
                u_int32_t          i, 
                u_int32_t          v,        // the vertex we're currently doing
                std::vector<bool>& x_seen,
                std::vector<bool>& y_seen,
                PBR const*         x, 
                PBR const*         y      );
};

#endif
