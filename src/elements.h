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

// template for the base class for elements of a semigroup

template <typename T>
class Element {

  public:

    Element (size_t degree) {
      _data = new std::vector<T>();
      _data->reserve(degree);
      for (size_t i = 0; i < degree; i++) {
        _data->push_back(0);
      }
    }

    Element (std::vector<T> const& data) {
      _data = new std::vector<T>(data);
    }

    Element (const Element& copy) : _data(copy._data) {}

    ~Element () {}

    bool operator == (const Element<T> & that) const {
      assert(this->degree() == that.degree());
      for (size_t i = 0; i < this->degree(); i++) {
        if ((*_data)[i] != (*that._data)[i]) {
          return false;
        }
      }
      return true;
    };

    inline size_t at (size_t pos) const {
      return (*_data)[pos];
    }

    inline void set (size_t pos, T val) {
      if (_data->size() > 0) {
        (*_data)[pos] = val;
      }
    }

    size_t degree () const {
      return _data->size();
    }

    Element* copy () const {
      return new Element(*_data);
    }

    void delete_data () {
      if (_data != nullptr) {
        delete _data;
      }
    }

  protected:
    std::vector<T>* _data;
};

// template for transformations

template <typename T>
class Transformation : public Element<T> {

  public:

    Transformation (T degree, Element<T>* sample = nullptr) 
      : Element<T>(degree) {}

    Transformation (std::vector<T> data) : Element<T>(data) {}

    // multiply x and y into this
    void redefine (Element<T> const* x, Element<T> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());

      for (T i = 0; i < this->degree(); i++) {
        this->set(i, y->at(x->at(i)));
      }
    }

    // the identity of this
    Element<T>* identity () {
      std::vector<T> image;
      image.reserve(this->degree());
      for (T i = 0; i < this->degree(); i++) {
        image.push_back(i);
      }
      return new Transformation(image);
    }
};

// hash function for unordered_map

template <typename T>
struct std::hash<const Transformation<T> > {
  size_t operator() (const Transformation<T>& x) const {
    size_t seed = 0;
    T deg = x.degree();
    for (T i = 0; i < deg; i++) {
      seed = ((seed * deg) + x.at(i));
    }
    return seed;
  }
};

class BooleanMat: public Element<bool> {

  public:

    BooleanMat (size_t degree, Element<bool>* sample = nullptr) 
      : Element<bool>(degree) {}

    BooleanMat (std::vector<bool> const& data) : Element<bool> (data) {}

    // multiply x and y into this
    void redefine (Element<bool> const* x,
                   Element<bool> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      size_t deg = sqrt(this->degree());

      for (size_t i = 0; i < deg; i++) {
        for (size_t j = 0; j < deg; j++) {
          size_t k;
          for (k = 0; k < deg; k++) {
            if (x->at(i * deg + k) && y->at(k * deg + j)) {
              break;
            }
          }
          this->set(i * deg + j, k < deg);
        }
      }
    }

    // the identity of this
    Element<bool>* identity () {
      std::vector<bool> mat;
      mat.reserve(this->degree());
      for (size_t i = 0; i < this->degree(); i++) {
        mat.push_back(false);
      }
      size_t dim = sqrt(this->degree());
      for (size_t i = 0; i < dim; i++) {
        mat.at(i * dim + i) =  true;
      }
      return new BooleanMat(mat);
    }
};

// hash function for unordered_map
template <>
struct std::hash<const BooleanMat> {
  size_t operator() (const BooleanMat& x) const {
    size_t seed = 0;
    size_t deg = x.degree();
    for (size_t i = 0; i < deg; i++) {
      seed = ((seed << 1) + x.at(i));
    }
    return seed;
  }
};

// bipartitions

class Bipartition : public Element<u_int32_t> {

  public:
    
    Bipartition (u_int32_t degree, 
                 Element<u_int32_t>* sample = nullptr) 
      : Element<u_int32_t>(degree) {}

    Bipartition (std::vector<u_int32_t> data) 
      : Element<u_int32_t>(data) {}

    // multiply x and y into this
    void redefine (Element<u_int32_t> const* x, Element<u_int32_t> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      u_int32_t n = this->degree() / 2;
      u_int32_t nrx = static_cast<const Bipartition*>(x)->nrblocks();
      u_int32_t nry = static_cast<const Bipartition*>(y)->nrblocks();

      std::vector<u_int32_t> fuse;
      std::vector<u_int32_t> lookup;
      fuse.reserve(nrx + nry);
      lookup.reserve(nrx + nry);

      // TODO maybe this should be pointer to local data, may slow down hashing
      // but speed up redefinition?
      for (size_t i = 0; i < nrx + nry; i++) {
        fuse.push_back(i);
        lookup.push_back(-1);
      }

      for (size_t i = 0; i < n; i++) {
        u_int32_t xx = fuseit(fuse, x->at(i + n));
        u_int32_t yy = fuseit(fuse, y->at(i) + nrx);
        if (xx != yy) {
          if (xx < yy) {
            fuse.at(yy) = xx;
          } else {
            fuse.at(xx) = yy;
          }
        }
      }

      u_int32_t next = 0;

      for (size_t i = 0; i < n; i++) {
        u_int32_t xx = fuseit(fuse, x->at(i));
        if (lookup.at(xx) == (u_int32_t) -1) {
          lookup.at(xx) = next;
          next++;
        }
        this->set(i, lookup.at(xx));
      }
      for (size_t i = n; i < 2 * n; i++) {
        u_int32_t xx = fuseit(fuse, y->at(i) + nrx);
        if (lookup.at(xx) == (u_int32_t) -1) {
          lookup.at(xx) = next;
          next++;
        }
        this->set(i, lookup.at(xx));
      }
    }

    // the identity of this
    Element<u_int32_t>* identity () {
      std::vector<u_int32_t> image;
      image.reserve(this->degree());
      for (u_int32_t i = 0; i < this->degree(); i++) {
        image.push_back(i);
      }
      return new Bipartition(image);
    }

  private:

    u_int32_t fuseit (std::vector<u_int32_t> const& fuse, u_int32_t pos) {
      while (fuse.at(pos) < pos) {
        pos = fuse.at(pos);
      }
      return pos;
    }

    // nr blocks
    u_int32_t nrblocks () const {
      size_t nr = 0;
      for (size_t i = 0; i < this->degree(); i++) {
        if (this->at(i) > nr) {
          nr = this->at(i);
        }
      }
      return nr + 1;
    }
};

// hash function for unordered_map
// TODO improve this!

template <>
struct std::hash<const Bipartition> {
  size_t operator() (const Bipartition& x) const {
    size_t seed = 0;
    u_int32_t deg = x.degree();
    for (u_int32_t i = 0; i < deg; i++) {
      seed = ((seed * deg) + x.at(i));
    }
    return seed;
  }
};

class MatrixOverSemiring: public Element<long> {
  public:
    MatrixOverSemiring (size_t degree, Semiring* semiring) 
      : Element<long>(degree), 
        _semiring(semiring) {}

    MatrixOverSemiring (size_t degree, Element* sample) 
      : Element<long>(degree), 
        _semiring(static_cast<MatrixOverSemiring*>(sample)->semiring()) {}

    MatrixOverSemiring (std::vector<long> const& data, Semiring* semiring) 
      : Element<long>(data),
        _semiring(semiring) {}
  
    void redefine (Element<long> const* x,
                   Element<long> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      size_t deg = sqrt(this->degree());

      for (size_t i = 0; i < deg; i++) {
        for (size_t j = 0; j < deg; j++) {
          long v = _semiring->zero();
          for (size_t k = 0; k < deg; k++) {
            v = _semiring->plus(v, _semiring->prod(x->at(i * deg + k), 
                                                   y->at(k * deg + j)));
          }
          this->set(i * deg + j, v);
        }
      }
    }

    // the identity
    Element<long>* identity () {
      std::vector<long> mat;
      mat.reserve(this->degree());

      for (size_t i = 0; i < this->degree(); i++) {
        mat.push_back(_semiring->zero());
      }
      size_t n = sqrt(this->degree());
      for (size_t i = 0; i < n; i++) {
        mat.at(i * n + i) =  _semiring->one();
      }
      return new MatrixOverSemiring(mat, _semiring);
    }
  
  Semiring* semiring () {
    return _semiring;
  }

  private: 
    Semiring* _semiring;
}; 

// hash function for unordered_map

template <>
struct std::hash<const MatrixOverSemiring> {
  size_t operator() (const MatrixOverSemiring& x) const {
    size_t seed = 0;
    for (size_t i = 0; i < x.degree(); i++) {
      seed = ((seed << 4) + x.at(i));
    }
    return seed;
  }
};


/*long inline PlusMinMax(long x, long y) {
  if (x == LONG_MAX || y == LONG_MAX)
    return LONG_MAX;
  if (x == LONG_MIN || y == LONG_MIN)
    return LONG_MIN;
  return x + y;
}




class MaxPlusMatrix: public MatrixOverSemiring {

  public:

    MaxPlusMatrix (size_t degree) : Element<long>(degree) {}
    MaxPlusMatrix (std::vector<long> const& data) : Element<long> (data) {}

    // multiply x and y into this
    void redefine (Element<long> const* x,
                   Element<long> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      size_t deg = sqrt(this->degree());

      for (size_t i = 0; i < deg; i++) {
        for (size_t j = 0; j < deg; j++) {
          long v = LONG_MIN;
          for (size_t k = 0; k < deg; k++) {
            v = std::max(v, PlusMinMax(x->at(i * deg + k), y-> at(k * deg + j)));
          }
          this->set(i * deg + j, v);
        }
      }
    }

    // the identity of this
    Element<long>* identity () {
      std::vector<long> mat;
      mat.reserve(this->degree());

      for (size_t i = 0; i < this->degree(); i++) {
        mat.push_back(LONG_MIN);
      }
      size_t n = sqrt(this->degree());
      for (size_t i = 0; i < n; i++) {
        mat.at(i * n + i) =  0;
      }
      return new MaxPlusMatrix(mat);
    }
};

// hash function for unordered_map
template <>
struct std::hash<const MaxPlusMatrix> {
  size_t operator() (const MaxPlusMatrix& x) const {
    size_t seed = 0;
    for (size_t i = 0; i < x.degree(); i++) {
      seed = ((seed << 4) + x.at(i));
    }
    return seed;
  }
};*/

#endif
