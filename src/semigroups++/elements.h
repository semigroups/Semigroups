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

// template for the base class for elements of a semigroup

template <typename T>
class Element {

  public:
    
    Element () {} // just for PBRs

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
      if (this->degree() != that.degree()) {
        return false;
      }

      for (size_t i = 0; i < this->degree(); i++) {
        if ((*_data)[i] != (*that._data)[i]) {
          return false;
        }
      }
      return true;
    };

    inline T at (size_t pos) const {
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

    virtual Element<T>* copy (size_t increase_deg_by = 0) const {
      assert(increase_deg_by == 0); 
      // this assert is here since any specialization of Element where the
      // degree can be changed (i.e. where increase_deg_by != 0) must have its
      // own method for copy, and must use it!
      return new Element(*_data);
    }

    void delete_data () {
      if (_data != nullptr) {
        delete _data;
      }
    }

    void push_back (T val) {
      _data->push_back(val);
    }

    std::vector<T>* data () const {
      return _data;
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
  
    Element<T>* copy (size_t increase_deg_by = 0) const override {
        
      Element<T>* out = new Element<T>(*this->data());
      size_t deg = out->degree();
      for (size_t i = deg; i < deg + increase_deg_by; i++) {
        out->push_back(i);
      }
      return out;
    }

    size_t complexity () const {
      return this->degree();
    }
};

// hash function for unordered_map
namespace std {
  template <typename T>
    struct hash<const Transformation<T> > {
    size_t operator() (const Transformation<T>& x) const {
      size_t seed = 0;
      T deg = x.degree();
      for (T i = 0; i < deg; i++) {
        seed = ((seed * deg) + x.at(i));
      }
      return seed;
    }
  };
}

// template for partial perms

template <typename T>
class PartialPerm : public Element<T> {

  public:

    PartialPerm (T degree, Element<T>* sample = nullptr) 
      : Element<T>(degree) {}

    PartialPerm (std::vector<T> data) : Element<T>(data) {}

    // multiply x and y into this
    void redefine (Element<T> const* x, Element<T> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());

      for (T i = 0; i < this->degree(); i++) {
        this->set(i, (x->at(i) == 0 ? 0 : y->at(x->at(i) - 1)));
      }
    }

    // the identity on [1 .. degree], FIXME is this right? 
    Element<T>* identity () {
      std::vector<T> image;
      image.reserve(this->degree());
      for (T i = 0; i < this->degree(); i++) {
        image.push_back(i + 1);
      }
      return new PartialPerm(image);
    }
    
    Element<T>* copy (size_t increase_deg_by = 0) const override {
      Element<T>* out = new Element<T>(*this->data());
      size_t deg = out->degree();
      for (size_t i = deg; i < deg + increase_deg_by; i++) {
        out->push_back(0);
      }
      return out;
    }

    size_t complexity () const {
      return this->degree();
    }
};

// hash function for unordered_map
namespace std {
  template <typename T>
    struct hash<const PartialPerm<T> > {
    size_t operator() (const PartialPerm<T>& x) const {
      size_t seed = 0;
      T deg = x.degree();
      for (T i = 0; i < deg; i++) {
        seed = ((seed * deg) + x.at(i));
      }
      return seed;
    }
  };
}

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

    size_t complexity () const {
      return pow(this->degree(), 3);
    }
};

// hash function for unordered_map
namespace std {
  template <>
    struct hash<const BooleanMat> {
    size_t operator() (const BooleanMat& x) const {
      size_t seed = 0;
      size_t deg = x.degree();
      for (size_t i = 0; i < deg; i++) {
        seed = ((seed << 1) + x.at(i));
      }
      return seed;
    }
  };
}

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
      for (size_t j = 0; j < 2; j++) {
        for (u_int32_t i = 0; i < this->degree() / 2; i++) {
          image.push_back(i);
        }
      }
      return new Bipartition(image);
    }
    
    size_t complexity () const {
      return pow(this->degree(), 2);
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
namespace std {
  template <>
    struct hash<const Bipartition> {
    size_t operator() (const Bipartition& x) const {
      size_t seed = 0;
      u_int32_t deg = x.degree();
      for (u_int32_t i = 0; i < deg; i++) {
        seed = ((seed * deg) + x.at(i));
      }
      return seed;
    }
  };
}

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
  
    Element* copy (size_t increase_degree_by = 0) const {
      return new MatrixOverSemiring(*_data, _semiring);
    }

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
      after(); // post process this
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
    
    size_t complexity () const {
      return pow(this->degree(), 3);
    }

  private: 

    // a function applied after redefinition 
    virtual void after () {}

    Semiring* _semiring;
}; 

// hash function for unordered_map
namespace std {
  template <>
    struct hash<const MatrixOverSemiring> {
    size_t operator() (const MatrixOverSemiring& x) const {
      size_t seed = 0;
      for (size_t i = 0; i < x.degree(); i++) {
        seed = ((seed << 4) + x.at(i));
      }
      return seed;
    }
  };
}

class ProjectiveMaxPlusMatrix: public MatrixOverSemiring {

  public:

    ProjectiveMaxPlusMatrix (size_t degree, Semiring* semiring) 
      : MatrixOverSemiring(degree, semiring) {}
    
    ProjectiveMaxPlusMatrix (size_t degree, Element* sample) 
      : MatrixOverSemiring(degree, sample) {}

  private:

    void after () {
      long norm = LONG_MIN;
      size_t deg = sqrt(this->degree());

      for (size_t i = 0; i < deg; i++) {
        for (size_t j = 0; j < deg; j++) {
          if ((long) this->at(i * deg + j) > norm)  {
            norm = this->at(i * deg + j);
          }
        }
      }
      for (size_t i = 0; i < deg; i++) {
        for (size_t j = 0; j < deg; j++) {
          if (this->at(i * deg + j) != LONG_MIN) {
            this->set(i * deg + j, this->at(i * deg + j) - norm);
          }
        }
      }
    }
};

namespace std {
  template <>
    struct hash<const ProjectiveMaxPlusMatrix> {
    size_t operator() (const ProjectiveMaxPlusMatrix& x) const {
      size_t seed = 0;
      for (size_t i = 0; i < x.degree(); i++) {
        seed = ((seed << 4) + x.at(i));
      }
      return seed;
    }
  };
}

// partitioned binary relations

class PBR: public Element<std::vector<u_int32_t> > {
  
  public:
  
    PBR (u_int32_t degree, 
         Element<std::vector<u_int32_t> >* sample = nullptr) 
      : Element<std::vector<u_int32_t> >() {
        _data = new std::vector<std::vector<u_int32_t> >();
        _data->reserve(degree);
        for (size_t i = 0; i < degree; i++) {
          _data->push_back(std::vector<u_int32_t>());
        }
      }
    
    PBR (std::vector<std::vector<u_int32_t> >
                               const& data)
      : Element<std::vector<u_int32_t> >(data) { }
    
    //FIXME this allocates lots of memory on every call, maybe better to keep
    //the data in the class and overwrite it.
    //FIXME also we repeatedly search in the same part of the graph, and so
    //there is probably a lot of repeated work in the dfs.
    void redefine (Element<std::vector<u_int32_t> > const* x, 
                   Element<std::vector<u_int32_t> > const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      u_int32_t n = this->degree() / 2;
      
      for (size_t i = 0; i < 2 * n; i++) {
        (*_data)[i].clear();
      }

      std::vector<bool> x_seen;
      std::vector<bool> y_seen;
      
      for (size_t i = 0; i < 2 * n; i++) {
        x_seen.push_back(false);
        y_seen.push_back(false);
      }
      
      for (size_t i = 0; i < n; i++) {
        x_dfs(n, i, i, x_seen, y_seen, x, y);
        for (size_t j = 0; j < 2 * n; j++) {
          x_seen.at(j) = false;
          y_seen.at(j) = false;
        }
      }
      
      for (size_t i = n; i < 2 * n; i++) {
        y_dfs(n, i, i, x_seen, y_seen, x, y);
        for (size_t j = 0; j < 2 * n; j++) {
          x_seen.at(j) = false;
          y_seen.at(j) = false;
        }
      }
    }
    
    Element<std::vector<u_int32_t> >* identity () {
      std::vector<std::vector<u_int32_t> > adj;
      size_t n = this->degree() / 2;
      adj.reserve(2 * n);
      for (u_int32_t i = 0; i < 2 * n; i++) {
        adj.push_back(std::vector<u_int32_t>());
      }
      for (u_int32_t i = 0; i < n; i++) {
        adj.at(i).push_back(i + n);
        adj.at(i + n).push_back(i);
      }
      return new PBR(adj);
    }
    
    size_t complexity () const {
      return pow((2 * this->degree()), 3);
    }

  private:

    // add vertex2 to the adjacency of vertex1
    void add_adjacency (size_t vertex1, size_t vertex2) {
      auto it = std::lower_bound(_data->at(vertex1).begin(),
                                 _data->at(vertex1).end(), 
                                 vertex2);
      if (it == _data->at(vertex1).end()) {
        _data->at(vertex1).push_back(vertex2);
      } else if ((*it) != vertex2) {
        _data->at(vertex1).insert(it, vertex2);
      }
    }

    void x_dfs (u_int32_t n,
                u_int32_t i, 
                u_int32_t v,  // the vertex we're currently doing
                std::vector<bool>& x_seen,
                std::vector<bool>& y_seen,
                Element<std::vector<u_int32_t> > const* x, 
                Element<std::vector<u_int32_t> > const* y) {

      if (!x_seen.at(i)) {
        x_seen.at(i) = true;
        for (auto j: x->at(i)) {
          if (j < n) {
            add_adjacency(v, j);
          } else {
            y_dfs(n, j - n, v, x_seen, y_seen, x, y);
          }
        }
      }
    }

    void y_dfs (u_int32_t n,
                u_int32_t i, 
                u_int32_t v, 
                std::vector<bool>& x_seen,
                std::vector<bool>& y_seen,
                Element<std::vector<u_int32_t> > const* x, 
                Element<std::vector<u_int32_t> > const* y) {

      if (!y_seen.at(i)) {
        y_seen.at(i) = true;
        for (auto j: y->at(i)) {
          if (j >= n) {
            add_adjacency(v, j);
          } else {
            x_dfs(n, j + n, v, x_seen, y_seen, x, y);
          }
        }
      }
    }
};

namespace std {
  template <>
    struct hash<const PBR> {
    size_t operator() (const PBR& x) const {
      size_t seed = 0;
      size_t pow = 101;
      for (size_t i = 0; i < x.degree(); i++) {
        for (size_t j = 0; j < x.at(i).size(); j++) { 
          seed = (seed * pow) + x.at(i).at(j);
        }
      }
      return seed;
    }
  };
}

#endif
