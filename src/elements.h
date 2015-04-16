/*
 * Semigroups++
 *
 * This file contains classes for creating elements of a semigroup.
 *
 */

#ifndef SEMIGROUPS_ELEMENTS_H
#define SEMIGROUPS_ELEMENTS_H
//#define NDEBUG 

#include <assert.h>
#include <functional>
#include <iostream>
#include <vector>
//#include "MurmurHash2_64.hpp"

// template for the base class for elements of a semigroup

template <typename T>
class Element {

  public:
    
    Element (T degree) {
      _data = new std::vector<T>();
      _data->reserve(degree);
      for (T i = 0; i < degree; i++) {
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
      for (T i = 0; i < this->degree(); i++) {
        if ((*_data)[i] != (*that._data)[i]) {
          return false;
        }
      }
      return true;
    };

    inline T at (T pos) const {
      return (*_data)[pos];
    }
    
    inline void set (T pos, T val) {
      (*_data)[pos] = val;
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
    
    Transformation (std::vector<T> data) : Element<T>(data) {}
    Transformation (T degree) : Element<T>(degree) {}
    
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

// template for bipartitions

template <typename T>
class Bipartition : public Element<T> {

  public: 
    
    Bipartition (std::vector<T> data) : Element<T>(data) {}
    Bipartition (T degree) : Element<T>(degree) {}
    
    // multiply x and y into this
    void redefine (Element<T> const* x, Element<T> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      T n = this->degree();
      T nrx = x->nrblocks();
      T nry = y->nrblocks();

      std::vector<T> fuse; 
      // TODO maybe this should be pointer to local data, may slow down hashing
      // but speed up redefinition? 
      for (size_t i = 0; i < nrx + nry; i++) {
        fuse.push_back(i);
      }

      for (size_t i = 0; i < n; i++) {
        T xx = fuseit(fuse, x->at(i + n));
        T yy = fuseit(fuse, y->at(i + nrx));
        if (xx != yy) {
          if (xx < yy) {
            fuse.at(yy) = xx;
          } else {
            fuse.at(xx) = yy;
          }
        }
      }
      
      T next = 0;
      std::vector<T> lookup;
      lookup.reserve(nrx + nry);
      
      for (size_t i = 0; i < n; i++) {
        T xx = fuseit(fuse, x->at(i));
        if (xx > lookup.size()) {
          lookup.push_back(next);
          next++;
        }
        this->set(i, lookup.at(xx));
      }
      for (size_t i = n; i < 2 * n; i++) {
        T xx = fuseit(fuse, y->at(i) + nrx);
        if (xx > lookup.size()) {
          lookup.push_back(next);
          next++;
        }
        this->set(i, lookup.at(xx));
      }
    }

    // the identity of this
    Element<T>* identity () {
      std::vector<T> image;
      image.reserve(this->degree());
      for (T i = 0; i < this->degree(); i++) {
        image.push_back(i);
      }
      return new Bipartition(image);
    }
    
  private: 
    
    T fuseit (std::vector<T> const& fuse, T pos) {
      while (fuse.at(pos) < pos) {
        pos = fuse.at(pos);
      }
      return pos;
    }

    // nr blocks
    T nrblocks () {
      size_t nr = 0;
      for (size_t i; i < this->degree(); i++) {
        if (this->at(i) > nr) {
          nr = this->at(i);
        }
      }
      return nr;
    }
};

// hash function for unordered_map
// TODO improve this!

template <typename T>
struct std::hash<const Bipartition<T> > {
  size_t operator() (const Bipartition<T>& x) const {
    size_t seed = 0;
    T deg = x.degree();
    for (T i = 0; i < deg; i++) {
      seed = ((seed * deg) + x.at(i));
    }
    return seed;
  }

};

#endif
