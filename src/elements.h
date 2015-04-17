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

class Bipartition : public Element<u_int32_t> {

  public: 
    
    Bipartition (std::vector<u_int32_t> data) : Element<u_int32_t>(data) {}
    Bipartition (u_int32_t degree) : Element<u_int32_t>(degree) {}
    
    // multiply x and y into this
    void redefine (Element<u_int32_t> const* x, Element<u_int32_t> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      u_int32_t n = this->degree() / 2;
      u_int32_t nrx = static_cast<const Bipartition*>(x)->nrblocks();
      u_int32_t nry = static_cast<const Bipartition*>(y)->nrblocks();

      std::vector<u_int32_t> fuse; 
      // TODO maybe this should be pointer to local data, may slow down hashing
      // but speed up redefinition? 
      for (size_t i = 0; i < nrx + nry; i++) {
        fuse.push_back(i);
      }

      for (size_t i = 0; i < n; i++) {
        u_int32_t xx = fuseit(fuse, x->at(i + n));
        u_int32_t yy = fuseit(fuse, y->at(i + nrx));
        if (xx != yy) {
          if (xx < yy) {
            fuse.at(yy) = xx;
          } else {
            fuse.at(xx) = yy;
          }
        }
      }
      
      u_int32_t next = 0;
      std::vector<u_int32_t> lookup;
      lookup.reserve(nrx + nry);
      
      for (size_t i = 0; i < n; i++) {
        u_int32_t xx = fuseit(fuse, x->at(i));
        if (xx > lookup.size()) {
          lookup.push_back(next);
          next++;
        }
        this->set(i, lookup.at(xx));
      }
      for (size_t i = n; i < 2 * n; i++) {
        u_int32_t xx = fuseit(fuse, y->at(i) + nrx);
        if (xx > lookup.size()) {
          lookup.push_back(next);
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

#endif
