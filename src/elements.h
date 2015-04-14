/*
 * Semigroups++
 *
 * This file contains classes for creating elements of a semigroup.
 *
 */

#ifndef SEMIGROUPS_ELEMENTS_H
#define SEMIGROUPS_ELEMENTS_H
//#define NDEBUG 

#include <iostream>
#include <vector>
#include <assert.h>
#include <functional>

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

#endif
