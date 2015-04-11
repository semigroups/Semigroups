/*
 * Semigroups++
 *
 * This file contains classes for creating elements of a semigroup.
 *
 */

#ifndef SEMIGROUPS_ELEMENTS_H
#define SEMIGROUPS_ELEMENTS_H
#define NDEBUG 

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
    
    //virtual void redefine (Element const*, Element const*) {}
    //virtual Element<T>* identity () = 0;
    
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
    /*
    Transformation (const Transformation& copy) : Element<T>(copy), _identity(nullptr) {}
    ~Transformation () { 
      if (_identity != nullptr) {
        delete _identity;
      }
    }*/
    
    // required methods 
    void redefine (Element<T> const* x, Element<T> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());

      for (T i = 0; i < this->degree(); i++) {
        this->set(i, y->at(x->at(i)));
      }
    }
    
    /*Element<T>* identity () {
      if (_identity == nullptr) {
        std::vector<T> image;
        image.reserve(this->degree());
        for (T i = 0; i < this->degree(); i++) {
          image.push_back(i);
        }
        _identity = static_cast<Element<T>*>(new Transformation(image));
      } 
      return _identity;
    }*/
};

template <typename T>
struct std::hash<const Transformation<T> > {
  size_t operator() (const Transformation<T>& x) const {
    size_t seed = 0;
    T deg = x.degree();
    //std::cout << "hashing!";
    for (T i = 0; i < deg; i++) {
      seed = ((seed * deg) + x.at(i));
      //seed ^= _int_hasher((seed * deg) + x.at(i)) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
  //std::hash<T> _int_hasher;
};

#endif
