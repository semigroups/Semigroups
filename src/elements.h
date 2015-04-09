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

// template for the abstract base class for elements of a semigroup

template <typename T>
class Element {

  public:
    
    Element (T degree) : _data() {
      _data.reserve(degree);
      for (T i = 0; i < degree; i++) {
        _data.push_back(0);
      }
    }
    
    Element (std::vector<T> data) : _data(data) {
    }
    
    Element (const Element& copy) : _data(copy.data()) {
    }

    virtual ~Element () {}
    
    virtual void redefine (Element const*, Element const*) = 0;
    virtual bool operator == (const Element<T> &) const = 0;
    virtual Element<T>* identity ()  = 0;
    virtual size_t hash_value () = 0;
    
    std::vector<T> data () const {
      return _data;
    }

    inline T at (T pos) const {
      return _data.at(pos);
    }
    
    inline void set (T pos, T val) {
      _data[pos] = val;
    }
    
    size_t degree () const {
      return _data.size();
    }

  protected:
    std::vector<T> _data;
};

// template for transformations

template <typename T>
class Transformation : public Element<T> {

  public: 
    
    Transformation (T degree) : Element<T>(degree), _identity(nullptr), _hash_value(0){}
    Transformation (std::vector<T> data) : Element<T>(data), _identity(nullptr), _hash_value(0) {}
    Transformation (const Transformation& copy) : Element<T>(copy), _identity(nullptr), _hash_value(copy._hash_value) {}
    ~Transformation () { 
      if (_identity != nullptr) {
        delete _identity;
      }
    }
    
    // required methods 
    void redefine (Element<T> const* x, Element<T> const* y) {
      assert(x->degree() == y->degree());
      assert(x->degree() == this->degree());
      _hash_value = 0;
      Transformation const* xx = static_cast<Transformation const*>(x);
      Transformation const* yy = static_cast<Transformation const*>(y);
       
      for (T i = 0; i < this->degree(); i++) {
        this->set(i, yy->at(xx->at(i)));
      }
    }
    
    bool operator == (const Element<T> &other) const {
      for (T i = 0; i < this->degree(); i++) {
        if (this->at(i) != other.at(i)) {
          return false;
        }
      }
      return true;
    }
    
    Element<T>* identity () {
      if (_identity == nullptr) {
        std::vector<T> image;
        image.reserve(this->degree());
        for (T i = 0; i < this->degree(); i++) {
          image.push_back(i);
        }
        _identity = static_cast<Element<T>*>(new Transformation(image));
      } 
      return _identity;
    }

    // FIXME this is not really ideal 
    size_t hash_value () {
      if (_hash_value == 0) {
        T deg = this->degree();
        for (size_t i = 0; i < deg; i++) {
          _hash_value = ((_hash_value * deg) + this->at(i));
          //_hash_value ^= std::hash<size_t>(_data[i]) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }
      }
      return _hash_value;
    }

  private:
    Element<T>*     _identity;
    size_t          _hash_value;
};

template <typename T>
struct std::hash<Transformation<T> > {
  size_t operator() (const Transformation<T>& x) const {
    size_t seed = 0;
    T deg = x.degree();
    //std::cout << "hashing!";
    for (T i = 0; i < deg; i++) {
      //seed = ((seed * deg) + x.at(i));
      seed ^= _int_hasher((seed * deg) + x.at(i)) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
  std::hash<T> _int_hasher;
};

#endif
