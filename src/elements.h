/*
 * Semigroups++: elements
 */

#ifndef SEMIGROUPS_ELEMENTS_H
#define SEMIGROUPS_ELEMENTS_H

#include <iostream>
#include <vector>
#include <assert.h>
#include <functional>

// template for the abstract base class for elements of a semigroup

template <typename T>
class Element {

  public:
    
    Element () {}
    Element (const Element& copy) {}
    virtual ~Element () {};
    //virtual Element& operator= (Element const& copy) = 0;
    virtual void redefine (Element const*, Element const*) = 0;
    virtual bool operator == (Element const*) const = 0;
    virtual T hash_value () = 0;
    virtual T degree () const = 0;
    virtual Element<T>* identity () = 0;

  private:
    T        _deg;
    T        _hash_value;
    Element* _identity;
};

// template for transformations

template <typename T>
class Transformation : public Element<T> {

  public: 
    
    Transformation (T deg) : _image(), _deg(deg) {
    }

    Transformation (std::vector<T> image) : _image(image),
                                            _deg(image.size()),
                                            _hash_value(0){
                                              std::cout << "transformation constructor!!\n";
    }
    
    Transformation (const Transformation& copy) : _image(copy.image()),
                                                  _deg(copy.degree()),
                                                  _hash_value(copy._hash_value){

                                                  std::cout << "copying transformation!!\n";
                                                  }

    ~Transformation () {std::cout << "transformation destructed!!\n";
      if (_identity != nullptr) {
        delete _identity;
      }
    }

    // required methods 
    void redefine (Element<T> const* x, Element<T> const* y) {
      assert(x->degree() == y->degree());
      _deg = x->degree();
      Transformation const* xx = static_cast<Transformation const*>(x);
      Transformation const* yy = static_cast<Transformation const*>(y);
      for (T i = 0; i < _deg; i++) {
        _image[i] = xx->_image[yy->_image[i]];
      }
      _hash_value = 0;
    }
    
    bool operator == (Element<T> const* other) const {
      Transformation const* that = static_cast<Transformation const*>(other);
      assert(that->degree() == this->degree());
      for (size_t i = 0; i < _deg; i++) {
        if (this->_image[i] != that->_image[i]) {
          return false;
        }
      }
      return true;
    }
    // FIXME this is not really ideal 
    // TODO test if it is better to do the commented out code below, or 
    // to actually define a hash function for Transformations
    T hash_value () {
      if (_hash_value == 0) {
        for (size_t i = 0; i < _deg; i++) {
          _hash_value = (_hash_value * (_deg) + _image[i]);
          //seed ^= std::hash<u_int16_t>(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }
      }
      return _hash_value;
    }

    Element<T>* identity () {
      if (_identity == nullptr) {
        std::vector<T> image;
        image.reserve(_deg);
        for (size_t i = 0; i < _deg; i++) {
          image.push_back(i);
        }
        //FIXME not sure this will work
        _identity = new Transformation(image);
      } 
      return static_cast<Element<T>*>(_identity);
    }

    // specific methods for this class
    std::vector<T> image () const {
      return _image;
    }

    T degree () const {
      return _deg;
    }

  private:
    std::vector<T>  _image;
    T               _deg;
    T               _hash_value;
    Transformation* _identity;

};

#endif
