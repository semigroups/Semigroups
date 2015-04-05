/*
 * Semigroups++: elements
 */

//#ifndef SEMIGROUPS_ELEMENTS_H
//#define SEMIGROUPS_ELEMENTS_H

#include <iostream>
#include <vector>
#include <assert.h>
#include <functional>

/*template<typename T> void hash_combine(size_t & seed, T const& v) {
  seed ^= std::hash<u_int16_t>(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

template<typename It> std::size_t hash_range(It first, It last) {
  size_t seed = 0;

  for(; first != last; ++first) {
    hash_combine(seed, *first);
  }
}*/

//#include <boost/functional/hash.hpp>
/* namespace std {

  template <>
  struct hash<Key>
  {
    std::size_t operator()(const Key& k) const
    {
      using std::size_t;
      using std::hash;
      using std::string;

      // Compute individual hash values for first,
      // second and third and combine them using XOR
      // and bit shifting:

      return ((hash<string>()(k.first)
               ^ (hash<string>()(k.second) << 1)) >> 1)
               ^ (hash<int>()(k.third) << 1);
    }
  };
}*/

class Element {

  public:
    
    Element () {}
    Element (const Element& copy) {}
    virtual ~Element () {};
    //virtual Element& operator= (Element const& copy) = 0;
    virtual void redefine (Element const*, Element const*) = 0;
    virtual bool operator == (Element const*) const = 0;
    virtual u_int16_t hash_value () = 0;
    virtual u_int16_t degree () const = 0;

  private:
    u_int16_t _deg;
    u_int16_t _hash_value;

};

class Transformation : public Element {

  public: 

    Transformation (std::vector<u_int16_t> image) : _image(image),
                                                    _deg(image.size()){
                                                      std::cout << "transformation constructor!!\n";
                                                    }
    
    Transformation (const Transformation& copy) : _image(copy.image()),
                                                  _deg(copy.degree()){
                                                  std::cout << "copying transformation!!\n";
                                                  }

    ~Transformation () {std::cout << "transformation destructed!!\n";}

    /* required methods */
    void redefine (Element const* x, Element const* y) {
      assert(x->degree() == this->degree());
      assert(x->degree() == y->degree());
      Transformation const* xx = static_cast<Transformation const*>(x);
      Transformation const* yy = static_cast<Transformation const*>(y);
      for (u_int16_t i = 0; i < _deg; i++) {
        _image[i] = xx->_image[yy->_image[i]];
      }
    }
    
    bool operator == (Element const* other) const {
      Transformation const* that = static_cast<Transformation const*>(other);
      assert(that->degree() == this->degree());
      for (size_t i = 0; i < _deg; i++) {
        if (this->_image[i] != that->_image[i]) {
          return false;
        }
      }
      return true;
    }
    
    u_int16_t hash_value () {
      if (_hash_value == 0) {
        for (size_t i = 0; i < _deg; i++) {
          _hash_value = (_hash_value * (_deg) + _image[i]);
        }
      }
      return _hash_value;
    }

    /* specific methods for this class */
    std::vector<u_int16_t> image () const {
      return _image;
    }

    u_int16_t degree () const {
      return _deg;
    }

  private:
    std::vector<u_int16_t> _image;
    u_int16_t _deg;
    u_int16_t _hash_value;

};

//#endif
