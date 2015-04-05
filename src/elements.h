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
    //Element& operator= (Element const& copy) {}
    virtual ~Element () {}
    //virtual Element operator * (Element other) {}
    //virtual Element operator == (Element other) {}

};

class Transformation;
namespace std {
  template<>
  class hash<Transformation> {
    public:
      size_t operator()(const Transformation &x) const;
  };
}

class Transformation : public Element {

  public: 

    Transformation (std::vector<u_int16_t> image) : _image(image),
                                                    _deg(image.size()) {
                                                      std::cout << "transformation constructor!!\n";
                                                    }
    ~Transformation () {std::cout << "transformation destructed!!\n";}

    /* required methods */
    Transformation operator * (Transformation* other) {
      assert(other->degree() == this->degree());
      std::vector<u_int16_t> out;
      out.reserve(_deg);
      for (u_int16_t i = 0; i < _deg; i++) {
        out[i] = this->_image[other->_image[i]];
      }
      return Transformation(out);
    }
    
    bool operator == (Transformation* other) const {
      assert(other->degree() == this->degree());
      for (size_t i = 0; i < _deg; i++) {
        if (this->_image[i] != other->_image[i]) {
          return false;
        }
      }
      return true;
    }

    std::vector<u_int16_t> image () const {
      return _image;
    }

    /* specific to Transformation methods */
    u_int16_t degree () const {
      return _deg;
    }
    
    //TODO hash function

    friend size_t std::hash<Transformation>::operator ()(const Transformation&) const;

  private:
    std::vector<u_int16_t> _image;
    u_int16_t _deg;

};

namespace std {
    size_t hash<Transformation>::operator()(const Transformation &x) const {
      size_t h = 0;
      u_int16_t deg = x.degree();
      std::vector<u_int16_t> img = x.image();
      for (size_t i = 0; i < deg; i++) {
        h = (h * (deg + 1) + img[i]);
      }
      return h;
    }
}

//#endif
