/*
 * Semigroups++
 *
 * This file contains declarations for semirings. 
 *
 */

#ifndef SEMIGROUPS_SEMIRING_H
#define SEMIGROUPS_SEMIRING_H

#include <algorithm>
#include <limits.h>

// Namespace for containing everything related to the Semiring.

namespace semiring {

  // Abstract base class.
  // A *semiring* is a ...

  class Semiring {

    public:
      virtual ~Semiring () {};
      
      // Semiring multiplicative identity.
      // Method for finding the multiplicative identity, or one, of the
      // semiring.
      //
      // @return the one of the semiring.
      virtual long one () const            = 0;

      // Semiring additive identity.
      // Method for finding the additive identity, or zero, of the
      // semiring.
      //
      // @return the zero of the semiring.
      virtual long zero () const           = 0;

      // Addition in the semiring.
      // @x any long int
      // @y any long int
      // 
      // Method for finding the sum of two elements in the
      // semiring.
      // @return the sum of x and y in the semiring.
      virtual long plus (long x, long y) const = 0;
      
      // Multiplication in the semiring.
      // @x any long int
      // @y any long int
      // 
      // Method for finding the product of two elements in the
      // semiring.
      // @return the product of x and y in the semiring.
      virtual long prod (long x, long y) const = 0;
      
      // Threshold of the semiring.
      //  
      // Method for finding the threshold of a semiring. The default value is
      // -1 (undefined).
      // @return -1
      virtual long threshold () const {
        return -1;
      }

      // Period of the semiring.
      //  
      // Method for finding the period of a semiring. The default value is
      // -1 (undefined).
      // @return -1
      virtual long period () const {
        return -1;
      }
  };
  
  // Finite field of prime order.
  //
  // This class implements finite fields of prime order only. 
  class PrimeField : public Semiring {

    public: 
      
      // Default constructor.
      // @n the size of the finite field, this must be a prime number but this
      // is not checked.
      PrimeField (long n) : Semiring(), _n(n) {}
      
      // Semiring multiplicative identity.
      // This method returns the multiplicative identity, or one, of the prime field.
      //
      // @return the integer 1.
      long one () const override {
        return 1;
      }
      
      // Semiring additive identity.
      // This method returns the additive identity, or zero, of the prime field.
      //
      // @return the integer 0.
      long zero () const override {
        return 0;
      }
       
      // Multiplication in the semiring.
      // @x any long int
      // @y any long int
      //
      // @return the integer (x * y) mod the size of the prime field.
      long prod (long x, long y) const override {
        return (x * y) % _n;
      }
      
      // Addition in the semiring.
      // @x any long int
      // @y any long int
      //
      // @return the integer (x + y) mod the size of the prime field.
      long plus (long x, long y) const override {
        return (x + y) % _n;
      }
      
      // Finite field size.
      //
      // @return the size of the prime field.
      long size () const {
        return _n;
      }

    private: 
      long _n;
  };

  // Max-plus semiring.
  //
  // The *max-plus semiring* is ... 
  class MaxPlusSemiring : public Semiring {

    public: 
      
      // Default constructor.
      MaxPlusSemiring () : Semiring() {}

      long one () const override {
        return 0;
      }

      long zero () const override {
        return LONG_MIN;
      }
      
      long prod (long x, long y) const override {
        if (x == LONG_MIN || y == LONG_MIN) {
          return LONG_MIN;
        }
        return x + y;
      }

      long plus (long x, long y) const override {
        return std::max(x, y);
      }
  };
  
  // Min-plus semiring.
  //
  // The *min-plus semiring* is ... 

  class MinPlusSemiring : public Semiring {

    public: 
      
      // Default constructor.
      MinPlusSemiring () : Semiring() {}

      long one () const override {
        return 0;
      }

      long zero () const override {
        return LONG_MAX;
      }
      
      long prod (long x, long y) const override {
        if (x == LONG_MAX || y == LONG_MAX) {
          return LONG_MAX;
        }
        return x + y;
      }

      long plus (long x, long y) const override {
        return std::min(x, y);
      }
  };
  
  // Tropical semiring base class.
  //
  // This class provides common methods for its subclasses
  // <TropicalMaxPlusSemiring> and <TropicalMinPlusSemiring>.
  class TropicalSemiring : public Semiring {

    public: 

      TropicalSemiring (long threshold) : Semiring(), _threshold(threshold) {}
      
      long clipped (long x) const {
        if (x > _threshold) {
          return _threshold;
        }
        return x;
      }
      
      long threshold () const override {
        return _threshold;
      }

    private: 
      
      long _threshold;
  };

  // Tropical max-plus semiring.
  //
  // The *tropical max-plus semiring* is ...
  class TropicalMaxPlusSemiring : public TropicalSemiring {

    public: 

      TropicalMaxPlusSemiring (long threshold) : TropicalSemiring(threshold) {}

      long one () const override {
        return 0;
      }

      long zero () const override {
        return LONG_MIN;
      }
      
      long prod (long x, long y) const override {
        if (x == LONG_MIN || y == LONG_MIN) {
          return LONG_MIN;
        }
        return clipped(x + y);
      }

      long plus (long x, long y) const override {
        return clipped(std::max(x, y));
      }
  };

  // Tropical min-plus semiring.
  //
  // The *tropical min-plus semiring* is ...
  class TropicalMinPlusSemiring : public TropicalSemiring {

    public: 

      TropicalMinPlusSemiring (long threshold) : TropicalSemiring(threshold) {}

      long one () const override {
        return 0;
      }

      long zero () const override {
        return LONG_MAX;
      }
      
      long prod (long x, long y) const override {
        if (x == LONG_MAX || y == LONG_MAX) {
          return LONG_MAX;
        }
        return clipped(x + y);
      }

      long plus (long x, long y) const override {
        if (x == LONG_MAX && y == LONG_MAX) {
          return LONG_MAX;
        }
        return clipped(std::min(x, y));
      }
  };
  
  // Semiring of natural numbers ...
  //
  // This class implements the *semiring* with ...
  
  class NaturalSemiring : public Semiring {

    public: 

      NaturalSemiring (long threshold, long period) 
        : Semiring(),
          _threshold(threshold), 
          _period(period)
         {}

      long one () const override {
        return 1;
      }

      long zero () const override {
        return 0;
      }
      
      long prod (long x, long y) const override {
        return thresholdperiod(x * y);
      }

      long plus (long x, long y) const override {
        return thresholdperiod(x + y);
      }
      
      long threshold () const override {
        return _threshold;
      }
      
      long period () const override {
        return _period;
      }

    private:

      long thresholdperiod (long x) const {
        if (x > _threshold) {
          return _threshold + (x - _threshold) % _period;
        }
        return x;
      }

      long _threshold;
      long _period;
  };
  
}
#endif
