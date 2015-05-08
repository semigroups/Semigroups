/*
 * Semigroups++
 *
 * This file contains declarations for semirings. 
 *
 */

#ifndef SEMIGROUPS_SEMIRING_H
#define SEMIGROUPS_SEMIRING_H
#include <limits.h>

//FIXME don't do this
using namespace std;

namespace semiring {
   
  class Semiring {

    public:
      virtual ~Semiring () = 0;
      virtual long one  () = 0;
      virtual long zero () = 0;
      virtual long plus (long, long) = 0;
      virtual long prod (long, long) = 0;
      virtual long threshold () {
        return -1;
      };
  };

  class PrimeField : public Semiring {

    public: 

      PrimeField (long n) : _n(n) {}

      long one () {
        return 1;
      }

      long zero () {
        return 0;
      }
      
      long prod (long x, long y) {
        return (x * y) % _n;
      }

      long plus (long x, long y) {
        return (x + y) % _n;
      }

      long size () {
        return _n;
      }

    private: 
      long _n;
  };

  class MaxPlusSemiring : public Semiring {

    public: 

      long one () {
        return 0;
      }

      long zero () {
        return LONG_MIN;
      }
      
      long prod (long x, long y) {
        if (x == LONG_MIN || y == LONG_MIN) {
          return LONG_MIN;
        }
        return x + y;
      }

      long plus (long x, long y) {
        return std::max(x, y);
      }
  };

  class MinPlusSemiring : public Semiring {

    public: 

      long one () {
        return 0;
      }

      long zero () {
        return LONG_MAX;
      }
      
      long prod (long x, long y) {
        if (x == LONG_MAX || y == LONG_MAX) {
          return LONG_MAX;
        }
        return x + y;
      }

      long plus (long x, long y) {
        return std::min(x, y);
      }
  };

  class TropicalSemiring : public Semiring {

    public: 

      TropicalSemiring (long threshold) : _threshold(threshold) {}
      
      long clipped (long x) {
        if (x > _threshold) {
          return _threshold;
        }
        return x;
      }
      
      long threshold () {
        return _threshold;
      }

    private: 
      
      long _threshold;
  };

  class TropicalMaxPlusSemiring : public TropicalSemiring {

    public: 

      TropicalMaxPlusSemiring (long threshold) : TropicalSemiring(threshold) {}

      long one () {
        return 0;
      }

      long zero () {
        return LONG_MIN;
      }
      
      long prod (long x, long y) {
        if (x == LONG_MIN || y == LONG_MIN) {
          return LONG_MIN;
        }
        return clipped(x + y);
      }

      long plus (long x, long y) {
        return clipped(std::max(x, y));
      }
  };

  class TropicalMinPlusSemiring : public TropicalSemiring {

    public: 

      TropicalMinPlusSemiring (long threshold) : TropicalSemiring(threshold) {}

      long one () {
        return 0;
      }

      long zero () {
        return LONG_MAX;
      }
      
      long prod (long x, long y) {
        if (x == LONG_MAX || y == LONG_MAX) {
          return LONG_MAX;
        }
        return clipped(x + y);
      }

      long plus (long x, long y) {
        if (x == LONG_MAX && y == LONG_MAX) {
          return LONG_MAX;
        }
        return clipped(std::min(x, y));
      }
  };
}
#endif
