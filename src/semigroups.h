/*
 * semigroups: Methods for semigroups
 */

#ifndef GAP_SEMIGROUPS_H
#define GAP_SEMIGROUPS_H

#include <vector>
#include <iostream>
extern "C" {
  #include "src/compiled.h"          /* GAP headers                */
}

#ifndef T_SEMI
#define T_SEMI T_SPARE2
#endif

template<typename T>
inline void SET_WRAP(Obj o, T* p) {
    ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(p);
}

template<typename T>
inline T* GET_WRAP(Obj o) {
    return reinterpret_cast<T*>(ADDR_OBJ(o)[0]);
}

class CayleyGraph {
        
      public:
        
        CayleyGraph (const CayleyGraph& copy) = delete;
        CayleyGraph& operator= (CayleyGraph const& copy) = delete;
        
        CayleyGraph(size_t nrgens) : _vec(new std::vector<u_int32_t>()), 
                                     _nrgens(nrgens) {
          for (size_t i = 0; i < nrgens * nrgens; i ++) {
            _vec->push_back(static_cast<u_int32_t>(0));
          }

        }
        
        ~CayleyGraph() {
          delete _vec;
        }
        
        u_int32_t get (size_t i, size_t j) {
          return _vec->at((i - 1) * _nrgens + (j - 1)); 
        }
        
        void set (size_t i, size_t j, u_int32_t val) {
          std::cout << (i - 1) * _nrgens + (j - 1) << "\n";
          std::cout << _vec->size() << "\n";
          _vec->at((i - 1) * _nrgens + (j - 1)) = val; 
        }

        void expand () {
          std::cout << _vec->size() << "\n";
          std::cout << _nrgens << "\n";

          for (size_t i = 0; i < _nrgens; i++) {
            _vec->push_back(static_cast<u_int32_t>(0));
          }
        }

        size_t size () {
          return _vec->size();
        }

      private:

        std::vector<u_int32_t>* _vec;
        size_t _nrgens;
};

#endif
