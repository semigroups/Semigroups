/*
 * Semigroups++
 *
 * This file some basic classes.
 *
 */

#ifndef SEMIGROUPS_BASICS_H
#define SEMIGROUPS_BASICS_H
#define NDEBUG 

#include <assert.h>
#include <iostream>
#include <vector>

template <typename T>
class RecVec {
        
      public:
        
        RecVec(size_t nrcols, size_t nrrows=0) : _vec(), 
                                                 _nrcols(nrcols),
                                                 _nrrows(0){
          this->expand(nrrows);
        }
        
        /*RecVec() : _vec(), 
                   _nrcols(0),
                   _nrrows(0){
          std::cout << "RecVec constructed 0 args!!\n";
        }*/

        RecVec (const RecVec& copy) : _vec(copy._vec),
                                      _nrcols(copy._nrcols), 
                                      _nrrows(copy._nrrows) {
        };

        RecVec& operator= (RecVec const& copy) = delete;
        
        ~RecVec() {
        }
        
        u_int32_t get (size_t i, size_t j) {
          assert(i < _nrrows && j  < _nrcols);
          return _vec.at(i * _nrcols + j); 
        }
        
        void setNrCols (size_t nrcols) {
          assert(_nrcols == 0);
          _nrcols = nrcols;
        }

        void set (size_t i, size_t j, T val) {
          assert(i < _nrrows && j < _nrcols);
          _vec.at(i * _nrcols + j) = val; 
        }

        void expand (size_t nr = 1) {
          _nrrows += nr;
          for (size_t i = 0; i < _nrcols * nr; i++) {
            _vec.push_back(0);
          }
        }

        size_t size () {
          return _vec.size();
        }

      private:

        std::vector<T> _vec;
        size_t _nrcols;
        size_t _nrrows;
};

#endif
