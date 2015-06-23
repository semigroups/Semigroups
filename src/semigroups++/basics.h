/*
 * Semigroups++
 *
 * This file some basic classes.
 *
 */

#ifndef SEMIGROUPS_BASICS_H
#define SEMIGROUPS_BASICS_H
//#define NDEBUG 

#include <assert.h>
#include <iostream>
#include <vector>

template <typename T>
class RecVec {
        
      public:

        RecVec () {
          //std::cout << "RecVec: default constructor!\n";
        };
        
        RecVec (size_t nrcols, size_t nrrows = 0) : _vec(), 
                                                    _nrcols(nrcols),
                                                    _nrrows(0){
          this->expand(nrrows);
        }
        
        RecVec (const RecVec& copy) 
          : _vec(copy._vec),
            _nrcols(copy._nrcols), 
            _nrrows(copy._nrrows) {
            //  std::cout << "RecVec: copy constructor!\n";
            
            }

        RecVec& operator= (RecVec const& copy) {
            //std::cout << "RecVec: assignment operator!\n";
            _vec = copy._vec;
            _nrcols = copy._nrcols;
            _nrrows = copy._nrrows;
            return *this;
        }
        
        ~RecVec() {}
        
        T inline get (size_t i, size_t j) const {
          assert(i < _nrrows && j  < _nrcols);
          return _vec.at(i * _nrcols + j); 
        }
         
        void inline set (size_t i, size_t j, T val) {
          assert(i < _nrrows && j < _nrcols);
          _vec.at(i * _nrcols + j) = val; 
        }

        void inline expand (size_t nr = 1) { // expand rows
          _nrrows += nr;
          for (size_t i = 0; i < _nrcols * nr; i++) {
            _vec.push_back(0);
          }
        }
          
        size_t size () {
          return _vec.size();
        }

        std::vector<T> vector () const {
          return _vec;
        }
        
        size_t nrrows () {
          return _nrrows;
        }
        
        size_t nrcols () {
          return _nrcols;
        }
        
        void add_cols (const RecVec<T>& copy, size_t nr) {
          assert(&copy != this);
          _vec.clear();
          _vec.reserve((_nrcols + nr) * _nrrows);
          for (size_t i = 0; i < _nrrows; i++) {
            for (size_t j = 0; j < _nrcols; j++) {
              _vec.push_back(copy.get(i, j));
            }
            for (size_t j = 0; j < nr; j++) {
              _vec.push_back(0);
            }
          }
          _nrcols += nr;
        }
          
        void add_cols (size_t nr) {
          RecVec<T> copy(*this);
          add_cols(copy, nr);
        }
        
      private:

        std::vector<T> _vec;
        size_t _nrcols;
        size_t _nrrows;
};

#endif
