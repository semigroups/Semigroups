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
          this->add_rows(nrrows);
        }
        
        RecVec (const RecVec& copy) 
          : _vec(copy._vec),
            _nrcols(copy._nrcols), 
            _nrrows(copy._nrrows) {
            
            }

        RecVec (const RecVec& copy, size_t add_cols) 
          : _vec(), _nrcols(add_cols + copy.nrcols()), _nrrows(copy.nrrows())
        {
          _vec.reserve((copy.nrcols() + add_cols) * copy.nrrows());
          for (size_t i = 0; i < copy.nrrows(); i++) {
            for (size_t j = 0; j < copy.nrcols(); j++) {
              _vec.push_back(copy.get(i, j));
            }
            for (size_t j = 0; j < add_cols; j++) {
              _vec.push_back(static_cast<T>(0));
            }
          }
        }
        
        ~RecVec() {}
       
        T inline get (size_t i, size_t j) const {
          assert(i < _nrrows && j  < _nrcols);
          return _vec.at(i * _nrcols + j); 
        }
         
        size_t size () const {
          return _vec.size();
        }

        size_t nrrows () const {
          return _nrrows;
        }
        
        size_t nrcols () const {
          return _nrcols;
        }
        
        void inline set (size_t i, size_t j, T val) {
          assert(i < _nrrows && j < _nrcols);
          _vec.at(i * _nrcols + j) = val; 
        }

        void inline add_rows (size_t nr = 1) { // add rows
          _nrrows += nr;
          for (size_t i = 0; i < _nrcols * nr; i++) {
            _vec.push_back(static_cast<T>(0));
          }
        }
        
        void add_cols (size_t nr) {
          RecVec<T> copy(*this);
          add_cols(copy, nr);
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
        
      private:

        std::vector<T> _vec;
        size_t _nrcols;
        size_t _nrrows;
};

#endif
