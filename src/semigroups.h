/*
 * Semigroups++
 *
 * This file contains the Froidure-Pin algorithm for arbitrary semigroups. 
 *
 */

// TODO
// 1) a bit of rearranging to move the expands() out of the inner loop to the
// end of the words of length n - 1 have been considered
// 1.5) maybe process the generators in the constructor to avoid the special
// cases in enumerate()
// 2) a proper hash function for transformations
// 2.5) looking etc. This probably won't work with GAP transformations, better
// use the other algorithm in that case.
// 3) the rest of the functionality, and interface!
// 4) other types of semigroups
// 5) a specialist version for small transformations/elements (using bit
// flipping)
// 6) the other functionality of Semigroupe.

#ifndef SEMIGROUPS_H
#define SEMIGROUPS_H

#define NDEBUG
//#define DEBUG

#include "basics.h"
#include "elements.h"

#include <unordered_map>
#include <vector>
#include <assert.h>
#include <iostream>

template <typename T>
class Semigroup {
  
  public:
    Semigroup (std::vector<T*> gens, size_t degree): 
      _degree     (degree),
      _elements   (),
      _final      (),
      _first      (),    
      _found_one  (false),
      _gens       (gens), 
      _genslookup (),
      _left       (RecVec<size_t>(gens.size())),
      _lenindex   (), 
      _map        (), 
      _nr         (0), 
      _nrgens     (gens.size()),
      _nrrules    (0), 
      _pos        (0), 
      _pos_one    (0), 
      _prefix     (), 
      _reduced    (RecVec<bool>(gens.size())),
      _right      (RecVec<size_t>(gens.size())),
      //_rules      (RecVec<size_t>(3)), 
      _schreiergen(), 
      _schreierpos(), 
      _suffix     (), 
      _wordlen    (0) // (length of the current word) - 1
    { 
      assert(_nrgens != 0);
      _lenindex.push_back(0);

      // init genslookup FIXME better way?
      for (size_t i = 0; i < _nrgens; i++) {
        _genslookup.push_back(0);
      }

      //_id = static_cast<T*>(_gens.at(0)->identity());
      // add the generators 
      for (size_t i = 0; i < _nrgens; i++) {
        T* x = _gens.at(i);
        auto it = _map.find(*x);
        if (it != _map.end()) { // duplicate generator
          _genslookup.at(i) = it->second;
          _nrrules++;
          //newrule(_undefined, i, it->second);
          //TODO don't forget these rules!
        } else {
          is_one(*x);
          _elements.push_back(static_cast<T*>(x->copy()));
          _first.push_back(i);
          _final.push_back(i);
          _genslookup.at(i) = _nr;
          _map.insert(std::make_pair(*_elements.back(), _nr));
          _prefix.push_back(_nr);
          _suffix.push_back(_nr);
          _nr++;
        }
      }
      expand();
    }

    Semigroup (const Semigroup& copy) = delete;
    Semigroup& operator= (Semigroup const& copy) = delete;
   
    ~Semigroup () {
      for (T* x: _elements) {
        x->delete_data();
        delete x;
      }
    }

    size_t size () {
      enumerate();
      return _elements.size();
    }

    //TODO reintroduce limit
    void enumerate () {
      if(_pos >= _nr) return;
      
      T x(_degree);

      //multiply the generators by every generator
      while (_pos < _lenindex.at(1)) { 
        for (size_t j = 0; j < _nrgens; j++) {
          x.redefine(_elements.at(_pos), _gens.at(j)); 
          auto it = _map.find(x); 

          if (it != _map.end()) {
            _right.set(_pos, j, it->second);
            _nrrules++;
          } else {
            is_one(x);
            _elements.push_back(static_cast<T*>(x.copy()));
            _first.push_back(_first.at(_pos));
            _final.push_back(j);
            _map.insert(std::make_pair(*_elements.back(), _nr));
            _prefix.push_back(_pos);
            _reduced.set(_pos, j, true);
            _right.set(_pos, j, _nr);
            _suffix.push_back(_genslookup.at(j));
            _nr++;
          }
        }
        _pos++;
      }
      for (size_t i = 0; i < _pos; i++) { 
        size_t b = _final.at(i); 
        for (size_t j = 0; j < _nrgens; j++) { 
          _left.set(i, j, _right.get(_genslookup.at(j), b));
        }
      }
      _wordlen++;
      expand();

      //multiply the words of length > 1 by every generator
      bool stop = false;

      while (_pos < _nr && !stop) {
        while (_pos < _lenindex.at(_wordlen + 1) && !stop) {
          size_t b = _first.at(_pos);
          size_t s = _suffix.at(_pos); 
          for (size_t j = 0; j < _nrgens; j++) {
            if (!_reduced.get(s, j)) {
              size_t r = _right.get(s, j);
              if (_found_one && r == _pos_one) {
                _right.set(_pos, j, _genslookup.at(b));
              } else {
                _right.set(_pos, j, _right.get(_left.get(_prefix.at(r), b),
                                               _final.at(r)));
              } 
            } else {
              x.redefine(_elements.at(_pos), _gens.at(j)); 
              auto it = _map.find(x); 

              if (it != _map.end()) {
               //newrule(_pos, j, it->second);
                _right.set(_pos, j, it->second);
                _nrrules++;
              } else {
                is_one(x);
                _elements.push_back(static_cast<T*>(x.copy()));
                _first.push_back(b);
                _final.push_back(j);
                _map.insert(std::make_pair(*_elements.back(), _nr));
                _prefix.push_back(_pos);
                _reduced.set(_pos, j, true);
                _right.set(_pos, j, _nr);
                _suffix.push_back(_right.get(s, j));
                _nr++;
                //stop = (_nr >= limit);
              }
            }
          } // finished applying gens to <_elements.at(_pos)>
          _pos++;
        } // finished words of length <wordlen> + 1
        if (_pos > _nr || _pos == _lenindex.at(_wordlen + 1)) {
          for (size_t i = _lenindex.at(_wordlen); i < _pos; i++) { 
            size_t p = _prefix.at(i);
            size_t b = _final.at(i); 
            for (size_t j = 0; j < _nrgens; j++) { 
              _left.set(i, j, _right.get(_left.get(p, j), b));
            }
          }
          _wordlen++;
          expand();
        }
      }
      //if (_pos > _nr) {
      // free stuff
      //}
    }
    
    size_t nrrules () {
      return _nrrules;
    }
    
    // spanning_tree () { // should look for places in reduced which are true
    // this gives a spanning tree
    //
    // trace (pos) { trace the spanning tree
    //
    // relations () { // have to check for places where neither _pos nor the
    // suffix of _pos is reduced, then trace the spanning tree
    //
    // left_cayley_graph () {
    // }
    //
    // right_cayley_graph () {
    // }
    // 
    // 
    //
      
  private:

    /*void inline newrule (size_t word1, size_t gen, size_t word2) {
      _rules.expand();
      _rules.set(_nrrules, 0, word1);
      _rules.set(_nrrules, 1, gen);
      _rules.set(_nrrules, 2, word2);
      _nrrules++;
    }*/

    void inline is_one (T& x) {
      /*
      if (!_found_one && x == (*_id)) {
        _pos_one = _nr;
        _found_one = true;
      }*/
    }
    
    void inline expand () {
      _lenindex.push_back(_nr); // words of length _wordlen + 1 start at position _nr
      _left.expand(_nr - _pos);
      _reduced.expand(_nr - _pos);
      _right.expand(_nr - _pos);
    }

    // TODO add stopper, found, lookfunc
    size_t                             _degree;
    std::vector<T*>                    _elements;
    std::vector<size_t>                _final;
    std::vector<size_t>                _first;
    bool                               _found_one;
    std::vector<T*>                    _gens;
    std::vector<size_t>                _genslookup;  
    T*                                 _id; 
    RecVec<size_t>                     _left;
    std::vector<size_t>                _lenindex;
    std::unordered_map<const T, size_t>      _map;         
    size_t                             _nr;
    size_t                             _nrgens;
    size_t                             _nrrules;
    size_t                             _pos;
    size_t                             _pos_one;
    std::vector<size_t>                _prefix;
    RecVec<bool>                       _reduced;
    RecVec<size_t>                     _right;
//  RecVec<size_t>                     _rules; // (word1 index, gen, word2 index) 
                                               // word1 * gen = word2
    std::vector<size_t>                _schreiergen;
    std::vector<size_t>                _schreierpos;
    std::vector<size_t>                _suffix;
    size_t                             _wordlen;
};
 
#endif
