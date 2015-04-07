/*
 * Semigroups++
 *
 * This file contains the Froidure-Pin algorithm for arbitrary semigroups. 
 *
 */

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
      _rules      (RecVec<size_t>(3)), 
      _schreiergen(), 
      _schreierpos(), 
      _suffix     (), 
      _undefined  (gens.size() + 1), 
      _wordlen    (0) // (length of the current word) - 1
    { 
      assert(_nrgens != 0);

      _lenindex.push_back(0);

      // init genslookup FIXME better way?
      for (size_t i = 0; i < _nrgens; i++) {
        _genslookup.push_back(0);
      }

      _id = static_cast<T*>(_gens.at(0)->identity());
      // add the generators 
      for (size_t i = 0; i < _nrgens; i++) {
        T* x = _gens.at(i);
        auto it = _map.find(x->hash_value());
        if (it != _map.end()) { // duplicate generator
          _genslookup.at(i) = it->second;
          newrule(_undefined, i, it->second);
        } else {
          // TODO make this an "new_element" method?
          if (!_found_one && x == _id) {
            _pos_one = _nr;
            _found_one = true;
          }
          _genslookup.at(i) = _nr;
          _map.insert(std::make_pair(x->hash_value(), _nr));
          _elements.push_back(new T(*x));
          _schreiergen.push_back(i);
          _schreierpos.push_back(0); // this has special meaning here!!
          _first.push_back(i);
          _final.push_back(i);
          _prefix.push_back(i);
          _suffix.push_back(i);
          _left.expand();
          _right.expand();
          _reduced.expand();
          _nr++;
        }
      }
      _lenindex.push_back(_nr); // words of length 2 start at position _nr
    }

    Semigroup (const Semigroup& copy) = delete;
    Semigroup& operator= (Semigroup const& copy) = delete;
   
    ~Semigroup () {
      for (T* x: _elements) {
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

      bool stop = false;
      T tmp(_degree);

      while (_pos < _nr && !stop) {
        while (_pos < _lenindex.at(_wordlen + 1) && !stop) {
          size_t b = _first.at(_pos);
          size_t s = _suffix.at(_pos); 
          for (size_t j = 0; j < _nrgens; j++) {
            if (s != b && !_reduced.get(s, j)) {
              size_t r = _right.get(s, j);
              _reduced.set(_pos, j, false);
              if (_prefix.at(r) != 0){
                _right.set(_pos, j, _right.get(_left.get(_prefix.at(r), b), _final.at(r)));
              } else if (_found_one && r == _pos_one) {
                _right.set(_pos, j, _genslookup.at(b));
              } else {
                _right.set(_pos, j, _right.get(_genslookup.at(b), _final.at(r)));
              }
            } else {
              tmp.redefine(_elements.at(_pos), _gens.at(j)); 
              auto it = _map.find(tmp.hash_value()); 

              if (it != _map.end()) {
                newrule(_pos, j, it->second);
                _right.set(_pos, j, it->second);
              } else {
                if (!_found_one && tmp == _id) {
                  _pos_one = _nr;
                  _found_one = true;
                }
                T* x = new T(tmp);
                _map.insert(std::make_pair(x->hash_value(), _nr));
                _elements.push_back(x);
                _schreiergen.push_back(j);
                _schreierpos.push_back(_pos); 
                _first.push_back(b);
                _final.push_back(j);
                _prefix.push_back(_pos);
                if (s != b) {
                  _suffix.push_back(_right.get(s, j));
                } else {
                  _suffix.push_back(_genslookup.at(j));
                }
                _reduced.set(_pos, j, true);
                _right.set(_pos, j, _nr);

                _left.expand();
                _right.expand();
                _reduced.expand();
                _nr++;
                //stop = (_nr >= limit);
              }
            }
          } // finished applying gens to <_elements.at(_pos)>
          _pos++;
        } // finished words of length <wordlen>
        if (_pos > _nr || _pos == _lenindex.at(_wordlen + 1)) {
          if (_wordlen > 0) {
            for (size_t i = _lenindex.at(_wordlen); i < _pos; i++) { 
              size_t p = _prefix.at(i);
              size_t b = _final.at(i); 
              for (size_t j = 0; j < _nrgens; j++) { 
                _left.set(i, j, _right.get(_left.get(p, j), b));
              }
            }
          } else if (_wordlen == 0) { // FIXME can't be anything other than 1
            for (size_t i = _lenindex.at(_wordlen); i < _pos; i++) { 
              size_t b = _final.at(i); 
              for (size_t j = 0; j < _nrgens; j++) { 
                _left.set(i, j, _right.get(_genslookup.at(j), b));
              }
            }
          }
          _lenindex.push_back(_nr);
          _wordlen++;
        }
      }
      //if (i > nr) {
      // free stuff
      //}
    }
      
  private:

    void newrule (size_t word1, size_t gen, size_t word2) {
      _rules.expand();
      _rules.set(_nrrules, 0, word1);
      _rules.set(_nrrules, 1, gen);
      _rules.set(_nrrules, 2, word2);
      _nrrules++;
    }

    // TODO add stopper, found, lookfunc
    size_t                             _degree;
    std::vector<T*>                    _elements;
    std::vector<size_t>                _final;
    std::vector<size_t>                _first;
    bool                               _found_one;
    std::vector<T*>                    _gens;
    std::vector<size_t>                _genslookup;  //TODO tuple?
    T*                                 _id; 
    RecVec<size_t>                     _left;
    std::vector<size_t>                _lenindex;
    std::unordered_map<size_t, size_t> _map;         // TODO should T = u_intmax here!?
    size_t                             _nr;
    size_t                             _nrgens;
    size_t                             _nrrules;
    size_t                             _pos;
    size_t                             _pos_one;
    std::vector<size_t>                _prefix;
    RecVec<bool>                       _reduced;
    RecVec<size_t>                     _right;
    RecVec<size_t>                     _rules; // (word1 index, gen, word2 index) 
                                               // word1 * gen = word2
    std::vector<size_t>                _schreiergen;
    std::vector<size_t>                _schreierpos;
    std::vector<size_t>                _suffix;
    size_t                             _undefined;   // for rules with no existing word
    size_t                             _wordlen;
};

  
#endif
