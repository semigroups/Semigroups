/*
 * Semigroups++
 *
 * This file contains the Froidure-Pin algorithm for arbitrary semigroups. 
 *
 */

// TODO
// 2.5) looking etc. This probably won't work with GAP transformations, better
// use the other algorithm in that case.
// 3) the rest of the functionality, and interface!
// 4) other types of semigroups
// 5) bit flipping?
// 6) the other functionality of Semigroupe.

#ifndef SEMIGROUPS_H
#define SEMIGROUPS_H

//#define NDEBUG
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
      _schreiergen(), 
      _schreierpos(), 
      _suffix     (), 
      _wordlen    (0) // (length of the current word) - 1
    { 
      assert(_nrgens != 0);
      
      _lenindex.push_back(0);
      _id = static_cast<T*>(_gens.at(0)->identity());

      // init genslookup
      for (size_t i = 0; i < _nrgens; i++) {
        _genslookup.push_back(0);
      }

      // add the generators 
      for (size_t i = 0; i < _nrgens; i++) {
        T* x = _gens.at(i);
        auto it = _map.find(*x);
        if (it != _map.end()) { // duplicate generator
          _genslookup.at(i) = it->second;
          _nrrules++;
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
      _id->delete_data();
      delete _id;
    }
   
    size_t nrgens () {
      return _gens.size();
    }

    size_t size () {
      enumerate();
      return _elements.size();
    }
    
    std::vector<T*> elements () {
      enumerate();
      return _elements;
    }
    
    size_t nrrules () {
      return _nrrules;
    }
    
    RecVec<size_t> right_cayley_graph () {
      enumerate();
      return _right;
    }
    
    RecVec<size_t> left_cayley_graph () {
      enumerate();
      return _left;
    }
    
    size_t schreierpos (size_t pos) { 
      spanning_tree();
      return _schreierpos[pos];
    }
    
    size_t schreiergen (size_t pos) { 
      spanning_tree();
      return _schreiergen[pos];
    }
    
    std::vector<size_t> trace (size_t pos) { // trace the spanning tree
      // caching the words seems to be slower ....
      /*if (_words.empty()) {
        _words.reserve(_elements.size());
        for (size_t i = 0; i < _elements.size(); i++) {
          _words.push_back(std::vector<size_t>());
        }
        for (size_t i = 0; i < _genslookup.size(); i++) {
          if (_words.at(_genslookup.at(i)).empty()) {
            _words.at(_genslookup.at(i)).push_back(i);
          }
        }
      }

      std::vector<size_t> word2;
      while (_words.at(pos).empty()) {
        word2.push_back(this->schreiergen(pos));
        pos = this->schreierpos(pos);
      }
      //word.push_back(_genslookup.at(pos));
      std::reverse(word2.begin(), word2.end());
      std::vector<size_t> word1(_words.at(pos));
      word1.insert(word1.end(), word2.begin(), word2.end());
      return word1;*/

      std::vector<size_t> word;
      while (pos > _genslookup.back()) {
        word.push_back(this->schreiergen(pos));
        pos = this->schreierpos(pos);
      }
      word.push_back(_genslookup.at(pos));
      std::reverse(word.begin(), word.end());
      return word;
    }

    std::vector<std::pair<std::vector<size_t>, std::vector<size_t> > > relations () {
      std::vector<std::pair<std::vector<size_t>, std::vector<size_t> > > relations;
      std::cout << "_nrrules = " << _nrrules << "\n";
      size_t nr = _nrrules;

      for (size_t i = 1; i < _gens.size(); i++) {
        if (_genslookup.at(i) <= _genslookup.at(i - 1)) {
          nr--;
          std::vector<size_t> lhs(i);
          std::vector<size_t> rhs(_genslookup.at(i));
          relations.push_back(std::make_pair(lhs, rhs));
        }
      }

      size_t i;
      for (i = 0; i <= _genslookup.back(); i++) {
        for (size_t j = 0; j < _reduced.nrcols(); j++) {
          if (!_reduced.get(i, j)) {
            nr--;
            relations.push_back(make_relation(i, j));
          }
        }
      }

      for (; i < _reduced.nrrows(); i++) {
        for (size_t j = 0; j < _reduced.nrcols(); j++) {
          if (_reduced.get(_suffix.at(i), j) && !_reduced.get(i, j)) {
            nr--;
            relations.push_back(make_relation(i, j));
          }
        }
      }
      assert(nr == 0);
      return relations;
    }
    
    //TODO reintroduce limit
    void enumerate () {
      if(_pos >= _nr) return;
      std::cout << "enumerating!\n";
      
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
              } else if (r > _genslookup.back()) {
                _right.set(_pos, j, _right.get(_left.get(_prefix.at(r), b),
                                               _final.at(r)));
              } else { // TODO it would be nice to get rid of this case somehow
                _right.set(_pos, j, _right.get(_genslookup.at(b), _final.at(r)));
              } 
            } else {
              x.redefine(_elements.at(_pos), _gens.at(j)); 
              auto it = _map.find(x); 

              if (it != _map.end()) {
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
      x.delete_data();
      //if (_pos > _nr) {
      // free stuff
      //}
    }
    
    // 
    // 
    //
      
  private:
    
    std::pair<std::vector<size_t>,std::vector<size_t> > make_relation (size_t i, size_t j) {
      std::vector<size_t> lhs(this->trace(i));
      lhs.push_back(j);
      std::vector<size_t> rhs(this->trace(_right.get(i, j)));
      return std::make_pair(lhs, rhs);
    }

    void inline is_one (T& x) {
      if (!_found_one && x == (*_id)) {
        _pos_one = _nr;
        _found_one = true;
      }
    }
    
    void inline expand () {
      _lenindex.push_back(_nr); // words of length _wordlen + 1 start at position _nr
      _left.expand(_nr - _pos);
      _reduced.expand(_nr - _pos);
      _right.expand(_nr - _pos);
    }

    void spanning_tree () {
      if (_schreierpos.size() == 0) { 
        // init _schreierpos/gen
        _schreierpos.reserve(this->size());
        _schreiergen.reserve(this->size());
        
        for (size_t i = 0; i < this->size(); i++) {
          _schreierpos.push_back(0);
          _schreiergen.push_back(0);
        }

        // find places in _reduced that are true
        for (size_t i = 0; i < this->size(); i++) {
          for (size_t j = 0; j < this->nrgens(); j++) {
            if (_reduced.get(i, j)) {
              size_t r = _right.get(i, j);
              _schreierpos.at(r) = i;
              _schreiergen.at(r) = j;
            }
          }
        }
      }
    }

    // TODO add stopper, found, lookfunc
    size_t                               _degree;
    std::vector<T*>                      _elements;
    std::vector<size_t>                  _final;
    std::vector<size_t>                  _first;
    bool                                 _found_one;
    std::vector<T*>                      _gens;
    std::vector<size_t>                  _genslookup;  
    T*                                   _id; 
    RecVec<size_t>                       _left;
    std::vector<size_t>                  _lenindex;
    std::unordered_map<const T, size_t>  _map;         
    size_t                               _nr;
    size_t                               _nrgens;
    size_t                               _nrrules;
    size_t                               _pos;
    size_t                               _pos_one;
    std::vector<size_t>                  _prefix;
    RecVec<bool>                         _reduced;
    RecVec<size_t>                       _right;
    std::vector<size_t>                  _schreiergen;
    std::vector<size_t>                  _schreierpos;
    std::vector<size_t>                  _suffix;
    size_t                               _wordlen;
    std::vector<std::vector<size_t> >    _words;
};
 
#endif
