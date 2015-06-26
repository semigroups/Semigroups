/*
 * Semigroups++
 *
 * This file contains the Froidure-Pin algorithm for arbitrary semigroups. 
 *
 */

// TODO
//
// 1) bit flipping for reduced?
// 2) remove RecVecs
// 6) the other functionality of Semigroupe.
// 7) rename degree to element_size or something
// 8) make sure relation_pos/gen are set everywhere

#ifndef SEMIGROUPS_H
#define SEMIGROUPS_H

//#define NDEBUG
//#define DEBUG

#ifndef BATCH_SIZE
#define BATCH_SIZE 8192
#endif

#include "basics.h"
#include "elements.h"

#include <algorithm>
#include <unordered_map>
#include <vector>
#include <assert.h>
#include <iostream>

class SemigroupBase { };

template <typename T>
class Semigroup : public SemigroupBase {
  
  typedef std::vector<size_t> Word;
  typedef std::pair<Word*, Word*> Relation;

  public:
    
    Semigroup& operator= (Semigroup const& copy) = delete;

    // constructors
    Semigroup (std::vector<T*> gens, size_t degree) : 
      _degree        (degree),
      _duplicate_gens(),
      _elements      (new std::vector<T*>()),
      _final         (),
      _first         (),    
      _found_one     (false),
      _gens          (gens), 
      _genslookup    (),
      _index         (),
      _left          (new RecVec<size_t>(gens.size())),
      _lenindex      (), 
      _map           (), 
      _nr            (0), 
      _nrgens        (gens.size()),
      _nrrules       (0), 
      _pos           (0), 
      _pos_one       (0), 
      _prefix        (), 
      _reduced       (RecVec<bool>(gens.size())),
      _relation_pos  (-1),
      _relation_gen  (0),
      _right         (new RecVec<size_t>(gens.size())),
      _suffix        (), 
      _wordlen       (0) // (length of the current word) - 1
    { 
      assert(_nrgens != 0);
      
      _lenindex.push_back(0);
      _id = static_cast<T*>(_gens.at(0)->identity());

      // add the generators 
      for (size_t i = 0; i < _nrgens; i++) {
        T* x = _gens.at(i);
        auto it = _map.find(*x);
        if (it != _map.end()) { // duplicate generator
          _genslookup.push_back(it->second);
          _nrrules++;
          _duplicate_gens.push_back(std::make_pair(i, it->second));
        } else {
          is_one(*x, _nr);
          _elements->push_back(static_cast<T*>(x->copy()));
          _first.push_back(i);
          _final.push_back(i);
          _genslookup.push_back(_nr);
          _map.insert(std::make_pair(*_elements->back(), _nr));
          _prefix.push_back(-1);
          _suffix.push_back(-1);
          _index.push_back(_nr);
          _nr++;
        }
      }
      expand(_nr);
      _lenindex.push_back(_index.size()); 
    }

    // TODO split into 2 methods! 
    Semigroup (const Semigroup& copy, const std::vector<T*>& coll = std::vector<T*>())
      // initialize as if <coll> is not empty . . .
      : _degree(copy._degree), // copy for comparison in add_generators
        _duplicate_gens(copy._duplicate_gens), 
        _elements(new std::vector<T*>()),
        _final(copy._final), // copy for assignment to specific positions in add_generators
        _first(copy._first), // copy for assignment to specific positions in add_generators
        _found_one(copy._found_one), // copy in case degree doesn't change in add_generators
        _genslookup(copy._genslookup),
        _index(),
        _lenindex(),
        _map(),
        _nr(copy._nr),
        _nrgens(copy._nrgens),
        _nrrules(0),
        _pos(0),
        _pos_one(copy._pos_one), // copy in case degree doesn't change in add_generators
        _prefix(copy._prefix),   // copy for assignment to specific positions in add_generators
        _relation_pos(-1),
        _relation_gen(0),
        _suffix(copy._suffix),   // copy for assignment to specific positions in add_generators
        _wordlen(0) 
    {
      _elements->reserve(copy._nr);
      _map.reserve(copy._nr);
      
      std::unordered_set<T*> new_gens;

      // check which of <coll> belong to <copy>
      for (T* x: coll) {
        if (copy._map.find(*x) == copy._map.end()) { 
          new_gens.insert(x);
        }
      }

      assert(new_gens.empty() || (*new_gens.begin())->degree() >= copy.degree());
      size_t deg_plus = (new_gens.empty() ? 0 : (*new_gens.begin())->degree() - copy.degree());

      if (new_gens.empty()) {// straight copy
        _found_one = copy._found_one;
        _index = copy._index;
        _lenindex = copy._lenindex;
        _nrrules = copy._nrrules;
        _left = new RecVec<size_t>(*copy._left);
        _pos = copy._pos;
        _pos_one = copy._pos_one;
        _reduced = copy._reduced;
        _relation_gen = copy._relation_gen;
        _relation_pos = copy._relation_pos;
        _right = new RecVec<size_t>(*copy._right);
        _wordlen = copy._wordlen;
      } else {
        if (deg_plus != 0) { 
          _degree = copy.degree() + deg_plus;
          _found_one = false;
          _pos_one = 0;
        } 
        _lenindex.push_back(0);
        _index.reserve(copy._nr);
        _nrrules = copy._duplicate_gens.size();
        _left = new RecVec<size_t>(*copy._left, new_gens.size());
        _right = new RecVec<size_t>(*copy._right, new_gens.size());
        _reduced = RecVec<bool>(_nrgens + coll.size(), _nr);
        
        // add the distinct old generators to new _index
        for (size_t i = 0; i < copy._lenindex.at(1); i++) {
          _index.push_back(copy._index.at(i));
        }
      }

      for (size_t i = 0; i < copy.nrgens(); i++) {
        _gens.push_back(static_cast<T*>(copy._gens.at(i)->copy(deg_plus)));
      }
      
      _id = static_cast<T*>(copy._id->copy(deg_plus));
      
      for (size_t i = 0; i < copy._elements->size(); i++) {
        _elements->push_back(static_cast<T*>(copy._elements->at(i)->T::copy(deg_plus)));
        is_one(*_elements->back(), i);
        _map.insert(std::make_pair(*_elements->back(), i));
      }
      
      add_generators(new_gens, false);
    }

    // destructor
    ~Semigroup () {
      // FIXME duplicate generators are not deleted?
      delete _left;
      delete _right;
      for (T* x: *_elements) {
        x->delete_data();
        delete x;
      }
      delete _elements;
      _id->delete_data();
      delete _id;
    }

    // short methods
    size_t max_word_length () {
      if (is_done()) {
        return _lenindex.size() - 2;
      } else if (_nr > _lenindex.back()) { 
        return _lenindex.size();
      } else {
        return _lenindex.size() - 1;
      }
    }
    
    size_t degree () const {
      return _degree;
    }
   
    size_t nrgens () const {
      return _gens.size();
    }
    
    std::vector<T*> gens () {
      return _gens;
    }
    
    bool is_done () {
      return (_pos >= _nr);
    }
    
    bool is_begun () {
      assert(_lenindex.size() > 1);
      return (_pos >= _lenindex.at(1));
    }

    size_t current_size () {
      return _elements->size();
    }
    
    size_t size (bool report) {
      enumerate(-1, report);
      return _elements->size();
    }
   
    size_t test_membership (T* x) {
      return (position(x) != (size_t) -1);
    }

    size_t position (T* x) {
      if (x->degree() != _degree) {
        return -1;
      }

      while (true) { 
        auto it = _map.find(*x);
        if (it != _map.end()) {
          return it->second;
        }
        if (is_done()) {
          return -1;
        }
        enumerate(_nr + 1); 
        // the _nr means we enumerate BATCH_SIZE more elements
      }
    }

    std::vector<T*>* elements (size_t limit) {
      enumerate(limit);
      return _elements;
    }
    
    size_t nrrules () {
      return _nrrules;
    }
    
    RecVec<size_t>* right_cayley_graph () {
      enumerate(-1);
      return _right;
    }
    
    RecVec<size_t>* left_cayley_graph () {
      enumerate(-1);
      return _left;
    }

    size_t prefix (size_t element_nr) {
      return _prefix.at(element_nr);
    }
    
    size_t suffix (size_t element_nr) {
      return _suffix.at(element_nr);
    }
    
    size_t first_letter (size_t element_nr) {
      return _first.at(element_nr);
    }

    size_t final_letter (size_t element_nr) {
      return _final.at(element_nr);
    }
    
    Word* factorisation (size_t pos) { 
      // factorisation of _elements.at(pos)

      Word* word = new Word();
      if (pos > _nr && !is_done()) {
        enumerate(pos);
      }
      
      if (pos < _nr) {
        while (pos != (size_t) -1) {
          word->push_back(_first.at(pos));
          pos = _suffix.at(pos);
        }
      }
      return word;
    }
    
    void reset_next_relation () {
      _relation_pos = -1;
      _relation_gen = 0;
    }

    // Modifies <relation> in place so that 
    //
    // _elements(relation.at(0)) * _gens(relation.at(1) =
    // _elements(relation.at(2))
    //
    // <relation> is empty if there are no more relations, and it has length 2
    // in the special case of duplicate generators.
    
    void next_relation (std::vector<size_t>& relation) {
      if (!is_done()) {
        enumerate(-1);
      }
      
      relation.clear();
      
      if (_relation_pos == _nr) { //no more relations
        return;
      }
    
      if (_relation_pos != (size_t) -1) {
        while (_relation_pos < _nr) {
          while (_relation_gen < _nrgens) {
            if (!_reduced.get(_index.at(_relation_pos), _relation_gen) 
                && (_relation_pos < _lenindex.at(1) || 
                    _reduced.get(_suffix.at(_index.at(_relation_pos)),
                                            _relation_gen))) {
              relation.push_back(_index.at(_relation_pos));
              relation.push_back(_relation_gen);
              relation.push_back(_right->get(_index.at(_relation_pos), _relation_gen));
              break;
            }
            _relation_gen++;
          }
          if (relation.empty()) {
            _relation_gen = 0;
            _relation_pos++;
          } else {
            break;
          }
        }
        if (_relation_gen == _nrgens) {
          _relation_gen = 0;
          _relation_pos++;
        } else {
          _relation_gen++;
        }
      } else {
        //duplicate generators
        if (_relation_gen < _duplicate_gens.size()) {
          relation.push_back(_duplicate_gens.at(_relation_gen).first);
          relation.push_back(_duplicate_gens.at(_relation_gen).second);
          _relation_gen++;
        } else {
          _relation_gen = 0;
          _relation_pos++;
          next_relation(relation);
        }
      }
    }
   
    void enumerate (size_t limit) {
      enumerate(limit, false);
    }

    void enumerate (size_t limit, bool report) {
      if (_pos >= _nr || limit <= _nr) return;
      limit = std::max(limit, _nr + BATCH_SIZE);
      
      std::cout << "C++ version\n";
      std::cout << "limit = " << limit << "\n";
      
      T x(_degree, _gens.at(0)); 
      // pass in sample object to, for example, pass on the semiring for
      // MatrixOverSemiring

      //multiply the generators by every generator
      if (_pos < _lenindex.at(1)) {
        size_t nr_shorter_elements = _nr;
        while (_pos < _lenindex.at(1)) { 
          size_t i = _index.at(_pos);
          for (size_t j = 0; j < _nrgens; j++) {
            x.redefine(_elements->at(i), _gens.at(j)); 
            auto it = _map.find(x); 

            if (it != _map.end()) {
              _right->set(i, j, it->second);
              _nrrules++;
            } else {
              is_one(x, _nr);
              _elements->push_back(static_cast<T*>(x.copy()));
              _first.push_back(_first.at(i));
              _final.push_back(j);
              _index.push_back(_nr);
              _map.insert(std::make_pair(*_elements->back(), _nr));
              _prefix.push_back(i);
              _reduced.set(i, j, true);
              _right->set(i, j, _nr);
              _suffix.push_back(_genslookup.at(j));
              _nr++;
            }
          }
          _pos++;
        }
        for (size_t i = 0; i < _pos; i++) { 
          size_t b = _final.at(_index.at(i)); 
          for (size_t j = 0; j < _nrgens; j++) { 
            _left->set(_index.at(i), j, _right->get(_genslookup.at(j), b));
          }
        }
        _wordlen++;
        expand(_nr - nr_shorter_elements);
        _lenindex.push_back(_index.size()); 
      }

      //multiply the words of length > 1 by every generator
      bool stop = (_nr >= limit);

      while (_pos < _nr && !stop) {
        size_t nr_shorter_elements = _nr;
        while (_pos < _lenindex.at(_wordlen + 1) && !stop) {
          size_t i = _index.at(_pos);
          size_t b = _first.at(i);
          size_t s = _suffix.at(i);
          for (size_t j = 0; j < _nrgens; j++) {
            if (!_reduced.get(s, j)) {
              size_t r = _right->get(s, j);
              if (_found_one && r == _pos_one) {
                _right->set(i, j, _genslookup.at(b));
              } else if (_prefix.at(r) != (size_t) -1) { // r is not a generator
                _right->set(i, j, _right->get(_left->get(_prefix.at(r), b),
                                              _final.at(r)));
              } else { 
                _right->set(i, j, _right->get(_genslookup.at(b), _final.at(r)));
              } 
            } else {
              x.redefine(_elements->at(i), _gens.at(j)); 
              auto it = _map.find(x); 

              if (it != _map.end()) {
                _right->set(i, j, it->second);
                _nrrules++;
              } else {
                is_one(x, _nr);
                _elements->push_back(static_cast<T*>(x.copy()));
                _first.push_back(b);
                _final.push_back(j);
                _map.insert(std::make_pair(*_elements->back(), _nr));
                _prefix.push_back(i);
                _reduced.set(i, j, true);
                _right->set(i, j, _nr);
                _suffix.push_back(_right->get(s, j));
                _index.push_back(_nr);
                _nr++;
                stop = (_nr >= limit);
              }
            }
          } // finished applying gens to <_elements->at(_pos)>
          _pos++;
        } // finished words of length <wordlen> + 1
        expand(_nr - nr_shorter_elements);

        if (_pos > _nr || _pos == _lenindex.at(_wordlen + 1)) {
          //TODO move this (except last 2 lines) out 
          for (size_t i = _lenindex.at(_wordlen); i < _pos; i++) { 
            size_t p = _prefix.at(_index.at(i));
            size_t b = _final.at(_index.at(i)); 
            for (size_t j = 0; j < _nrgens; j++) { 
              _left->set(_index.at(i), j, _right->get(_left->get(p, j), b));
            }
          }
          _wordlen++;
          _lenindex.push_back(_index.size()); 
        }
        if (report) {
          std::cout << "found " << _nr << " elements, ";
          std::cout << _nrrules << " rules, ";
          std::cout << "max word length " << _wordlen + 1 << ", so far" << std::endl;
        }
      }
      x.delete_data();
    }
    
    // add generators to <this>, use whatever information is already known.
    
    void add_generators (const std::unordered_set <T*>&  coll, 
                         bool                            report) {
      if (report) {
        std::cout << "semigroups++: add_generators";
      }

      if (coll.empty()) {
        return;
      }

      assert(degree() == (*coll.begin())->degree());

      size_t old_nrgens  = _nrgens; 
      size_t old_nr      = _nr;
      size_t nr_old_left = _nr;
      
      std::vector<bool> old_new; // have we seen _elements->at(i) yet in new?
      old_new.reserve(old_nr);
      for (size_t i = 0; i < old_nr; i++) {
        old_new.push_back(false);
      }
      for (size_t i = 0; i < _genslookup.size(); i++) {
        old_new.at(_genslookup.at(i)) = true;
        nr_old_left--;
      }

      // add the new generators to new _gens, elements, and _index
      for (T* x: coll) {
        if (_map.find(*x) == _map.end()) {
          _first.push_back(_gens.size());
          _final.push_back(_gens.size());

          _gens.push_back(x);
          _elements->push_back(x);
          _genslookup.push_back(_nr);
          _index.push_back(_nr);

          is_one(*x, _nr);
          _map.insert(std::make_pair(*x, _nr));
          _prefix.push_back(-1);
          _suffix.push_back(-1);
          _nr++;
        }
      }
      _nrgens = _gens.size();

      // make room for new generators if necessary
      if (_reduced.nrcols() != _nrgens) {
        _reduced = RecVec<bool>(_nrgens, _nr);
      }
      if (_right->nrcols() != _nrgens) {
        // tmp_right = RecVec<size_t>(_right, nrcols)
        // delete _right
        _right->add_cols(coll.size());
      }
      if (_left->nrcols() != _nrgens) {
        _left->add_cols(coll.size());
      }
      
      // expand left/right/reduced by the number of newly added elements
      expand(_nrgens - old_nrgens);
      _lenindex.clear();
      _lenindex.push_back(0);
      _lenindex.push_back(_index.size()); 
      _wordlen = 0;

      size_t nr_shorter_elements;
      T x(_degree, _gens.at(0)); 
      // pass in sample object to, for example, pass on the semiring for
      // MatrixOverSemiring

      // Multiply all elements by all generators (old and new) until we have
      // all of the elements of <old> in our new data structure. 
      while (nr_old_left > 0) {
        nr_shorter_elements = _nr;
        while (_pos < _lenindex.at(_wordlen + 1) && nr_old_left > 0) {
          size_t i = _index.at(_pos); // position in _elements
          size_t b = _first.at(i);
          size_t s = _suffix.at(i); 
          if (i < old_nr) { // FIXME this is wrong should be i < old_pos
            // _elements.at(i) is in old semigroup
            for (size_t j = 0; j < old_nrgens; j++) {
              size_t k = _right->get(i, j);
              if (!old_new.at(k)) { // it's new!
                is_one(*_elements->at(k), k);
                _first.at(k) = _first.at(i);
                _final.at(k) = j;
                _prefix.at(k) = i;
                _reduced.set(i, j, true);
                if (_wordlen == 0) {
                  _suffix.at(k) = _genslookup.at(j);
                } else {
                  _suffix.at(k) = _right->get(s, j);
                }
                _index.push_back(k);
                old_new.at(k) = true;
                nr_old_left--;
              } else if (s == (size_t) -1 || _reduced.get(s, j)) {
                // TODO remove this clause or make it available in DEBUG mode
                // only
                _nrrules++;
              }
            }
            for (size_t j = old_nrgens; j < _nrgens; j++) {
              closure_update(i, j, b, s, x, old_new, nr_old_left, old_nr);
            }
            
          } else {
            // _elements.at(i) is not in old
            for (size_t j = 0; j < _nrgens; j++) {
              closure_update(i, j, b, s, x, old_new, nr_old_left, old_nr);
            }
          }
          _pos++;
        } // finished words of length <wordlen> + 1

        if (report) {
          std::cout << "found " << _nr << " elements, ";
          std::cout << _nrrules << " rules, ";
          std::cout << "max word length " << max_word_length() << std::endl;

        }
        expand(_nr - nr_shorter_elements);
        
        if (_wordlen == 0) {
          for (size_t i = 0; i < _pos; i++) { 
            size_t b = _final.at(_index.at(i)); 
            for (size_t j = 0; j < _nrgens; j++) { 
              _left->set(_index.at(i), j, _right->get(_genslookup.at(j), b));
            }
          }
        } else {
          for (size_t i = _lenindex.at(_wordlen); i < _pos; i++) { 
            size_t p = _prefix.at(_index.at(i));
            size_t b = _final.at(_index.at(i)); 
            for (size_t j = 0; j < _nrgens; j++) { 
              _left->set(_index.at(i), j, _right->get(_left->get(p, j), b));
            }
          }
        }
        if (_pos == _lenindex.at(_wordlen + 1)) {
          _lenindex.push_back(_index.size()); 
          _wordlen++;
          if (report) {
            std::cout << "found all words of length " << _wordlen << std::endl;
          }
        }
      }
      x.delete_data();
    }
      
  private:

    void inline expand (size_t nr) {
      _left->add_rows(nr);
      _reduced.add_rows(nr);
      _right->add_rows(nr);
    }
    
    void inline closure_update (size_t i, 
                                size_t j, 
                                size_t b, 
                                size_t s, 
                                T& x, 
                                std::vector<bool>& old_new, 
                                size_t& nr_old_left, 
                                size_t old_nr) {
      if (_wordlen != 0 && !_reduced.get(s, j)) {
        size_t r = _right->get(s, j);
        if (_found_one && r == _pos_one) {
          _right->set(i, j, _genslookup.at(b));
        } else if (_prefix.at(r) != (size_t) -1) {
          _right->set(i, j, _right->get(_left->get(_prefix.at(r), b),
                                        _final.at(r)));
        } else { 
          _right->set(i, j, _right->get(_genslookup.at(b), _final.at(r)));
        } 
      } else {
        x.redefine(_elements->at(i), _gens.at(j)); 
        auto it = _map.find(x); 
        if (it == _map.end()) { //it's new!
          is_one(x, _nr);
          _elements->push_back(static_cast<T*>(x.copy()));
          _first.push_back(b);
          _final.push_back(j);
          _map.insert(std::make_pair(*_elements->back(), _nr));
          _prefix.push_back(i);
          _reduced.set(i, j, true);
          _right->set(i, j, _nr);
          if (_wordlen == 0) { 
            _suffix.push_back(_genslookup.at(j));
          } else {
            _suffix.push_back(_right->get(s, j));
          }
          _index.push_back(_nr);
          _nr++;
        } else if (it->second < old_nr && !old_new.at(it->second)) {
          // we didn't process it yet!
          is_one(x, it->second);
          _first.at(it->second) = b;
          _final.at(it->second) = j;
          _prefix.at(it->second) = i;
          _reduced.set(i, j, true);
          _right->set(i, j, it->second);
          if (_wordlen == 0) { 
            _suffix.at(it->second) = _genslookup.at(j);
          } else {
            _suffix.at(it->second) = _right->get(s, j);
          }
          _index.push_back(it->second);
          old_new.at(it->second) = true;
          nr_old_left--;
        } else { // it->second >= old->_nr || old_new.at(it->second)
          // it's old
          _right->set(i, j, it->second);
          _nrrules++;
        }
      }
    }

    void inline is_one (T& x, size_t element_nr) {
      if (!_found_one && x == (*_id)) {
        _pos_one = element_nr;
        _found_one = true;
      }
    }

    size_t                                  _degree;
    std::vector<std::pair<size_t, size_t> > _duplicate_gens;
    std::vector<T*>*                        _elements;
    std::vector<size_t>                     _final;
    std::vector<size_t>                     _first;
    bool                                    _found_one;
    std::vector<T*>                         _gens;
    std::vector<size_t>                     _genslookup;  
    T*                                      _id; 
    std::vector<size_t>                     _index;
    RecVec<size_t>*                         _left;
    std::vector<size_t>                     _lenindex;
    std::unordered_map<const T, size_t>     _map;         
    size_t                                  _nr;
    size_t                                  _nrgens;
    size_t                                  _nrrules;
    size_t                                  _pos;
    size_t                                  _pos_one;
    std::vector<size_t>                     _prefix;
    RecVec<bool>                            _reduced;
    size_t                                  _relation_pos;
    size_t                                  _relation_gen;
    RecVec<size_t>*                         _right;
    std::vector<size_t>                     _suffix;
    size_t                                  _wordlen;
};

#endif
