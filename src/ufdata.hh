/*
 * Semigroups GAP package
 *
 * This file defines UFData, a class used to make an equivalence relation on
 * the integers {1 .. n}, using the UNION-FIND METHOD: new pairs can be added
 * and the appropriate classes combined quickly.
 *
 */

#include <vector>

#include "src/compiled.h"

class UFData {
 public:
  typedef std::vector<size_t>   table_t;
  typedef std::vector<table_t*> blocks_t;

  // Copy constructor
  UFData (const UFData& copy) : _size(copy._size),
                                _table(new table_t(*copy._table)),
                                _blocks(nullptr),
                                _haschanged(copy._haschanged) {
    if (copy._blocks != nullptr) {
      // Copy the blocks as well
      _blocks = new blocks_t();
      _blocks->reserve(copy._blocks->size());
      for (auto block: *copy._blocks) {
        if (block == nullptr) {
          _blocks->push_back(nullptr);
        } else {
          _blocks->push_back(new table_t(*block));
        }
      }
    }
  }
  UFData& operator= (UFData const& copy) = delete;

  // Constructor by table
  UFData (const table_t& table) : _size(table.size()),
                                  _table(new table_t(table)),
                                  _blocks(nullptr),
                                  _haschanged(true) { }

  // Constructor by size
  UFData (size_t size) : _size(size),
                         _table(new table_t()),
                         _blocks(nullptr),
                         _haschanged(false) {
    _table->reserve(size);
    for (size_t i=0; i<size; i++) {
      _table->push_back(i);
    }
  }

  // Destructor
  ~UFData () {
    delete _table;
    if (_blocks != nullptr) {
      for (size_t i=0; i<_blocks->size(); i++) {
        delete _blocks->at(i);
      }
      delete _blocks;
    }
  }

  // Getters
  size_t   get_size () { return _size; }
  table_t  *get_table () { return _table; }

  // get_blocks
  blocks_t *get_blocks () {
    table_t *block;
    // Is _blocks "bound" yet?
    if (_blocks == nullptr) {
      _blocks = new blocks_t();
      _blocks->reserve(_size);
      for (size_t i=0; i<_size; i++) {
        block = new table_t(1, i);
        _blocks->push_back(block);
      }
    }
    // Do we need to update the blocks?
    if (_haschanged) {
      size_t ii;
      for (size_t i=0; i<_size; i++) {
        if (_blocks->at(i) != nullptr) {
          ii = find(i);
          if (ii != i) {
            // Combine the two blocks
            _blocks->at(ii)->reserve(_blocks->at(ii)->size()
                                     + _blocks->at(i)->size());
            _blocks->at(ii)->insert(_blocks->at(ii)->end(),
                                    _blocks->at(i)->begin(),
                                    _blocks->at(i)->end());
            delete _blocks->at(i);
            _blocks->at(i) = nullptr;
          }
        }
      }
      _haschanged = false;
    }
    return _blocks;
  }

  // find
  size_t find (size_t i) {
    size_t ii;
    do {
      ii = i;
      i = _table->at(ii);
    } while (ii != i);
    return i;
  }

  // union
  void unite (size_t i, size_t j) {
    size_t ii, jj;
    ii = find(i);
    jj = find(j);
    if (ii < jj) {
      _table->at(jj) = ii;
    } else {
      _table->at(ii) = jj;
    }
    _haschanged = true;
  }

  // flatten
  void flatten() {
    for (size_t i=0; i<_size; i++) {
      _table->at(i) = find(i);
    }
  }

private:
  size_t    _size;
  table_t*  _table;
  blocks_t* _blocks;
  bool      _haschanged;
};

// GAP level functions

Obj UF_NEW (Obj self, Obj size);
Obj UF_COPY (Obj self, Obj ufdata);
Obj UF_SIZE (Obj self, Obj ufdata);
Obj UF_FIND (Obj self, Obj ufdata, Obj i);
Obj UF_UNION (Obj self, Obj ufdata, Obj pair);
Obj UF_FLATTEN (Obj self, Obj ufdata);
Obj UF_TABLE (Obj self, Obj ufdata);
Obj UF_BLOCKS (Obj self, Obj ufdata);
