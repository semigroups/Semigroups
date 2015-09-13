/*******************************************************************************
 * Semigroups++
 *
 * This file contains classes for creating elements of a semigroup.
 *
 *******************************************************************************/

#include "elements.h"

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// BooleanMat
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

bool BooleanMat::at (size_t pos) const {
  return _matrix->at(pos);
}

size_t BooleanMat::complexity () const {
  return pow(this->degree(), 3);
}

size_t BooleanMat::degree () const {
  return sqrt(_matrix->size());
}

bool BooleanMat::equals (const Element* that) const {
  return *(static_cast<const BooleanMat*>(that)->_matrix) == *(this->_matrix);
}

size_t BooleanMat::hash_value () const {
  size_t seed = 0;
  for (size_t i = 0; i < _matrix->size(); i++) {
    seed = ((seed << 1) + _matrix->at(i));
  }
  return seed;
}

// the identity of this
Element* BooleanMat::identity () const {
  std::vector<bool>* matrix(new std::vector<bool>());
  matrix->resize(_matrix->size(), false);
  for (size_t i = 0; i < this->degree(); i++) {
    matrix->at(i * this->degree() + i) =  true;
  }
  return new BooleanMat(matrix);
}

Element* BooleanMat::really_copy (size_t increase_deg_by) const {
  assert(increase_deg_by == 0);
  std::vector<bool>* matrix(new std::vector<bool>(*_matrix));
  return new BooleanMat(matrix);
}

void BooleanMat::really_delete () {
  delete _matrix;
}

// multiply x and y into this
void BooleanMat::redefine (Element const* x,
                           Element const* y) {
  assert(x->degree() == y->degree());
  assert(x->degree() == this->degree());
  
  size_t k;
  size_t dim = this->degree();
  std::vector<bool>* xx(static_cast<BooleanMat const*>(x)->_matrix);
  std::vector<bool>* yy(static_cast<BooleanMat const*>(y)->_matrix);

  for (size_t i = 0; i < dim; i++) {
    for (size_t j = 0; j < dim; j++) {
      for (k = 0; k < dim; k++) {
        if (xx->at(i * dim + k) && yy->at(k * dim + j)) {
          break;
        }
      }
      _matrix->at(i * dim + j) =  (k < dim);
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Bipartition
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

u_int32_t Bipartition::block (size_t pos) const {
  return _blocks->at(pos);
}

size_t Bipartition::complexity () const {
  return pow(_blocks->size(), 2);
}

size_t Bipartition::degree () const {
  return _blocks->size() / 2;
}

bool Bipartition::equals (const Element* that) const {
  return *(static_cast<const Bipartition*>(that)->_blocks) == *(this->_blocks);
}

size_t Bipartition::hash_value () const {
  size_t seed = 0;
  for (size_t i = 0; i < _blocks->size(); i++) {
    seed = ((seed * _blocks->size()) + _blocks->at(i));
  }
  return seed;
}

// the identity of this
Element* Bipartition::identity () const {
  std::vector<u_int32_t>* blocks(new std::vector<u_int32_t>());
  blocks->reserve(this->_blocks->size());
  for (size_t j = 0; j < 2; j++) {
    for (u_int32_t i = 0; i < this->degree(); i++) {
      blocks->push_back(i);
    }
  }
  return new Bipartition(blocks);
}

Element* Bipartition::really_copy (size_t increase_deg_by) const {
  assert(increase_deg_by == 0);
  std::vector<u_int32_t>* blocks(new std::vector<u_int32_t>(*_blocks));
  return new Bipartition(blocks);
}

void Bipartition::really_delete () {
  delete _blocks;
}

// multiply x and y into this
void Bipartition::redefine (Element const* x, Element const* y) {
  assert(x->degree() == y->degree());
  assert(x->degree() == this->degree());
  u_int32_t n = this->degree();

  Bipartition const* xx = static_cast<Bipartition const*>(x);
  Bipartition const* yy = static_cast<Bipartition const*>(y);

  std::vector<u_int32_t>* xblocks(xx->_blocks);
  std::vector<u_int32_t>* yblocks(yy->_blocks);

  u_int32_t nrx(xx->nrblocks());
  u_int32_t nry(yy->nrblocks());

  std::vector<u_int32_t> fuse;
  std::vector<u_int32_t> lookup;
  fuse.reserve(nrx + nry);
  lookup.reserve(nrx + nry);

  // maybe this should be pointer to local data, may slow down hashing
  // but speed up redefinition?
  for (size_t i = 0; i < nrx + nry; i++) {
    fuse.push_back(i);
    lookup.push_back(-1);
  }

  for (size_t i = 0; i < n; i++) {
    u_int32_t j = fuseit(fuse, xblocks->at(i + n));
    u_int32_t k = fuseit(fuse, yblocks->at(i) + nrx);
    if (j != k) {
      if (j < k) {
        fuse.at(k) = j;
      } else {
        fuse.at(j) = k;
      }
    }
  }

  u_int32_t next = 0;

  for (size_t i = 0; i < n; i++) {
    u_int32_t j = fuseit(fuse, xblocks->at(i));
    if (lookup.at(j) == (u_int32_t) -1) {
      lookup.at(j) = next;
      next++;
    }
    this->_blocks->at(i) = lookup.at(j);
  }

  for (size_t i = n; i < 2 * n; i++) {
    u_int32_t j = fuseit(fuse, yblocks->at(i) + nrx);
    if (lookup.at(j) == (u_int32_t) -1) {
      lookup.at(j) = next;
      next++;
    }
    this->_blocks->at(i) = lookup.at(j);
  }
}

inline u_int32_t Bipartition::fuseit (std::vector<u_int32_t> const& fuse, 
                                      u_int32_t pos                      ) {
  while (fuse.at(pos) < pos) {
    pos = fuse.at(pos);
  }
  return pos;
}

// nr blocks

u_int32_t Bipartition::nrblocks () const {
  size_t nr = 0;
  for (size_t i = 0; i < _blocks->size(); i++) {
    if (_blocks->at(i) > nr) {
      nr = _blocks->at(i);
    }
  }
  return nr + 1;
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Matrices over semirings
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

long MatrixOverSemiring::at (size_t pos) const {
  return _matrix->at(pos);
}

Semiring* MatrixOverSemiring::semiring () const {
  return _semiring;
}

size_t MatrixOverSemiring::complexity () const {
  return pow(this->degree(), 3);
}

size_t MatrixOverSemiring::degree () const {
  return sqrt(_matrix->size());
}

bool MatrixOverSemiring::equals (const Element* that) const {
  return *(static_cast<const MatrixOverSemiring*>(that)->_matrix) == *(this->_matrix);
}

size_t MatrixOverSemiring::hash_value () const {
  size_t seed = 0;
  for (size_t i = 0; i < _matrix->size(); i++) {
    seed = ((seed << 4) + _matrix->at(i));
  }
  return seed;
}

// the identity
Element* MatrixOverSemiring::identity () const {
  std::vector<long>* matrix(new std::vector<long>());
  matrix->resize(_matrix->size(), _semiring->zero());

  size_t n = this->degree();
  for (size_t i = 0; i < n; i++) {
    matrix->at(i * n + i) = _semiring->one();
  }
  return new MatrixOverSemiring(matrix, _semiring);
}

Element* MatrixOverSemiring::really_copy (size_t increase_degree_by) const {
  assert(increase_degree_by == 0);
  std::vector<long>* matrix(new std::vector<long>(*_matrix));
  return new MatrixOverSemiring(matrix, _semiring);
}

void MatrixOverSemiring::really_delete () {
  delete _matrix;
}

void MatrixOverSemiring::redefine (Element const* x,
                                   Element const* y) {

  MatrixOverSemiring const* xx(static_cast<MatrixOverSemiring const*>(x));
  MatrixOverSemiring const* yy(static_cast<MatrixOverSemiring const*>(y));

  assert(xx->degree() == yy->degree());
  assert(xx->degree() == this->degree());
  size_t deg = sqrt(this->degree());

  for (size_t i = 0; i < deg; i++) {
    for (size_t j = 0; j < deg; j++) {
      long v = _semiring->zero();
      for (size_t k = 0; k < deg; k++) {
        v = _semiring->plus(v, _semiring->prod(xx->at(i * deg + k), 
                                               yy->at(k * deg + j)));
      }
      _matrix->at(i * deg + j) = v;
    }
  }
  after(); // post process this
}
