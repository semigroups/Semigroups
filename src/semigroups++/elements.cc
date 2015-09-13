
#include "elements.h"

BooleanMat::BooleanMat (std::vector<bool>* matrix) : _matrix(matrix) {}

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

