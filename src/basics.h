/*
 * semigroups: Methods for semigroups
 */

// TODO make this a template 
template <typename T>
class RecVec {
        
      public:
        
        RecVec (const RecVec& copy) = delete;
        RecVec& operator= (RecVec const& copy) = delete;
        
        RecVec(size_t nrcols, size_t nrrows=0) : _vec(), 
                                                 _nrcols(nrcols),
                                                 _nrrows(0){
          std::cout << "constructed!!\n";
          this->expand(nrrows);
        }
        
        ~RecVec() {
          std::cout << "destructed!!\n";
          delete _vec;
        }
        
        u_int32_t get (size_t i, size_t j) {
          assert(i - 1 < _nrrows && j - 1 < _nrcols);
          return _vec->at((i - 1) * _nrcols + (j - 1)); 
        }
        
        void set (size_t i, size_t j, T val) {
          assert(i - 1 < _nrrows && j - 1 < _nrcols);
          _vec->at((i - 1) * _nrcols + (j - 1)) = val; 
        }

        void expand (size_t nr = 1) {
          _nrrows += nr;
          for (size_t i = 0; i < _nrcols * nr; i++) {
            _vec->push_back(0);
          }
        }

        size_t size () {
          return _vec->size();
        }

      private:

        std::vector<T>* _vec;
        size_t _nrcols;
        size_t _nrrows;
};
