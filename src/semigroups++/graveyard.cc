/*// method for Semigroup class
size_t simple_size () {
  T x(_degree, _gens.at(0)); 
  size_t report = 0;
  while (_pos < _nr) {
    for (size_t j = 0; j < _nrgens; j++) {
      x.redefine(_elements->at(_pos), _gens.at(j)); 
      auto it = _map.find(x); 
      if (it == _map.end()) {
        _elements->push_back(static_cast<T*>(x.copy()));
        _map.insert(std::make_pair(*_elements->back(), _nr));
        _nr++;
      }
    }
    _pos++;
    if (_nr > report + 10000) {
      report = _nr;
      std::cout << "found " << _nr << " elements so far\n";
    }
  }
  x.delete_data();
  return _nr;
}*/
    /*std::vector<Relation>* relations () {
      enumerate(-1);
      std::vector<Relation>* relations = new std::vector<Relation>();
      int nr = (int) _nrrules;
      
      size_t tmp = 0;

      for (size_t i = 1; i < _gens.size(); i++) {
        if (_genslookup.at(i) <= _genslookup.at(i - 1)) {
          nr--;
          relations->push_back(make_relation(i, _genslookup.at(i)));
        }
      }
      std::cout << "nr of relations = " << relations->size() - tmp << "\n";
      tmp = relations->size();
      
      size_t i;
      for (i = 0; i < _lenindex.at(1); i++) {
        for (size_t j = 0; j < _reduced.nrcols(); j++) {
          if (!_reduced.get(i, j)) {
            nr--;
            relations->push_back(make_relation(i, j));
          }
        }
      }
      std::cout << "nr of relations = " << relations->size() - tmp << "\n";
      tmp = relations->size();
      
      for (; i < _reduced.nrrows(); i++) {
        for (size_t j = 0; j < _reduced.nrcols(); j++) {
          if (_reduced.get(_suffix.at(i), j) && !_reduced.get(i, j)) {
            nr--;
            relations->push_back(make_relation(i, j));
          }
        }
      }
      std::cout << "nr of relations = " << relations->size() - tmp << "\n";
      
      std::cout << "_nrrules = " << _nrrules << "\n";
      assert(nr == 0);
      return relations;
    }*/
