/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ semigroups. 
 *
 */

#include "converter.h"
#include "interface.h"
#include "types.h"

#include <assert.h>

static Int RNam_words = RNamName("words");

/*******************************************************************************
 * Temporary stuff goes here!
*******************************************************************************/

/*******************************************************************************
 * Helper functions for getting information from data
*******************************************************************************/

long inline Threshold (Obj data) {
  Obj x = Representative(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_TROP_MAT(x)||IS_NAT_MAT(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
}

long inline Period (Obj data) {
  Obj x = Representative(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_NAT_MAT(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2));
}

long inline SizeOfFF (Obj data) {
  Obj x = Representative(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_MAT_OVER_PF(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);
  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
}

/*******************************************************************************
 * Class for containing a C++ semigroup and accessing its methods
*******************************************************************************/

template<typename T>
class Interface : public InterfaceBase {
  public: 

    Interface () = delete;

    // constructor

    // in the case that we are taking the closure of the semigroup of
    // <old> with some new generators, these new generators are stored in
    // the <gens> component of <data>. I.e. the meaning of the <gens> component
    // of the <data> is different if we are taking the closure than if we are
    // not. 
    Interface (Obj data, Converter<T>* converter, SemigroupBase* old) 
      : _converter(converter) {
      assert(IsbPRec(data, RNamName("gens")));
      assert(LEN_LIST(ElmPRec(data, RNamName("gens"))) > 0);
      
      Obj gens = ElmPRec(data, RNamName("gens"));

      std::vector<T*> gens_c;
      size_t deg_c = INT_INTOBJ(ElmPRec(data, RNamName("degree")));
      PLAIN_LIST(gens);
      for(size_t i = 1; i <= (size_t) LEN_PLIST(gens); i++) {
        gens_c.push_back(converter->convert(ELM_PLIST(gens, i), deg_c));
      }
        
      if (old == nullptr) {
        _semigroup = new Semigroup<T>(gens_c, deg_c);
      } else {
        _semigroup = new Semigroup<T>(*static_cast<Semigroup<T>*>(old), gens_c); 
        for (size_t i = 0; i < _semigroup->nrgens(); i++) {
          AssPlist(gens, i + 1, converter->unconvert(_semigroup->gens().at(i)));
        }
      }
      _semigroup->set_batch_size(BatchSize(data));
    }
    
    // destructor
    ~Interface() {
      delete _converter;
      delete _semigroup;
    };
   

    Semigroup<T>* semigroup () const {
      return _semigroup;
    }


    bool is_done () const {
      return _semigroup->is_done();
    }
    
    size_t current_size () const {
      return _semigroup->current_size();
    }
    
    size_t current_nrrules () const {
      return _semigroup->current_nrrules();
    }
    
    size_t current_max_word_length () const {
      return _semigroup->max_word_length();
    }

    void enumerate (Obj data, Obj limit) {
      _semigroup->enumerate(INT_INTOBJ(limit), Report(data));
    }

    // get the size of the C++ semigroup
    size_t size (Obj data) {
      return _semigroup->size(Report(data));
    }
   
    /*size_t simple_size () {
      return _semigroup->simple_size();
    }*/

    // get the right Cayley graph from C++ semgroup, store it in data
    void right_cayley_graph (Obj data) {
      AssPRec(data, RNamName("right"), 
              ConvertFromRecVec(_semigroup->right_cayley_graph(Report(data))));
      CHANGED_BAG(data);
    }

    // get the left Cayley graph from C++ semgroup, store it in data
    void left_cayley_graph (Obj data) {
      AssPRec(data, RNamName("left"), 
              ConvertFromRecVec(_semigroup->left_cayley_graph(Report(data))));
      CHANGED_BAG(data);
    }
    
    // get the elements of the C++ semigroup, store them in data
    void elements (Obj data, size_t limit) {
      std::vector<T*>* elements = _semigroup->elements(limit, Report(data));
      if (! IsbPRec(data, RNamName("elts"))) {
        Obj out = NEW_PLIST(T_PLIST, elements->size());
        SET_LEN_PLIST(out, elements->size());
        for (size_t i = 0; i < elements->size(); i++) {
          SET_ELM_PLIST(out, i + 1, _converter->unconvert(elements->at(i)));
          CHANGED_BAG(out);
        }
        AssPRec(data, RNamName("elts"), out);
      } else {
        Obj out = ElmPRec(data, RNamName("elts"));
        for (size_t i = LEN_PLIST(out); i < elements->size(); i++) {
          AssPlist(out, i + 1, _converter->unconvert(elements->at(i)));
        }
      }
      CHANGED_BAG(data);
    }

    // FIXME this might be dangerous! Since this method can be confused for the
    // previous one
    void elements (Obj data, Obj limit) {
      elements(data, INT_INTOBJ(limit));
    }
    
    // return first place in _semigroup starting from pos, for which lookfunc
    // returns true
    void find (Obj data, Obj lookfunc, Obj start, Obj end) {
      std::cout << "finding!!\n";
      
      AssPRec(data, RNamName("found"), False);
      size_t pos = INT_INTOBJ(start);
      size_t limit;
      if (pos < _semigroup->current_size()) {
        limit = _semigroup->current_size();
      } else {
        limit = pos + _semigroup->batch_size(); 
      }

      elements(data, limit); // get the elements out of _semigroup into "elts"
      
      size_t nr = std::min(LEN_PLIST(ElmPRec(data, RNamName("elts"))),
                           INT_INTOBJ(end));
      bool found = false;

      while (!found && pos < nr) {
        for (; pos < nr; pos++) {
          if (CALL_2ARGS(lookfunc, data, INTOBJ_INT(pos)) == True) {
            AssPRec(data,  RNamName("found"), INTOBJ_INT(pos)); 
            found = true;
            break;
          }
        }
        if (!found) {
          limit += _semigroup->batch_size();
          elements(data, limit); 
          // get the elements out of _semigroup into "elts"
          nr = std::min(LEN_PLIST(ElmPRec(data, RNamName("elts"))),
                        INT_INTOBJ(end));
        }
      }
    }
    
    Obj position (Obj data, Obj x) {
      size_t deg_c = INT_INTOBJ(ElmPRec(data, RNamName("degree")));
      size_t pos = _semigroup->position(_converter->convert(x, deg_c), Report(data));
      return (pos == ((size_t) -1) ? Fail : INTOBJ_INT(pos + 1));
    }
   
    Obj word (Obj data, Obj pos) {
      return word(data, pos, Report(data));
    }

    // get the word from the C++ semigroup, store it in data
    Obj word (Obj data, Obj pos, bool report) {
      size_t pos_c = INT_INTOBJ(pos);
      Obj words; 
      if (! IsbPRec(data, RNam_words)) {
        words = NEW_PLIST(T_PLIST, pos_c);
        SET_LEN_PLIST(words, pos_c);
        SET_ELM_PLIST(words, pos_c, ConvertFromVec(_semigroup->factorisation(pos_c - 1, report)));
        CHANGED_BAG(words);
        AssPRec(data, RNam_words, words);
      } else {
        words = ElmPRec(data, RNam_words);
        if (pos_c > (size_t) LEN_PLIST(words) || ELM_PLIST(words, pos_c) == 0) {
          //avoid retracing the Schreier tree if possible
          size_t prefix = _semigroup->prefix(pos_c - 1) + 1;
          size_t suffix = _semigroup->suffix(pos_c - 1) + 1;
          if (prefix != 0 && prefix <= (size_t) LEN_PLIST(words) 
              && ELM_PLIST(words, prefix) != 0) {
            Obj old_word = ELM_PLIST(words, prefix);
            Obj new_word = NEW_PLIST(T_PLIST_CYC, LEN_PLIST(old_word) + 1);
            memcpy((void *)((char *)(ADDR_OBJ(new_word)) + sizeof(Obj)), 
                   (void *)((char *)(ADDR_OBJ(old_word)) + sizeof(Obj)), 
                   (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
            SET_ELM_PLIST(new_word, LEN_PLIST(old_word) + 1, 
                          INTOBJ_INT(_semigroup->final_letter(pos_c - 1) + 1));
            SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);
            AssPlist(words, pos_c, new_word);
          } else if (suffix != 0 && suffix <= (size_t) LEN_PLIST(words) 
                     && ELM_PLIST(words, suffix) != 0) {
            Obj old_word = ELM_PLIST(words, suffix);
            Obj new_word = NEW_PLIST(T_PLIST_CYC, LEN_PLIST(old_word) + 1);
            memcpy((void *)((char *)(ADDR_OBJ(new_word)) + 2 * sizeof(Obj)), 
                   (void *)((char *)(ADDR_OBJ(old_word)) + sizeof(Obj)), 
                   (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
            SET_ELM_PLIST(new_word, 1,
                          INTOBJ_INT(_semigroup->first_letter(pos_c - 1) + 1));
            SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);
            AssPlist(words, pos_c, new_word);
          } else {
            AssPlist(words, pos_c, ConvertFromVec(_semigroup->factorisation(pos_c - 1, report)));
          }
        }
      }
      CHANGED_BAG(data);
      assert(IsbPRec(data, RNam_words));
      assert(IS_PLIST(ElmPRec(data, RNam_words)));
      assert(pos_c <= (size_t) LEN_PLIST(ElmPRec(data, RNam_words)));
      return ELM_PLIST(ElmPRec(data, RNam_words), pos_c);
    }

    // get the relations of the C++ semigroup, store them in data
    void relations (Obj data) {
      if (! IsbPRec(data, RNamName("rules"))) {
        bool report = Report(data);
        Obj rules = NEW_PLIST(T_PLIST, _semigroup->nrrules(report));
        SET_LEN_PLIST(rules, _semigroup->nrrules(report));
        size_t nr = 0;

        std::vector<size_t> relation;
        _semigroup->next_relation(relation, report);

        while (relation.size() == 2) {
          Obj next = NEW_PLIST(T_PLIST, 2);
          SET_LEN_PLIST(next, 2);
          for (size_t i = 0; i < 1; i++) {
            Obj w = NEW_PLIST(T_PLIST_CYC, 1);
            SET_LEN_PLIST(w, 1);
            SET_ELM_PLIST(w, 1, INTOBJ_INT(relation.at(i) + 1));
            SET_ELM_PLIST(next, i + 1, w);
            CHANGED_BAG(next);
          }
          nr++;
          SET_ELM_PLIST(rules, nr, next);
          CHANGED_BAG(rules);
          _semigroup->next_relation(relation, report);
        }
        
        while (!relation.empty()) {

          Obj old_word = word(data, INTOBJ_INT(relation.at(0) + 1), report);
          Obj new_word = NEW_PLIST(T_PLIST_CYC, LEN_PLIST(old_word) + 1);
          memcpy((void *)((char *)(ADDR_OBJ(new_word)) + sizeof(Obj)), 
                 (void *)((char *)(ADDR_OBJ(old_word)) + sizeof(Obj)), 
                 (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
          SET_ELM_PLIST(new_word, LEN_PLIST(old_word) + 1, INTOBJ_INT(relation.at(1) + 1));
          SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);

          Obj next = NEW_PLIST(T_PLIST, 2);
          SET_LEN_PLIST(next, 2);
          SET_ELM_PLIST(next, 1, new_word);
          CHANGED_BAG(next);
          SET_ELM_PLIST(next, 2, word(data, INTOBJ_INT(relation.at(2) + 1), report));
          CHANGED_BAG(next);
          nr++; 
          SET_ELM_PLIST(rules, nr, next);
          CHANGED_BAG(rules);
          _semigroup->next_relation(relation, report);
        } 
        AssPRec(data, RNamName("rules"), rules);
        CHANGED_BAG(data);
      }
    }
    
  private:
    
    // helper function to convert a RecVec to a GAP plist of GAP plists.
    Obj ConvertFromRecVec (RecVec<size_t>* rv) {
      Obj out = NEW_PLIST(T_PLIST, rv->nrrows());
      SET_LEN_PLIST(out, rv->nrrows());

      for (size_t i = 0; i < rv->nrrows(); i++) {
        Obj next = NEW_PLIST(T_PLIST_CYC, rv->nrcols());
        SET_LEN_PLIST(next, rv->nrcols());
        for (size_t j = 0; j < rv->nrcols(); j++) {
          SET_ELM_PLIST(next, j + 1, INTOBJ_INT(rv->get(i, j) + 1));
        }
        SET_ELM_PLIST(out, i + 1, next);
        CHANGED_BAG(out);
      }
      return out;
    }
    
    // helper function to convert a vector to a plist of GAP integers
    Obj ConvertFromVec (std::vector<size_t>* vec) {
      Obj out = NEW_PLIST(T_PLIST_CYC, vec->size());
      SET_LEN_PLIST(out, vec->size());

      for (size_t i = 0; i < vec->size(); i++) {
        SET_ELM_PLIST(out, i + 1, INTOBJ_INT(vec->at(i) + 1));
      }
      CHANGED_BAG(out);
      return out;
    }

    Semigroup<T>* _semigroup;
    Converter<T>* _converter;
};

/*******************************************************************************
 * Instantiate Interface for the particular type of semigroup passed from GAP
*******************************************************************************/

InterfaceBase* InterfaceFromData (Obj data, SemigroupBase* old) {
  if (IsbPRec(data, RNamName("Interface_CC"))) {
    return CLASS_OBJ<InterfaceBase>(ElmPRec(data, RNamName("Interface_CC")));
  }

  InterfaceBase* interface;

  switch (TypeSemigroup(data)) {
    case TRANS2:{
      auto tc2 = new TransConverter<u_int16_t>();
      interface = new Interface<Transformation<u_int16_t> >(data, tc2, old);
      break;
    }
    case TRANS4:{
      auto tc4 = new TransConverter<u_int32_t>();
      interface = new Interface<Transformation<u_int32_t> >(data, tc4, old);
      break;
    }
    case PPERM2:{
      auto pc2 = new PPermConverter<u_int16_t>();
      interface = new Interface<PartialPerm<u_int16_t> >(data, pc2, old);
      break;
    }
    case PPERM4:{
      auto pc4 = new PPermConverter<u_int32_t>();
      interface = new Interface<PartialPerm<u_int32_t> >(data, pc4, old);
      break;
    }
    case BIPART: {
      auto bc = new BipartConverter();
      interface = new Interface<Bipartition>(data, bc, old);
      break;
    }
    case BOOL_MAT:{ 
      auto bmc = new BoolMatConverter();
      interface = new Interface<BooleanMat>(data, bmc, old);
      break;
    }
    case MAX_PLUS_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new MaxPlusSemiring(), 
                                                  Ninfinity, 
                                                  MaxPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc, old);
      break;
    }
    case MIN_PLUS_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new MinPlusSemiring(), 
                                                  infinity, 
                                                  MinPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc, old);
      break;
    }
    case TROP_MAX_PLUS_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new TropicalMaxPlusSemiring(Threshold(data)), 
                                                  Ninfinity, 
                                                  TropicalMaxPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc, old);
      break;
    }
    case TROP_MIN_PLUS_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new TropicalMinPlusSemiring(Threshold(data)), 
                                                  infinity, 
                                                  TropicalMinPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc, old);
      break;
    }
    case PROJ_MAX_PLUS_MAT:{
      auto pmpmc = new ProjectiveMaxPlusMatrixConverter(new MaxPlusSemiring(), 
                                                        Ninfinity, 
                                                        ProjectiveMaxPlusMatrixType);
      interface = new Interface<ProjectiveMaxPlusMatrix>(data, pmpmc, old);
      break;

    }
    case NAT_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new NaturalSemiring(Threshold(data),
                                                                      Period(data)), 
                                                  INTOBJ_INT(0), 
                                                  NaturalMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc, old);
      break;
    }
    case MAT_OVER_PF:{
      auto mopfc = new MatrixOverPrimeFieldConverter(new PrimeField(SizeOfFF(data)));
      interface = new Interface<MatrixOverSemiring>(data, mopfc, old);
      break;
    }
    case PBR:{
      auto pbrc = new PBRConverter();
      interface = new Interface<PartitionedBinaryRelation>(data, pbrc, old);
      break;
    }
    default: {
      assert(false);
    }
  }
  AssPRec(data, RNamName("Interface_CC"), NewSemigroupsBag(interface, INTERFACE));
  return interface;
}

/*******************************************************************************
 * GAP level functions
*******************************************************************************/

Obj ENUMERATE_SEMIGROUP (Obj self, Obj data, Obj limit, Obj lookfunc, Obj looking);

/*Obj SIMPLE_SIZE(Obj self, Obj data) {
  if (IsCCSemigroup(data)) { 
    return INTOBJ_INT(InterfaceFromData(data)->simple_size());
  }
  std::cout << "don't call this function with non-CC semigroups\n";
  return 0;
}*/

//FIXME redo everything else here like RIGHT_CAYLEY_GRAPH!!
Obj RIGHT_CAYLEY_GRAPH (Obj self, Obj data) {
  if (IsCCSemigroup(data)) {
    if (! IsbPRec(data, RNamName("right"))) { 
      InterfaceFromData(data)->right_cayley_graph(data);
    }
  } else {
    ENUMERATE_SEMIGROUP(self, data, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(data, RNamName("right"));
}

Obj LEFT_CAYLEY_GRAPH (Obj self, Obj data) {
  if (IsCCSemigroup(data) && ! IsbPRec(data, RNamName("left"))) { 
    //TODO free _left 
    InterfaceFromData(data)->left_cayley_graph(data);
  } else {
    ENUMERATE_SEMIGROUP(self, data, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(data, RNamName("left"));
}

Obj RELATIONS_SEMIGROUP (Obj self, Obj data) {
  if (IsCCSemigroup(data) && ! IsbPRec(data, RNamName("rules"))) { 
    //TODO free _suffix and _first
    InterfaceFromData(data)->relations(data);
  } else {
    ENUMERATE_SEMIGROUP(self, data, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(data, RNamName("rules"));
}

Obj SIZE_SEMIGROUP (Obj self, Obj data) {
  if (IsCCSemigroup(data)) { 
    return INTOBJ_INT(InterfaceFromData(data)->size(data));
  } else {
    ENUMERATE_SEMIGROUP(self, data, INTOBJ_INT(-1), 0, False);
  }
  return INTOBJ_INT(LEN_PLIST(ElmPRec(data, RNamName("elts"))));
}

Obj ELEMENTS_SEMIGROUP (Obj self, Obj data, Obj limit) {
  if (IsCCSemigroup(data)) { 
    InterfaceFromData(data)->elements(data, limit);
  } else {
    ENUMERATE_SEMIGROUP(self, data, limit, 0, False);
  }
  return ElmPRec(data, RNamName("elts"));
}

Obj WORD_SEMIGROUP (Obj self, Obj data, Obj pos) {
  if (IsCCSemigroup(data)) { 
    return InterfaceFromData(data)->word(data, pos);
  } else {
    ENUMERATE_SEMIGROUP(self, data, INTOBJ_INT(pos), 0, False);
    return ELM_PLIST(ElmPRec(data, RNam_words), INT_INTOBJ(pos));
  }
}

Obj FIND_SEMIGROUP (Obj self, Obj data, Obj lookfunc, Obj start, Obj end) {
  if (IsCCSemigroup(data)) { 
    InterfaceFromData(data)->find(data, lookfunc, start, end);
  } else {
    ENUMERATE_SEMIGROUP(self, data, end, lookfunc, True);
  }
  return ElmPRec(data, RNamName("found"));
}

Obj LENGTH_SEMIGROUP (Obj self, Obj data) {
  if (IsCCSemigroup(data)) { 
    return INTOBJ_INT(InterfaceFromData(data)->current_size());
  }
  return INTOBJ_INT(LEN_PLIST(ElmPRec(data, RNamName("elts"))));
}

Obj NR_RULES_SEMIGROUP (Obj self, Obj data) {
  if (IsCCSemigroup(data)) { 
    return INTOBJ_INT(InterfaceFromData(data)->current_nrrules());
  }
  return INTOBJ_INT(ElmPRec(data, RNamName("nrrules")));
}

Obj POSITION_SEMIGROUP (Obj self, Obj data, Obj x) {
  if (IsCCSemigroup(data)) { 
    return InterfaceFromData(data)->position(data, x);
  }

  Obj ht = ElmPRec(data, RNamName("ht"));
  size_t pos, nr;
  
  do { 
    Obj val = HTValue_TreeHash_C(self, ht, x);
    if (val != Fail) {
      return val; 
    }
    Obj limit = SumInt(ElmPRec(data, RNamName("nr")), INTOBJ_INT(1));
    ENUMERATE_SEMIGROUP(self, data,  limit, 0, False);
    pos = INT_INTOBJ(ElmPRec(data, RNamName("pos")));
    nr = INT_INTOBJ(ElmPRec(data, RNamName("nr")));
  } while (pos <= nr);
  return HTValue_TreeHash_C(self, ht, x);
}

Obj IS_CLOSED_SEMIGROUP (Obj self, Obj data) {
  if (IsCCSemigroup(data)) {
    return (InterfaceFromData(data)->is_done() ? True : False);
  }

  size_t pos = INT_INTOBJ(ElmPRec(data, RNamName("pos")));
  size_t nr = INT_INTOBJ(ElmPRec(data, RNamName("nr")));
  return (pos > nr ? True : False);
}

//new_data.gens is the generators to add to the generators of old_data

Obj CLOSURE_SEMIGROUP (Obj self, Obj old_data, Obj new_data) {
  if (TypeSemigroup(old_data) == UNKNOWN) { 
    ErrorQuit("CLOSURE_SEMIGROUP: this shouldn't happen!", 0L, 0L);
  }
  InterfaceFromData(new_data, InterfaceFromData(old_data)->semigroup());
  return new_data;
}

Obj MAX_WORD_LEN_SEMIGROUP(Obj self, Obj data) {
  if (TypeSemigroup(data) != UNKNOWN) { 
    return INTOBJ_INT(InterfaceFromData(data)->current_max_word_length());
  } else {
    if (IsbPRec(data, RNam_words) && LEN_PLIST(ElmPRec(data, RNam_words) > 0)) {
      Obj words = ElmPRec(data, RNam_words);
      return INTOBJ_INT(LEN_PLIST(ELM_PLIST(words, LEN_PLIST(words))));
    } else {
      return INTOBJ_INT(1);
    }
  }
}

