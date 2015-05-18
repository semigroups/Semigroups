/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ semigroups. 
 *
 */

#include "converter.h"
#include "interface.h"
#include "types.h"

#include "semigroups++/semigroups.h"

#include <assert.h>

#define BATCH_SIZE 8192

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
    Interface (Obj data, Converter<T>* converter) : _converter(converter) {
      assert(IsbPRec(data, RNamName("gens")));
      assert(LEN_LIST(ElmPRec(data, RNamName("gens"))) > 0);
      
      Obj gens =  ElmPRec(data, RNamName("gens"));

      if (IsbPRec(data, RNamName("report"))) {
        assert(ElmPRec(data, RNamName("report")) == True || 
            ElmPRec(data, RNamName("report")) == False);
        _report = (ElmPRec(data, RNamName("report")) == True ? true : false);
      } else {
        _report = false;
      }

      std::vector<T*> gens_c;
      size_t deg_c = INT_INTOBJ(ElmPRec(data, RNamName("degree")));

      PLAIN_LIST(gens);
      for(size_t i = 1; i <= (size_t) LEN_PLIST(gens); i++) {
        gens_c.push_back(converter->convert(ELM_PLIST(gens, i), deg_c));
      }
      
      _semigroup = new Semigroup<T>(gens_c, deg_c);
    }

    // destructor
    ~Interface() {
      delete _converter;
      delete _semigroup;
    };
   
    // TODO replace the methods from HERE . . .

    // enumerate the C++ semigroup
    void enumerate (Obj limit) {
      _semigroup->enumerate(INT_INTOBJ(limit), _report);
    }
    
    bool is_done () {
      return _semigroup->is_done();
    }

    // get the size of the C++ semigroup
    size_t size () {
      return _semigroup->size(_report);
    }
    
    size_t simple_size () {
      return _semigroup->simple_size();
    }
    
    size_t current_size () {
      return _semigroup->current_size();
    }
    
    size_t nrrules () {
      return _semigroup->nrrules();
    }
    // . . . to HERE with calls to semigroup()->nrrules()

    // get the right Cayley graph from C++ semgroup, store it in data
    void right_cayley_graph (Obj data) {
      _semigroup->enumerate(-1, _report);
      AssPRec(data, RNamName("right"), 
              ConvertFromRecVec(_semigroup->right_cayley_graph()));
      CHANGED_BAG(data);
      // TODO do something like this
      //delete _semigroup->right_cayley_graph();
    }

    // get the left Cayley graph from C++ semgroup, store it in data
    void left_cayley_graph (Obj data) {
      _semigroup->enumerate(-1, _report);
      AssPRec(data, RNamName("left"), 
              ConvertFromRecVec(_semigroup->left_cayley_graph()));
      CHANGED_BAG(data);
      // TODO do something like this
      //delete _semigroup->left_cayley_graph();
    }
    
    // get the elements of the C++ semigroup, store them in data
    void elements (Obj data, size_t limit) {
      std::vector<T*>* elements = _semigroup->elements(limit);
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
        limit = pos + BATCH_SIZE; 
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
          limit += BATCH_SIZE;
          elements(data, limit); 
          // get the elements out of _semigroup into "elts"
          nr = std::min(LEN_PLIST(ElmPRec(data, RNamName("elts"))),
                        INT_INTOBJ(end));
        }
      }
    }
    
    // get the word from the C++ semigroup, store it in data
    void word (Obj data, Obj pos) {
      size_t pos_c = INT_INTOBJ(pos);
      _semigroup->enumerate(pos_c - 1, _report);
      if (! IsbPRec(data, RNamName("words"))) {
        Obj words = NEW_PLIST(T_PLIST, pos_c);
        SET_LEN_PLIST(words, pos_c);
        SET_ELM_PLIST(words, pos_c, ConvertFromVec(_semigroup->trace(pos_c - 1)));
        CHANGED_BAG(words);
        AssPRec(data, RNamName("words"), words);
      } else {
        Obj words = ElmPRec(data, RNamName("words"));
        if (pos_c > (size_t) LEN_PLIST(words) || ELM_PLIST(words, pos_c) == 0){
          AssPlist(words, pos_c, ConvertFromVec(_semigroup->trace(pos_c - 1)));
        }
      }
      CHANGED_BAG(data);
    }

    Obj position (Obj data, Obj x) {
      size_t deg_c = INT_INTOBJ(ElmPRec(data, RNamName("degree")));
      size_t pos = _semigroup->position(_converter->convert(x, deg_c));
      return (pos == ((size_t) -1) ? Fail : INTOBJ_INT(pos + 1));
    }

    // get the relations of the C++ semigroup, store them in data
    // FIXME improve this, it repeatedly traces the schreier tree
    void relations (Obj data) {
      auto relations = _semigroup->relations();
      Obj out = NEW_PLIST(T_PLIST, relations->size());
      SET_LEN_PLIST(out, relations->size());
      for (size_t i = 0; i < relations->size(); i++) {
        Obj next = NEW_PLIST(T_PLIST, 2);
        SET_LEN_PLIST(next, 2);
        SET_ELM_PLIST(next, 1, ConvertFromVec(relations->at(i).first));
        CHANGED_BAG(next);
        SET_ELM_PLIST(next, 2, ConvertFromVec(relations->at(i).second));
        CHANGED_BAG(next);
        SET_ELM_PLIST(out, i + 1, next);
        CHANGED_BAG(out);
      }
      AssPRec(data, RNamName("rules"), out);
      CHANGED_BAG(data);
      //FIXME check that this doesn't leak memory
      /*for (auto pair: relations) {
        delete pair.first;
        delete pair.second;
      }*/
      delete relations;
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
    bool          _report;
};

/*******************************************************************************
 * Instantiate Interface for the particular type of semigroup passed from GAP
*******************************************************************************/

InterfaceBase* InterfaceFromData (Obj data) {
  if (IsbPRec(data, RNamName("Interface_CC"))) {
    return CLASS_OBJ<InterfaceBase>(ElmPRec(data, RNamName("Interface_CC")));
  }
  
  InterfaceBase* interface;

  switch (TypeSemigroup(data)) {
    case TRANS2:{
      auto tc2 = new TransConverter<u_int16_t>();
      interface = new Interface<Transformation<u_int16_t> >(data, tc2);
      break;
    }
    case TRANS4:{
      auto tc4 = new TransConverter<u_int32_t>();
      interface = new Interface<Transformation<u_int32_t> >(data, tc4);
      break;
    }
    case PPERM2:{
      auto pc2 = new PPermConverter<u_int16_t>();
      interface = new Interface<PartialPerm<u_int16_t> >(data, pc2);
      break;
    }
    case PPERM4:{
      auto pc4 = new PPermConverter<u_int32_t>();
      interface = new Interface<PartialPerm<u_int32_t> >(data, pc4);
      break;
    }
    case BIPART: {
      auto bc = new BipartConverter();
      interface = new Interface<Bipartition>(data, bc);
      break;
    }
    case BOOL_MAT:{ 
      auto bmc = new BoolMatConverter();
      interface = new Interface<BooleanMat>(data, bmc);
      break;
    }
    case MAX_PLUS_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new MaxPlusSemiring(), 
                                                  Ninfinity, 
                                                  MaxPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc);
      break;
    }
    case MIN_PLUS_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new MinPlusSemiring(), 
                                                  infinity, 
                                                  MinPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc);
      break;
    }
    case TROP_MAX_PLUS_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new TropicalMaxPlusSemiring(Threshold(data)), 
                                                  Ninfinity, 
                                                  TropicalMaxPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc);
      break;
    }
    case TROP_MIN_PLUS_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new TropicalMinPlusSemiring(Threshold(data)), 
                                                  infinity, 
                                                  TropicalMinPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc);
      break;
    }
    case PROJ_MAX_PLUS_MAT:{
      auto pmpmc = new ProjectiveMaxPlusMatrixConverter(new MaxPlusSemiring(), 
                                                        Ninfinity, 
                                                        ProjectiveMaxPlusMatrixType);
      interface = new Interface<ProjectiveMaxPlusMatrix>(data, pmpmc);
      break;

    }
    case NAT_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new NaturalSemiring(Threshold(data),
                                                                      Period(data)), 
                                                  INTOBJ_INT(0), 
                                                  NaturalMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc);
      break;
    }
    case MAT_OVER_PF:{
      auto mopfc = new MatrixOverPrimeFieldConverter(new PrimeField(SizeOfFF(data)));
      interface = new Interface<MatrixOverSemiring>(data, mopfc);
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

Obj SIMPLE_SIZE(Obj self, Obj data) {
  if (IsCCSemigroup(data)) { 
    return INTOBJ_INT(InterfaceFromData(data)->simple_size());
  }
  std::cout << "don't call this function with non-CC semigroups\n";
  return 0;
}

Obj RIGHT_CAYLEY_GRAPH (Obj self, Obj data) {
  if (IsCCSemigroup(data) && ! IsbPRec(data, RNamName("right"))) { 
    InterfaceFromData(data)->right_cayley_graph(data);
  } else {
    ENUMERATE_SEMIGROUP(self, data, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(data, RNamName("right"));
}

Obj LEFT_CAYLEY_GRAPH (Obj self, Obj data) {
  if (IsCCSemigroup(data) && ! IsbPRec(data, RNamName("left"))) { 
    InterfaceFromData(data)->left_cayley_graph(data);
  } else {
    ENUMERATE_SEMIGROUP(self, data, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(data, RNamName("left"));
}

Obj RELATIONS_SEMIGROUP (Obj self, Obj data) {
  if (IsCCSemigroup(data) && ! IsbPRec(data, RNamName("rules"))) { 
    InterfaceFromData(data)->relations(data);
  } else {
    ENUMERATE_SEMIGROUP(self, data, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(data, RNamName("rules"));
}

Obj SIZE_SEMIGROUP (Obj self, Obj data) {
  if (IsCCSemigroup(data)) { 
    return INTOBJ_INT(InterfaceFromData(data)->size());
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
    InterfaceFromData(data)->word(data, pos);
  } else {
    ENUMERATE_SEMIGROUP(self, data, INTOBJ_INT(pos), 0, False);
  }
  return ELM_PLIST(ElmPRec(data, RNamName("words")), INT_INTOBJ(pos));
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
    return INTOBJ_INT(InterfaceFromData(data)->nrrules());
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
