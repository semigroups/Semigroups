/*
 * Semigroups GAP package
 *
 * This file contains an interface between GAP and Semigroups++.
 *
 */

// TODO 
// 1) better asserts!
//
// 2) improve relations probably don't make the relations in semigroups.h and then transfer them 
// over here, just create them here using trace from semigroups.h and the
// memmove method from ENUMERATE_SEMIGROUP in this file
//
// 3) set data!.pos and data!.nr so that the filter IsClosedData gets set at
// the GAP level

#include "gap-debug.h"
#include "interface.h"

#include <assert.h>
#include <iostream>
#include <tuple>
#include <unordered_map>
#include <time.h>

using namespace semiring;

//#define DEBUG
//#define NDEBUG 

#define BATCH_SIZE 8192

#define IS_PRIME_INT(x)          (CALL_1ARGS(IsPrimeInt, INTOBJ_INT(x)) == True)

#define IS_BOOL_MAT(x)           (CALL_1ARGS(IsBooleanMat, x) == True)
#define IS_BIPART(x)             (CALL_1ARGS(IsBipartition, x) == True)
#define IS_MAT_OVER_SEMI_RING(x) (CALL_1ARGS(IsMatrixOverSemiring, x) == True)
#define IS_MAX_PLUS_MAT(x)       (CALL_1ARGS(IsMaxPlusMatrix, x) == True)
#define IS_MIN_PLUS_MAT(x)       (CALL_1ARGS(IsMinPlusMatrix, x) == True)
#define IS_TROP_MAT(x)           (CALL_1ARGS(IsTropicalMatrix, x) == True)
#define IS_TROP_MAX_PLUS_MAT(x)  (CALL_1ARGS(IsTropicalMaxPlusMatrix, x) == True)
#define IS_TROP_MIN_PLUS_MAT(x)  (CALL_1ARGS(IsTropicalMinPlusMatrix, x) == True)
#define IS_MAT_OVER_PF(x)        (CALL_1ARGS(IsMatrixOverPrimeField, x) == True)

/*******************************************************************************
 * Imported types from the library
*******************************************************************************/

Obj Objectify;
Obj IsPrimeInt;

Obj infinity;
Obj Ninfinity;

Obj IsBipartition;
Obj BipartitionByIntRepNC;   

Obj IsBooleanMat;
Obj BooleanMatType;   

Obj IsMatrixOverSemiring;

Obj IsMaxPlusMatrix;
Obj MaxPlusMatrixType;   

Obj IsMinPlusMatrix;
Obj MinPlusMatrixType;   

Obj IsTropicalMatrix;

Obj IsTropicalMinPlusMatrix;
Obj TropicalMinPlusMatrixType;   

Obj IsTropicalMaxPlusMatrix;
Obj TropicalMaxPlusMatrixType;

Obj IsMatrixOverPrimeField;
Obj AsMatrixOverPrimeFieldNC;

/*******************************************************************************
 * Temporary debug area
*******************************************************************************/

Obj IsColTrimBooleanMat (Obj self, Obj x) {
  assert(IS_BOOL_MAT(x));
  size_t n = LEN_BLIST(ELM_PLIST(x, 1));
  
  for (size_t i = 1; i < n; i++) {
    for (size_t j = i + 1; j <= n; j++) {
      size_t k;
      bool contained = true;
      for (k = 1; k <= n; k++) {
        Obj row = ELM_PLIST(x, k);
        if ((ELM_BLIST(row, j) == True && ELM_BLIST(row, i) == False)) {
          contained = false;
          break;
        }
      }
      if (contained) {
        return False;
      }
    }
  }
  return True;
}

/*******************************************************************************
 * Can we use Semigroup++?
*******************************************************************************/

Obj inline Representative (Obj data) {
  assert(IsbPRec(data, RNamName("gens")));
  assert(LEN_LIST(ElmPRec(data, RNamName("gens"))) > 0);
  return ELM_PLIST(ElmPRec(data, RNamName("gens")), 1);
}

long inline Threshold (Obj data) {
  Obj x = Representative(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_TROP_MAT(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
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

// TODO use IsMatrixOverSemiring ?
enum SemigroupType {
  UNKNOWN,
  TRANS2, 
  TRANS4, 
  PPERM2, 
  PPERM4, 
  BOOL_MAT, 
  BIPART,
  MAX_PLUS_MAT,
  MIN_PLUS_MAT,
  TROP_MAX_PLUS_MAT,
  TROP_MIN_PLUS_MAT,
  MAT_OVER_FF
};

SemigroupType TypeSemigroup (Obj data) {
  Obj x = Representative(data);
  switch (TNUM_OBJ(x)) {
    case T_TRANS2:
      return TRANS2;
    case T_TRANS4:
      return TRANS4;
    case T_PPERM2:
      return PPERM2;
    case T_PPERM4:
      return PPERM4;
    case T_POSOBJ:
      if (IS_BOOL_MAT(x)) {
        return BOOL_MAT;
      } else if (IS_MAX_PLUS_MAT(x)) {
        return MAX_PLUS_MAT;
      } else if (IS_MIN_PLUS_MAT(x)) {
        return MIN_PLUS_MAT;
      } else if (IS_TROP_MAX_PLUS_MAT(x)) {
        return TROP_MAX_PLUS_MAT;
      } else if (IS_TROP_MIN_PLUS_MAT(x)) {
        return TROP_MIN_PLUS_MAT;
        // TODO handle non-prime fields too!
      } else if (IS_MAT_OVER_PF(x) && IS_PRIME_INT(SizeOfFF(data))) {
        return MAT_OVER_FF;
      } 
      return UNKNOWN;
    case T_COMOBJ:
      if (IS_BIPART(x)) {
        return BIPART;
      }
      // intentional fall through
    default: 
      return UNKNOWN;
  }
}

bool inline IsCCSemigroup (Obj data) {
  return TypeSemigroup(data) != UNKNOWN;
}

/*******************************************************************************
 * wrap C++ semigroup object in a GAP bag for garbage collection
*******************************************************************************/

class InterfaceBase {
  public:
    virtual ~InterfaceBase () {};
    virtual void enumerate (Obj limit) = 0;
    virtual bool is_done () = 0;
    virtual void find (Obj data, Obj lookfunc, Obj start, Obj end) = 0;
    virtual size_t size () = 0;
    virtual size_t current_size () = 0;
    virtual size_t nrrules () = 0;
    virtual void right_cayley_graph (Obj data) = 0;
    virtual void left_cayley_graph (Obj data) = 0;
    virtual void elements (Obj data, Obj limit) = 0;
    virtual Obj position (Obj data, Obj x) = 0;
    virtual void word (Obj data, Obj pos) = 0;
    virtual void relations (Obj data) = 0;
};

// put C++ semigroup into GAP object

Obj OBJ_INTERFACE(InterfaceBase* interface){
  Obj o = NewBag(T_SEMI, 1 * sizeof(Obj));
  ADDR_OBJ(o)[0] = reinterpret_cast<Obj>(interface);
  return o;
}

// get C++ semigroup from GAP object

inline InterfaceBase* INTERFACE_OBJ(Obj o) {
    return reinterpret_cast<InterfaceBase*>(ADDR_OBJ(o)[0]);
}

// free C++ semigroup inside GAP object
void InterfaceFreeFunc(Obj o) { 
#ifdef DEBUG
  std::cout << "FREEING1!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
#endif
  delete INTERFACE_OBJ(o);
#ifdef DEBUG
  std::cout << "FREEING2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
#endif
}

/*******************************************************************************
 * Converters to and from GAP objects
*******************************************************************************/

// abstract base class

template <typename T>
class Converter {
  public:
    virtual ~Converter () {};
    virtual T* convert (Obj, size_t) = 0;
    virtual Obj unconvert (T*) = 0;
};

// converter between C++ transformations and GAP transformations

template <typename T>
class TransConverter : public Converter<Transformation<T> > {
  public: 

    Transformation<T>* convert (Obj o, size_t n) {
      assert(IS_TRANS(o));
      //assert(DEG_TRANS(o) <= n);
      
      auto x = new Transformation<T>(n);
      T* pto = ADDR_TRANS(o);
      T i;
      for (i = 0; i < DEG_TRANS(o); i++) {
        x->set(i, pto[i]);
      }
      for (; i < n; i++) {
        x->set(i, i);
      }
      return x;
    }

    Obj unconvert (Transformation<T>* x) {
      Obj o = NEW_TRANS(x->degree());
      T* pto = ADDR_TRANS(o);
      for (T i = 0; i < x->degree(); i++) {
        pto[i] = x->at(i);
      }
      return o;
    }

  private:
    
    inline Obj NEW_TRANS (size_t deg) {
      if (deg < 65536) {
        return NEW_TRANS2(deg);
      } else {
        return NEW_TRANS4(deg);
      }
    }

    // helper for getting ADDR_TRANS2/4
    inline T* ADDR_TRANS (Obj x) {
      return ((T*)((Obj*)(ADDR_OBJ(x))+3));
    }
};

template <typename T>
class PPermConverter : public Converter<PartialPerm<T> > {
  public: 

    PartialPerm<T>* convert (Obj o, size_t n) {
      assert(IS_PPERM(o));
      
      auto x = new PartialPerm<T>(n);
      T* pto = ADDR_PPERM(o);
      T i;
      for (i = 0; i < DEG_PPERM(o); i++) {
        x->set(i, pto[i]);
      }
      for (; i < n; i++) {
        x->set(i, 0);
      }
      return x;
    }

    // similar to FuncDensePartialPermNC in gap/src/pperm.c
    Obj unconvert (PartialPerm<T>* x) {
      T deg = x->degree(); 

      //remove trailing 0s
      while (deg > 0 && x->at(deg - 1) == 0) {
        deg--;
      }
      
      Obj o = NEW_PPERM(deg);
      T* pto = ADDR_PPERM(o);
      T codeg = 0;
      for (T i = 0; i < deg; i++) {
        pto[i] = x->at(i);
        if (pto[i] > codeg) {
          codeg = pto[i];
        }
      }
      set_codeg(o, deg, codeg);
      return o;
    }
  
  private:
   
    void set_codeg (Obj o, T deg, T codeg) {
      if (deg < 65536) {
        CODEG_PPERM2(o) = codeg;
      } else {
        CODEG_PPERM4(o) = codeg;
      }
    }

    inline Obj NEW_PPERM (size_t deg) {
      if (deg < 65536) {
        return NEW_PPERM2(deg);
      } else {
        return NEW_PPERM4(deg);
      }
    }

    // helper for getting ADDR_PPERM2/4
    inline T* ADDR_PPERM (Obj x) {
      return ((T*)((Obj*)(ADDR_OBJ(x))+2)+1);
    }
};

class BipartConverter : public Converter<Bipartition> {
  public: 

    Bipartition* convert (Obj o, size_t n) {
      assert(IS_BIPART(o));
      assert(IsbPRec(o, RNamName("blocks")));
      
      Obj blocks = ElmPRec(o, RNamName("blocks"));
      PLAIN_LIST(blocks);

      assert((size_t) LEN_PLIST(blocks) == n);
      
      auto x = new Bipartition(n);
      for (u_int32_t i = 0; i < n; i++) {
        x->set(i, INT_INTOBJ(ELM_PLIST(blocks, i + 1)) - 1);
      }
      return x;
    }

    Obj unconvert (Bipartition* x) {
      Obj o = NEW_PLIST(T_PLIST_CYC, x->degree());
      SET_LEN_PLIST(o, x->degree());
      for (u_int32_t i = 0; i < x->degree(); i++) {
        SET_ELM_PLIST(o, i + 1, INTOBJ_INT(x->at(i) + 1));
      }
      o = CALL_1ARGS(BipartitionByIntRepNC, o);
      return o;
    }
};

class BoolMatConverter : public Converter<BooleanMat> {
  public: 

    BooleanMat* convert (Obj o, size_t n) {
      assert(IS_BOOL_MAT(o));
      assert(LEN_PLIST(o) > 0);
      assert(IS_BLIST_REP(ELM_PLIST(o, 1)));
      assert(sqrt(n) == LEN_BLIST(ELM_PLIST(o, 1)));
       
      auto x = new BooleanMat(n);
      n = LEN_BLIST(ELM_PLIST(o, 1));
      for (size_t i = 0; i < n; i++) {
        Obj row = ELM_PLIST(o, i + 1);
        assert(IS_BLIST_REP(row));
        for (size_t j = 0; j < n; j++) {
          if (ELM_BLIST(row, j + 1) == True) {
            x->set(i * n + j, true);
          } else {
            x->set(i * n + j, false);
          }
        }
      }
      return x;
    }

    Obj unconvert (BooleanMat* x) {
      size_t n = sqrt(x->degree());
      Obj o = NEW_PLIST(T_PLIST, n);
      SET_LEN_PLIST(o, n);
      
      for (size_t i = 0; i < n; i++) {
        Obj blist = NewBag(T_BLIST, SIZE_PLEN_BLIST(n));
        SET_LEN_BLIST(blist, n);
        for (size_t j = 0; j < n; j++) {
          if (x->at(i * n + j)) {
            SET_ELM_BLIST(blist, j + 1, True);
          } else {
            SET_ELM_BLIST(blist, j + 1, False);
          }
        }
        SET_ELM_PLIST(o, i + 1, blist);
        CHANGED_BAG(o);
      }
      return CALL_2ARGS(Objectify, BooleanMatType, o);
    }
};

class MatrixOverSemiringConverter : public Converter<MatrixOverSemiring> {

  public:

    ~MatrixOverSemiringConverter () {
      delete _semiring;
    }

    MatrixOverSemiringConverter (Semiring* semiring, 
                                 Obj       gap_zero, 
                                 Obj       gap_type) 
      : _semiring(semiring), 
        _gap_zero(gap_zero),
        _gap_type(gap_type) {}

    MatrixOverSemiring* convert (Obj o, size_t n) {
      assert(IS_MAT_OVER_SEMI_RING(o));
      assert(LEN_PLIST(o) > 0);
      assert(IS_PLIST(ELM_PLIST(o, 1)));
      assert(sqrt(n) == LEN_PLIST(ELM_PLIST(o, 1)));
       
      auto x = new MatrixOverSemiring(n, _semiring);
      n = LEN_PLIST(ELM_PLIST(o, 1));
      for (size_t i = 0; i < n; i++) {
        Obj row = ELM_PLIST(o, i + 1);
        for (size_t j = 0; j < n; j++) {
          Obj entry = ELM_PLIST(row, j + 1);
          if (EQ(_gap_zero, entry)) {
            x->set(i * n + j, _semiring->zero());
          } else {
            x->set(i * n + j, INT_INTOBJ(entry));
          }
        }
      }
      return x;
    }

    virtual Obj unconvert (MatrixOverSemiring* x) {
      size_t n = sqrt(x->degree());
      Obj plist = NEW_PLIST(T_PLIST, n + 1);
      SET_LEN_PLIST(plist, n + 1);
      SET_ELM_PLIST(plist, n + 1, INTOBJ_INT(_semiring->threshold()));
      
      for (size_t i = 0; i < n; i++) {
        Obj row = NEW_PLIST(T_PLIST_CYC, n);
        SET_LEN_PLIST(row, n);
        for (size_t j = 0; j < n; j++) {
          long entry = x->at(i * n + j);
          if (entry == _semiring->zero()) {
            SET_ELM_PLIST(row, j + 1, _gap_zero);
          } else {
            SET_ELM_PLIST(row, j + 1, INTOBJ_INT(entry));
          }
        }
        SET_ELM_PLIST(plist, i + 1, row);
        CHANGED_BAG(plist);
      }
      return CALL_2ARGS(Objectify, _gap_type, plist);
    }

  protected: 
    
    Semiring* _semiring;
    Obj       _gap_zero;
    Obj       _gap_type;
};

class MatrixOverPrimeFieldConverter : public Converter<MatrixOverSemiring> {

  public:

    ~MatrixOverPrimeFieldConverter () {
      delete _field;
    }

    MatrixOverPrimeFieldConverter (PrimeField* field) 
      : _field(field) {}

    MatrixOverSemiring* convert (Obj o, size_t n) {
      assert(IS_MAT_OVER_PF(o));
      assert(LEN_PLIST(o) > 0);
      assert(IS_PLIST(ELM_PLIST(o, 1)));
      assert(sqrt(n) == LEN_PLIST(ELM_PLIST(o, 1)));
       
      auto x = new MatrixOverSemiring(n, _field);
      n = LEN_PLIST(ELM_PLIST(o, 1));
      for (size_t i = 0; i < n; i++) {
        Obj row = ELM_PLIST(o, i + 1);
        for (size_t j = 0; j < n; j++) {
          FFV entry = VAL_FFE(ELM_PLIST(row, j + 1));
          x->set(i * n + j, entry);
        }
      }
      return x;
    }

    virtual Obj unconvert (MatrixOverSemiring* x) {
      size_t n = sqrt(x->degree());
      Obj plist = NEW_PLIST(T_PLIST, n);
      SET_LEN_PLIST(plist, n);
      
      for (size_t i = 0; i < n; i++) {
        Obj row = NEW_PLIST(T_PLIST_CYC, n);
        SET_LEN_PLIST(row, n);
        for (size_t j = 0; j < n; j++) {
          long entry = x->at(i * n + j);
          SET_ELM_PLIST(row, j + 1, INTOBJ_INT(x->at(i * n + j)));
        }
        SET_ELM_PLIST(plist, i + 1, row);
        CHANGED_BAG(plist);
      }
      return CALL_2ARGS(AsMatrixOverPrimeFieldNC, INTOBJ_INT(_field->size()), plist);
    }

  protected: 
    
    PrimeField* _field;
};


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
    return INTERFACE_OBJ(ElmPRec(data, RNamName("Interface_CC")));
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
      auto tmc = new MatrixOverSemiringConverter(new TropicalMaxPlusSemiring(Threshold(data)), 
                                                 Ninfinity, 
                                                 TropicalMaxPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, tmc);
      break;
    }
    case TROP_MIN_PLUS_MAT:{
      auto tmc = new MatrixOverSemiringConverter(new TropicalMinPlusSemiring(Threshold(data)), 
                                                 infinity, 
                                                 TropicalMinPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, tmc);
      break;
    }
    case MAT_OVER_FF:{
      auto mopfc = new MatrixOverPrimeFieldConverter(new PrimeField(SizeOfFF(data)));
      interface = new Interface<MatrixOverSemiring>(data, mopfc);
      break;
    }
    default: {
      assert(false);
    }
  }
  AssPRec(data, RNamName("Interface_CC"), OBJ_INTERFACE(interface));
  return interface;
}

/*******************************************************************************
 * GAP level functions
*******************************************************************************/

Obj ENUMERATE_SEMIGROUP (Obj self, Obj data, Obj limit, Obj lookfunc, Obj looking);

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

/*******************************************************************************
 * GAP kernel version of the algorithm for other types of semigroups
*******************************************************************************/

// macros for the GAP version of the algorithm 

#define ELM_PLIST2(plist, i, j)       ELM_PLIST(ELM_PLIST(plist, i), j)
#define INT_PLIST(plist, i)           INT_INTOBJ(ELM_PLIST(plist, i))
#define INT_PLIST2(plist, i, j)       INT_INTOBJ(ELM_PLIST2(plist, i, j))

inline void SET_ELM_PLIST2(Obj plist, UInt i, UInt j, Obj val) {
  SET_ELM_PLIST(ELM_PLIST(plist, i), j, val);
  SET_LEN_PLIST(ELM_PLIST(plist, i), j);
  CHANGED_BAG(plist);
}

// assumes the length of data!.elts is at most 2^28
// TODO move this into the previous section, and put the actual function in another function

Obj ENUMERATE_SEMIGROUP (Obj self, Obj data, Obj limit, Obj lookfunc, Obj looking) {
  Obj   found, elts, gens, genslookup, right, left, first, final, prefix, suffix, 
        reduced, words, ht, rules, lenindex, newElt, newword, objval, newrule,
        empty, oldword, x;
  UInt  i, nr, len, stopper, nrrules, b, s, r, p, j, k, int_limit, nrgens,
        intval, stop, one;

  if (IsCCSemigroup(data)) {
    InterfaceFromData(data)->enumerate(limit);
    return data;
  }

  // TODO if looking check that something in elts doesn't already satisfy the
  // lookfunc 

  //remove nrrules 
  if(looking==True){
    AssPRec(data, RNamName("found"), False);
  }

  i = INT_INTOBJ(ElmPRec(data, RNamName("pos")));
  nr = INT_INTOBJ(ElmPRec(data, RNamName("nr")));

  if (i > nr || (size_t) INT_INTOBJ(limit) <= nr) {
    return data;
  }
  int_limit = std::max((size_t) INT_INTOBJ(limit), (size_t) (nr + BATCH_SIZE));

//#ifdef DEBUG
  std::cout << "GAP kernel version\n";
//#endif
  
  #ifdef DEBUG
    Pr("here 1\n", 0L, 0L);
  #endif 
  // get everything out of <data>
  
  //lists of integers, objects
  elts = ElmPRec(data, RNamName("elts")); 
  // the so far enumerated elements
  gens = ElmPRec(data, RNamName("gens")); 
  // the generators
  genslookup = ElmPRec(data, RNamName("genslookup"));     
  // genslookup[i]=Position(elts, gens[i], this is not always <i+1>!
  lenindex = ElmPRec(data, RNamName("lenindex"));         
  // lenindex[len]=position in <words> and <elts> of first element of length
  // <len> 
  first = ElmPRec(data, RNamName("first"));
  // elts[i]=gens[first[i]]*elts[suffix[i]], first letter 
  final = ElmPRec(data, RNamName("final"));
  // elts[i]=elts[prefix[i]]*gens[final[i]]
  prefix = ElmPRec(data, RNamName("prefix"));
  // see final, 0 if prefix is empty i.e. elts[i] is a gen
  suffix = ElmPRec(data, RNamName("suffix"));             
  // see first, 0 if suffix is empty i.e. elts[i] is a gen
  
  #ifdef DEBUG
    Pr("here 2\n", 0L, 0L);
  #endif 
  //lists of lists
  right = ElmPRec(data, RNamName("right")); 
  // elts[right[i][j]]=elts[i]*gens[j], right Cayley graph
  left = ElmPRec(data, RNamName("left"));                 
  // elts[left[i][j]]=gens[j]*elts[i], left Cayley graph
  reduced = ElmPRec(data, RNamName("reduced"));           
  // words[right[i][j]] is reduced if reduced[i][j]=true
  words = ElmPRec(data, RNamName("words"));
  // words[i] is a word in the gens equal to elts[i]
  rules = ElmPRec(data, RNamName("rules"));               
  if(TNUM_OBJ(rules)==T_PLIST_EMPTY){
    RetypeBag(rules, T_PLIST_CYC);
  }
  // the relations
 
  //hash table
  ht = ElmPRec(data, RNamName("ht"));                     
  // HTValue(ht, x)=Position(elts, x)
 
  #ifdef DEBUG
    Pr("here 3\n", 0L, 0L);
  #endif 
  //integers
  len=INT_INTOBJ(ElmPRec(data, RNamName("len"))); 
  // current word length
  if(IS_INTOBJ(ElmPRec(data, RNamName("one")))){
    one=INT_INTOBJ(ElmPRec(data, RNamName("one")));
  } else {
    one=0;
  }
  // <elts[one]> is the mult. neutral element
  stopper=INT_INTOBJ(ElmPRec(data, RNamName("stopper")));
  // stop when we have applied generators to elts[stopper] 
  nrrules=INT_INTOBJ(ElmPRec(data, RNamName("nrrules")));           
  // Length(rules)
  
  nrgens=LEN_PLIST(gens);
  stop = 0;
  found = False;
  
  #ifdef DEBUG
    Pr("here 4\n", 0L, 0L);
  #endif 

  while(i<=nr&&!stop){
    while(i<=nr&&(UInt) LEN_PLIST(ELM_PLIST(words, i))==len&&!stop){
      b=INT_INTOBJ(ELM_PLIST(first, i)); 
      s=INT_INTOBJ(ELM_PLIST(suffix, i));
      RetypeBag(ELM_PLIST(right, i), T_PLIST_CYC); //from T_PLIST_EMPTY
      for(j = 1;j <= nrgens;j++){
        #ifdef DEBUG
          Pr("i=%d\n", (Int) i, 0L);
          Pr("j=%d\n", (Int) j, 0L);
        #endif
        if(s != 0&&ELM_PLIST2(reduced, s, j) == False){
          r = INT_PLIST2(right, s, j);
          if(INT_PLIST(prefix, r) != 0){
            #ifdef DEBUG
              Pr("Case 1!\n", 0L, 0L);
            #endif
            intval = INT_PLIST2(left, INT_PLIST(prefix, r), b);
            SET_ELM_PLIST2(right, i, j, ELM_PLIST2(right, intval, INT_PLIST(final, r)));
            SET_ELM_PLIST2(reduced, i, j, False);
          } else if (r == one){
            #ifdef DEBUG
              Pr("Case 2!\n", 0L, 0L);
            #endif
            SET_ELM_PLIST2(right, i, j, ELM_PLIST(genslookup, b));
            SET_ELM_PLIST2(reduced, i, j, False);
          } else {
            #ifdef DEBUG
              Pr("Case 3!\n", 0L, 0L);
            #endif
            SET_ELM_PLIST2(right, i, j, 
              ELM_PLIST2(right, INT_PLIST(genslookup, b), INT_PLIST(final, r)));
            SET_ELM_PLIST2(reduced, i, j, False);
          }
        } else {
          newElt = PROD(ELM_PLIST(elts, i), ELM_PLIST(gens, j)); 
          oldword = ELM_PLIST(words, i);
          len = LEN_PLIST(oldword);
          newword = NEW_PLIST(T_PLIST_CYC, len+1);

          memcpy( (void *)((char *)(ADDR_OBJ(newword)) + sizeof(Obj)), 
                  (void *)((char *)(ADDR_OBJ(oldword)) + sizeof(Obj)), 
                  (size_t)(len*sizeof(Obj)) );
          SET_ELM_PLIST(newword, len+1, INTOBJ_INT(j));
          SET_LEN_PLIST(newword, len+1);

          objval = HTValue_TreeHash_C(self, ht, newElt); 
          if(objval!=Fail){
            #ifdef DEBUG
              Pr("Case 4!\n", 0L, 0L);
            #endif
            newrule = NEW_PLIST(T_PLIST, 2);
            SET_ELM_PLIST(newrule, 1, newword);
            SET_ELM_PLIST(newrule, 2, ELM_PLIST(words, INT_INTOBJ(objval)));
            SET_LEN_PLIST(newrule, 2);
            CHANGED_BAG(newrule);
            nrrules++;
            AssPlist(rules, nrrules, newrule);
            SET_ELM_PLIST2(right, i, j, objval);
          } else {
            #ifdef DEBUG
              Pr("Case 5!\n", 0L, 0L);
            #endif
            nr++;
            
            HTAdd_TreeHash_C(self, ht, newElt, INTOBJ_INT(nr));

            if(one==0){
              one=nr;
              for(k=1;k<=nrgens;k++){
                x = ELM_PLIST(gens, k);
                if(!EQ(PROD(newElt, x), x)){
                  one=0;
                  break;
                }
                if(!EQ(PROD(x, newElt), x)){
                  one=0;
                  break;
                }
              }
            }

            if(s!=0){
              AssPlist(suffix, nr, ELM_PLIST2(right, s, j));
            } else {
              AssPlist(suffix, nr, ELM_PLIST(genslookup, j));
            }
          
            AssPlist(elts, nr, newElt);
            AssPlist(words, nr, newword);
            AssPlist(first, nr, INTOBJ_INT(b));
            AssPlist(final, nr, INTOBJ_INT(j));
            AssPlist(prefix, nr, INTOBJ_INT(i));
            
            empty = NEW_PLIST(T_PLIST_EMPTY, nrgens);
            SET_LEN_PLIST(empty, 0);
            AssPlist(right, nr, empty);
            
            empty = NEW_PLIST(T_PLIST_EMPTY, nrgens);
            SET_LEN_PLIST(empty, 0);
            AssPlist(left, nr, empty);
            
            empty = NEW_PLIST(T_PLIST_CYC, nrgens);
            for(k=1;k<=nrgens;k++){
              SET_ELM_PLIST(empty, k, False);
            }
            SET_LEN_PLIST(empty, nrgens);
            AssPlist(reduced, nr, empty);
            
            SET_ELM_PLIST2(reduced, i, j, True);
            SET_ELM_PLIST2(right, i, j, INTOBJ_INT(nr));            
            if(looking==True&&found==False&&
                CALL_2ARGS(lookfunc, data, INTOBJ_INT(nr))==True){
                found=True;
                stop=1;
                AssPRec(data,  RNamName("found"), INTOBJ_INT(nr)); 
            } else {
              stop=(nr>=int_limit);
            }
          }
        }
      }//finished applying gens to <elts[i]>
      stop=(stop||i==stopper);
      i++;
    }//finished words of length <len> or <stop>
    #ifdef DEBUG
      Pr("finished processing words of len %d\n", (Int) len, 0L);
    #endif
    if(i>nr||(UInt)LEN_PLIST(ELM_PLIST(words, i))!=len){
      if(len>1){
        #ifdef DEBUG
          Pr("Case 6!\n", 0L, 0L);
        #endif
        for(j=INT_INTOBJ(ELM_PLIST(lenindex, len));j<=i-1;j++){ 
          RetypeBag(ELM_PLIST(left, j), T_PLIST_CYC); //from T_PLIST_EMPTY
          p=INT_INTOBJ(ELM_PLIST(prefix, j)); 
          b=INT_INTOBJ(ELM_PLIST(final, j));
          for(k=1;k<=nrgens;k++){ 
            SET_ELM_PLIST2(left, j, k, ELM_PLIST2(right, INT_PLIST2(left, p, k), b));
          }
        }
      } else if(len==1){ 
        #ifdef DEBUG
          Pr("Case 7!\n", 0L, 0L);
        #endif
        for(j=INT_INTOBJ(ELM_PLIST(lenindex, len));j<=i-1;j++){ 
          RetypeBag(ELM_PLIST(left, j), T_PLIST_CYC); //from T_PLIST_EMPTY
          b=INT_INTOBJ(ELM_PLIST(final, j));
          for(k=1;k<=nrgens;k++){ 
            SET_ELM_PLIST2(left, j, k, ELM_PLIST2(right, INT_PLIST(genslookup, k) , b));
          }
        }
      }
      len++;
      AssPlist(lenindex, len, INTOBJ_INT(i));  
    }
  }
  
  AssPRec(data, RNamName("nr"), INTOBJ_INT(nr));  
  AssPRec(data, RNamName("nrrules"), INTOBJ_INT(nrrules));  
  AssPRec(data, RNamName("one"), ((one!=0)?INTOBJ_INT(one):False));  
  AssPRec(data, RNamName("pos"), INTOBJ_INT(i));  
  AssPRec(data, RNamName("len"), INTOBJ_INT(len));  

  CHANGED_BAG(data); 
  
  return data;
}

/****************************************************************************
**
*F  FuncGABOW_SCC
**
** `digraph' should be a list whose entries and the lists of out-neighbours
** of the vertices. So [[2,3],[1],[2]] represents the graph whose edges are
** 1->2, 1->3, 2->1 and 3->2.
**
** returns a newly constructed record with two components 'comps' and 'id' the
** elements of 'comps' are lists representing the strongly connected components
** of the directed graph, and in the component 'id' the following holds:
** id[i]=PositionProperty(comps, x-> i in x);
** i.e. 'id[i]' is the index in 'comps' of the component containing 'i'.  
** Neither the components, nor their elements are in any particular order.
**
** The algorithm is that of Gabow, based on the implementation in Sedgwick:
**   http://algs4.cs.princeton.edu/42directed/GabowSCC.java.html
** (made non-recursive to avoid problems with stack limits) and 
** the implementation of STRONGLY_CONNECTED_COMPONENTS_DIGRAPH in listfunc.c.
*/

Obj SEMIGROUPS_GABOW_SCC (Obj self, Obj digraph) {
  UInt end1, end2, count, level, w, v, n, nr, idw, *frames, *stack2;
  Obj  id, stack1, out, comp, comps, adj; 
 
  PLAIN_LIST(digraph);
  n = LEN_PLIST(digraph);
  if (n == 0){
    out = NEW_PREC(2);
    AssPRec(out, RNamName("id"), NEW_PLIST(T_PLIST_EMPTY+IMMUTABLE,0));
    AssPRec(out, RNamName("comps"), NEW_PLIST(T_PLIST_EMPTY+IMMUTABLE,0));
    CHANGED_BAG(out);
    return out;
  }

  end1 = 0; 
  stack1 = NEW_PLIST(T_PLIST_CYC, n); 
  //stack1 is a plist so we can use memcopy below
  SET_LEN_PLIST(stack1, n);
  
  id = NEW_PLIST(T_PLIST_CYC+IMMUTABLE, n);
  SET_LEN_PLIST(id, n);
  
  //init id
  for(v=1;v<=n;v++){
    SET_ELM_PLIST(id, v, INTOBJ_INT(0));
  }
  
  count = n;
  
  comps = NEW_PLIST(T_PLIST_TAB+IMMUTABLE, n);
  SET_LEN_PLIST(comps, 0);
  
  stack2 = static_cast<UInt*>(malloc( (4 * n + 2) * sizeof(UInt) ));
  frames = stack2 + n + 1;
  end2 = 0;
  
  for (v = 1; v <= n; v++) {
    if(INT_INTOBJ(ELM_PLIST(id, v)) == 0){
      adj =  ELM_PLIST(digraph, v);
      PLAIN_LIST(adj);
      level = 1;
      frames[0] = v; // vertex
      frames[1] = 1; // index
      frames[2] = (UInt)adj; 
      SET_ELM_PLIST(stack1, ++end1, INTOBJ_INT(v));
      stack2[++end2] = end1;
      SET_ELM_PLIST(id, v, INTOBJ_INT(end1)); 
      
      while (1) {
        if (frames[1] > (UInt) LEN_PLIST(frames[2])) {
          if (stack2[end2] == (UInt) INT_INTOBJ(ELM_PLIST(id, frames[0]))) {
            end2--;
            count++;
            nr = 0;
            do{
              nr++;
              w = INT_INTOBJ(ELM_PLIST(stack1, end1--));
              SET_ELM_PLIST(id, w, INTOBJ_INT(count));
            }while (w != frames[0]);
            
            comp = NEW_PLIST(T_PLIST_CYC+IMMUTABLE, nr);
            SET_LEN_PLIST(comp, nr);
           
            memcpy( (void *)((char *)(ADDR_OBJ(comp)) + sizeof(Obj)), 
                    (void *)((char *)(ADDR_OBJ(stack1)) + (end1 + 1) * sizeof(Obj)), 
                    (size_t)(nr * sizeof(Obj)));

            nr = LEN_PLIST(comps) + 1;
            SET_ELM_PLIST(comps, nr, comp);
            SET_LEN_PLIST(comps, nr);
            CHANGED_BAG(comps);
          }
          level--;
          if (level == 0) {
            break;
          }
          frames -= 3;
        } else {
          
          w = INT_INTOBJ(ELM_PLIST(frames[2], frames[1]++));
          idw = INT_INTOBJ(ELM_PLIST(id, w));
          
          if(idw==0){
            adj = ELM_PLIST(digraph, w);
            PLAIN_LIST(adj);
            level++;
            frames += 3; 
            frames[0] = w; // vertex
            frames[1] = 1; // index
            frames[2] = (UInt) adj;
            SET_ELM_PLIST(stack1, ++end1, INTOBJ_INT(w));
            stack2[++end2] = end1;
            SET_ELM_PLIST(id, w, INTOBJ_INT(end1)); 
          } else {
            while (stack2[end2] > idw) {
              end2--; // pop from stack2
            }
          }
        }
      }
    }
  }

  for (v = 1; v <= n; v++) {
    SET_ELM_PLIST(id, v, INTOBJ_INT(INT_INTOBJ(ELM_PLIST(id, v)) - n));
  }

  out = NEW_PREC(2);
  SHRINK_PLIST(comps, LEN_PLIST(comps));
  AssPRec(out, RNamName("id"), id);
  AssPRec(out, RNamName("comps"), comps);
  CHANGED_BAG(out);
  free(stack2);
  return out;
}

// using the output of GABOW_SCC on the right and left Cayley graphs of a
// semigroup, the following function calculates the strongly connected
// components of the union of these two graphs.

Obj SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS(Obj self, Obj scc1, Obj scc2) {
  UInt  *ptr;
  Int   i, n, len, nr, j, k, l;
  Obj   comps1, id2, comps2, id, comps, seen, comp1, comp2, new_comp, x, out;

  n = LEN_PLIST(ElmPRec(scc1, RNamName("id")));
  
  if (n == 0){
    out = NEW_PREC(2);
    AssPRec(out, RNamName("id"), NEW_PLIST(T_PLIST_EMPTY+IMMUTABLE,0));
    AssPRec(out, RNamName("comps"), NEW_PLIST(T_PLIST_EMPTY+IMMUTABLE,0));
    CHANGED_BAG(out);
    return out;
  }

  comps1 = ElmPRec(scc1, RNamName("comps"));
  id2 = ElmPRec(scc2, RNamName("id"));
  comps2 = ElmPRec(scc2, RNamName("comps"));
   
  id = NEW_PLIST(T_PLIST_CYC+IMMUTABLE, n);
  SET_LEN_PLIST(id, n);
  seen = NewBag(T_DATOBJ, (LEN_PLIST(comps2)+1)*sizeof(UInt));
  ptr = (UInt *)ADDR_OBJ(seen);
  
  //init id
  for (i = 1; i <= n; i++) {
    SET_ELM_PLIST(id, i, INTOBJ_INT(0));
    ptr[i] = 0;
  }
  
  comps = NEW_PLIST(T_PLIST_TAB + IMMUTABLE, LEN_PLIST(comps1));
  SET_LEN_PLIST(comps, 0);
 
  nr = 0;
  
  for (i = 1; i <= LEN_PLIST(comps1); i++) {
    comp1 = ELM_PLIST(comps1, i);
    if (INT_INTOBJ(ELM_PLIST(id, INT_INTOBJ(ELM_PLIST(comp1, 1)))) == 0) {
      nr++;
      new_comp = NEW_PLIST(T_PLIST_CYC+IMMUTABLE, LEN_PLIST(comp1));
      SET_LEN_PLIST(new_comp, 0);
      for (j = 1; j <= LEN_PLIST(comp1); j++) {
        k = INT_INTOBJ(ELM_PLIST(id2, INT_INTOBJ(ELM_PLIST(comp1, j))));
        if((UInt *)ADDR_OBJ(seen)[k] == 0){
          ((UInt *)ADDR_OBJ(seen))[k] = 1;
          comp2 = ELM_PLIST(comps2, k);
          for(l = 1; l <= LEN_PLIST(comp2); l++) {
            x = ELM_PLIST(comp2, l);
            SET_ELM_PLIST(id, INT_INTOBJ(x), INTOBJ_INT(nr));
            len = LEN_PLIST(new_comp);
            AssPlist(new_comp, len + 1, x);
            SET_LEN_PLIST(new_comp, len + 1);
          }
        }
      }
      SHRINK_PLIST(new_comp, LEN_PLIST(new_comp));
      len = LEN_PLIST(comps) + 1;
      SET_ELM_PLIST(comps, len, new_comp);
      SET_LEN_PLIST(comps, len);
      CHANGED_BAG(comps);
    }
  }

  out = NEW_PREC(2);
  SHRINK_PLIST(comps, LEN_PLIST(comps));
  AssPRec(out, RNamName("id"), id);
  AssPRec(out, RNamName("comps"), comps);
  CHANGED_BAG(out);
  return out;
}

// <right> and <left> should be scc data structures for the right and left
// Cayley graphs of a semigroup, as produced by GABOW_SCC. This function find
// the H-classes of the semigroup from <right> and <left>. The method used is
// that described in:
// http://www.liafa.jussieu.fr/~jep/PDF/Exposes/StAndrews.pdf

Obj FIND_HCLASSES(Obj self, Obj right, Obj left){ 
  UInt  *nextpos, *sorted, *lookup, init;
  Int   n, nrcomps, i, hindex, rindex, j, k, len;
  Obj   rightid, leftid, comps, buf, id, out, comp;
  
  rightid = ElmPRec(right, RNamName("id"));
  leftid = ElmPRec(left, RNamName("id"));
  n = LEN_PLIST(rightid);
  
  if (n == 0){
    out = NEW_PREC(2);
    AssPRec(out, RNamName("id"), NEW_PLIST(T_PLIST_EMPTY+IMMUTABLE,0));
    AssPRec(out, RNamName("comps"), NEW_PLIST(T_PLIST_EMPTY+IMMUTABLE,0));
    CHANGED_BAG(out);
    return out;
  }
  comps = ElmPRec(right, RNamName("comps"));
  nrcomps = LEN_PLIST(comps);
  
  buf = NewBag(T_DATOBJ, (2*n+nrcomps+1)*sizeof(UInt));
  nextpos = (UInt *)ADDR_OBJ(buf);
  
  nextpos[1] = 1;
  for(i=2;i<=nrcomps;i++){
    nextpos[i]=nextpos[i-1]+LEN_PLIST(ELM_PLIST(comps, i-1));
  }
  
  sorted = (UInt *)ADDR_OBJ(buf) + nrcomps;
  lookup = (UInt *)ADDR_OBJ(buf) + nrcomps + n;
  for(i = 1;i <= n; i++){
    j = INT_INTOBJ(ELM_PLIST(rightid, i));
    sorted[nextpos[j]] = i;
    nextpos[j]++;
    lookup[i] = 0;
  }
  
  id = NEW_PLIST(T_PLIST_CYC+IMMUTABLE, n);
  SET_LEN_PLIST(id, n);
  comps = NEW_PLIST(T_PLIST_TAB+IMMUTABLE, n);
  SET_LEN_PLIST(comps, 0);
  
  sorted = (UInt *)ADDR_OBJ(buf) + nrcomps;
  lookup = (UInt *)ADDR_OBJ(buf) + nrcomps + n;

  hindex = 0;
  rindex = 0;
  init = 0;
  
  for(i=1;i<=n;i++){
    j = sorted[i];
    k = INT_INTOBJ(ELM_PLIST(rightid, j));
    if(k > rindex){
      rindex = k;
      init = hindex;
    }
    k = INT_INTOBJ(ELM_PLIST(leftid, j));
    if(lookup[k]<=init){
      hindex++;
      lookup[k] = hindex;

      comp = NEW_PLIST(T_PLIST_CYC+IMMUTABLE, 1);
      SET_LEN_PLIST(comp, 0);
      SET_ELM_PLIST(comps, hindex, comp);
      SET_LEN_PLIST(comps, hindex);
      CHANGED_BAG(comps);

      sorted = (UInt *)ADDR_OBJ(buf) + nrcomps;
      lookup = (UInt *)ADDR_OBJ(buf) + nrcomps + n;
    }
    k = lookup[k];
    comp = ELM_PLIST(comps, k);
    len = LEN_PLIST(comp) + 1;
    AssPlist(comp, len, INTOBJ_INT(j)); 
    SET_LEN_PLIST(comp, len);
    
    SET_ELM_PLIST(id, j, INTOBJ_INT(k));
  }

  SHRINK_PLIST(comps, LEN_PLIST(comps));
  for(i=1;i<=LEN_PLIST(comps);i++){
    comp = ELM_PLIST(comps, i);
    SHRINK_PLIST(comp, LEN_PLIST(comp));
  }
  
  out = NEW_PREC(2);
  AssPRec(out, RNamName("id"), id);
  AssPRec(out, RNamName("comps"), comps);
  CHANGED_BAG(out);

  return out;
}

/*******************************************************************************
 * GAP level print for a T_SEMI
*******************************************************************************/

void PrintSemi (Obj o) {
  Pr("<interface to C++ semigroup object>", 0L, 0L);
}

/*****************************************************************************/

typedef Obj (* GVarFunc)(/*arguments*/);

#define GVAR_FUNC_TABLE_ENTRY(srcfile, name, nparam, params) \
  {#name, nparam, \
   params, \
   (GVarFunc)name, \
   srcfile ":Func" #name }

// Table of functions to export
static StructGVarFunc GVarFuncs [] = {
    GVAR_FUNC_TABLE_ENTRY("interface.c", IsColTrimBooleanMat, 1, 
                          "x"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", ENUMERATE_SEMIGROUP, 4, 
                          "data, limit, lookfunc, looking"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", FIND_SEMIGROUP, 4, 
                          "data, lookfunc, start, end"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", RIGHT_CAYLEY_GRAPH, 1, 
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", LEFT_CAYLEY_GRAPH, 1, 
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", ELEMENTS_SEMIGROUP, 2, 
                          "data, limit"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", RELATIONS_SEMIGROUP, 1, 
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", WORD_SEMIGROUP, 2, 
                          "data, pos"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", SIZE_SEMIGROUP, 1, 
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", LENGTH_SEMIGROUP, 1, 
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", NR_RULES_SEMIGROUP, 1, 
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", POSITION_SEMIGROUP, 2, 
                          "data, x"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", IS_CLOSED_SEMIGROUP, 1, 
                          "data"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", SEMIGROUPS_GABOW_SCC, 1, 
                          "digraph"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", SCC_UNION_LEFT_RIGHT_CAYLEY_GRAPHS, 2, 
                          "scc1, scc2"),
    GVAR_FUNC_TABLE_ENTRY("interface.c", FIND_HCLASSES, 2, 
                          "left, right"),
    { 0, 0, 0, 0, 0 } /* Finish with an empty entry */
};

/******************************************************************************
*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel( StructInitInfo *module )
{
    /* init filters and functions                                          */
    InitHdlrFuncsFromTable( GVarFuncs );
    InfoBags[T_SEMI].name = "Semigroups package C++ type";
    PrintObjFuncs[T_SEMI] = PrintSemi;
    InitMarkFuncBags(T_SEMI, &MarkNoSubBags);
    InitFreeFuncBag(T_SEMI, &InterfaceFreeFunc);
    
    ImportGVarFromLibrary( "Objectify", &Objectify);
    ImportGVarFromLibrary( "IsPrimeInt", &IsPrimeInt);

    ImportGVarFromLibrary( "infinity", &infinity);
    ImportGVarFromLibrary( "Ninfinity", &Ninfinity);

    ImportGVarFromLibrary( "IsBipartition", &IsBipartition );
    ImportGVarFromLibrary( "BipartitionByIntRepNC", &BipartitionByIntRepNC );

    ImportGVarFromLibrary( "IsBooleanMat", &IsBooleanMat );
    ImportGVarFromLibrary( "BooleanMatType", &BooleanMatType );

    ImportGVarFromLibrary( "IsMatrixOverSemiring", &IsMatrixOverSemiring);

    ImportGVarFromLibrary( "IsMaxPlusMatrix", &IsMaxPlusMatrix );
    ImportGVarFromLibrary( "MaxPlusMatrixType", &MaxPlusMatrixType );
    
    ImportGVarFromLibrary( "IsMinPlusMatrix", &IsMinPlusMatrix );
    ImportGVarFromLibrary( "MinPlusMatrixType", &MinPlusMatrixType );
    
    ImportGVarFromLibrary( "IsTropicalMatrix", &IsTropicalMatrix );

    ImportGVarFromLibrary( "IsTropicalMaxPlusMatrix", &IsTropicalMaxPlusMatrix );
    ImportGVarFromLibrary( "TropicalMaxPlusMatrixType", &TropicalMaxPlusMatrixType );
    
    ImportGVarFromLibrary( "IsTropicalMinPlusMatrix", &IsTropicalMinPlusMatrix );
    ImportGVarFromLibrary( "TropicalMinPlusMatrixType", &TropicalMinPlusMatrixType );

    ImportGVarFromLibrary( "IsMatrixOverPrimeField", &IsMatrixOverPrimeField );
    ImportGVarFromLibrary( "AsMatrixOverPrimeFieldNC", &AsMatrixOverPrimeFieldNC );

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitLibrary( <module> ) . . . . . . .  initialise library data structures
*/
static Int InitLibrary( StructInitInfo *module )
{
    /* init filters and functions */
    InitGVarFuncsFromTable( GVarFuncs );

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitInfopl()  . . . . . . . . . . . . . . . . . table of init functions
*/
static StructInitInfo module = {
 /* type        = */ MODULE_DYNAMIC,
 /* name        = */ "semigroups",
 /* revision_c  = */ 0,
 /* revision_h  = */ 0,
 /* version     = */ 0,
 /* crc         = */ 0,
 /* initKernel  = */ InitKernel,
 /* initLibrary = */ InitLibrary,
 /* checkInit   = */ 0,
 /* preSave     = */ 0,
 /* postSave    = */ 0,
 /* postRestore = */ 0,
 /* filename    = */ (char*) "pkg/semigroups/src/interface.cc",
 /* isGapRootR  = */ true
};

extern "C"
StructInitInfo * Init__Dynamic ( void )
{
  return &module;
}
