/*
 * Semigroups GAP package
 *
 * This file contains . . . 
 *
 */

#include "data.h"

//TODO  data_is_valid

/*******************************************************************************
********************************************************************************
 * Get the value of a C++ object out of the data
********************************************************************************
*******************************************************************************/

/*******************************************************************************
* data_type: 
*******************************************************************************/

DataType data_type (Obj data) {
  Obj x = data_rep(data);
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
      } else if (IS_PROJ_MAX_PLUS_MAT(x)) {
        return PROJ_MAX_PLUS_MAT;
      } else if (IS_NAT_MAT(x)) {
        return NAT_MAT;
      } else if (IS_MAT_OVER_PF(x)) {
        // TODO handle non-prime fields too!
        return MAT_OVER_PF;
      } else if (IS_PBR(x)) {
        return PBR_TYPE;
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

/*******************************************************************************
* data_init: 
*******************************************************************************/

void data_init (Obj data) {
  data_init_converter(data);
  data_init_semigroup(data);
}

/*******************************************************************************
* data_init_converter: 
*******************************************************************************/

void data_init_converter (Obj data) {
  
  if (IsbPRec(data, RNam_converter)) {
    return;
  }

  Converter* converter;
  switch (data_type(data)) {
    case TRANS2:{
      converter = new TransConverter<u_int16_t>();
      break;
    }
    case TRANS4:{
      converter = new TransConverter<u_int32_t>();
      break;
    }
    case PPERM2:{
      converter = new PPermConverter<u_int16_t>();
      break;
    }
    case PPERM4:{
      converter = new PPermConverter<u_int32_t>();
      break;
    }
    case BOOL_MAT:{ 
      converter = new BoolMatConverter();
      break;
    }
    case BIPART: {
      converter = new BipartConverter();
      break;
    }
    case MAX_PLUS_MAT:{
      converter = new MatrixOverSemiringConverter(new MaxPlusSemiring(), 
                                                  Ninfinity, 
                                                  MaxPlusMatrixType);
      break;
    }
    case MIN_PLUS_MAT:{
      converter = new MatrixOverSemiringConverter(new MinPlusSemiring(), 
                                                  infinity, 
                                                  MinPlusMatrixType);
      break;
    }
    case TROP_MAX_PLUS_MAT:{
      converter = new MatrixOverSemiringConverter(new TropicalMaxPlusSemiring(data_threshold(data)), 
                                                  Ninfinity, 
                                                  TropicalMaxPlusMatrixType);
      break;
    }
    case TROP_MIN_PLUS_MAT:{
      converter = new MatrixOverSemiringConverter(new TropicalMinPlusSemiring(data_threshold(data)), 
                                                  infinity, 
                                                  TropicalMinPlusMatrixType);
      break;
    }
    case NAT_MAT:{
      converter = new MatrixOverSemiringConverter(new NaturalSemiring(data_threshold(data),
                                                                      data_period(data)), 
                                                  INTOBJ_INT(0), 
                                                  NaturalMatrixType);
      break;
    }
    case PROJ_MAX_PLUS_MAT:{
      converter = new ProjectiveMaxPlusMatrixConverter(new MaxPlusSemiring(), 
                                                        Ninfinity, 
                                                        ProjectiveMaxPlusMatrixType);
      break;

    }
    case MAT_OVER_PF:{
      converter = new MatrixOverPrimeFieldConverter(new PrimeField(data_size_ff(data)));
      break;
    }
    /*case PBR_TYPE:{
      converter = new PBRConverter();
      break;
    }*/
    default: {
      assert(false);
    }
  }

  AssPRec(data, RNam_converter, NewSemigroupsBag(converter, CONVERTER));
}

/*******************************************************************************
* data_init_semigroup: the default value for <semigroup> is nullptr, which is
* set in the header file. 
*******************************************************************************/

void data_init_semigroup (Obj data, Semigroup* semigroup) {

  if (semigroup != nullptr) {
    if (IsbPRec(data, RNam_semigroup)) {
      assert(false);
    }
    semigroup->set_batch_size(data_batch_size(data));
    AssPRec(data, RNam_semigroup, NewSemigroupsBag(semigroup, SEMIGROUP));
    return;
  }

  if (IsbPRec(data, RNam_semigroup)) {
    return;
  }
  
  data_init_converter(data);
  Converter* converter = data_converter(data);
  
  assert(IsbPRec(data, RNam_gens));
  assert(LEN_LIST(ElmPRec(data, RNam_gens)) > 0);
  
  Obj gens_gap = ElmPRec(data, RNam_gens);
  PLAIN_LIST(gens_gap);

  std::vector<Element*>* gens(new std::vector<Element*>());
  size_t degree = data_degree(data);

  for(size_t i = 1; i <= (size_t) LEN_PLIST(gens_gap); i++) {
    gens->push_back(converter->convert(ELM_PLIST(gens_gap, i), degree));
  }
    
  semigroup = new Semigroup(gens, degree);
  semigroup->set_batch_size(data_batch_size(data));

  AssPRec(data, RNam_semigroup, NewSemigroupsBag(semigroup, SEMIGROUP));
  
  for (Element* x: *gens) {
    x->really_delete();
  }
  delete gens;
}

/*******************************************************************************
* data_semigroup: 
*******************************************************************************/

Semigroup* data_semigroup (Obj data) {
  if (!IsbPRec(data, RNam_semigroup)) {
    data_init(data);
  }
  return CLASS_OBJ<Semigroup>(ElmPRec(data, RNam_semigroup));
}

/*******************************************************************************
* data_converter:
*******************************************************************************/

Converter* data_converter (Obj data) {
  if (!IsbPRec(data, RNam_converter)) {
    data_init(data);
  }
  return CLASS_OBJ<Converter>(ElmPRec(data, RNam_converter));
}

/*******************************************************************************
* data_delete
*******************************************************************************/

void data_delete (Obj data) {
  if (IsbPRec(data, RNam_semigroup)) {
    delete data_semigroup(data);
  }
  if (IsbPRec(data, RNam_converter)) {
    delete data_converter(data);
  }
}

/*******************************************************************************
********************************************************************************
 * Get the value of a GAP object out of the data
********************************************************************************
*******************************************************************************/

/*******************************************************************************
* 
*******************************************************************************/

long data_threshold (Obj data) {
  Obj x = data_rep(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_TROP_MAT(x)||IS_NAT_MAT(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
}

/*******************************************************************************
* 
*******************************************************************************/

long data_period (Obj data) {
  Obj x = data_rep(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_NAT_MAT(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2));
}

/*******************************************************************************
* 
*******************************************************************************/

long data_size_ff (Obj data) {
  Obj x = data_rep(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_MAT_OVER_PF(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);
  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
}

/*******************************************************************************
* 
*******************************************************************************/

Obj data_rep (Obj data) {
  // TODO more asserts 
  assert(IsbPRec(data, RNam_gens));
  assert(LEN_LIST(ElmPRec(data, RNam_gens)) > 0);
  return ELM_PLIST(ElmPRec(data, RNam_gens), 1);
}

/*******************************************************************************
* 
*******************************************************************************/

size_t data_batch_size (Obj data) {
  assert(IsbPRec(data, RNam_batch_size));
  assert(IS_INTOBJ(ElmPRec(data, RNam_batch_size)));
  return INT_INTOBJ(ElmPRec(data, RNam_batch_size));
}

/*******************************************************************************
* 
*******************************************************************************/

bool data_report (Obj data) {
  if (IsbPRec(data, RNam_report)) {
    assert(ElmPRec(data, RNam_report) == True || ElmPRec(data, RNam_report) == False);
    return (ElmPRec(data, RNam_report) == True ? true : false);
  }
  return false;
}

/*******************************************************************************
* 
*******************************************************************************/

size_t data_degree (Obj data) {
  //TODO add asserts 
  return INT_INTOBJ(ElmPRec(data, RNamName("degree")));
}

