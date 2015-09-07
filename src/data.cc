/*
 * Semigroups GAP package
 *
 * This file contains . . . 
 *
 */

#include "data.h"

/*******************************************************************************
********************************************************************************
 * Get the value of a GAP object out of the data
********************************************************************************
*******************************************************************************/

long inline data_data_threshold (Obj data) {
  Obj x = data_rep(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_TROP_MAT(x)||IS_NAT_MAT(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
}

long inline data_data_period (Obj data) {
  Obj x = data_rep(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_NAT_MAT(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2));
}

long inline data_size_ff (Obj data) {
  Obj x = data_rep(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(IS_MAT_OVER_PF(x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);
  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
}

Obj inline data_rep (Obj data) {
  // TODO more asserts 
  assert(IsbPRec(data, RNam_gens));
  assert(LEN_LIST(ElmPRec(data, RNam_gens)) > 0);
  return ELM_PLIST(ElmPRec(data, RNam_gens), 1);
}

size_t inline data_batch_size (Obj data) {
  assert(IsbPRec(data, RNam_batch_size));
  assert(IS_INTOBJ(ElmPRec(data, RNam_batch_size)));
  return INT_INTOBJ(ElmPRec(data, RNam_batch_size));
}

bool inline data_report (Obj data) {
  if (IsbPRec(data, RNam_report)) {
    assert(ElmPRec(data, RNam_report) == True || ElmPRec(data, RNam_report) == False);
    return (ElmPRec(data, RNam_report) == True ? true : false);
  }
  return false;
}

size_t data_degree (Obj data) {
  //TODO add asserts 
  return INT_INTOBJ(ElmPRec(data, RNamName("degree")));
}

/*******************************************************************************
********************************************************************************
 * Get the value of a C++ object out of the data
********************************************************************************
*******************************************************************************/

Semigroup* data_semigroup (Obj data) {
  if (!IsbPRec(data, RNam_semigroup)) {
    data_init(data);
  }
  return CLASS_OBJ<Semigroup>(ElmPRec(data, RNam_semigroup));
}

Converter* data_converter (Obj data) {
  if (!IsbPRec(data, RNam_converter)) {
    data_init(data);
  }
  return CLASS_OBJ<Converter>(ElmPRec(data, RNam_converter));
}

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

void data_init (Obj data) {
  assert(!IsbPRec(data, RNam_semigroup) && !IsbPRec(data, RNam_converter));

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
    /*case PPERM2:{
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
      auto mosc = new MatrixOverSemiringConverter(new TropicalMaxPlusSemiring(data_threshold(data)), 
                                                  Ninfinity, 
                                                  TropicalMaxPlusMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc, old);
      break;
    }
    case TROP_MIN_PLUS_MAT:{
      auto mosc = new MatrixOverSemiringConverter(new TropicalMinPlusSemiring(data_threshold(data)), 
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
      auto mosc = new MatrixOverSemiringConverter(new NaturalSemiring(data_threshold(data),
                                                                      data_period(data)), 
                                                  INTOBJ_INT(0), 
                                                  NaturalMatrixType);
      interface = new Interface<MatrixOverSemiring>(data, mosc, old);
      break;
    }
    case MAT_OVER_PF:{
      auto mopfc = new MatrixOverPrimeFieldConverter(new PrimeField(data_size_ff(data)));
      interface = new Interface<MatrixOverSemiring>(data, mopfc, old);
      break;
    }
    case PBR_TYPE:{
      auto pbrc = new PBRConverter();
      interface = new Interface<PBR>(data, pbrc, old);
      break;
    }*/
    default: {
      assert(false);
    }
  }

  assert(IsbPRec(data, RNam_gens));
  assert(LEN_LIST(ElmPRec(data, RNam_gens)) > 0);
  
  Obj gens_gap = ElmPRec(data, RNam_gens);
  PLAIN_LIST(gens_gap);

  std::vector<Element*>* gens(new std::vector<Element*>());
  size_t degree = data_degree(data);

  for(size_t i = 1; i <= (size_t) LEN_PLIST(gens_gap); i++) {
    gens->push_back(converter->convert(ELM_PLIST(gens_gap, i), degree));
  }
    
  Semigroup* semigroup = new Semigroup(gens, degree);
  semigroup->set_batch_size(data_batch_size(data));

  AssPRec(data, RNam_converter, NewSemigroupsBag(converter, CONVERTER));
  AssPRec(data, RNam_semigroup, NewSemigroupsBag(semigroup, SEMIGROUP));
  
  for (Element* x: *gens) {
    x->really_delete();
  }
  delete gens;
}

void data_delete (Obj data) {
  if (IsbPRec(data, RNam_semigroup)) {
    delete data_semigroup(data);
  }
  if (IsbPRec(data, RNam_converter)) {
    delete data_converter(data);
  }
}
