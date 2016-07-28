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

  if (IS_TRANS(x)) {
    if (data_degree(data) < 65536) {
      return TRANS2;
    } else {
      return TRANS4;
    }
  }

  if (IS_PPERM(x)) {
    if (data_degree(data) < 65535) {
      return PPERM2;
    } else {
      return PPERM4;
    }
  }
  if (TNUM_OBJ(x) == T_BIPART) {
    return BIPART;
  }

  switch (TNUM_OBJ(x)) {
    case T_POSOBJ:
      if (CALL_1ARGS(IsBooleanMat, x) == True) {
        return BOOL_MAT;
      } else if (CALL_1ARGS(IsMaxPlusMatrix, x) == True) {
        return MAX_PLUS_MAT;
      } else if (CALL_1ARGS(IsMinPlusMatrix, x) == True) {
        return MIN_PLUS_MAT;
      } else if (CALL_1ARGS(IsTropicalMaxPlusMatrix, x) == True) {
        return TROP_MAX_PLUS_MAT;
      } else if (CALL_1ARGS(IsTropicalMinPlusMatrix, x) == True) {
        return TROP_MIN_PLUS_MAT;
      } else if (CALL_1ARGS(IsProjectiveMaxPlusMatrix, x) == True) {
        return PROJ_MAX_PLUS_MAT;
      } else if (CALL_1ARGS(IsNTPMatrix, x) == True) {
        return NTP_MAT;
      } else if (CALL_1ARGS(IsIntegerMatrix, x) == True) {
        return INT_MAT;
      } else if (CALL_1ARGS(IsPBR, x) == True) {
        return PBR_TYPE;
      }
      return UNKNOWN;
      // intentional fall through
    default:
      return UNKNOWN;
  }
}

bool data_has_cpp_semigroup (Obj data) {
  return IsbPRec(data, RNam_semigroup)
    && CLASS_OBJ<Semigroup>(ElmPRec(data, RNam_semigroup)) != nullptr;
}

bool data_has_cpp_converter (Obj data) {
  return IsbPRec(data, RNam_converter)
    && CLASS_OBJ<Converter>(ElmPRec(data, RNam_converter)) != nullptr;
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

  initRNams();

  if (data_has_cpp_converter(data)) {
    return;
  }

  Converter* converter;
  switch (data_type(data)) {
    case TRANS2: {
      converter = new TransConverter<u_int16_t>();
      break;
    }
    case TRANS4: {
      converter = new TransConverter<u_int32_t>();
      break;
    }
    case PPERM2: {
      converter = new PPermConverter<u_int16_t>();
      break;
    }
    case PPERM4: {
      converter = new PPermConverter<u_int32_t>();
      break;
    }
    case BOOL_MAT: {
      converter = new BoolMatConverter();
      break;
    }
    case BIPART: {
      converter = new BipartConverter();
      break;
    }
    case MAX_PLUS_MAT: {
      converter = new MatrixOverSemiringConverter(new MaxPlusSemiring(),
                                                  Ninfinity,
                                                  MaxPlusMatrixType);
      break;
    }
    case MIN_PLUS_MAT: {
      converter = new MatrixOverSemiringConverter(new MinPlusSemiring(),
                                                  infinity,
                                                  MinPlusMatrixType);
      break;
    }
    case TROP_MAX_PLUS_MAT: {
      converter = new MatrixOverSemiringConverter(new TropicalMaxPlusSemiring(data_threshold(data)),
                                                  Ninfinity,
                                                  TropicalMaxPlusMatrixType);
      break;
    }
    case TROP_MIN_PLUS_MAT: {
      converter = new MatrixOverSemiringConverter(new TropicalMinPlusSemiring(data_threshold(data)),
                                                  infinity,
                                                  TropicalMinPlusMatrixType);
      break;
    }
    case NTP_MAT: {
      converter = new MatrixOverSemiringConverter(new NaturalSemiring(data_threshold(data),
                                                                      data_period(data)),
                                                  INTOBJ_INT(0),
                                                  NTPMatrixType);
      break;
    }
    case INT_MAT: {
      converter = new MatrixOverSemiringConverter(new Integers(),
                                                  INTOBJ_INT(0),
                                                  IntegerMatrixType);
      break;
    }
    case PROJ_MAX_PLUS_MAT: {
      converter = new ProjectiveMaxPlusMatrixConverter(new MaxPlusSemiring(),
                                                        Ninfinity,
                                                        ProjectiveMaxPlusMatrixType);
      break;
    }
    case PBR_TYPE: {
      converter = new PBRConverter();
      break;
    }
    default: {
      assert(false);
    }
  }

  AssPRec(data, RNam_converter, OBJ_CLASS(converter, T_SEMI_SUBTYPE_CONVER));
}

/*******************************************************************************
* data_init_semigroup: the default value for <semigroup> is nullptr, which is
* set in the header file.
*******************************************************************************/

void data_init_semigroup (Obj data, Semigroup* semigroup) {

  initRNams();

  if (semigroup != nullptr) {
    if (data_has_cpp_semigroup(data)) {
      assert(false);
    }
    semigroup->set_batch_size(data_batch_size(data));
    AssPRec(data, RNam_semigroup, OBJ_CLASS(semigroup, T_SEMI_SUBTYPE_SEMIGP));
    return;
  }

  if (data_has_cpp_semigroup(data)) {
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

  for (size_t i = 1; i <= (size_t) LEN_PLIST(gens_gap); i++) {
    gens->push_back(converter->convert(ELM_PLIST(gens_gap, i), degree));
  }

  semigroup = new Semigroup(gens, degree);
  semigroup->set_batch_size(data_batch_size(data));

  AssPRec(data, RNam_semigroup, OBJ_CLASS(semigroup, T_SEMI_SUBTYPE_SEMIGP));

  for (Element* x: *gens) {
    x->really_delete();
  }
  delete gens;
}

/*******************************************************************************
* data_semigroup:
*******************************************************************************/

Semigroup* data_semigroup (Obj data) {

  initRNams();

  if (!data_has_cpp_semigroup(data)) {
    data_init(data);
  }
  return CLASS_OBJ<Semigroup>(ElmPRec(data, RNam_semigroup));
}

/*******************************************************************************
* data_converter:
*******************************************************************************/

Converter* data_converter (Obj data) {

  initRNams();

  if (!data_has_cpp_converter(data)) {
    data_init(data);
  }
  return CLASS_OBJ<Converter>(ElmPRec(data, RNam_converter));
}

/*******************************************************************************
* data_delete
*******************************************************************************/

void data_delete (Obj data) {

  initRNams();

  if (data_has_cpp_semigroup(data)) {
    delete data_semigroup(data);
  }
  if (data_has_cpp_converter(data)) {
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

  initRNams();

  Obj x = data_rep(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(CALL_1ARGS(IsTropicalMatrix, x) || CALL_1ARGS(IsNTPMatrix, x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 1));
}

/*******************************************************************************
*
*******************************************************************************/

long data_period (Obj data) {

  initRNams();

  Obj x = data_rep(data);
  assert(TNUM_OBJ(x) == T_POSOBJ);
  assert(CALL_1ARGS(IsNTPMatrix, x));
  assert(ELM_PLIST(x, 1) != 0);
  assert(IS_PLIST(ELM_PLIST(x, 1)));
  assert(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2) != 0);

  return INT_INTOBJ(ELM_PLIST(x, LEN_PLIST(ELM_PLIST(x, 1)) + 2));
}

/*******************************************************************************
*
*******************************************************************************/

Obj data_rep (Obj data) {
  // TODO more asserts

  initRNams();

  assert(IsbPRec(data, RNam_gens));
  assert(LEN_LIST(ElmPRec(data, RNam_gens)) > 0);
  return ELM_PLIST(ElmPRec(data, RNam_gens), 1);
}

/*******************************************************************************
*
*******************************************************************************/

size_t data_batch_size (Obj data) {

  initRNams();

  assert(IsbPRec(data, RNam_batch_size));
  assert(IS_INTOBJ(ElmPRec(data, RNam_batch_size)));
  return INT_INTOBJ(ElmPRec(data, RNam_batch_size));
}

/*******************************************************************************
*
*******************************************************************************/

bool rec_get_report (Obj rec) {
  //TODO assert(o is a prec)

  initRNams();

  if (IsbPRec(rec, RNam_report)) {
    assert(ElmPRec(rec, RNam_report) == True || ElmPRec(rec, RNam_report) == False);
    return (ElmPRec(rec, RNam_report) == True ? true : false);
  }
  return false;
}

/*******************************************************************************
*
*******************************************************************************/

size_t data_degree (Obj data) {
  //TODO add asserts

  initRNams();

  return INT_INTOBJ(ElmPRec(data, RNamName("degree")));
}

