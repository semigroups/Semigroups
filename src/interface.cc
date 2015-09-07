/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ semigroups. 
 *
 */

#include "interface.h"

/*******************************************************************************
 * Class for containing a C++ semigroup and accessing its methods
*******************************************************************************/

/*
    // in the case that we are taking the closure of the semigroup of
    // <old> with some new generators, these new generators are stored in
    // the <gens> component of <data>. I.e. the meaning of the <gens> component
    // of the <data> is different if we are taking the closure than if we are
    // not. 
    Interface (Obj data, Converter* converter, SemigroupBase* old) 
      : _converter(converter) {
      assert(IsbPRec(data, RNam_gens));
      assert(LEN_LIST(ElmPRec(data, RNam_gens)) > 0);
      
      Obj gens = ElmPRec(data, RNam_gens);

      std::vector<Element*>* gens_c(new std::vector<Element*>());
      size_t deg_c = INT_INTOBJ(ElmPRec(data, RNamName("degree")));
      PLAIN_LIST(gens);
      for(size_t i = 1; i <= (size_t) LEN_PLIST(gens); i++) {
        gens_c->push_back(converter->convert(ELM_PLIST(gens, i), deg_c));
      }
      // full deletion of things in gens_c is responsibility of the semigroup 
        
      if (old == nullptr) {
        _semigroup = new Semigroup(gens_c, deg_c);
      } else {
        _semigroup = new Semigroup(*static_cast<Semigroup*>(old), gens_c, data_report(data)); 
        for (size_t i = 0; i < _semigroup->nrgens(); i++) {
          AssPlist(gens, i + 1, converter->unconvert(_semigroup->gens()->at(i)));
        }
      }
      for (Element* x: *gens_c) {
        x->really_delete();
      }
      delete gens_c;
      _semigroup->set_batch_size(BatchSize(data));
    }*/
    

// helper function to convert a CayleyGraph to a GAP plist of GAP plists.
Obj ConvertFromCayleyGraph (CayleyGraph* graph) {
  assert(graph->size() != 0);
  Obj out = NEW_PLIST(T_PLIST, graph->nr_rows());
  SET_LEN_PLIST(out, graph->nr_rows());

  for (size_t i = 0; i < graph->nr_rows(); i++) {
    Obj next = NEW_PLIST(T_PLIST_CYC, graph->nr_cols());
    SET_LEN_PLIST(next, graph->nr_cols());
    for (size_t j = 0; j < graph->nr_cols(); j++) { //TODO reinstate this
      SET_ELM_PLIST(next, j + 1, INTOBJ_INT(graph->get(i, j) + 1));
    }
    SET_ELM_PLIST(out, i + 1, next);
    CHANGED_BAG(out);
  }
  return out;
}

Obj ConvertFromWord (Word* vec) {
  Obj out = NEW_PLIST(T_PLIST_CYC, vec->size());
  SET_LEN_PLIST(out, vec->size());

  for (size_t i = 0; i < vec->size(); i++) {
    SET_ELM_PLIST(out, i + 1, INTOBJ_INT(vec->at(i) + 1));
  }
  CHANGED_BAG(out);
  return out;
}


/*******************************************************************************
 * GAP level functions
*******************************************************************************/

Obj SEMIGROUP_RIGHT_CAYLEY_GRAPH (Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    if (! IsbPRec(data, RNam_right)) { 
      Semigroup* semigroup = data_semigroup(data);
      AssPRec(data, RNam_right, 
              ConvertFromCayleyGraph(semigroup->right_cayley_graph(data_report(data))));
      CHANGED_BAG(data);
    }
  } else {
    enumerate_semigroup(self, data, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(data, RNam_right);
}

Obj SEMIGROUP_LEFT_CAYLEY_GRAPH (Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    if (! IsbPRec(data, RNam_left)) { 
      Semigroup* semigroup = data_semigroup(data);
      AssPRec(data, RNam_left, 
              ConvertFromCayleyGraph(semigroup->left_cayley_graph(data_report(data))));
      CHANGED_BAG(data);
    }
  } else {
    enumerate_semigroup(self, data, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(data, RNam_left);
}

Obj SEMIGROUP_RELATIONS (Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    if (! IsbPRec(data, RNam_rules)) { 
      Semigroup* semigroup = data_semigroup(data);
      bool report          = data_report(data);

      Obj rules = NEW_PLIST(T_PLIST, semigroup->nrrules(report));
      SET_LEN_PLIST(rules, semigroup->nrrules(report));
      size_t nr = 0;

      std::vector<size_t> relation;
      semigroup->next_relation(relation, report);

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
        semigroup->next_relation(relation, report);
      }
      
      while (!relation.empty()) {

        Obj old_word = SEMIGROUP_FACTORIZATION(self, data, INTOBJ_INT(relation.at(0) + 1));
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
        SET_ELM_PLIST(next, 2, SEMIGROUP_FACTORIZATION(self, data, INTOBJ_INT(relation.at(2) + 1)));
        CHANGED_BAG(next);
        nr++; 
        SET_ELM_PLIST(rules, nr, next);
        CHANGED_BAG(rules);
        semigroup->next_relation(relation, report);
      } 
      AssPRec(data, RNam_rules, rules);
      CHANGED_BAG(data);
    }
  } else {
    enumerate_semigroup(self, data, INTOBJ_INT(-1), 0, False);
  }
  return ElmPRec(data, RNam_rules);
}

Obj SEMIGROUP_SIZE (Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
      return INTOBJ_INT(data_semigroup(data)->size(data_report(data)));
  } else {
    enumerate_semigroup(self, data, INTOBJ_INT(-1), 0, False);
  }
  return INTOBJ_INT(LEN_PLIST(ElmPRec(data, RNam_elts)));
}

Obj SEMIGROUP_FACTORIZATION (Obj self, Obj data, Obj pos) {
  if (data_type(data) != UNKNOWN) {
    size_t pos_c = INT_INTOBJ(pos);
    Obj words; 
    Semigroup* semigroup = data_semigroup(data);
    if (! IsbPRec(data, RNam_words)) {
      
      words = NEW_PLIST(T_PLIST, pos_c);
      SET_LEN_PLIST(words, pos_c);
      SET_ELM_PLIST(words, pos_c,
                    ConvertFromWord(semigroup->factorisation(pos_c - 1, data_report(data))));
      CHANGED_BAG(words);
      AssPRec(data, RNam_words, words);
    } else {
      words = ElmPRec(data, RNam_words);
      if (pos_c > (size_t) LEN_PLIST(words) || ELM_PLIST(words, pos_c) == 0) {
        //avoid retracing the Schreier tree if possible
        size_t prefix = semigroup->prefix(pos_c - 1) + 1;
        size_t suffix = semigroup->suffix(pos_c - 1) + 1;
        if (prefix != 0 && prefix <= (size_t) LEN_PLIST(words) 
            && ELM_PLIST(words, prefix) != 0) {
          Obj old_word = ELM_PLIST(words, prefix);
          Obj new_word = NEW_PLIST(T_PLIST_CYC, LEN_PLIST(old_word) + 1);
          memcpy((void *)((char *)(ADDR_OBJ(new_word)) + sizeof(Obj)), 
                 (void *)((char *)(ADDR_OBJ(old_word)) + sizeof(Obj)), 
                 (size_t)(LEN_PLIST(old_word) * sizeof(Obj)));
          SET_ELM_PLIST(new_word, LEN_PLIST(old_word) + 1, 
                        INTOBJ_INT(semigroup->final_letter(pos_c - 1) + 1));
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
                        INTOBJ_INT(semigroup->first_letter(pos_c - 1) + 1));
          SET_LEN_PLIST(new_word, LEN_PLIST(old_word) + 1);
          AssPlist(words, pos_c, new_word);
        } else {
          AssPlist(words, pos_c, ConvertFromWord(semigroup->factorisation(pos_c - 1, data_report(data))));
        }
      }
    }
    CHANGED_BAG(data);
    assert(IsbPRec(data, RNam_words));
    assert(IS_PLIST(ElmPRec(data, RNam_words)));
    assert(pos_c <= (size_t) LEN_PLIST(ElmPRec(data, RNam_words)));
    return ELM_PLIST(ElmPRec(data, RNam_words), pos_c);
  } else {
    enumerate_semigroup(self, data, INTOBJ_INT(pos), 0, False);
    return ELM_PLIST(ElmPRec(data, RNam_words), INT_INTOBJ(pos));
  }
}

// get the elements of the C++ semigroup, store them in data
Obj SEMIGROUP_ELEMENTS (Obj self, Obj data, Obj limit) {
  if (data_type(data) != UNKNOWN) {
    std::vector<Element*>* elements =
      data_semigroup(data)->elements(INT_INTOBJ(limit), data_report(data));
    Converter* converter = data_converter(data);

    if (! IsbPRec(data, RNam_elts)) {
      Obj out = NEW_PLIST(T_PLIST, elements->size());
      SET_LEN_PLIST(out, elements->size());
      for (size_t i = 0; i < elements->size(); i++) {
        SET_ELM_PLIST(out, i + 1, converter->unconvert(elements->at(i)));
        CHANGED_BAG(out);
      }
      AssPRec(data, RNam_elts, out);
    } else {
      Obj out = ElmPRec(data, RNam_elts);
      for (size_t i = LEN_PLIST(out); i < elements->size(); i++) {
        AssPlist(out, i + 1, converter->unconvert(elements->at(i)));
      }
    }
    CHANGED_BAG(data);
  } else {
    enumerate_semigroup(self, data, limit, 0, False);
  }
  return ElmPRec(data, RNam_elts);
}

// return first place in _semigroup starting from pos, for which lookfunc
// returns true

Obj SEMIGROUP_FIND (Obj self, Obj data, Obj lookfunc, Obj start, Obj end) {
  if (data_type(data) != UNKNOWN) {
    
    Semigroup* semigroup = data_semigroup(data);
    AssPRec(data, RNamName("found"), False);
    size_t pos = INT_INTOBJ(start);
    Obj limit;
    if (pos < semigroup->current_size()) {
      limit = INTOBJ_INT(semigroup->current_size());
    } else {
      limit = INTOBJ_INT(pos + semigroup->batch_size()); 
    }

    // get the elements out of _semigroup into "elts"
    SEMIGROUP_ELEMENTS(self, data, limit); 
    
    size_t nr = std::min(LEN_PLIST(ElmPRec(data, RNam_elts)),
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
        limit = INTOBJ_INT(INT_INTOBJ(limit) + semigroup->batch_size());
        // get the elements out of semigroup into "elts"
        SEMIGROUP_ELEMENTS(self, data, limit); 
        nr = std::min(LEN_PLIST(ElmPRec(data, RNam_elts)),
                      INT_INTOBJ(end));
      }
    }
  } else {
    enumerate_semigroup(self, data, end, lookfunc, True);
  }
  return ElmPRec(data, RNamName("found"));
}

Obj SEMIGROUP_CURRENT_SIZE (Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    return INTOBJ_INT(data_semigroup(data)->current_size());
  }
  return INTOBJ_INT(LEN_PLIST(ElmPRec(data, RNam_elts)));
}

Obj SEMIGROUP_CURRENT_NR_RULES (Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    return INTOBJ_INT(data_semigroup(data)->current_nrrules());
  }
  return INTOBJ_INT(ElmPRec(data, RNamName("nrrules")));
}

Obj SEMIGROUP_POSITION (Obj self, Obj data, Obj x) {

  if (data_type(data) != UNKNOWN) {
    size_t deg = data_degree(data);
    Semigroup* semigroup = data_semigroup(data);
    Converter* converter = data_converter(data);
    size_t pos = semigroup->position(converter->convert(x, deg), data_report(data));
    return (pos == ((size_t) -1) ? Fail : INTOBJ_INT(pos + 1));
  }

  Obj ht = ElmPRec(data, RNamName("ht"));
  size_t pos, nr;
  
  do { 
    Obj val = HTValue_TreeHash_C(self, ht, x);
    if (val != Fail) {
      return val; 
    }
    Obj limit = SumInt(ElmPRec(data, RNamName("nr")), INTOBJ_INT(1));
    enumerate_semigroup(self, data,  limit, 0, False);
    pos = INT_INTOBJ(ElmPRec(data, RNamName("pos")));
    nr = INT_INTOBJ(ElmPRec(data, RNamName("nr")));
  } while (pos <= nr);
  return HTValue_TreeHash_C(self, ht, x);
}

Obj SEMIGROUP_IS_DONE (Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    return (data_semigroup(data)->is_done() ? True : False);
  }

  size_t pos = INT_INTOBJ(ElmPRec(data, RNamName("pos")));
  size_t nr = INT_INTOBJ(ElmPRec(data, RNamName("nr")));
  return (pos > nr ? True : False);
}

//new_data.gens is the generators to add to the generators of old_data

/*Obj SEMIGROUP_CLOSURE (Obj self, Obj old_data, Obj new_data) {
  if (data_type(data) == UNKNOWN) {
    ErrorQuit("SEMIGROUP_CLOSURE: this shouldn't happen!", 0L, 0L);
  }
  InterfaceFromData(new_data, InterfaceFromData(old_data)->semigroup());
  return new_data;
}*/

Obj SEMIGROUP_ADD_GENERATORS (Obj self, Obj data, Obj coll_gap) {
  if (data_type(data) == UNKNOWN) {
    ErrorQuit("SEMIGROUP_ADD_GENERATORS: this shouldn't happen!", 0L, 0L);
  }
  
  assert(IS_PLIST(coll_gap));
  assert(LEN_PLIST(coll_gap) > 0);

  Semigroup* semigroup = data_semigroup(data);
  Converter* converter = data_converter(data);
  std::unordered_set<Element*> coll;

  for(size_t i = 1; i <= (size_t) LEN_PLIST(coll_gap); i++) {
    coll.insert(converter->convert(ELM_PLIST(coll_gap, i), semigroup->degree()));
  }
  semigroup->add_generators(coll, data_report(data));

  Obj gens = ElmPRec(data, RNam_gens); // TODO make this safe
  
  for(size_t i = 0; i < semigroup->nrgens(); i++) {
    AssPlist(gens, i + 1, converter->unconvert(semigroup->gens()->at(i)));
  }

  if (IsbPRec(data, RNam_left)) {
    UnbPRec(data, RNam_left);
  }
  if (IsbPRec(data, RNam_right)) {
    UnbPRec(data, RNam_right);
  }
  if (IsbPRec(data, RNam_rules)) {
    UnbPRec(data, RNam_rules);
  }
  if (IsbPRec(data, RNam_words)) {
    UnbPRec(data, RNam_words);
  }
  
  return data;
}

Obj SEMIGROUP_CURRENT_MAX_WORD_LENGTH (Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    return INTOBJ_INT(data_semigroup(data)->current_max_word_length());
  } else {
    if (IsbPRec(data, RNam_words) && LEN_PLIST(ElmPRec(data, RNam_words)) > 0) {
      Obj words = ElmPRec(data, RNam_words);
      return INTOBJ_INT(LEN_PLIST(ELM_PLIST(words, LEN_PLIST(words))));
    } else {
      return INTOBJ_INT(1);
    }
  }
}

Obj SEMIGROUP_LENGTH_ELEMENT (Obj self, Obj data, Obj pos) {
  if (data_type(data) != UNKNOWN) {
    return INTOBJ_INT(data_semigroup(data)->length(INT_INTOBJ(pos)));
  } else {
    //TODO
  }
}

Obj SEMIGROUP_NR_IDEMPOTENTS (Obj self, Obj data) {
  if (data_type(data) != UNKNOWN) {
    ErrorQuit("SEMIGROUP_NR_IDEMPOTENTS: this shouldn't happen!", 0L, 0L);
  }
  return INTOBJ_INT(data_semigroup(data)->nr_idempotents(data));
}

Obj SEMIGROUP_ENUMERATE (Obj self, Obj data, Obj limit) {
  if (data_type(data) != UNKNOWN) {
    data_semigroup(data)->enumerate(INT_INTOBJ(limit), data_report(data));
  } else {
    enumerate_semigroup(self, data, limit, 0, False);
  }
  return data;
}
