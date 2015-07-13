/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ semigroups. 
 *
 */

#ifndef SEMIGROUPS_GAP_INTERFACE_H
#define SEMIGROUPS_GAP_INTERFACE_H 1

/*******************************************************************************
 * GAP headers
*******************************************************************************/

#include "src/compiled.h"

#include "semigroups++/semigroups.h"

extern "C" {
  Obj HTValue_TreeHash_C ( Obj self, Obj ht, Obj obj );
}

/*******************************************************************************
 * Class for the interface from a GAP semigroup to a C++ semigroup and back 
*******************************************************************************/

class InterfaceBase {
  public:
    virtual                ~InterfaceBase () {};
    virtual SemigroupBase* semigroup () const = 0;
    virtual size_t         current_size () const = 0;
    virtual size_t         current_nrrules () const = 0;
    virtual size_t         current_max_word_length () const = 0;
    virtual bool           is_done () const = 0;
    virtual void           find (Obj data, Obj lookfunc, Obj start, Obj end) = 0;
    virtual void           enumerate (Obj data, Obj limit) = 0;
    virtual size_t         size (Obj data) = 0;
    virtual size_t         nr_idempotents (Obj data) = 0;
    virtual void           right_cayley_graph (Obj data) = 0;
    virtual void           left_cayley_graph (Obj data) = 0;
    virtual void           elements (Obj data, Obj limit) = 0;
    virtual Obj            position (Obj data, Obj x) = 0;
    virtual Obj            word (Obj data, Obj pos) = 0;
    virtual void           relations (Obj data) = 0;
    virtual void           add_generators (Obj data, Obj coll) = 0;
    //virtual size_t         simple_size () = 0;
};

/*******************************************************************************
 * Instantiate an Interface from a generic semigroup data
*******************************************************************************/

InterfaceBase* InterfaceFromData (Obj data, SemigroupBase* old = nullptr);

/*******************************************************************************
 * GAP level functions
*******************************************************************************/

Obj RIGHT_CAYLEY_GRAPH (Obj self, Obj data);

Obj LEFT_CAYLEY_GRAPH (Obj self, Obj data);

Obj RELATIONS_SEMIGROUP (Obj self, Obj data);

//Obj SIMPLE_SIZE (Obj self, Obj data);

Obj SIZE_SEMIGROUP (Obj self, Obj data);

Obj ELEMENTS_SEMIGROUP (Obj self, Obj data, Obj limit);

Obj WORD_SEMIGROUP (Obj self, Obj data, Obj pos);

Obj FIND_SEMIGROUP (Obj self, Obj data, Obj lookfunc, Obj start, Obj end);

Obj LENGTH_SEMIGROUP (Obj self, Obj data);

Obj NR_RULES_SEMIGROUP (Obj self, Obj data);

Obj POSITION_SEMIGROUP (Obj self, Obj data, Obj x);

Obj IS_CLOSED_SEMIGROUP (Obj self, Obj data);

Obj CLOSURE_SEMIGROUP (Obj self, Obj old_data, Obj new_data);

Obj ADD_GENERATORS_SEMIGROUP (Obj self, Obj data, Obj coll);

Obj MAX_WORD_LEN_SEMIGROUP (Obj self, Obj old_data);

Obj NR_IDEMPOTENTS_SEMIGROUP (Obj self, Obj data);

#endif
