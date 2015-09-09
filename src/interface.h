/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ semigroups. 
 *
 */

#ifndef SEMIGROUPS_GAP_INTERFACE_H
#define SEMIGROUPS_GAP_INTERFACE_H 1

/*******************************************************************************
 * 
*******************************************************************************/

extern "C" {
  Obj HTValue_TreeHash_C ( Obj self, Obj ht, Obj obj );
}

Obj enumerate_semigroup (Obj self, Obj data, Obj limit, Obj lookfunc, Obj looking);

/*******************************************************************************
 * GAP level functions
*******************************************************************************/

Obj SEMIGROUP_ADD_GENERATORS          (Obj self, Obj data, Obj coll);
Obj SEMIGROUP_CLOSURE                 (Obj self, Obj old_data, Obj new_data);
Obj SEMIGROUP_CURRENT_MAX_WORD_LENGTH (Obj self, Obj old_data);
Obj SEMIGROUP_CURRENT_NR_RULES        (Obj self, Obj data);
Obj SEMIGROUP_CURRENT_SIZE            (Obj self, Obj data);
Obj SEMIGROUP_ELEMENTS                (Obj self, Obj data, Obj limit);
Obj SEMIGROUP_ENUMERATE               (Obj self, Obj data, Obj limit);
Obj SEMIGROUP_FACTORIZATION           (Obj self, Obj data, Obj pos);
Obj SEMIGROUP_FIND                    (Obj self, Obj data, Obj lookfunc, Obj start, Obj end);
Obj SEMIGROUP_IS_DONE                 (Obj self, Obj data);
Obj SEMIGROUP_LEFT_CAYLEY_GRAPH       (Obj self, Obj data);
Obj SEMIGROUP_LENGTH_ELEMENT          (Obj self, Obj data, Obj pos);
Obj SEMIGROUP_NR_IDEMPOTENTS          (Obj self, Obj data);
Obj SEMIGROUP_POSITION                (Obj self, Obj data, Obj x);
Obj SEMIGROUP_RELATIONS               (Obj self, Obj data);
Obj SEMIGROUP_RIGHT_CAYLEY_GRAPH      (Obj self, Obj data);
Obj SEMIGROUP_SIZE                    (Obj self, Obj data);

#endif
