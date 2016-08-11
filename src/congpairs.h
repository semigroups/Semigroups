/*
 * Semigroups GAP package
 *
 * This file contains the interface from GAP to the C++ congruence objects.
 *
 */

#ifndef SRC_CONGPAIRS_H_
#define SRC_CONGPAIRS_H_

// GAP level functions

Obj CONG_PAIRS_NR_CLASSES(Obj, Obj);
Obj CONG_PAIRS_IN(Obj, Obj, Obj);
Obj CONG_PAIRS_LOOKUP_PART(Obj, Obj);
Obj CONG_PAIRS_CLASS_COSET_ID(Obj, Obj);

#endif // SRC_CONGPAIRS_H_
