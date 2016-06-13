/*
 * Semigroups GAP package
 *
 * This file contains some methods for bipartitions
 *
 */

#ifndef SRC_BIPART_H_
#define SRC_BIPART_H_

#include <assert.h>

#include "src/compiled.h"
#include "gap.hh"
#include "semigroups++/elements.h"

// C functions

inline Bipartition* bipart_get_cpp (Obj x) {
  assert(TNUM_OBJ(x) == T_BIPART);
  return CLASS_OBJ<Bipartition>(x);
}

Obj          bipart_new_obj (Bipartition*);

// GAP level functions

Int BIPART_EQ             (Obj, Obj);
Int BIPART_LT             (Obj, Obj);
Obj BIPART_PROD           (Obj, Obj);

Obj BIPART_NC             (Obj, Obj);
Obj BIPART_EXT_REP        (Obj, Obj);
Obj BIPART_INT_REP        (Obj, Obj);
Obj BIPART_HASH           (Obj, Obj, Obj);
Obj BIPART_DEGREE         (Obj, Obj);
Obj BIPART_RANK           (Obj, Obj, Obj);
Obj BIPART_NR_BLOCKS      (Obj, Obj);
Obj BIPART_NR_LEFT_BLOCKS (Obj, Obj);
Obj BIPART_PERM_LEFT_QUO  (Obj, Obj, Obj);
Obj BIPART_LEFT_PROJ      (Obj, Obj);
Obj BIPART_RIGHT_PROJ     (Obj, Obj);
Obj BIPART_STAR           (Obj, Obj);
Obj BIPART_LAMBDA_CONJ    (Obj, Obj, Obj);
Obj BIPART_STAB_ACTION    (Obj, Obj, Obj);
Obj BIPART_LEFT_BLOCKS    (Obj, Obj);
Obj BIPART_RIGHT_BLOCKS   (Obj, Obj);

Int BLOCKS_EQ             (Obj, Obj);
Int BLOCKS_LT             (Obj, Obj);

Obj BLOCKS_NC             (Obj, Obj);
Obj BLOCKS_EXT_REP        (Obj, Obj);
Obj BLOCKS_HASH           (Obj, Obj, Obj);
Obj BLOCKS_DEGREE         (Obj, Obj);
Obj BLOCKS_RANK           (Obj, Obj);
Obj BLOCKS_NR_BLOCKS      (Obj, Obj);
Obj BLOCKS_PROJ           (Obj, Obj);
Obj BLOCKS_E_TESTER       (Obj, Obj, Obj);
Obj BLOCKS_E_CREATOR      (Obj, Obj, Obj);
Obj BLOCKS_LEFT_ACT       (Obj, Obj, Obj);
Obj BLOCKS_RIGHT_ACT      (Obj, Obj, Obj);
Obj BLOCKS_INV_LEFT       (Obj, Obj, Obj);
Obj BLOCKS_INV_RIGHT      (Obj, Obj, Obj);

Obj BIPART_NR_IDEMPOTENTS  (Obj, Obj, Obj, Obj, Obj, Obj);

#endif // SRC_BIPART_H_
