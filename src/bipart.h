/*
 * Semigroups GAP package
 *
 * This file contains some methods for bipartitions
 *
 */


#ifndef SRC_BIPART_H_
#define SRC_BIPART_H_

#include "src/compiled.h"
#include "src/types.h"
#include <assert.h>

Obj BIPART_NC             (Obj, Obj);
Obj BIPART_EXT_REP        (Obj, Obj);
Obj BIPART_INT_REP        (Obj, Obj);
Obj BIPART_HASH           (Obj, Obj);
Obj BIPART_DEGREE         (Obj, Obj);
Obj BIPART_RANK           (Obj, Obj);
Obj BIPART_NR_BLOCKS      (Obj, Obj);
Obj BIPART_NR_LEFT_BLOCKS (Obj, Obj);
Obj BIPART_PROD           (Obj, Obj, Obj);
Obj BIPART_EQ             (Obj, Obj, Obj);
Obj BIPART_LT             (Obj, Obj, Obj);
Obj BIPART_PERM_LEFT_QUO  (Obj, Obj, Obj);
Obj BIPART_LEFT_PROJ      (Obj, Obj);
Obj BIPART_RIGHT_PROJ     (Obj, Obj);
Obj BIPART_STAR           (Obj, Obj);
Obj BIPART_LAMBDA_CONJ    (Obj, Obj, Obj);
// OnRightBlocksBipartitionByPerm
Obj BIPART_STAB_ACTION    (Obj, Obj, Obj);
Obj BIPART_LEFT_BLOCKS    (Obj, Obj);
Obj BIPART_RIGHT_BLOCKS   (Obj, Obj);

Obj BLOCKS_DEGREE         (Obj, Obj);
Obj BLOCKS_NR_BLOCKS      (Obj, Obj);
Obj BLOCKS_ELM_LIST       (Obj, Obj, Obj);
Obj BLOCKS_E_TESTER       (Obj, Obj, Obj);
Obj BLOCKS_E_CREATOR      (Obj, Obj, Obj);
Obj BLOCKS_LEFT_ACT       (Obj, Obj, Obj);

#endif // SRC_BIPART_H_
