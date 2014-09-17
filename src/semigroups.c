/***************************************************************************
**
*A  semigroups.c               Semigroups package             J. D. Mitchell 
**
**  Copyright (C) 2014 - J. D. Mitchell 
**  This file is free software, see license information at the end.
**  
*/

#include <stdlib.h>

#include "src/compiled.h"          /* GAP headers                */

#undef PACKAGE
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_URL
#undef PACKAGE_VERSION

//#include "pkgconfig.h"             /* our own configure results */

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

static Obj FuncGABOW_SCC(Obj self, Obj digraph)
{
  UInt len1, len2, count, level, w, v, n, l, idw, *fptr, *stkptr;
  Obj  id, frames, adj, stack1, stack2, out, comp, comps; 
 
  n = LEN_LIST(digraph);
  if (n == 0){
    out = NEW_PREC(2);
    AssPRec(out, RNamName("id"), NEW_PLIST(T_PLIST_EMPTY+IMMUTABLE,0));
    AssPRec(out, RNamName("comps"), NEW_PLIST(T_PLIST_EMPTY+IMMUTABLE,0));
    CHANGED_BAG(out);
    return out;
  }

  len1 = 0; 
  stack1 = NEW_PLIST(T_PLIST_CYC, n); 
  //stack1 is a plist so we can use memcopy below
  SET_LEN_PLIST(stack1, n);

  len2 = 0;
  stack2 = NewBag(T_DATOBJ, (n+1)*sizeof(UInt));
   
  id = NEW_PLIST(T_PLIST_CYC+IMMUTABLE, n);
  SET_LEN_PLIST(id, n);
  //init id
  for(v=1;v<=n;v++){
    SET_ELM_PLIST(id, v, INTOBJ_INT(0));
  }
  count = n;
  
  comps = NEW_PLIST(T_PLIST_TAB+IMMUTABLE, n);
  SET_LEN_PLIST(comps, 0);
  
  frames = NewBag(T_DATOBJ, (3*n+1)*sizeof(UInt));
  
  for(v=1;v<=n;v++){
    if(INT_INTOBJ(ELM_PLIST(id, v))==0){
      level=1;
      adj = ELM_LIST(digraph, v);
      PLAIN_LIST(adj);
      fptr = (UInt *)ADDR_OBJ(frames);
      fptr[0] = v; // vertex
      fptr[1] = 1; // index
      fptr[2] = (UInt)adj;
      SET_ELM_PLIST(stack1, ++len1, INTOBJ_INT(v));
      ((UInt *)ADDR_OBJ(stack2))[++len2] = len1;
      SET_ELM_PLIST(id, v, INTOBJ_INT(len1)); 
      
      while(level>0){
        if(fptr[1]>LEN_PLIST(fptr[2])){
          if(((UInt *)ADDR_OBJ(stack2))[len2]==INT_INTOBJ(ELM_PLIST(id, fptr[0]))){
            len2--;
            count++;
            l=0;
            do{
              l++;
              w=INT_INTOBJ(ELM_PLIST(stack1, len1--));
              SET_ELM_PLIST(id, w, INTOBJ_INT(count));
            }while(w!=fptr[0]);
            
            comp = NEW_PLIST(T_PLIST_CYC+IMMUTABLE, l);
            SET_LEN_PLIST(comp, l);
           
            memcpy( (void *)((char *)(ADDR_OBJ(comp)) + sizeof(Obj)), 
                    (void *)((char *)(ADDR_OBJ(stack1)) + (len1+1)*sizeof(Obj)), 
                    (size_t)(l*sizeof(Obj)));

            l=LEN_PLIST(comps)+1;
            SET_ELM_PLIST(comps, l, comp);
            SET_LEN_PLIST(comps, l);
            CHANGED_BAG(comps);
            fptr = (UInt *)ADDR_OBJ(frames)+(level-1)*3;
          }
          level--;
          fptr -= 3;
        } else {
          
          w = INT_INTOBJ(ELM_PLIST(fptr[2], fptr[1]++));
          idw = INT_INTOBJ(ELM_PLIST(id, w));
          
          if(idw==0){
      
            level++;
            adj = ELM_LIST(digraph, w);
            PLAIN_LIST(adj);
            fptr += 3; 
            fptr[0] = w; // vertex
            fptr[1] = 1; // index
            fptr[2] = (UInt)adj;                          
            SET_ELM_PLIST(stack1, ++len1, INTOBJ_INT(w));
            ((UInt *)ADDR_OBJ(stack2))[++len2] = len1;
            SET_ELM_PLIST(id, w, INTOBJ_INT(len1)); 
          
          } else {
            stkptr=((UInt*)ADDR_OBJ(stack2));
            while(stkptr[len2]>idw){
              len2--; // pop from stack2
            }
          }
        }
      }
    }
  }

  for(v=1;v<=n;v++){
    SET_ELM_PLIST(id, v, INTOBJ_INT(INT_INTOBJ(ELM_PLIST(id, v))-n));
  }

  out = NEW_PREC(2);
  SHRINK_PLIST(comps, LEN_PLIST(comps));
  AssPRec(out, RNamName("id"), id);
  AssPRec(out, RNamName("comps"), comps);
  CHANGED_BAG(out);
  return out;
}

static Obj FuncIS_ACYCLIC_DIGRAPH(Obj self, Obj adj)
{ UInt  nr, i, j, k;
  UInt  level;
  Obj   buf, nbs;
  UInt  *stack, *ptr1, *ptr2;
  
  nr = LEN_PLIST(adj);
  buf = NewBag(T_DATOBJ, (4 * nr + 6) * sizeof(UInt));
  
  //init the buf
  ptr1 = ((UInt*)ADDR_OBJ(buf)); 
  ptr2 = ptr1 + nr + 1;
  stack = ptr2 + nr + 1;
  
  for (i = 1; i <= nr; i++) {
    nbs = ELM_PLIST(adj, i);
    if (LEN_PLIST(nbs) == 0) {
      ptr1[i] = 1;
    } else if (ptr1[i] == 0) {
      level = 1;
      stack[1] = i;
      stack[2] = 1;
      while (1) {
        j = stack[level * 2 - 1];
        k = stack[level * 2];
        if (ptr2[j] == 1) { 
          return False;  // We have just travelled around a cycle
        }
        // Check whether:
        // 1. We've previously finished with this vertex, OR 
        // 2. Whether we've now investigated all branches descending from it
        nbs = ELM_PLIST(adj, j);
        if( ptr1[j] == 1 || k > LEN_PLIST(nbs)) {
          ptr1[j] = 1;
          level--;
          if (level==0) { 
            break;
          }
          // Backtrack and choose next available branch
          stack[2 * level]++;
          ptr2[stack[2 * level - 1]] = 0;
        } else { //Otherwise move onto the next available branch
          ptr2[j]=1;
          level++;
          nbs = ELM_PLIST(adj, j);
          stack[2 * level - 1] = INT_INTOBJ(ELM_PLIST(nbs, k));
          stack[2 * level] = 1;
        }
      }
    }
  }
  return True;
}

/*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * */

/******************************************************************************
*V  GVarFuncs . . . . . . . . . . . . . . . . . . list of functions to export
*/

static StructGVarFunc GVarFuncs [] = {

  { "GABOW_SCC", 1, "digraph",
    FuncGABOW_SCC, 
    "src/semigroups.c:GABOW_SCC" },

  { "IS_ACYCLIC_DIGRAPH", 1, "digraph",
    FuncIS_ACYCLIC_DIGRAPH, 
    "src/semigroups.c:FuncIS_ACYCLIC_DIGRAPH" },
  
  { 0 }

};

/******************************************************************************
*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel ( StructInitInfo *module )
{
    /* init filters and functions                                          */
    InitHdlrFuncsFromTable( GVarFuncs );

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitLibrary( <module> ) . . . . . . .  initialise library data structures
*/
static Int InitLibrary ( StructInitInfo *module )
{
    Int             i, gvar;
    Obj             tmp;

    /* init filters and functions */
    for ( i = 0;  GVarFuncs[i].name != 0;  i++ ) {
      gvar = GVarName(GVarFuncs[i].name);
      AssGVar(gvar,NewFunctionC( GVarFuncs[i].name, GVarFuncs[i].nargs,
                                 GVarFuncs[i].args, GVarFuncs[i].handler ) );
      MakeReadOnlyGVar(gvar);
    }

    tmp = NEW_PREC(0);
    gvar = GVarName("SEMIGROUPSC"); 
    AssGVar( gvar, tmp ); 
    MakeReadOnlyGVar(gvar);

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitInfopl()  . . . . . . . . . . . . . . . . . table of init functions
*/
static StructInitInfo module = {
#ifdef SEMIGROUPSSTATIC
 /* type        = */ MODULE_STATIC,
#else
 /* type        = */ MODULE_DYNAMIC,
#endif
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
 /* postRestore = */ 0
};

#ifndef SEMIGROUPSSTATIC
StructInitInfo * Init__Dynamic ( void )
{
  return &module;
}
#endif

StructInitInfo * Init__semigroups ( void )
{
  return &module;
}

/*
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */


