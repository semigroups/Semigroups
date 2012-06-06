
#include "ptrans.h"

/*******************************************************************************
**
** A partial transformation is of the form:
**
** [deg = max_dom, len_dom, rank, min ran, max ran, min, max, dense img list, 
**  canonical trans same kernel, dom, ran, Set(ran)]
** 
** and hence has length
**
** 7+2*(len_dom)+2*(dom_size)+rank
**
** where <len_dom> is the length of <img list>, 
** <dom_size> is the number of non-zero entries in <dense img list>,
** <rank> is the length of <ran>, <min ran>, <max ran> are self explanatory,
** <min> is min(<min dom>, <min ran>), <max> is max(<max dom>, <max ran>),
** <dense img list> is the dense image list (no trailing zeros!),
** <canonical trans same kernel> is the dense image list of what it says,
** <dom> is the domain, <ran> is the range, <Set(ran)> is the 
** range as a set (not calculated until needed).
**
** An element of the internal rep of a partial trans must be at most 
** 65535 and be of pttype, but the length and indices can be larger than 65535
** and so these currently have type Int. 
**
*******************************************************************************/

/*******************************************************************************
** Internal functions
*******************************************************************************/
    
/* comparison for qsort */
int cmp (const void *a, const void *b)
{ pttype aa, bb;

 aa = *((const pttype *)a);
 bb = *((const pttype *)b);
 return (int) (aa-bb);
}

/*******************************************************************************
** Macros for partial trans specifically
*******************************************************************************/

/* length of the partial trans internal rep */
static inline Int LEN_PT(Obj f)
{
  return (ELM_PT(f,1)==0?8:7+2*ELM_PT(f,1)+2*ELM_PT(f,2)+ELM_PT(f,3));
}

/*******************************************************************************
** GAP functions
*******************************************************************************/

/* method for f[i] */
Obj FuncELM_LIST_PT( Obj self, Obj f, Obj i)
{   
  if(INT_INTOBJ(i)>LEN_PT(f)) return Fail;
  return INTOBJ_INT(ELM_PT(f, INT_INTOBJ(i)));
} 

/* method for f{list} */ 
Obj FuncELMS_LIST_PT(Obj self, Obj f, Obj list) 
{   Int len, i; 
    Obj out; 
     
    len = LEN_LIST(list); 
    if(len>LEN_PT(f)) len = LEN_PT(f); 
    out = NEW_PLIST(T_PLIST_CYC, len); 
    SET_LEN_PLIST(out, len); 
     
    for(i=1;i<=len;i++){ 
      SET_ELM_PLIST(out,i,   
        INTOBJ_INT(ELM_PT(f, INT_INTOBJ(ELM_LIST(list, i))))); 
    } 
 
    return out; 
} 

/* create partial trans from sparse representation */
Obj FuncSparsePartialTransNC( Obj self, Obj dom, Obj ran )
{ 
  len_dom:=LEN_LIST(dom);
  if(len_dom==0) return NEW_EMPTY_PT();

  deg=INT_INTOBJ(ELM_LIST(dom,len_dom));
  
  TOO_MANY_PTS_ERROR(len_dom>65535||deg>65535); 
  
  f=NEW_PT(7+2*deg+3*len_dom);
  SET_ELM_PT(f, 1, (pttype) deg);
  SET_ELM_PT(f, 2, (pttype) len_dom);

  max_ran=0;
  min_ran=65535;




}
