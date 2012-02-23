

const char * Revision_citrus_c =
   "$Id: citrus.c,v$";

#include <stdlib.h>

#include "src/compiled.h" 

/*******************************************************************************
**
** A partial permutation is of the form:
**
** [max dom, rank, min ran, max ran, min, max, img list, dom, ran,
**  Set(ran)]
**
** where <degree> is the length of <img list>, <rank> is the number of none zero
** entries in the <img list>,  <min ran>, <max ran> are
** self explanatory,  <min> is min(<min dom>, <min ran>), <max> is max(<max 
** dom>, <max ran>), <dom> is the domain, <ran> is the range, <Set(ran)> is the 
** range as a set (not calculated until needed), and <img list> is the list of 
** images with 0 for undefined.
**
** An element of the internal rep of a partial perm must be at most 65536 and be
** of pptype, but the length and indices can be larger than 65535 and so these
** currently have type Int. 
**
*******************************************************************************/

/*******************************************************************************
** Internal functions
*******************************************************************************/

/* from permutat.c */
#define IMAGE(i,pt,dg)  (((i) < (dg)) ? (pt)[(i)] : (i))

/* import the type from GAP */
Obj PartialPermType;

/* define the type for entries in part. perm */
typedef UInt2 pptype;

/* retrieve entry pos of internal rep of partial perm f */
static inline pptype ELM_PP(Obj f, Int pos)
{
    pptype *data = (pptype *) (ADDR_OBJ(f) + 1);
    return data[pos-1];
}

/* define entry pos of internal rep of partial perm f to be nr */
static inline void SET_ELM_PP(Obj f, Int pos, pptype nr)
{
    pptype *data = (pptype *) (ADDR_OBJ(f) + 1);
    data[pos-1] = nr;
}

/* create a new partial perm */
static inline Obj NEW_PP(Int len)
{
    Obj f;

    f = NewBag(T_DATOBJ, sizeof(pptype)*(len)+sizeof(UInt));
    TYPE_DATOBJ(f) = PartialPermType;
    return f;
}

/* create a new empty partial perm */
static inline Obj NEW_EMPTY_PP()
{ pptype i;
  Obj f;
  f = NewBag(T_DATOBJ, sizeof(pptype)*7+sizeof(UInt));
  TYPE_DATOBJ(f) = PartialPermType;
  for(i=1;i<=7;i++)
    SET_ELM_PP(f, i, 0);
  return f;
}

/* create a new empty plist */
static inline Obj NEW_EMPTY_PLIST()
{ Obj out;
  out = NEW_PLIST(T_PLIST_EMPTY, 0);
  SET_LEN_PLIST(out, 0);
  return out;
}

/* error if 65535 points are exceeded */
static inline Int TOO_MANY_PTS_ERROR(int cond)
{
  if(cond)
  { 
    ErrorQuit("usage: can only create partial perms on at most 65535 pts,", 
      0L, 0L); 
  }
  return 0L;
}

/* length of the partial perm internal rep */
static inline Int LEN_PP(Obj f)
{
  return (ELM_PP(f, 1)==0)?7:ELM_PP(f,1)+3*ELM_PP(f,2)+6;
}

/* comparison for qsort */
int cmp (const void *a, const void *b)
{ pptype aa, bb;

 aa = *((const pptype *)a);
 bb = *((const pptype *)b);
 return (int) (aa-bb);
}

/* set the range set of a partial perm */
static inline void SET_RANSET_PP(Obj f, pptype deg, pptype rank)
{   
  pptype i;
 
  for(i=1;i<=rank;i++)
  {
    SET_ELM_PP(f,2*rank+deg+6+i, ELM_PP(f,rank+deg+6+i));
  }
  qsort((pptype *)(ADDR_OBJ(f)+1)+6+deg+2*rank, rank, sizeof(pptype), cmp);
}  

/*******************************************************************************
** GAP functions
*******************************************************************************/

/* method for f[i] */
Obj FuncELM_LIST_PP( Obj self, Obj f, Obj i)
{ 
  if(INT_INTOBJ(i)>LEN_PP(f)) return Fail;
  return INTOBJ_INT(ELM_PP(f, INT_INTOBJ(i)));
}

/* method for f{list} */
Obj FuncELMS_LIST_PP(Obj self, Obj f, Obj list)
{   Int len, i;
    Obj out;
    
    len = LEN_LIST(list);
    if(len>LEN_PP(f)) len = LEN_PP(f);
    out = NEW_PLIST(T_PLIST_CYC, len);
    SET_LEN_PLIST(out, len);
    
    for(i=1;i<=len;i++){
      SET_ELM_PLIST(out,i,  
        INTOBJ_INT(ELM_PP(f, INT_INTOBJ(ELM_LIST(list, i)))));
    }

    return out;
}

/* create partial perm from sparse representation */
Obj FuncSparsePartialPermNC( Obj self, Obj dom, Obj ran )
{   Int rank, deg, j, k, max_ran, min_ran;
    pptype i;
    Obj f;

    rank=LEN_LIST(dom);

    if(rank==0) return NEW_EMPTY_PP();
    
    deg=INT_INTOBJ(ELM_LIST(dom,rank));
   
    TOO_MANY_PTS_ERROR(rank>65535||deg>65535);

    f = NEW_PP(6+deg+3*rank);

    SET_ELM_PP(f, 1, (pptype) deg);
    SET_ELM_PP(f, 2, (pptype) rank);

    max_ran=0; 
    min_ran=65535; 

    /* find dense img list, max_ran, min_ran */
    for(i=1;i<=rank;i++){
      j = INT_INTOBJ(ELM_LIST(dom, i));
      SET_ELM_PP(f, 6+deg+i, (pptype) j);
      
      k = INT_INTOBJ(ELM_LIST(ran, i));
      SET_ELM_PP(f, 6+deg+rank+i, (pptype) k);
      SET_ELM_PP(f, j+6, (pptype) k);

      if(k>max_ran) max_ran=k;
      if(k<min_ran) min_ran=k;
    }

    TOO_MANY_PTS_ERROR(max_ran>65535);

    SET_ELM_PP(f,3,(pptype) min_ran);
    SET_ELM_PP(f,4,(pptype) max_ran);

    /* set min */
    j=INT_INTOBJ(ELM_LIST(dom,1));
    SET_ELM_PP(f,5,min_ran<j?(pptype) min_ran:(pptype) j);
    
    /* set max */
    SET_ELM_PP(f,6,max_ran>deg?(pptype) max_ran:(pptype) deg);
    return f;
}

/* create dense partial perm */
Obj FuncDensePartialPermNC( Obj self, Obj img )
{ 
    Obj f;
    Int deg, i, max_ran, min_ran, rank, j;
    Int ran[513];

    if(LEN_LIST(img)==0) return NEW_EMPTY_PP();
    
    deg = 0;
    for(i=LEN_LIST(img);1<=i;i--)
    {
      if(INT_INTOBJ(ELM_LIST(img, i))!=0) 
      {
        deg = i;
        break;
      }
    }

    TOO_MANY_PTS_ERROR(deg>65535);

    if(deg==0) return NEW_EMPTY_PP();
    
    f = NEW_PP(3*deg+6); /* the output*/
    SET_ELM_PP(f, 1, (pptype) deg);
    
    max_ran=0; 
    min_ran=65535; 
    rank=0;

    /* find dom, rank, max_ran, min_ran */
    for(i=1;i<=deg;i++){
      j = INT_INTOBJ(ELM_LIST(img, i));
      SET_ELM_PP(f, i+6, (pptype) j);
      if(j!=0){
        rank++;
        SET_ELM_PP(f, deg+rank+6, (pptype) i); /* dom*/
        ran[rank]=j;
        if(j>max_ran){
          max_ran=j;
          }
        if(j<min_ran){
          min_ran=j;
          }
        }
      }
    
    TOO_MANY_PTS_ERROR(max_ran>65535);

    SET_ELM_PP(f,2,(pptype) rank);
    SET_ELM_PP(f,3,(pptype) min_ran);
    SET_ELM_PP(f,4,(pptype) max_ran);

    /* set range */
    for(i=1;i<=rank;i++){
      SET_ELM_PP(f,deg+rank+6+i,(pptype) ran[i]);
    } 

    /* set min */
    j=ELM_PP(f,deg+7); /* min. dom. */
    if(min_ran<(short int) j){
      SET_ELM_PP(f,5,(pptype) min_ran);
    }else{
      SET_ELM_PP(f,5,j);
    }
    
    /* set max */
    j=ELM_PP(f,deg+rank+6); /* max. dom. */
    if(max_ran>(short int) j){
      SET_ELM_PP(f,6,(pptype) max_ran);
    }else{
      SET_ELM_PP(f,6,j);
    }

    ResizeBag(f, sizeof(pptype)*(LEN_PP(f))+sizeof(UInt));
    return f;
}

/* product of partial permutations */

Obj FuncProdPP( Obj self, Obj f, Obj g )
{
    Int n, m, deg, rank_f, i, j, r, k, l, max_ran, min_ran, rank;
    Obj fg;
    Int ran[513];

    n = (short int) ELM_PP(f,1);
    m = (short int) ELM_PP(g,1);
    
    if(n==0||m==0) return NEW_EMPTY_PP();
   
    deg = 0;
    rank_f = (short int) ELM_PP(f,2);

    /* find degree/max. dom */
    for(i=rank_f;1<=i;i--)
    {
      j = (short int) ELM_PP(f,6+n+rank_f+i);
      if( j<=m && (short int) ELM_PP(g,j+6)!=0)
      {
        deg = (short int) ELM_PP(f,6+n+i);
        r = i;
        break;
      }
    } 

   if(deg==0) return NEW_EMPTY_PP();

    fg = NEW_PP(3*deg+6);
    SET_ELM_PP(fg, 1, (pptype) deg);
   
    max_ran=0;
    min_ran=(short int) ELM_PP(g,4);
    rank=0;

    for (i=1;i<=r;i++)
    {
      j = (short int) ELM_PP(f,6+n+rank_f+i);
      if(j<=m)
      {
        k = (short int) ELM_PP(g,j+6);
        if(k!=0)
        { 
          rank++;
          l = (short int) ELM_PP(f,6+n+i);
          SET_ELM_PP(fg,deg+rank+6,(pptype) l);
          SET_ELM_PP(fg,l+6, (pptype) k);
          ran[rank]=k;
          if(k>max_ran) max_ran=k;
          if(k<min_ran) min_ran=k;
        }
      }
    }

    SET_ELM_PP(fg,2,(pptype) rank);
    SET_ELM_PP(fg,3,(pptype) min_ran);
    SET_ELM_PP(fg,4,(pptype) max_ran);
    
    for(i=1;i<=rank;i++)
    {
      SET_ELM_PP(fg,deg+rank+6+i,(pptype) ran[i]);
    }
 
    k=ELM_PP(fg,deg+7);
    if(min_ran<(short int) k)
    {
      SET_ELM_PP(fg,5,(pptype) min_ran);
    }
    else
    {
      SET_ELM_PP(fg,5,k);
    }

    k=ELM_PP(fg,deg+rank+6);
    if(max_ran>(short int) k)
    {
      SET_ELM_PP(fg,6,(pptype) max_ran);
    }
    else
    {
      SET_ELM_PP(fg,6,k);
    }

    ResizeBag(fg, sizeof(pptype)*(LEN_PP(fg))+sizeof(UInt));
    return fg;
}

/* range set of partial permutation */

Obj FuncRanSet ( Obj self, Obj f )
{ Int deg, rank, i;
  Obj out;

  deg = (short int) ELM_PP(f, 1);
  
  if(deg==0) return NEW_EMPTY_PLIST();

  rank = (short int) ELM_PP(f, 2);

  if((short int) ELM_PP(f, 7+deg+2*rank)==0)
  {
    SET_RANSET_PP(f, deg, rank);
   }

  out = NEW_PLIST(T_PLIST_CYC,rank);
  SET_LEN_PLIST(out,rank);
  for(i=1;i<=rank;i++)
  {
    SET_ELM_PLIST(out,i, INTOBJ_INT((short int) ELM_PP(f,2*rank+deg+6+i)));
  }
  return out;
}

/* inverse of a partial permutation */

Obj FuncInvPP ( Obj self, Obj f )
{
    Int deg_f, rank, i, deg_f_inv, n, j, k;
    Obj f_inv;

    deg_f = (short int) ELM_PP(f, 1);
    if(deg_f==0) return f;

    rank = (short int) ELM_PP(f, 2);

    /* check if f knows Set(Ran(f)) if not set it */
    if((short int) ELM_PP(f, 7+deg_f+2*rank)==0)
    {
      SET_RANSET_PP(f, deg_f, rank);
    }
     
    deg_f_inv = (short int) ELM_PP(f, 4);
    n = 3*rank + deg_f_inv + 6; 
    f_inv = NEW_PP(6+deg_f_inv+3*rank);

    SET_ELM_PP(f_inv, 1, ELM_PP(f, 4));
    SET_ELM_PP(f_inv, 2, ELM_PP(f, 2));
    SET_ELM_PP(f_inv, 3, ELM_PP(f, 7+deg_f));
    SET_ELM_PP(f_inv, 4, ELM_PP(f, 6+deg_f+rank));
    SET_ELM_PP(f_inv, 5, ELM_PP(f, 5));
    SET_ELM_PP(f_inv, 6, ELM_PP(f, 6));

    /* set dense img, dom, and ran set */
    for(i=1;i<=rank;i++)
    {
      j = ELM_PP(f,i+deg_f+6);
      k = ELM_PP(f,i+deg_f+rank+6);
      SET_ELM_PP(f_inv, k+6, j); /* dense img */
      SET_ELM_PP(f_inv, 6+deg_f_inv+i, ELM_PP(f, 6+deg_f+2*rank+i)); /*dom*/
      SET_ELM_PP(f_inv, i+6+deg_f_inv+2*rank, j); /*ran set*/
    }

    /* set ran */
    for(i=1;i<=rank;i++)
    {
      n = ELM_PP(f_inv, 6+deg_f_inv+i);
      SET_ELM_PP(f_inv, i+6+deg_f_inv+rank, ELM_PP(f_inv, n+6));
    }

    return f_inv;
}

/* on sets for a partial permutation */ 

Obj FuncOnIntegerSetsWithPP (Obj self, Obj set, Obj f)
{ Int deg, n, m, i, j, k;
  Obj out;

  deg = (short int) ELM_PP(f, 1);
  n = LEN_PLIST(set);
  if(n==0||deg==0) return NEW_EMPTY_PLIST();

  out = NEW_PLIST(T_PLIST_CYC, n);
  m = 0;

  for(i=1;i<=n;i++)
  {
    j = INT_INTOBJ(ELM_PLIST(set, i));
    if(j<=deg)
    {
      k = (short int) ELM_PP(f, j+6);
      if(k!=0)
      {
        m++;
        SET_ELM_PLIST(out, m, INTOBJ_INT(k));
      }
    }
  }
  SET_LEN_PLIST(out, m);
  SHRINK_PLIST(out, m);
  SortDensePlist(out);
  return out;
}

/* equality test for partial permutations */

Obj FuncEqPP (Obj self, Obj f, Obj g)
{ Int deg_f, deg_g, rank_f, rank_g, i;

    deg_f = (short int) ELM_PP(f, 1);
    deg_g = (short int) ELM_PP(g, 1);
    
    if(deg_f!=deg_g) return False;
    if(deg_f==0) return True;

    rank_f = (short int) ELM_PP(f, 2);
    rank_g = (short int) ELM_PP(g, 2);

    if(rank_f!=rank_g) return False;

    /* search for a difference and return False if you find one          */
    for(i=7+deg_f;i<=6+deg_f+rank_f;i++)
    {
      if(ELM_PP(f, i)!=ELM_PP(g, i)||ELM_PP(f, i+rank_f)!=ELM_PP(g, i+rank_f))
      { 
        return False;
      }
    }

    /* otherwise they must be equal                                        */
    return True;
}

/* idempotent on domain of partial perm */

Obj FuncLeftOne(Obj self, Obj f)
{ Obj one;
  Int deg, rank, min, max, i, j;

  deg = ELM_PP(f, 1);
  
  if(deg==0) return f;

  one = NEW_PP(LEN_PP(f));
  rank=ELM_PP(f, 2);

  SET_ELM_PP(one, 1, deg);
  SET_ELM_PP(one, 2, rank);
  
  min = ELM_PP(f, 7+deg);
  max = ELM_PP(f, 6+deg+rank);
  
  SET_ELM_PP(one, 3, min);
  SET_ELM_PP(one, 4, max);
  SET_ELM_PP(one, 5, min);
  SET_ELM_PP(one, 6, max);
  
  for(i=7+deg;i<=6+deg+rank;i++)
  {
    j = ELM_PP(f, i);
    SET_ELM_PP(one, i, j);        /* dom */
    SET_ELM_PP(one, i+rank, j);   /* ran */
    SET_ELM_PP(one, i+2*rank, j); /* ran set */
    SET_ELM_PP(one, 6+j, j); 
  }

  return one;
}

/* idempotent on range of partial perm */

Obj FuncRightOne(Obj self, Obj f)
{ Obj one;
  Int deg, rank, min, max, i, j;

  deg = ELM_PP(f, 1);
  
  if(deg==0) return f;

  rank=ELM_PP(f, 2);
  min = ELM_PP(f, 3);
  max = ELM_PP(f, 4);
  
  one = NEW_PP(6+max+3*rank);
  
  SET_ELM_PP(one, 1, max);
  SET_ELM_PP(one, 2, rank);
  SET_ELM_PP(one, 3, min);
  SET_ELM_PP(one, 4, max);
  SET_ELM_PP(one, 5, min);
  SET_ELM_PP(one, 6, max);
  
  if(ELM_PP(f, 7+deg+2*rank)==0) SET_RANSET_PP(f, deg, rank);

  for(i=1;i<=rank;i++)
  {
    j = ELM_PP(f, 6+deg+2*rank+i);
    SET_ELM_PP(one, 6+max+i, j);        /* dom */
    SET_ELM_PP(one, 6+max+rank+i, j);   /* ran */
    SET_ELM_PP(one, 6+max+2*rank+i, j); /* ran set */
    SET_ELM_PP(one, 6+j, j); 
  }

  return one;
}

/* fixed points */

Obj FuncFixedPointsPP(Obj self, Obj f)
{ Int deg, rank, m, i;
  Obj out;

  deg = ELM_PP(f, 1);
  if(deg==0) return NEW_EMPTY_PLIST();
  
  rank = ELM_PP(f, 2);
  out = NEW_PLIST(T_PLIST_CYC, rank);
  m = 0;

  for(i=7+deg;i<=6+deg+rank;i++)
  {
    if(ELM_PP(f, i)==ELM_PP(f, i+rank))
    { 
      m++;
      SET_ELM_PLIST(out, m, INTOBJ_INT(ELM_PP(f, i)));
    }
  }
  SET_LEN_PLIST(out, m);
  SHRINK_PLIST(out, m);
  SortDensePlist(out);
  return out;
}

/* less than or equal */

Obj FuncLeqPP(Obj self, Obj f, Obj g)
{
  Obj x;
  Int deg_f, deg_g, rank_f, rank_g, rank, i, j, k;

  deg_f = ELM_PP(f, 1);
  if(deg_f==0) return True;

  deg_g = ELM_PP(g, 1);
  if(deg_g==0) return False;

  rank_f = ELM_PP(f, 2);
  rank_g = ELM_PP(g, 2);
  if(rank_f<=rank_g)
  {
    rank = rank_f;
    x = True;
  }
  else
  {
    rank = rank_g;
    x = False;
  }
  for(i=1;i<=rank;i++)
  {
    j = ELM_PP(f,6+deg_f+rank_f+i);
    k = ELM_PP(g,6+deg_g+rank_g+i);
    if(j<k) return True;
    if(j>k) return False;
  }

  return x;
}

/* restricted partial perm */

Obj FuncRestrictedPP(Obj self, Obj f, Obj set)
{
  Int deg_f, n, deg_g, i, j, r, min_ran, max_ran, rank, k;
  Obj g;
  Int ran[513];

  deg_f = ELM_PP(f, 1);
  n = LEN_LIST(set);
  if(n==0||deg_f==0) return NEW_EMPTY_PP();

  /* find degree of restricted part perm */
  deg_g = 0;
  for(i=n;1<=i;i--)
  {
    j = INT_INTOBJ(ELM_LIST(set, i));
    if(j<=deg_f&&ELM_PP(f, j)!=0)
    {
      deg_g = j;
      r = i;
      break;
    }
  }

  if(deg_g==0) return NEW_EMPTY_PP();

  g = NEW_PP(3*deg_g+6);
  SET_ELM_PP(g, 1, (pptype) deg_g);

  max_ran=0;
  min_ran=65535;
  rank=0;

  for(i=1;i<=r;i++)
  {
    j = INT_INTOBJ(ELM_LIST(set, i));
    k = ELM_PP(f, 6+j);
    if(k!=0)
    {
      rank++;
      SET_ELM_PP(g, 6+j, k); /* dense img list */
      SET_ELM_PP(g, 6+deg_g+rank, (pptype) j); /* dom */
      ran[rank]=k;
      if(k>max_ran) max_ran=k;
      if(k<min_ran) min_ran=k;
    }
  }
  
  SET_ELM_PP(g,2,(pptype) rank);
  SET_ELM_PP(g,3,(pptype) min_ran);
  SET_ELM_PP(g,4,(pptype) max_ran); 

  /* set range */
  for(i=1;i<=rank;i++){
    SET_ELM_PP(g,deg_g+rank+6+i,(pptype) ran[i]);
  }

  /* set min */
  j=ELM_PP(g,deg_g+7); /* min. dom. */
  if(min_ran<j)
  {
    SET_ELM_PP(g,5,(pptype) min_ran);
  }
  else
  {
    SET_ELM_PP(g,5,j);
  }
   
  /* set max */
  if(max_ran>deg_g){
    SET_ELM_PP(g,6,(pptype) max_ran);
  }else{
    SET_ELM_PP(g,6,(pptype) deg_g);
  }

  ResizeBag(g, sizeof(pptype)*(LEN_PP(g))+sizeof(UInt));
  return g;
} 

/* less than or equal in natural partial order */

Obj FuncNaturalLeqPP(Obj self, Obj f, Obj g)
{ 
  Int deg, rank, i;
  deg = ELM_PP(f, 1);

  if(deg==0) return True;

  rank = ELM_PP(f, 2);

  for(i=1;i<=rank;i++)
  {
    if(ELM_PP(g, 6+ELM_PP(f, 6+deg+i))!=ELM_PP(f, 6+deg+rank+i))
    { 
      return False;
    }
  }

  return True;
}

/* right quotient */

Obj FuncQuoPP(Obj self, Obj f, Obj g)
{
  Int deg_f, deg_g, rank_f, rank_g, i, deg_lookup, deg, j, r, max_ran;
  Int min_ran, k, l, rank, min_dom, max_dom;
  Obj lookup, fg;
  Int ran[513];

  deg_f = ELM_PP(f, 1);
  deg_g = ELM_PP(g, 1);
  
  if(deg_f==0||deg_g==0) return NEW_EMPTY_PP();

  rank_f = ELM_PP(f, 2);
  rank_g = ELM_PP(g, 2);

  /* find lookup for g^-1 */
  lookup = NEW_PP(ELM_PP(g, 4));

  for(i=1;i<=rank_g;i++)
  {
    SET_ELM_PP(lookup, ELM_PP(g, 6+deg_g+rank_g+i), ELM_PP(g, 6+deg_g+i));
  }

  /* find degree/max dom */
  deg_lookup = ELM_PP(g, 4); /* max dom g^-1 = max ran g */
  deg = 0;

  for(i=rank_f;1<=i;i--)
  {
    j = ELM_PP(f,6+deg_f+rank_f+i);
    if( j<=deg_g && ELM_PP(lookup,j)!=0)
    {
      deg = ELM_PP(f,6+deg_f+i);
      r = i;
      break;
    }
  }

  if(deg==0) return NEW_EMPTY_PP();
  
  /* initialize the quotient */
  fg = NEW_PP(3*deg+6);
  SET_ELM_PP(fg, 1, deg);
  
  max_ran=0;
  min_ran=ELM_PP(g, 7+deg_g);           /* max dom g = max ran g^-1 */
  rank=0;
  
  for (i=1;i<=r;i++)
  {
    j = ELM_PP(f, 6+deg_f+rank_f+i);    /* from ran(f) */
    if(j<=deg_lookup)
    {
      k = ELM_PP(lookup,j);             /* from dom(g^-1) */
      if(k!=0)
      {
        rank++;
        l = ELM_PP(f,6+deg_f+i);        /* from dom(f) */ 
        SET_ELM_PP(fg,deg+rank+6,l);    /* dom(fg) */
        SET_ELM_PP(fg,l+6, k);          /* dense img fg */ 
        ran[rank]=k;                    /* ran(fg) */
        if(k>max_ran) max_ran=k;
        if(k<min_ran) min_ran=k;
      }
    }
  }
  
  SET_ELM_PP(fg,2, rank);
  SET_ELM_PP(fg,3, min_ran);
  SET_ELM_PP(fg,4, max_ran);
  min_dom = ELM_PP(fg, 7+deg);
  SET_ELM_PP(fg,5, min_ran<min_dom?min_ran:min_dom);
  max_dom = ELM_PP(fg, 6+deg+rank);
  SET_ELM_PP(fg,6, max_ran>max_dom?max_ran:max_dom);

  for(i=1;i<=rank;i++)
  {
    SET_ELM_PP(fg,deg+rank+6+i,ran[i]);
  }
  
  ResizeBag(fg, sizeof(pptype)*(LEN_PP(fg))+sizeof(UInt));
  return fg;
}

/* product of partial perm and perm */

Obj FuncProdPPPerm(Obj self, Obj f, Obj p)
{
  Int deg_f, rank_f, deg_p, max_ran, min_ran, i, j, k, min_dom, max_dom;
  UInt2 * ptp;
  Obj fp;
 
  if(TNUM_OBJ(p) != T_PERM2)
  {
    ErrorQuit( 
        "usage: can only multiply a partial perm and perm on at most 65535 pts", 
        0L, 0L );
    return 0L;
  }
  deg_f = ELM_PP(f, 1);
  if(deg_f==0) return NEW_EMPTY_PP();

  fp = NEW_PP(LEN_PP(f));
  rank_f = ELM_PP(f, 2);
  deg_p = DEG_PERM2(p);
  ptp = ADDR_PERM2(p);

  SET_ELM_PP(fp, 1, deg_f);
  SET_ELM_PP(fp, 2, rank_f);

  max_ran=0;
  min_ran=65535;

  for(i=1;i<=rank_f;i++)
  {
    j = ELM_PP(f, 6+deg_f+i);
    SET_ELM_PP(fp, 6+deg_f+i, j);           /* dom */
    k = IMAGE(ELM_PP(f, 6+deg_f+rank_f+i)-1, ptp, deg_p)+1;       
    SET_ELM_PP(fp, 6+deg_f+rank_f+i, k);    /* ran */
    SET_ELM_PP(fp, 6+j, k);                 /* dense img */ 
    if(k>max_ran) max_ran=k;
    if(k<min_ran) min_ran=k;
  }    
   
  SET_ELM_PP(fp, 3, min_ran); 
  SET_ELM_PP(fp, 4, max_ran);
  min_dom=ELM_PP(fp, 7+deg_f);
  SET_ELM_PP(fp, 5, min_ran<min_dom?min_ran:min_dom);
  max_dom=ELM_PP(fp, 6+deg_f+rank_f);
  SET_ELM_PP(fp, 6, max_ran>max_dom?max_ran:max_dom);
  
  return fp;
}

/* product of perm and partial perm */

Obj FuncProdPermPP(Obj self, Obj p, Obj f)
{ pptype deg_f, rank, deg_p, deg, i, j, max_ran, min_ran, k, l, min_dom, max_dom;
  UInt2 * ptp;
  Obj pf;
  
  deg_f = ELM_PP(f, 1);
  if(deg_f==0) return NEW_EMPTY_PP();

  rank = ELM_PP(f, 2);
  deg_p = (pptype) DEG_PERM2(p);
  ptp = ADDR_PERM2(p);

  if(deg_p>=deg_f)
  {
    deg=0;

    /* find degree/max. dom */
    for(i=deg_p;1<=i;i--)
    {
      j = IMAGE(i-1, ptp, deg_p)+1; 
      if( j<=deg_f && ELM_PP(f,j+6)!=0)
      {
        deg=i;
        break;
      }
    }  
    if(deg==0) return NEW_EMPTY_PP();
  }else{
    deg=deg_f;
  }    
 
  pf=NEW_PP(deg+2*rank+6);
  
  SET_ELM_PP(pf, 1, deg);
  SET_ELM_PP(pf, 2, rank);

  max_ran=0; 
  min_ran=ELM_PP(f, 4);
  l=0;

  for(i=1;i<=deg;i++)
  {
    j = IMAGE(i-1, ptp, deg_p)+1;
    if(j<=deg_f)
    { 
      k=ELM_PP(f,j+6);
      if(k!=0)
      { 
        l++;
        SET_ELM_PP(pf,deg+l+6,i);       /* dom */
        SET_ELM_PP(pf,deg+rank+l+6,k);  /* ran */
        SET_ELM_PP(pf,i+6,k);           /* dense img */
        if(k>max_ran) max_ran=k;                
        if(k<min_ran) min_ran=k;
      }
    }
  }

  SET_ELM_PP(pf, 3, min_ran);
  SET_ELM_PP(pf, 4, max_ran);
  min_dom=ELM_PP(pf, 7+deg);
  SET_ELM_PP(pf, 5, min_ran<min_dom?min_ran:min_dom);
  max_dom=ELM_PP(pf, 6+deg+rank);
  SET_ELM_PP(pf, 6, max_ran>max_dom?max_ran:max_dom);
  
  return pf;
}

/*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * */

/******************************************************************************
*V  GVarFuncs . . . . . . . . . . . . . . . . . . list of functions to export
*/
static StructGVarFunc GVarFuncs [] = {

  { "ELM_LIST_PP", 2, "f,i", 
    FuncELM_LIST_PP, 
    "pkg/citrus/src/citrus.c:ELM_LIST_PP" },

  { "ELMS_LIST_PP", 2, "f,list",
    FuncELMS_LIST_PP,
    "pkg/citrus/src/citrus.c:ELMS_LIST_PP" },
 
  { "SparsePartialPermNC", 2, "dom,ran",
    FuncSparsePartialPermNC,
    "pkg/citrus/src/citrus.c:FuncSparsePartialPermNC" },

  { "DensePartialPermNC", 1, "img",
    FuncDensePartialPermNC,
    "pkg/citrus/src/citrus.c:FuncDensePartialPermNC" },
 
  { "ProdPP", 2, "f,g",
    FuncProdPP,
    "pkg/citrus/src/citrus.c:FuncProdPP" },

  { "RanSet", 1, "f",
    FuncRanSet,
    "pkg/citrus/src/citrus.c:FuncRanSet" },

  { "InvPP", 1, "f",
    FuncInvPP,
    "pkg/citrus/src/citrus.c:FuncInvPP" },

  { "OnIntegerSetsWithPP", 2, "set,f",
    FuncOnIntegerSetsWithPP,
    "pkg/citrus/src/citrus.c:FuncOnIntegerSetsWithPP" },
  
  { "EqPP", 2, "f,g",
    FuncEqPP,
    "pkg/citrus/src/citrus.c:FuncEqPP" },

  { "LeftOne", 1, "f",
    FuncLeftOne,
    "pkg/citrus/src/citrus.c:FuncLeftOne" },

  { "RightOne", 1, "f",
    FuncRightOne,
    "pkg/citrus/src/citrus.c:FuncRightOne" },

  { "FixedPointsPP", 1, "f",
    FuncFixedPointsPP,
    "pkg/citrus/src/citrus.c:FuncFixedPointsPP" },

  { "LeqPP", 2, "f, g",
    FuncLeqPP,
    "pkg/citrus/src/citrus.c:FuncLeqPP" },

  { "RestrictedPP", 2, "f, set", 
    FuncRestrictedPP, 
    "pkg/citrus/src/citrus.c:FuncRestrictedPP" },

  { "NaturalLeqPP", 2, "f, g", 
    FuncNaturalLeqPP,
    "pkg/citrus/src/citrus.c:FuncNaturalLeqPP" },

  { "QuoPP", 2, "f, g", 
    FuncQuoPP,
    "pkg/citrus/src/citrus.c:FuncQuoPP" },

  { "ProdPPPerm", 2, "f, p",
    FuncProdPPPerm, 
    "pkg/citrus/src/citrus.c:FuncProdPPPerm" },

  { "ProdPermPP", 2, "p, f",
    FuncProdPermPP, 
    "pkg/citrus/src/citrus.c:FuncProdPermPP" },

  { 0 }

};

/******************************************************************************
*F  InitKernel( <module> )  . . . . . . . . initialise kernel data structures
*/
static Int InitKernel ( StructInitInfo *module )
{
    /* init filters and functions                                          */
    InitHdlrFuncsFromTable( GVarFuncs );

    ImportGVarFromLibrary( "PartialPermType", &PartialPermType );

    /* return success                                                      */
    return 0;
}

Obj FuncADD_SET(Obj self, Obj set, Obj obj);

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
    gvar = GVarName("CITRUSC"); AssGVar( gvar, tmp ); MakeReadOnlyGVar(gvar);

    /* return success                                                      */
    return 0;
}

/******************************************************************************
*F  InitInfopl()  . . . . . . . . . . . . . . . . . table of init functions
*/
static StructInitInfo module = {
#ifdef CITRUSSTATIC
 /* type        = */ MODULE_STATIC,
#else
 /* type        = */ MODULE_DYNAMIC,
#endif
 /* name        = */ "citrus",
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

#ifndef CITRUSSTATIC
StructInitInfo * Init__Dynamic ( void )
{
  module.revision_c = Revision_citrus_c;
  return &module;
}
#endif

StructInitInfo * Init__citrus ( void )
{
  module.revision_c = Revision_citrus_c;
  return &module;
}




