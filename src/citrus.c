
const char * Revision_citrus_c =
   "$Id: citrus.c,v$";

#include "citrus.h"

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
** An element of the internal rep of a partial perm must be at most 65535 and be
** of pttype, but the length and indices can be larger than 65535 and so these
** currently have type Int. 
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
** Macros for partial perms specifically
*******************************************************************************/

/* length of the partial perm internal rep */
static inline Int LEN_PP(Obj f)
{
  return (ELM_PT(f,1)==0?7:ELM_PT(f,1)+3*ELM_PT(f,2)+6);
}

/* set the range set of a partial perm */
static inline void SET_RANSET_PP(Obj f, pttype deg, pttype rank)
{   
  pttype i;
 
  for(i=1;i<=rank;i++)
  {
    SET_ELM_PT(f,2*rank+deg+6+i, ELM_PT(f,rank+deg+6+i));
  }
  qsort((pttype *)(ADDR_OBJ(f)+1)+6+deg+2*rank, rank, sizeof(pttype), cmp);
}  

/*******************************************************************************
** GAP functions for partial permutations
*******************************************************************************/

/* method for f[i] */
Obj FuncELM_LIST_PP( Obj self, Obj f, Obj i)
{ 
  if(INT_INTOBJ(i)>LEN_PP(f)) return Fail;
  return INTOBJ_INT(ELM_PT(f, INT_INTOBJ(i)));
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
        INTOBJ_INT(ELM_PT(f, INT_INTOBJ(ELM_LIST(list, i)))));
    }

    return out;
}

/* create partial perm from full representation */
Obj FuncFullPartialPermNC( Obj self, Obj rep )
{   Int len, i;
    Obj f;

    len=LEN_PLIST(rep);
    f=NEW_PP(len);
    for(i=1;i<=len;i++)
    { 
      SET_ELM_PT(f,i,(pttype) INT_INTOBJ(ELM_PLIST(rep, i)));
    }
  
    return f;
}

/* create partial perm from sparse representation */
Obj FuncSparsePartialPermNC( Obj self, Obj dom, Obj ran )
{   Int rank, deg, j, k, max_ran, min_ran;
    pttype i;
    Obj f;

    rank=LEN_LIST(dom);

    if(rank==0) return NEW_EMPTY_PT();
    
    deg=INT_INTOBJ(ELM_LIST(dom,rank));
   
    TOO_MANY_PTS_ERROR(rank>65535||deg>65535);

    f=NEW_PP(6+deg+3*rank);

    SET_ELM_PT(f, 1, (pttype) deg);
    SET_ELM_PT(f, 2, (pttype) rank);

    max_ran=0; 
    min_ran=65535; 

    /* find dense img list, max_ran, min_ran */
    for(i=1;i<=rank;i++){
      j=INT_INTOBJ(ELM_LIST(dom, i));
      SET_ELM_PT(f, 6+deg+i, (pttype) j);
      
      k=INT_INTOBJ(ELM_LIST(ran, i));
      SET_ELM_PT(f, 6+deg+rank+i, (pttype) k);
      SET_ELM_PT(f, j+6, (pttype) k);

      if(k>max_ran) max_ran=k;
      if(k<min_ran) min_ran=k;
    }

    TOO_MANY_PTS_ERROR(max_ran>65535);

    SET_ELM_PT(f,3,(pttype) min_ran);
    SET_ELM_PT(f,4,(pttype) max_ran);

    /* set min */
    j=INT_INTOBJ(ELM_LIST(dom,1));
    SET_ELM_PT(f,5,min_ran<j?(pttype) min_ran:(pttype) j);
    
    /* set max */
    SET_ELM_PT(f,6,max_ran>deg?(pttype) max_ran:(pttype) deg);
    return f;
}

/* create dense partial perm */
Obj FuncDensePartialPermNC( Obj self, Obj img )
{ 
    Obj f, buf;
    Int deg, i, max_ran, min_ran, rank, j, k;
    Int ran[512];

    if(LEN_LIST(img)==0) return NEW_EMPTY_PT();
    
    /* remove trailing zeros */
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

    if(deg==0) return NEW_EMPTY_PT();
    
    f = NEW_PP(3*deg+6); /* the output*/
    SET_ELM_PT(f, 1, (pttype) deg);
    
    max_ran=0; 
    min_ran=65535; 
    rank=0;
    i=0;

    /* find dom, rank, max_ran, min_ran */
    while(i<deg&&rank<512)
    {
      i++;
      j = INT_INTOBJ(ELM_LIST(img, i));
      SET_ELM_PT(f, i+6, (pttype) j);
      if(j!=0)
      {
        rank++;
        SET_ELM_PT(f, deg+rank+6, (pttype) i); /* dom*/
        ran[rank]=j;
        if(j>max_ran) max_ran=j;
        if(j<min_ran) min_ran=j;
      }
    }
   
    if(i<deg){
      buf=NEW_PP(65023); /* internal only */
      /* carry on */
      for(k=i+1;k<=deg;k++)
      {
        j = INT_INTOBJ(ELM_LIST(img, k));
        SET_ELM_PT(f, k+6, (pttype) j);
        if(j!=0)
        {
          rank++;
          SET_ELM_PT(f, deg+rank+6, (pttype) k); /* dom*/
          SET_ELM_PT(buf, rank, (pttype) j);
          if(j>max_ran) max_ran=j;
          if(j<min_ran) min_ran=j;
        }
      }
    }

    TOO_MANY_PTS_ERROR(max_ran>65535);

    SET_ELM_PT(f,2,(pttype) rank);
    SET_ELM_PT(f,3,(pttype) min_ran);
    SET_ELM_PT(f,4,(pttype) max_ran);

    /* set range */
    for(i=1;i<=(rank<513?rank:512);i++)
    {
      SET_ELM_PT(f,deg+rank+6+i,(pttype) ran[i]);
    }
      
    for(i=513;i<=rank;i++) 
    {      
      SET_ELM_PT(f,deg+rank+6+i,ELM_PT(buf,i)); 
    }   

    /* set min */
    j=ELM_PT(f,deg+7); /* min. dom. */
    SET_ELM_PT(f,5,min_ran<j?(pttype) min_ran:j);

    /* set max */
    SET_ELM_PT(f,6,max_ran>deg?(pttype) max_ran:(pttype) deg); 
    
    ResizeBag(f, sizeof(pttype)*(LEN_PP(f))+sizeof(UInt));
    return f;
}

/* product of partial permutations */
Obj FuncProdPP( Obj self, Obj f, Obj g )
{   pttype deg_f, deg_g, deg, rank_f, i, r, max_ran, min_ran, rank, j, k, l;
    Obj fg;
    pttype ran[ELM_PT(f,2)<ELM_PT(g,2)?ELM_PT(f,2):ELM_PT(g,2)];

    deg_f=ELM_PT(f,1);
    deg_g=ELM_PT(g,1);
    
    if(deg_f==0||deg_g==0) return NEW_EMPTY_PT();
   
    deg=0;
    rank_f=ELM_PT(f,2);

    /* find degree/max. dom */
    for(i=rank_f;1<=i;i--)
    {
      j =ELM_PT(f,6+deg_f+rank_f+i);
      if(j<=deg_g&&ELM_PT(g,j+6)!=0)
      {
        deg=ELM_PT(f,6+deg_f+i);
        r=i;
        break;
      }
    } 

    if(deg==0) return NEW_EMPTY_PT();

    fg = NEW_PP(3*deg+6);
    SET_ELM_PT(fg,1,deg);
   
    max_ran=0;
    min_ran=ELM_PT(g,4); /* max ran(g) */
    rank=0;

    for (i=1;i<=r;i++)
    {
      j=ELM_PT(f,6+deg_f+rank_f+i);
      if(j<=deg_g)
      {
        k=ELM_PT(g,j+6);
        if(k!=0)
        { 
          rank++;
          l=ELM_PT(f,6+deg_f+i);
          SET_ELM_PT(fg,deg+rank+6,l);
          SET_ELM_PT(fg,l+6,k);
          ran[rank]=k;
          if(k>max_ran) max_ran=k;
          if(k<min_ran) min_ran=k;
        }
      }
    }

    SET_ELM_PT(fg,2,(pttype) rank);
    SET_ELM_PT(fg,3,min_ran);
    SET_ELM_PT(fg,4,max_ran);
   
    /* install the range */
    for(i=1;i<=rank;i++)
    {
      SET_ELM_PT(fg,deg+rank+6+i,ran[i]);
    }
 
    /* set min and max */
    j=ELM_PT(fg,deg+7);
    SET_ELM_PT(fg,5,min_ran<j?min_ran:j);
    SET_ELM_PT(fg,6,max_ran>deg?max_ran:deg);

    ResizeBag(fg, sizeof(pttype)*(LEN_PP(fg))+sizeof(UInt));
    return fg;
}

/* domain of partial permutation */
Obj FuncDomPP (Obj self, Obj f )
{ pttype deg, rank, i;
  Obj out;

  deg=ELM_PT(f, 1);
  if(deg==0) return NEW_EMPTY_PLIST();
  rank=ELM_PT(f, 2);
  out=NEW_PLIST(T_PLIST_CYC,rank);
  SET_LEN_PLIST(out,(Int) rank);
  for(i=1;i<=rank;i++)
  {
    SET_ELM_PLIST(out,i,INTOBJ_INT(ELM_PT(f,6+deg+i)));
  }
  return out;
}

/* range of partial permutation */
Obj FuncRanPP (Obj self, Obj f )
{ pttype deg, rank, i;
  Obj out;

  deg=ELM_PT(f, 1);
  if(deg==0) return NEW_EMPTY_PLIST();
  rank=ELM_PT(f, 2);
  out=NEW_PLIST(T_PLIST_CYC,rank);
  SET_LEN_PLIST(out,(Int) rank);
  for(i=1;i<=rank;i++)
  {
    SET_ELM_PLIST(out,i,INTOBJ_INT(ELM_PT(f,6+deg+rank+i)));
  }
  return out;
}

/* range set of partial permutation */
Obj FuncRanSetPP ( Obj self, Obj f )
{ pttype deg, rank, i;
  Obj out;

  deg=ELM_PT(f, 1);
  
  if(deg==0) return NEW_EMPTY_PLIST();

  rank=ELM_PT(f, 2);

  if(ELM_PT(f, 7+deg+2*rank)==0) SET_RANSET_PP(f, deg, rank);

  out = NEW_PLIST(T_PLIST_CYC,rank);
  SET_LEN_PLIST(out,(Int) rank);
  for(i=1;i<=rank;i++)
  {
    SET_ELM_PLIST(out,i, INTOBJ_INT(ELM_PT(f,2*rank+deg+6+i)));
  }
  return out;
}

/* inverse of a partial permutation */
Obj FuncInvPP ( Obj self, Obj f )
{
  pttype deg_f, rank, i, deg_f_inv, j;
  Obj f_inv;

  deg_f=ELM_PT(f,1);
  if(deg_f==0) return NEW_EMPTY_PT();

  rank=ELM_PT(f,2);

  /* check if f knows Set(Ran(f)) if not set it */
  if(ELM_PT(f,7+deg_f+2*rank)==0) SET_RANSET_PP(f,deg_f,rank);
   
  deg_f_inv=ELM_PT(f,4); /* max ran(f) */
  f_inv=NEW_PP(6+deg_f_inv+3*rank);

  SET_ELM_PT(f_inv, 1, ELM_PT(f, 4));
  SET_ELM_PT(f_inv, 2, ELM_PT(f, 2));
  SET_ELM_PT(f_inv, 3, ELM_PT(f, 7+deg_f));
  SET_ELM_PT(f_inv, 4, ELM_PT(f, 6+deg_f+rank));
  SET_ELM_PT(f_inv, 5, ELM_PT(f, 5));
  SET_ELM_PT(f_inv, 6, ELM_PT(f, 6));

  /* set dense img, dom, and ran set */
  for(i=1;i<=rank;i++)
  {
    j = ELM_PT(f,i+deg_f+6);
    SET_ELM_PT(f_inv, ELM_PT(f,i+deg_f+rank+6)+6, j); /* dense img */
    SET_ELM_PT(f_inv, 6+deg_f_inv+i, ELM_PT(f, 6+deg_f+2*rank+i)); /*dom*/
    SET_ELM_PT(f_inv, i+6+deg_f_inv+2*rank, j); /*ran set*/
  }

  /* specify ran */
  for(i=1;i<=rank;i++)
  {
    j = ELM_PT(f_inv, 6+deg_f_inv+i);
    SET_ELM_PT(f_inv, i+6+deg_f_inv+rank, ELM_PT(f_inv, j+6));
  }
  return f_inv;
}

/* on sets for a partial permutation */ 
Obj FuncOnIntegerSetsWithPP (Obj self, Obj set, Obj f)
{ pttype deg, k;
  Int n, i, j, m;
  Obj out;

  deg=ELM_PT(f,1);
  n=LEN_LIST(set);
  if(n==0||deg==0) return NEW_EMPTY_PLIST();

  out = NEW_PLIST(T_PLIST_CYC, n);
  m = 0;

  for(i=1;i<=n;i++)
  {
    j=INT_INTOBJ(ELM_LIST(set, i));
    if(j<=deg)
    {
      k=ELM_PT(f, j+6);
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

/* on tuples for a partial permutation */ 
Obj FuncOnIntegerTuplesWithPP (Obj self, Obj set, Obj f)
{ pttype deg, k;
  Int n, i, j, m;
  Obj out;

  deg=ELM_PT(f,1);
  n=LEN_LIST(set);
  if(n==0||deg==0) return NEW_EMPTY_PLIST();

  out = NEW_PLIST(T_PLIST_CYC, n);
  m = 0;

  for(i=1;i<=n;i++)
  {
    j=INT_INTOBJ(ELM_LIST(set, i));
    if(j<=deg)
    {
      k=ELM_PT(f, j+6);
      if(k!=0)
      {
        m++;
        SET_ELM_PLIST(out, m, INTOBJ_INT(k));
      }
    }
  }
  SET_LEN_PLIST(out, m);
  SHRINK_PLIST(out, m);
  return out;
}

/* equality test for partial permutations */
Obj FuncEqPP (Obj self, Obj f, Obj g)
{ pttype deg_f, rank_f, i;

  deg_f=ELM_PT(f, 1);
  
  if(deg_f!=ELM_PT(g,1)) return False;
  if(deg_f==0) return True;

  rank_f=ELM_PT(f, 2);

  if(rank_f!=ELM_PT(g,2)) return False;

  /* search for a difference */
  for(i=7+deg_f;i<=6+deg_f+rank_f;i++)
  {
    if(ELM_PT(f, i)!=ELM_PT(g, i)||ELM_PT(f, i+rank_f)!=ELM_PT(g, i+rank_f))
    { 
      return False;
    }
  }

  return True;
}

/* idempotent on domain of partial perm */
Obj FuncLeftOne(Obj self, Obj f)
{ pttype deg, rank, min, max, i, j;
  Obj one;

  deg=ELM_PT(f, 1);
  
  if(deg==0) return NEW_EMPTY_PT();

  one=NEW_PP(LEN_PP(f));
  rank=ELM_PT(f, 2);

  SET_ELM_PT(one, 1, deg);
  SET_ELM_PT(one, 2, rank);
  
  min = ELM_PT(f, 7+deg);
  max = ELM_PT(f, 6+deg+rank);
  
  SET_ELM_PT(one, 3, min);
  SET_ELM_PT(one, 4, max);
  SET_ELM_PT(one, 5, min);
  SET_ELM_PT(one, 6, max);
  
  for(i=7+deg;i<=6+deg+rank;i++)
  {
    j = ELM_PT(f, i);
    SET_ELM_PT(one, i, j);        /* dom */
    SET_ELM_PT(one, i+rank, j);   /* ran */
    SET_ELM_PT(one, i+2*rank, j); /* ran set */
    SET_ELM_PT(one, 6+j, j);      /* dense img */
  }

  return one;
}

/* idempotent on range of partial perm */
Obj FuncRightOne(Obj self, Obj f)
{ Obj one;
  pttype deg, rank, min, max, i, j;

  deg = ELM_PT(f, 1);
  
  if(deg==0) return f;

  rank=ELM_PT(f, 2);
  min=ELM_PT(f, 3);
  max=ELM_PT(f, 4);
  
  one=NEW_PP(6+max+3*rank);
  
  SET_ELM_PT(one, 1, max);
  SET_ELM_PT(one, 2, rank);
  SET_ELM_PT(one, 3, min);
  SET_ELM_PT(one, 4, max);
  SET_ELM_PT(one, 5, min);
  SET_ELM_PT(one, 6, max);
  
  if(ELM_PT(f, 7+deg+2*rank)==0) SET_RANSET_PP(f, deg, rank);

  for(i=1;i<=rank;i++)
  {
    j = ELM_PT(f, 6+deg+2*rank+i);
    SET_ELM_PT(one, 6+max+i, j);        /* dom */
    SET_ELM_PT(one, 6+max+rank+i, j);   /* ran */
    SET_ELM_PT(one, 6+max+2*rank+i, j); /* ran set */
    SET_ELM_PT(one, 6+j, j); 
  }

  return one;
}

/* fixed points */
Obj FuncFixedPointsPP(Obj self, Obj f)
{ pttype deg, rank, i;
  Int m;
  Obj out;

  deg=ELM_PT(f, 1);
  if(deg==0) return NEW_EMPTY_PLIST();
  
  rank=ELM_PT(f, 2);
  out=NEW_PLIST(T_PLIST_CYC, rank);
  m=0;

  for(i=7+deg;i<=6+deg+rank;i++)
  {
    if(ELM_PT(f, i)==ELM_PT(f, i+rank))
    { 
      m++;
      SET_ELM_PLIST(out, m, INTOBJ_INT(ELM_PT(f, i)));
    }
  }
  SET_LEN_PLIST(out, m);
  SHRINK_PLIST(out, m);
  SortDensePlist(out);
  return out;
}

/* moved points */
Obj FuncMovedPointsPP(Obj self, Obj f)
{ pttype deg, rank, i;
  Int m;
  Obj out;

  deg=ELM_PT(f, 1);
  if(deg==0) return NEW_EMPTY_PLIST();
  
  rank=ELM_PT(f, 2);
  out=NEW_PLIST(T_PLIST_CYC, rank);
  m=0;

  for(i=7+deg;i<=6+deg+rank;i++)
  {
    if(ELM_PT(f, i)!=ELM_PT(f, i+rank))
    { 
      m++;
      SET_ELM_PLIST(out, m, INTOBJ_INT(ELM_PT(f, i)));
    }
  }
  if(m==0) return NEW_EMPTY_PLIST();
  /* why is the above line required here but not in FixedPoints?? */
  SET_LEN_PLIST(out, m);
  SHRINK_PLIST(out, m);
  SortDensePlist(out);
  return out;
}

/* nr moved points */
Obj FuncNrMovedPointsPP(Obj self, Obj f)
{ pttype deg, rank, i;
  Int m;

  deg=ELM_PT(f, 1);
  if(deg==0) return INTOBJ_INT(0);
  
  rank=ELM_PT(f, 2);
  m=0;

  for(i=7+deg;i<=6+deg+rank;i++)
  {
    if(ELM_PT(f, i)!=ELM_PT(f, i+rank)) m++;
  }
  return INTOBJ_INT(m);
}

/* largest moved points */
Obj FuncLargestMovedPointPP(Obj self, Obj f)
{ pttype deg, rank, i;
  Int m;

  deg=ELM_PT(f, 1);
  if(deg==0) return INTOBJ_INT(0);
  
  rank=ELM_PT(f, 2);
  m=0;

  for(i=6+deg+rank;i>=7+deg;i--)
  {
    if(ELM_PT(f, i)!=ELM_PT(f, i+rank)) return INTOBJ_INT(ELM_PT(f, i));
  }
  return INTOBJ_INT(0);
}

/* smallest moved points */
Obj FuncSmallestMovedPointPP(Obj self, Obj f)
{ pttype deg, rank, i;
  Int m;

  deg=ELM_PT(f, 1);
  if(deg==0) return INTOBJ_INT(0);
  
  rank=ELM_PT(f, 2);
  m=0;

  for(i=7+deg;i<=6+deg+rank;i++)
  {
    if(ELM_PT(f, i)!=ELM_PT(f, i+rank)) return INTOBJ_INT(ELM_PT(f, i));
  }
  return INTOBJ_INT(0);
}

/* less than or equal */
Obj FuncLeqPP(Obj self, Obj f, Obj g)
{
  pttype deg_f, deg_g, rank_f, rank_g, i, j, k;

  deg_g=ELM_PT(g, 1);
  if(deg_g==0) return False;
  
  deg_f=ELM_PT(f, 1);
  if(deg_f==0) return True;
  
  rank_f=ELM_PT(f, 2);
  rank_g=ELM_PT(g, 2);
  if(rank_f<rank_g) return True;
  if(rank_g<rank_f) return False;

  for(i=1;i<=2*rank_f;i++)
  {
    j=ELM_PT(f,6+deg_f+i);
    k=ELM_PT(g,6+deg_g+i);
    if(j<k) return True;
    if(j>k) return False;
  }

  return False;
}

/* restricted partial perm */
Obj FuncRestrictedPP(Obj self, Obj f, Obj set)
{ pttype deg_f, deg_g, min_ran, max_ran, rank, k;
  Int n, i, j, r; 
  Obj g;
  Int ran[LEN_LIST(set)];

  deg_f=ELM_PT(f, 1);
  n=LEN_LIST(set);
  if(n==0||deg_f==0) return NEW_EMPTY_PT();

  /* find degree of restricted part perm */
  deg_g=0;
  for(i=n;1<=i;i--)
  {
    j=INT_INTOBJ(ELM_LIST(set, i));
    if(j<=deg_f&&ELM_PT(f, j)!=0)
    {
      deg_g=(pttype) j;
      r=i;
      break;
    }
  }

  if(deg_g==0) return NEW_EMPTY_PT();

  g=NEW_PP(3*deg_g+6);
  SET_ELM_PT(g,1,deg_g);

  max_ran=0;
  min_ran=ELM_PT(f,4);
  rank=0;

  for(i=1;i<=r;i++)
  {
    j = INT_INTOBJ(ELM_LIST(set, i));
    k = ELM_PT(f, 6+j);
    if(k!=0)
    {
      rank++;
      SET_ELM_PT(g, 6+j, k); /* dense img list */
      SET_ELM_PT(g, 6+deg_g+rank, (pttype) j); /* dom */
      ran[rank]=k;
      if(k>max_ran) max_ran=k;
      if(k<min_ran) min_ran=k;
    }
  }
  
  SET_ELM_PT(g,2,rank);
  SET_ELM_PT(g,3,min_ran);
  SET_ELM_PT(g,4,max_ran); 

  /* set range */
  for(i=1;i<=rank;i++){
    SET_ELM_PT(g,deg_g+rank+6+i,ran[i]);
  }

  /* set min */
  j=ELM_PT(g,deg_g+7); /* min. dom. */
  SET_ELM_PT(g,5,min_ran<j?min_ran:j);
   
  /* set max */
  SET_ELM_PT(g,6,max_ran>deg_g?max_ran:deg_g); 

  ResizeBag(g, sizeof(pttype)*(LEN_PP(g))+sizeof(UInt));
  return g;
} 

/* less than or equal in natural partial order */
Obj FuncNaturalLeqPP(Obj self, Obj f, Obj g)
{ pttype deg, rank, i;
  deg = ELM_PT(f, 1);

  if(deg==0) return True;

  rank = ELM_PT(f, 2);

  for(i=1;i<=rank;i++)
  {
    if(ELM_PT(g, 6+ELM_PT(f, 6+deg+i))!=ELM_PT(f, 6+deg+rank+i))
    { 
      return False;
    }
  }

  return True;
}

/* right quotient */
Obj FuncQuoPP(Obj self, Obj f, Obj g)
{ pttype deg_f, deg_g, rank_f, rank_g, i, deg_lookup, deg, j, r, max_ran;
  pttype min_ran, k, l, rank;
  pttype ran[ELM_PT(f,2)<ELM_PT(g,2)?ELM_PT(f,2):ELM_PT(g,2)];
  pttype lookup[ELM_PT(g,4)];
  Obj fg;

  deg_f = ELM_PT(f, 1);
  deg_g = ELM_PT(g, 1);
  
  if(deg_f==0||deg_g==0) return NEW_EMPTY_PT();

  rank_f = ELM_PT(f, 2);
  rank_g = ELM_PT(g, 2);

  /* find lookup for g^-1 */
  deg_lookup = ELM_PT(g, 4); /* max dom g^-1 = max ran g */
  
  for(i=1;i<=deg_lookup;i++) lookup[i]=0;

  for(i=1;i<=rank_g;i++)
  {
    lookup[ELM_PT(g, 6+deg_g+rank_g+i)]=ELM_PT(g, 6+deg_g+i);
  }

  /* find degree/max dom */
  deg = 0;

  for(i=rank_f;1<=i;i--)
  {
    j = ELM_PT(f,6+deg_f+rank_f+i);
    if( j<=deg_lookup && lookup[j]!=0)
    {
      deg = ELM_PT(f,6+deg_f+i);
      r = i;
      break;
    }
  }

  if(deg==0) return NEW_EMPTY_PT();
  
  /* initialize the quotient */
  fg = NEW_PP(3*deg+6);
  SET_ELM_PT(fg, 1, deg);
  
  max_ran=0;
  min_ran=ELM_PT(g, 6+deg_g);             /* max dom g = max ran g^-1 */
  rank=0;
  
  for (i=1;i<=r;i++)
  {
    j = ELM_PT(f, 6+deg_f+rank_f+i);    /* from ran(f) */
    if(j<=deg_lookup)
    {
      k = lookup[j];                    /* from dom(g^-1) */
      if(k!=0)
      {
        rank++;
        l = ELM_PT(f,6+deg_f+i);        /* from dom(f) */ 
        SET_ELM_PT(fg,deg+rank+6,l);    /* dom(fg) */
        SET_ELM_PT(fg,l+6, k);          /* dense img fg */ 
        ran[rank]=k;                    /* ran(fg) */
        if(k>max_ran) max_ran=k;
        if(k<min_ran) min_ran=k;
      }
    }
  }
  
  SET_ELM_PT(fg,2,rank);
  SET_ELM_PT(fg,3,min_ran);
  SET_ELM_PT(fg,4,max_ran);
  j=ELM_PT(fg,7+deg);
  SET_ELM_PT(fg,5, min_ran<j?min_ran:j);
  j=ELM_PT(fg, 6+deg+rank);
  SET_ELM_PT(fg,6, max_ran>j?max_ran:j);

  for(i=1;i<=rank;i++)
  {
    SET_ELM_PT(fg,deg+rank+6+i,ran[i]);
  }
  
  ResizeBag(fg, sizeof(pttype)*(LEN_PP(fg))+sizeof(UInt));
  return fg;
}

/* product of partial perm and perm */
Obj FuncProdPPPerm(Obj self, Obj f, Obj p)
{ pttype deg_f, rank_f, deg_p, max_ran, min_ran, i, j, k;
  UInt2 * ptp;
  Obj fp, lmp;
 
  if(TNUM_OBJ(p)==T_PERM4)
  { lmp=FuncLARGEST_MOVED_POINT_PERM(self,p);
    if (INT_INTOBJ(lmp) <= 65535) 
    {
      FuncTRIM_PERM(self,p,lmp);
    }else{
      ErrorQuit( 
     "usage: can only multiply a partial perm and perm on at most 65535 pts", 
        0L, 0L );
      return 0L;
    } 
  }
  
  deg_f = ELM_PT(f, 1);
  if(deg_f==0) return NEW_EMPTY_PT();

  fp = NEW_PP(LEN_PP(f));
  rank_f = ELM_PT(f, 2);
  deg_p = DEG_PERM2(p);
  ptp = ADDR_PERM2(p);

  SET_ELM_PT(fp, 1, deg_f);
  SET_ELM_PT(fp, 2, rank_f);

  max_ran=0;
  min_ran=65535;

  for(i=1;i<=rank_f;i++)
  {
    j = ELM_PT(f, 6+deg_f+i);
    SET_ELM_PT(fp, 6+deg_f+i, j);           /* dom */
    k = IMAGE(ELM_PT(f, 6+deg_f+rank_f+i)-1, ptp, deg_p)+1;       
    SET_ELM_PT(fp, 6+deg_f+rank_f+i, k);    /* ran */
    SET_ELM_PT(fp, 6+j, k);                 /* dense img */ 
    if(k>max_ran) max_ran=k;
    if(k<min_ran) min_ran=k;
  }    
   
  SET_ELM_PT(fp, 3, min_ran); 
  SET_ELM_PT(fp, 4, max_ran);
  j=ELM_PT(fp, 7+deg_f);
  SET_ELM_PT(fp, 5, min_ran<j?min_ran:j);
  SET_ELM_PT(fp, 6, max_ran>deg_f?max_ran:deg_f);
  
  return fp;
}

/* product of perm and partial perm */
Obj FuncProdPermPP(Obj self, Obj p, Obj f)
{ pttype deg_f, rank, deg_p, deg, i, j, max_ran, min_ran, k, l;
  UInt2 * ptp;
  Obj pf, lmp;
  
  if(TNUM_OBJ(p)==T_PERM4)
  { lmp=FuncLARGEST_MOVED_POINT_PERM(self,p);
    if (INT_INTOBJ(lmp) <= 65535) 
    {
      FuncTRIM_PERM(self,p,lmp);
    }else{
      ErrorQuit( 
     "usage: can only multiply a partial perm and perm on at most 65535 pts", 
        0L, 0L );
      return 0L;
    } 
  }

  deg_f = ELM_PT(f, 1);
  if(deg_f==0) return NEW_EMPTY_PT();

  rank = ELM_PT(f, 2);
  deg_p = (pttype) DEG_PERM2(p);
  ptp = ADDR_PERM2(p);

  if(deg_p>=deg_f)
  {
    deg=0;

    /* find degree/max. dom */
    for(i=deg_p;1<=i;i--)
    {
      j = IMAGE(i-1, ptp, deg_p)+1; 
      if( j<=deg_f && ELM_PT(f,j+6)!=0)
      {
        deg=i;
        break;
      }
    }  
    if(deg==0) return NEW_EMPTY_PT();
  }else{
    deg=deg_f;
  }    
 
  pf=NEW_PP(deg+3*rank+6);
  
  SET_ELM_PT(pf, 1, deg);
  SET_ELM_PT(pf, 2, rank);

  max_ran=0; 
  min_ran=ELM_PT(f, 4);
  l=0;

  for(i=1;i<=deg;i++)
  {
    j = IMAGE(i-1, ptp, deg_p)+1;
    if(j<=deg_f)
    { 
      k=ELM_PT(f,j+6);
      if(k!=0)
      { 
        l++;
        SET_ELM_PT(pf,deg+l+6,i);       /* dom */
        SET_ELM_PT(pf,deg+rank+l+6,k);  /* ran */
        SET_ELM_PT(pf,i+6,k);           /* dense img */
        if(k>max_ran) max_ran=k;                
        if(k<min_ran) min_ran=k;
      }
    }
  }

  SET_ELM_PT(pf, 3, min_ran);
  SET_ELM_PT(pf, 4, max_ran);
  j=ELM_PT(pf, 7+deg);
  SET_ELM_PT(pf, 5, min_ran<j?min_ran:j);
  SET_ELM_PT(pf, 6, max_ran>deg?max_ran:deg);
  
  return pf;
}

/* i^f */ 
Obj FuncOnPointsPP(Obj self, Obj i, Obj f)
{   pttype j;
    j=INT_INTOBJ(i);
    if(j>ELM_PT(f, 1)) return Fail;
    j=ELM_PT(f, 6+j);
    if(j!=0) return INTOBJ_INT(j);
    return Fail;
}

/*******************************************************************************
**
** A transformation is of the form:
**
** [deg, rank, min ran, max ran, img list, canonical trans same kernel,
**  Set(ran)]
** 
** and hence has length
**
** 4+2*deg+rank
**
** An element of the internal rep of a partial trans must be at most 
** 65535 and be of pttype, but the length and indices can be larger than 65535
** and so these currently have type Int. 
**
*******************************************************************************/

/*******************************************************************************
** Macros for transformations specifically
*******************************************************************************/

/* length of trans internal rep */
static inline Int LEN_T(Obj f)
{
  return (ELM_PT(f,1)==0?5:4+2*ELM_PT(f,1)+ELM_PT(f,2));
}

/*******************************************************************************
** GAP functions for transformations
*******************************************************************************/

/* method for f[i] */
Obj FuncELM_LIST_T( Obj self, Obj f, Obj i)
{ 
  if(INT_INTOBJ(i)>LEN_T(f)) return Fail;
  return INTOBJ_INT(ELM_PT(f, INT_INTOBJ(i)));
}

/* method for f{list} */
Obj FuncELMS_LIST_T(Obj self, Obj f, Obj list)
{   Int len, i;
    Obj out;
    
    len = LEN_LIST(list);
    if(len>LEN_T(f)) len = LEN_T(f);
    out = NEW_PLIST(T_PLIST_CYC, len);
    SET_LEN_PLIST(out, len);
    
    for(i=1;i<=len;i++){
      SET_ELM_PLIST(out,i,  
        INTOBJ_INT(ELM_PT(f, INT_INTOBJ(ELM_LIST(list, i)))));
    }

    return out;
}

Obj FuncTransformationNC( Obj self, Obj img )
{ Int deg, max_ran, min_ran, rank, i, j;
  Obj f; 
  Int lookup[512];

  deg=LEN_LIST(img);
  
  if(deg==0){
    ErrorQuit("usage: cannot create an empty transformation,", 0L, 0L);
    return 0L;
  }
  
  TOO_MANY_PTS_ERROR(deg>65535); 
  
  f=NEW_T(4+3*deg);
  SET_ELM_PT(f, 1, (pttype) deg);

  max_ran=0;
  min_ran=65535;
  rank=0;

  /* init the kernel lookup */
  for(i=1;i<=deg;i++) lookup[i]=0;

  /* find dense img list, kernel, max_ran, min_ran, and rank? */
  for(i=1;i<=deg;i++){
    j=INT_INTOBJ(ELM_LIST(img, i)); /* img[i] */
    SET_ELM_PT(f, 4+i, (pttype) j);
    
    if(lookup[j]==0){ 
      rank++;
      lookup[j]=rank;
      SET_ELM_PT(f, 4+2*deg+rank, (pttype) j); /* range set */
    }
    
    SET_ELM_PT(f, 4+deg+i, (pttype) lookup[j]); /* kernel */

    if(j>max_ran) max_ran=j;
    if(j<min_ran) min_ran=j;
  }
 
  TOO_MANY_PTS_ERROR(max_ran>65535);

  SET_ELM_PT(f, 2, (pttype) rank);

  /* set min and max */
  SET_ELM_PT(f, 3, (pttype) min_ran);
  SET_ELM_PT(f, 4, (pttype) max_ran);

  /* sort the range set*/
  qsort((pttype *)(ADDR_OBJ(f)+1)+4+2*deg, rank, sizeof(pttype), cmp);

  ResizeBag(f, sizeof(pttype)*(4+2*deg+rank)+sizeof(UInt));
  return f; 
}

/* range of transformation */
Obj FuncRanT (Obj self, Obj f )
{ pttype deg, i;
  Obj out;
    
  deg=ELM_PT(f, 1);
  out=NEW_PLIST(T_PLIST_CYC,deg);
  SET_LEN_PLIST(out,(Int) deg);
  for(i=1;i<=deg;i++)
  { 
    SET_ELM_PLIST(out,i,INTOBJ_INT(ELM_PT(f,4+i)));
  }
  return out;
} 

/* kernel of transformation */
Obj FuncKerT (Obj self, Obj f )
{ pttype deg, i;
  Obj out;
    
  deg=ELM_PT(f, 1);
  out=NEW_PLIST(T_PLIST_CYC,deg);
  SET_LEN_PLIST(out,(Int) deg);
  for(i=1;i<=deg;i++)
  { 
    SET_ELM_PLIST(out,i,INTOBJ_INT(ELM_PT(f,4+deg+i)));
  }
  return out;
} 

/* range set of transformation */
Obj FuncRanSetT (Obj self, Obj f )
{ pttype deg, rank, i;
  Obj out;
    
  deg=ELM_PT(f, 1);
  rank=ELM_PT(f, 2);
  out=NEW_PLIST(T_PLIST_CYC,rank);
  SET_LEN_PLIST(out,(Int) rank);
  for(i=1;i<=rank;i++)
  { 
    SET_ELM_PLIST(out,i,INTOBJ_INT(ELM_PT(f,4+2*deg+i)));
  }
  return out;
} 

/* image set-kernel of transformation */
Obj FuncRanSetKerT(Obj self, Obj f)
{ pttype deg, rank, i;
  Obj out;
    
  deg=ELM_PT(f, 1); 
  rank=ELM_PT(f, 2);
  out=NEW_PLIST(T_PLIST_CYC, deg+rank);
  SET_LEN_PLIST(out,(Int) deg+rank);

  for(i=1;i<=rank;i++) SET_ELM_PLIST(out,i,INTOBJ_INT(ELM_PT(f,4+2*deg+i)));
  for(i=1;i<=deg;i++){
    SET_ELM_PLIST(out,i+rank,INTOBJ_INT(ELM_PT(f,4+deg+i)));
  }
  return out;
} 

/* product of transformations */
Obj FuncProdTT(Obj self, Obj f, Obj g)
{ pttype deg, min_ran, max_ran, j, i, rank;
  pttype lookup[ELM_PT(f, 1)];
  Obj fg;

  deg=ELM_PT(f, 1);
  if(deg!=ELM_PT(g, 1)){
    ErrorQuit("usage: transformations have different degrees", 0L, 0L);
    return 0L;
  }
  
  fg=NEW_T(4+3*deg);
  SET_ELM_PT(fg, 1, deg);

  min_ran=ELM_PT(g, 4);
  max_ran=ELM_PT(g, 3);
  rank=0;

  /* init the kernel lookup */
  for(i=1;i<=deg;i++) lookup[i]=0;

  for(i=1;i<=deg; i++){
    j=ELM_PT(g, 4+ELM_PT(f, 4+i)); /* g(f(i)) */
    SET_ELM_PT(fg, 4+i, j);
    
    if(lookup[j]==0){
      rank++;
      lookup[j]=rank;
      SET_ELM_PT(fg, 4+2*deg+rank, j); /* range set */
    }

    SET_ELM_PT(fg, 4+deg+i, lookup[j]); /* kernel */
    
    if(j>max_ran) max_ran=j;
    if(j<min_ran) min_ran=j;
  }

  SET_ELM_PT(fg, 2, rank);
  SET_ELM_PT(fg, 3, min_ran);
  SET_ELM_PT(fg, 4, max_ran);

  qsort((pttype *)(ADDR_OBJ(fg)+1)+4+2*deg, rank, sizeof(pttype), cmp);

  ResizeBag(fg, sizeof(pttype)*(4+2*deg+rank)+sizeof(UInt)); 
  return fg;
}

/* product of transformation and permutation */
Obj FuncProdTPerm(Obj self, Obj f, Obj p)
{ pttype deg, rank, deg_p, max_ran, min_ran, i, j;
  UInt2 * ptp;
  Obj fp, lmp;

  deg=ELM_PT(f, 1);
  lmp=FuncLARGEST_MOVED_POINT_PERM(self, p);
  
  if(INT_INTOBJ(lmp)>deg){
    ErrorQuit("usage: cannot multiply a transformation and a perm acting on different sets", 0L, 0L);
    return 0L;
  }

  if(TNUM_OBJ(p)==T_PERM4){ 
    FuncTRIM_PERM(self,p,lmp);
  }

  rank = ELM_PT(f, 2);
  deg_p  = DEG_PERM2(p);
  ptp = ADDR_PERM2(p);

  fp = NEW_T(LEN_T(f));
  SET_ELM_PT(fp, 1, deg);
  SET_ELM_PT(fp, 2, rank);  
  
  max_ran=1;
  min_ran=deg;

  for(i=1;i<=deg; i++){
    j = IMAGE(ELM_PT(f, 4+i)-1, ptp, deg_p)+1; /* p(f(i)) */
    SET_ELM_PT(fp, 4+i, j);

    /* ker(fp)=ker(f) */
    SET_ELM_PT(fp, 4+deg+i, ELM_PT(f, 4+deg+i));

    if(j>max_ran) max_ran=j;
    if(j<min_ran) min_ran=j;
  }

  SET_ELM_PT(f, 3, min_ran);
  SET_ELM_PT(f, 4, max_ran);

  /* ran set */
  for(i=1;i<=rank; i++){
    j=IMAGE(ELM_PT(f, 4+2*deg+i)-1, ptp, deg_p)+1;
    SET_ELM_PT(fp, 4+2*deg+i, j);
  }
  qsort((pttype *)(ADDR_OBJ(fp)+1)+4+2*deg, rank, sizeof(pttype), cmp);
  
  return fp;
}

/* product of permutation and transformation */
Obj FuncProdPermT(Obj self, Obj p, Obj f)
{ pttype deg, rank, deg_p, i, j;
  UInt2 * ptp;
  Obj pf, lmp;
  pttype lookup[ELM_PT(f, 1)];

  deg=ELM_PT(f, 1);
  lmp=FuncLARGEST_MOVED_POINT_PERM(self, p);
  
  if(INT_INTOBJ(lmp)>deg){
    ErrorQuit("usage: cannot multiply transformations and perms acting on different sets", 0L, 0L);
    return 0L;
  }

  if(TNUM_OBJ(p)==T_PERM4){ 
    FuncTRIM_PERM(self,p,lmp);
  }

  deg_p  = DEG_PERM2(p);
  ptp = ADDR_PERM2(p);

  pf = NEW_T(LEN_T(f));
  SET_ELM_PT(pf, 1, deg);
  SET_ELM_PT(pf, 2, ELM_PT(f, 2));  
  SET_ELM_PT(pf, 3, ELM_PT(f, 3));
  SET_ELM_PT(pf, 4, ELM_PT(f, 4));

  rank=0;
  
  for(i=1;i<=deg;i++) lookup[i]=0;
  
  for(i=1;i<=deg;i++){
    j = ELM_PT(f, IMAGE(i-1, ptp, deg_p)+5); /* f(p(i)) */
    SET_ELM_PT(pf, 4+i, j);
    
    if(lookup[j]==0){
      rank++;
      lookup[j]=rank;
      SET_ELM_PT(pf, 4+2*deg+rank, ELM_PT(f, 4+2*deg+rank)); /* ran set */
    }
    /* kernel */
    SET_ELM_PT(pf, 4+deg+i, lookup[j]);
  }
  
  return pf;
}

/* on sets for a transformation */ 
Obj FuncOnIntegerSetsWithT (Obj self, Obj set, Obj f)
{ pttype deg, k;
  Int n, i, j, m;
  Obj out;
  Int seen[ELM_PT(f,1)];

  deg=ELM_PT(f,1);
  n=LEN_LIST(set);
  if(n==0) return NEW_EMPTY_PLIST();

  out = NEW_PLIST(T_PLIST_CYC, n);
  m = 0;
  
  for(i=1;i<=deg;i++) seen[i]=0;

  for(i=1;i<=n;i++)
  {
    j=INT_INTOBJ(ELM_LIST(set, i));
    if(j<=deg){
      k=ELM_PT(f, j+4);
      if(seen[k]==0){
        seen[k]=1; 
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

Obj FuncPermList(Obj self, Obj list);

/* on tuples for a transformation */ 
Obj FuncOnIntegerTuplesWithT (Obj self, Obj set, Obj f)
{ pttype deg, k;
  Int n, i, j, m;
  Obj out;

  deg=ELM_PT(f,1);
  n=LEN_LIST(set);
  if(n==0) return NEW_EMPTY_PLIST();

  out = NEW_PLIST(T_PLIST_CYC, n);
  m = 0;

  for(i=1;i<=n;i++)
  {
    j=INT_INTOBJ(ELM_LIST(set, i));
    if(j<=deg)
    {
      k=ELM_PT(f, j+4);
      m++;
      SET_ELM_PLIST(out, m, INTOBJ_INT(k));
    }
  }
  SET_LEN_PLIST(out, m);
  SHRINK_PLIST(out, m);
  return out;
}

Obj FuncPermLeftQuoTransformationNC(Obj self, Obj f, Obj g)
{ pttype deg, i, x;
  Obj pl;

    deg = ELM_PT(f, 1);
    pl = NEW_PLIST(T_PLIST_CYC,deg);
    SET_LEN_PLIST(pl, (Int) deg);
    /* From now on no more garbage collections! */
    for (i = 1;i <= deg;i++) {
        x = ELM_PT(f,i+4);
        if (ELM_PLIST(pl,x) == NULL) {
            SET_ELM_PLIST(pl,x,INTOBJ_INT(ELM_PT(g,i+4)));
        }
    }
    for (i = 1;i <= deg;i++) {
        if (ELM_PLIST(pl,i) == NULL) {
            SET_ELM_PLIST(pl,i,INTOBJ_INT(i));
        }
    }
    return FuncPermList(self,pl);
}

/* less than or equal for transformations */
Obj FuncLeqT(Obj self, Obj f, Obj g)
{ pttype i, j, k, deg;

  deg=ELM_PT(f,1);
  if(deg!=ELM_PT(g,1)){
    ErrorQuit("usage: transformations should have equal degree,", 0L, 0L);
    return 0L;
  }

  for(i=1;i<=deg;i++){
    j=ELM_PT(f,4+i);
    k=ELM_PT(g,4+i);
    if(j<k) return True;
    if(j>k) return False;
  }

  return False;
}

/* less than or equal for transformations */
Obj FuncEqT(Obj self, Obj f, Obj g)
{ pttype i, deg;

  deg=ELM_PT(f,1);
  if(deg!=ELM_PT(g,1)) return False;

  for(i=1;i<=deg;i++){
    if(ELM_PT(f,4+i)!=ELM_PT(g,4+i)) return False;
  }
  return True;
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

  { "FullPartialPermNC", 1, "rep",
    FuncFullPartialPermNC,
    "pkg/citrus/src/citrus.c:FuncFullPartialPermNC" },

  { "SparsePartialPermNC", 2, "dom,ran",
    FuncSparsePartialPermNC,
    "pkg/citrus/src/citrus.c:FuncSparsePartialPermNC" },

  { "DensePartialPermNC", 1, "img",
    FuncDensePartialPermNC,
    "pkg/citrus/src/citrus.c:FuncDensePartialPermNC" },
 
  { "ProdPP", 2, "f,g",
    FuncProdPP,
    "pkg/citrus/src/citrus.c:FuncProdPP" },

  { "DomPP", 1, "f",
    FuncDomPP,
    "pkg/citrus/src/citrus.c:FuncDomPP" },

  { "RanPP", 1, "f",
    FuncRanPP,
    "pkg/citrus/src/citrus.c:FuncRanPP" },

  { "RanSetPP", 1, "f",
    FuncRanSetPP,
    "pkg/citrus/src/citrus.c:FuncRanSetPP" },

  { "InvPP", 1, "f",
    FuncInvPP,
    "pkg/citrus/src/citrus.c:FuncInvPP" },

  { "OnIntegerTuplesWithPP", 2, "tup,f",
    FuncOnIntegerTuplesWithPP,
    "pkg/citrus/src/citrus.c:FuncOnIntegerTuplesWithPP" },
  
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

  { "MovedPointsPP", 1, "f",
    FuncMovedPointsPP,
    "pkg/citrus/src/citrus.c:FuncMovedPointsPP" },

  { "NrMovedPointsPP", 1, "f",
    FuncNrMovedPointsPP,
    "pkg/citrus/src/citrus.c:FuncNrMovedPointsPP" },

  { "LargestMovedPointPP", 1, "f",
    FuncLargestMovedPointPP,
    "pkg/citrus/src/citrus.c:FuncLargestMovedPointPP" },

  { "SmallestMovedPointPP", 1, "f",
    FuncSmallestMovedPointPP,
    "pkg/citrus/src/citrus.c:FuncSmallestMovedPointPP" },

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

  { "OnPointsPP", 2, "i, f",
    FuncOnPointsPP, 
    "pkg/citrus/src/citrus.c:FuncOnPointsPP" },

  /* transformations start here */

  { "ELM_LIST_T", 2, "f,i",
    FuncELM_LIST_T,
    "pkg/citrus/src/citrus.c:ELM_LIST_T" },
  
  { "ELMS_LIST_T", 2, "f,list",
    FuncELMS_LIST_T,
    "pkg/citrus/src/citrus.c:ELMS_LIST_T" },

  { "TransformationNC", 1, "img",
     FuncTransformationNC,
    "pkg/citrus/src/citrus.c:FuncTransformationNC" },

  { "RanT", 1, "f",
     FuncRanT,
    "pkg/citrus/src/citrus.c:FuncRanT" },

  { "KerT", 1, "f",
     FuncKerT,
    "pkg/citrus/src/citrus.c:FuncKerT" },

  { "RanSetT", 1, "f",
     FuncRanSetT,
    "pkg/citrus/src/citrus.c:FuncRanSetT" },

  { "RanSetKerT", 1, "f",
     FuncRanSetKerT,
    "pkg/citrus/src/citrus.c:FuncRanSetKerT" },

  { "ProdTT", 2, "f, g",
     FuncProdTT,
    "pkg/citrus/src/citrus.c:FuncProdTT" },

  { "ProdTPerm", 2, "f, p",
     FuncProdTPerm,
    "pkg/citrus/src/citrus.c:FuncProdTPerm" },

  { "ProdPermT", 2, "p, f",
     FuncProdPermT,
    "pkg/citrus/src/citrus.c:FuncProdPermT" },

  { "OnIntegerSetsWithT", 2, "set, f",
     FuncOnIntegerSetsWithT,
    "pkg/citrus/src/citrus.c:FuncOnIntegerSetsWithT" },

  { "OnIntegerTuplesWithT", 2, "tup, f",
     FuncOnIntegerTuplesWithT,
    "pkg/citrus/src/citrus.c:FuncOnIntegerTuplesWithT" },

  { "PermLeftQuoTransformationNC", 2, "f, g",
     FuncPermLeftQuoTransformationNC,
    "pkg/citrus/src/citrus.c:FuncPermLeftQuoTransformationNC" },

  { "LeqT", 2, "f, g",
     FuncLeqT,
    "pkg/citrus/src/citrus.c:FuncLeqT" },

  { "EqT", 2, "f, g",
     FuncEqT,
    "pkg/citrus/src/citrus.c:FuncEqT" },

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
    ImportGVarFromLibrary( "TransformationType", &TransformationType );
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

