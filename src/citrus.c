

const char * Revision_citrus_c =
   "$Id: citrus.c,v$";

#include <stdlib.h>

#include "src/compiled.h" 

/*******************************************************************************
**
** A partial permutation is of the form:
**
** [degree, rank, min ran, max ran, min, max, img list, dom, ran, Set(ran)] 
**
** where <degree> is the length of <img list>, <rank> is the number of none zero
** entries in the <img list>,  <min ran>, <max ran> are
** self explanatory,  <min> is min(<min dom>, <min ran>), <max> is max(<max 
** dom>, <max ran>), <dom> is the domain, <ran> is the range, <Set(ran)> is the 
** range as a set (not calculated until needed), and <img list> is the list of 
** images with 0 for undefined.
*/

Obj FuncDenseCreatePartPerm_C( Obj self, Obj img )
{ 
    Obj f, j;
    Int deg, max_ran, min_ran, rank, i, jj;
    Int ran[513];

    if(LEN_PLIST(img)==0){
      f = NEW_PLIST(T_PLIST_CYC,1);
      SET_LEN_PLIST(f, 1);
      SET_ELM_PLIST(f, 1, INTOBJ_INT(0));
      return f;
    }
    
    for(i=LEN_PLIST(img);1<=i;i--){
      if(INT_INTOBJ(ELM_PLIST(img, i))!=0) {
        deg = i;
        break;
        }
      }

    if(deg==0){
      f = NEW_PLIST(T_PLIST_CYC,1);
      SET_LEN_PLIST(f, 1);
      SET_ELM_PLIST(f, 1, INTOBJ_INT(0));
      return f;
    }
    
    f = NEW_PLIST(T_PLIST_CYC,3*deg+6); /* the output*/
    SET_ELM_PLIST(f, 1, INTOBJ_INT(deg));
    
    max_ran=0; 
    min_ran=deg; 
    rank=0;

    /* find dom, rank, max_ran, min_ran */
    for(i=1;i<=deg;i++){
      j = ELM_PLIST(img, i);
      SET_ELM_PLIST(f, i+6, j);
      jj = INT_INTOBJ(j);
      if(jj!=0){
        rank++;
        SET_ELM_PLIST(f,deg+rank+6,INTOBJ_INT(i)); /* dom*/
        ran[rank]=jj;
        if(jj>max_ran){
          max_ran=jj;
          }
        if(jj<min_ran){
          min_ran=jj;
          }
        }
      }

    SET_ELM_PLIST(f,2,INTOBJ_INT(rank));
    SET_ELM_PLIST(f,3,INTOBJ_INT(min_ran));
    SET_ELM_PLIST(f,4,INTOBJ_INT(max_ran));

    /* set range */
    for(i=1;i<=rank;i++){
      SET_ELM_PLIST(f,deg+rank+6+i,INTOBJ_INT(ran[i]));
    } 

    /* set min */
    j=ELM_PLIST(f,deg+7); /* min. dom. */
    if(min_ran<INT_INTOBJ(j)){
      SET_ELM_PLIST(f,5,INTOBJ_INT(min_ran));
    }else{
      SET_ELM_PLIST(f,5,j);
    }
    
    /* set max */
    j=ELM_PLIST(f,deg+rank+6); /* max. dom. */
    if(max_ran>INT_INTOBJ(j)){
      SET_ELM_PLIST(f,6,INTOBJ_INT(max_ran));
    }else{
      SET_ELM_PLIST(f,6,j);
    }

    SET_LEN_PLIST(f,deg+2*rank+6);
    SHRINK_PLIST(f,deg+3*rank+6);
    return f;
}

/* read off partial permutation */

Obj FuncReadOffPartPerm_C( Obj self, Obj f, Obj i, Obj j)
{   
    Int len, k;
    Obj out;

    len = INT_INTOBJ(j)-INT_INTOBJ(i)+1;
    out = NEW_PLIST(T_PLIST_CYC, len);
    SET_LEN_PLIST(out, len);
    
    for(k=0;k<=len-1;k++){
      SET_ELM_PLIST(out,k+1, ELM_PLIST(f, INT_INTOBJ(i)+k));
    }

    return out;
}

/* product of partial permutations */

Obj FuncProdPartPerm_C( Obj self, Obj f, Obj g )
{
    Int n,m,i,j,deg,max_ran,min_ran,rank,kk;
    Obj fg,k;
    Int ran[513];

    n = INT_INTOBJ(ELM_PLIST(f,1));
    m = INT_INTOBJ(ELM_PLIST(g,1));
    
    if(n==0||m==0){
      fg = NEW_PLIST(T_PLIST_CYC,1);
      SET_LEN_PLIST(fg, 1);
      SET_ELM_PLIST(fg, 1, INTOBJ_INT(0));
      return fg;
    }
   
    deg = 0;
    for(i=n;1<=i;i--){
      j = INT_INTOBJ(ELM_PLIST(f,i+6));
      if(j!=0 && j<=m && INT_INTOBJ(ELM_PLIST(g,j+6))!=0){
        deg = i;
        break;
        }
    } 

   if(deg==0){
      fg = NEW_PLIST(T_PLIST_CYC,1);
      SET_LEN_PLIST(fg, 1);
      SET_ELM_PLIST(fg, 1, INTOBJ_INT(0));
      return fg;
    }

    fg = NEW_PLIST(T_PLIST_CYC,3*deg+6);
    SET_ELM_PLIST(fg, 1, INTOBJ_INT(deg));
    
    max_ran=0;
    min_ran=0;
    rank=0;

    for (i=1;i<=deg;i++) {
        j = INT_INTOBJ(ELM_PLIST(f,i+6));
        if(j > m || j == 0){
          SET_ELM_PLIST(fg,i+6,INTOBJ_INT(0));
        } else {
          k = ELM_PLIST(g,j+6);
          kk = INT_INTOBJ(k);
          SET_ELM_PLIST(fg,i+6,k);
          if(kk!=0){ 
            rank++;
            SET_ELM_PLIST(fg,deg+rank+6,INTOBJ_INT(i));
            ran[rank]=kk;
            if(kk>max_ran){
              max_ran=kk;
              }
            if((min_ran==0)||(kk<min_ran)){
              min_ran=kk;
              }
            }
        }
      }

    SET_ELM_PLIST(fg,2,INTOBJ_INT(rank));
    SET_ELM_PLIST(fg,3,INTOBJ_INT(min_ran));
    SET_ELM_PLIST(fg,4,INTOBJ_INT(max_ran));
    
    for(i=1;i<=rank;i++){
      SET_ELM_PLIST(fg,deg+rank+6+i,INTOBJ_INT(ran[i]));
    }
 
    k=ELM_PLIST(fg,deg+7);
    if(min_ran<INT_INTOBJ(k)){
      SET_ELM_PLIST(fg,5,INTOBJ_INT(min_ran));
    }else{
      SET_ELM_PLIST(fg,5,k);
    }

    k=ELM_PLIST(fg,deg+rank+6);
    if(max_ran>INT_INTOBJ(k)){
      SET_ELM_PLIST(fg,6,INTOBJ_INT(max_ran));
    }else{
      SET_ELM_PLIST(fg,6,k);
    }

    SET_LEN_PLIST(fg,deg+2*rank+6);
    SHRINK_PLIST(fg,deg+3*rank+6);
    return fg;
}

/* inverse of a partial permutation */
/* use SortDensePlist here for domain and range */

Obj FuncInvPartPerm_C ( Obj self, Obj f )
{
    Obj deg,rank,max_ran,f_inv;
    Int n,i,j;

    if(INT_INTOBJ(ELM_PLIST(f, 1)) == 0) return f;

    deg = ELM_PLIST(f, 1);
    rank =  ELM_PLIST(f, 2);
    max_ran = ELM_PLIST(f, 4);
    n = 2*INT_INTOBJ(rank) + INT_INTOBJ(max_ran) + 6;
    f_inv = NEW_PLIST(T_PLIST_CYC, n);
    SET_LEN_PLIST(f_inv, n); 
 
    SET_ELM_PLIST(f_inv, 1, max_ran);
    SET_ELM_PLIST(f_inv, 2, rank);
    SET_ELM_PLIST(f_inv, 3, ELM_PLIST(f, 7+INT_INTOBJ(deg)));
    SET_ELM_PLIST(f_inv, 4, ELM_PLIST(f, 6+INT_INTOBJ(deg)+INT_INTOBJ(rank)));
    SET_ELM_PLIST(f_inv, 5, ELM_PLIST(f, 5));
    SET_ELM_PLIST(f_inv, 6, ELM_PLIST(f, 6));

    for(i=7;i<=INT_INTOBJ(max_ran)+6;i++){
      SET_ELM_PLIST(f_inv, i, INTOBJ_INT(0));
    }

    for(i=1;i<=INT_INTOBJ(deg);i++){/*go from min dom to max dom here instead*/
      j = INT_INTOBJ(ELM_PLIST(f,i+6));
      if(j!=0){
        SET_ELM_PLIST(f_inv, j+6, INTOBJ_INT(i));
      }
    }
    
    /* sort range of f to get dom f_inv */
    /* read off f_inv[j+6] for each j in dom f_inv to get ran f_inv */

    return f_inv;
}

/* on sets for a partial permutation 

Obj FuncOnIntegerSetsWithPartPerm_C (Obj self, Obj f, Obj set)
and carrot! ^
{

  if (IS_POSOBJ(f)){
    ff = ELM_PLIST(f,1);
  }
  else{
    ff = f;
  }
  
  n = LEN_LIST(ff);
  

} */
    

/*F * * * * * * * * * * * * * initialize package * * * * * * * * * * * * * * */

/******************************************************************************
*V  GVarFuncs . . . . . . . . . . . . . . . . . . list of functions to export
*/
static StructGVarFunc GVarFuncs [] = {

  { "DenseCreatePartPerm_C", 1, "img",
    FuncDenseCreatePartPerm_C,
    "pkg/citrus/src/citrus.c:FuncDenseCreatePartPerm_C" },
  
  { "ReadOffPartPerm_C", 3, "f,i,j",
    FuncReadOffPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncReadOffPartPerm_C" },
  
  { "ProdPartPerm_C", 2, "f,g",
    FuncProdPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncProdPartPerm_C" },

  { "InvPartPerm_C", 1, "f",
    FuncInvPartPerm_C,
    "pkg/citrus/src/citrus.c:FuncInvPartPerm_C" },

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




