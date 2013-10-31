#############################################################################
##
#W  attributes.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# Note about the difference between One and MultiplicativeNeutralElement 
# (the same goes for Zero and MultplicativeZero):
#
# One(s) returns One(Representative(s)) if it belongs to s, so that 
# One(s)=Transformation([1..DegreeOfTransformationSemigroup(s)]) if s is a
# transformation semigroup and it returns fail otherwise, or it returns 
# PartialPerm([1..DegreeOfPartialPermSemigroup]) if this belongs to s. 
#
# MultiplicativeNeutralElement on the other hand returns the element of s that
# acts as the identity, note that this can be equal to One(s) but it can also
# not be equal t One(s). 
#
# A semigroup satisfies IsMonoidAsSemigroup(s) if
# MultiplicativeNeutralElement(x)<>fail, so it could be that One(s) returns
# fail but IsMonoidAsSemigroup is still true. 

InstallMethod(IsGreensDLeq, "for an acting semigroup",
[IsActingSemigroup],
function(S)
  local partial, data, comp_index;

  partial:=PartialOrderOfDClasses(S);
  data:=SemigroupData(S);

  comp_index:=function(x, y)
    if y in partial[x] then
      return true;
    elif Length(partial[x])=1 and partial[partial[x][1]]=partial[x] then
      return false;
    fi;
    return ForAny(partial[x], z-> z<>x and comp_index(z,y));
  end;

  return function(x,y)
    return comp_index(OrbSCCLookup(data)[Position(data, x)]-1,
      OrbSCCLookup(data)[Position(data, y)]-1);
  end;
end);

#

InstallMethod(IsMaximalSubsemigroup, "for a semigroup and semigroup", 
[IsSemigroup, IsSemigroup],
function(S, T)
  if IsSubsemigroup(S, T) and S<>T then 
    return ForAll(S, x-> x in T or Semigroup(GeneratorsOfSemigroup(T), x)=S);
  else
    return false;
  fi;
end); 

# the following method comes from Remark 1 in Graham, Graham, and Rhodes.
# and only works for Rees matrix semigroup over groups

InstallMethod(MaximalSubsemigroups, "for a Rees matrix subsemigroup",
[IsReesMatrixSubsemigroup], 
function(R)
  local G, out, mat, I, J, P, U, H, j, i;
  
  if not IsReesMatrixSemigroup(R) then 
    TryNextMethod(); 
    return;
  fi;
 
  G:=UnderlyingSemigroup(R);    
  
  if not IsGroup(G) then 
    if IsSimpleSemigroup(R) then 
      #take an isomorphism to a Rees 0-matrix semigroup, then find its maximal
      #subsemigroups, then pull those back, (should specify some methods for the
      #pulling back part)
      Error("not yet implemented,");
      return;
    else
      TryNextMethod();
      return;
    fi;
  fi;

  out:=[];
  mat:=Matrix(R);     I:=Rows(R);         J:=Columns(R);
 
  # Case 1: maximal subsemigroups of the form (IxHxJ) where H is a
  # maximal subgroup of G
  Info(InfoSemigroups, 3, 
   "Case 1: maximal subsemigroups arising from maximal subgroups...");
  for H in MaximalSubgroups(G) do
    U:=Semigroup(GeneratorsOfReesMatrixSemigroupNC(R, I, H, J));
    if Size(U)<Size(R) then 
      Add(out, U);
    fi;
  od;
  Info(InfoSemigroups, 3, "...found ", Length(out));

  # Case 2: maximal subsemigroup of the form (IxGxJ') where J'=J\{j} for some j
  # in J.
  Info(InfoSemigroups, 3, 
   "Case 2: maximal subsemigroups obtained by removing a column...");
  if Length(J)>1 then 
    for j in J do 
      Add(out, ReesMatrixSubsemigroupNC(R, I, G, Difference(J, [j])));
    od;
  fi;
  
  # Case 3: the dual of case 2.
  Info(InfoSemigroups, 3, 
   "Case 3: maximal subsemigroups obtained by removing a row...");
  if Length(I)>1 then 
    for i in I do 
      Add(out, ReesMatrixSubsemigroupNC(R, Difference(I, [i]), G, J));
    od;
  fi;

  return out;
end);

# the following method comes from Remark 1 in Graham, Graham, and Rhodes.
# and only works for Rees 0-matrix semigroup over groups

if not IsBound(GAPInfo.PackagesLoaded.grape) then 
  InstallMethod(MaximalSubsemigroups, "for a Rees 0-matrix subsemigroup",
  [IsReesZeroMatrixSubsemigroup], 
  function(R)
    Info(InfoWarning, 1, "the GRAPE is not loaded, and so this function does",
         " not work");
    return fail;
  end); 
else
  InstallMethod(MaximalSubsemigroups, "for a Rees 0-matrix subsemigroup",
  [IsReesZeroMatrixSubsemigroup], 
  function(R)
    local G, out, I, J, mat, P, new, pos, U, JJ, solo, II, len, graph, names, rectangles, gens, i, j, H, r, k;
    
    if not IsReesZeroMatrixSemigroup(R) then 
      TryNextMethod(); 
      return;
    fi;
   
    G:=UnderlyingSemigroup(R);    
    
    if not IsGroup(G) then 
      if IsZeroSimpleSemigroup(R) then 
        #take an isomorphism to a Rees 0-matrix semigroup, then find its maximal
        #subsemigroups, then pull those back, (should specify some methods for
        #the pulling back part)
        Error("not yet implemented,");
        return;
      else
        TryNextMethod();
        return;
      fi;
    fi;

    out:=[]; I:=Rows(R); J:=Columns(R); mat:=Matrix(R);     
   
    # find the set of group elements in the matrix
    P:=Union(mat{J}{I});
    if P[1]=0 then 
      Remove(P, 1);
    else  # S\{0} is a maximal subsemigroup 
          # the unique case when the missing D-class is {0}
      new:=ShallowCopy(GeneratorsOfSemigroup(R));
      if Size(new)>1 then  
        pos:=Position(new, MultiplicativeZero(R)); 
        Remove(new, pos); #remove the zero, which has to be present
      fi;
      Add(out, Semigroup(new));
    fi;
    
    if Length(I)=1 and Length(J)=1 and IsTrivial(G) then 
      # the unique case when {0} is a maximal subsemigroup, if <G> is
      # non-trivial then {0, 1_G} is a subsemigroup containing <0> and not
      # equal to <R>. 
      Add(out, Semigroup(MultiplicativeZero(R)));
    fi;
    
    # Case 1: maximal subsemigroups of the form (IxHxJ)\cup\{0\} where H is a
    # maximal subgroup of G
    
    Info(InfoSemigroups, 3, 
     "Case 1: maximal subsemigroups arising from maximal subgroups...");
    
    for H in MaximalSubgroups(G) do
      U:=Semigroup(GeneratorsOfReesZeroMatrixSemigroupNC(R, I, H, J));
      if Size(U)<Size(R) then 
        Add(out, U);
      fi;
    od;

    Info(InfoSemigroups, 3, "...found ", Length(out));

    # Case 2: maximal subsemigroup of the form (IxGxJ')\cup\{0\} where J'=J\{j}
    # for some j in J, and where the resultant matrix has no zero columns or
    # rows.

    # in the Graham-Houghton IxJ bipartite graph, we can remove any vertex <j>
    # in <J> which is not adjacent to a vertex <i> which is only adjacent to
    # <j>.  So, we run through the vertices <i> of <I> and find the ones of
    # degree 1, and we discard the vertices <j> adjacent to such <i>. 
    
    Info(InfoSemigroups, 3, 
     "Case 2: maximal subsemigroups obtained by removing a column...");
    
    JJ:=ShallowCopy(J);

    for i in I do  
      solo:=false; # keep track of whether <i> has degree 1
      for j in J do
        if mat[j][i]<>0 then
          if solo<>false then    # <i> has degree greater than 1
            solo:=false;         
            break;               # so skip it
          else
            solo:=j;             # <i> is adjacent to <j> and we don't know if
                                 # it is adjacent to any other vertex
          fi;
        fi;
      od;
      if solo<>false then        # <i> is adjacent to <solo> and nothing else
        RemoveSet(JJ, solo);     # so remove it. 
      fi;
    od;
    
    for j in JJ do 
      U:=ReesZeroMatrixSubsemigroupNC(R, I, G, Difference(J, [j]));
      if not MultiplicativeZero(R) in U then 
        U:=Semigroup(U, MultiplicativeZero(R));
        SetIsReesZeroMatrixSemigroup(U, true);
      fi;
      Add(out, U);
    od;
    Info(InfoSemigroups, 3, "...found ", Length(JJ));

    # Case 3: the dual of case 2.
    
    Info(InfoSemigroups, 3, 
     "Case 3: maximal subsemigroups obtained by removing a row...");
    
    II:=ShallowCopy(I);

    for j in J do  
      solo:=false; # keep track of whether <i> has degree 1
      for i in I do
        if mat[j][i]<>0 then
          if solo<>false then    # <i> has degree greater than 1
            solo:=false;         
            break;               # so skip it
          else
            solo:=i;             # <i> is adjacent to <j> and we don't know if
                                 # it is adjacent to any other vertex
          fi;
        fi;
      od;
      if solo<>false then        # <i> is adjacent to <solo> and nothing else
        RemoveSet(II, solo);     # so remove it. 
      fi;
    od;
    
    for i in II do 
      U:=ReesZeroMatrixSubsemigroupNC(R, Difference(I, [i]), G, J);
      if not MultiplicativeZero(R) in U then 
        U:=Semigroup(U, MultiplicativeZero(R));
        SetIsReesZeroMatrixSemigroup(U, true);
      fi;
      Add(out, U);
    od;

    Info(InfoSemigroups, 3, "...found ", Length(II));

    # Case 4: maximal rectangle of zeros in the matrix
    Info(InfoSemigroups, 3, 
     "Case 4: maximal subsemigroups obtained by removing a rectangle...");
    Info(InfoSemigroups, 3, "finding rectangles...");

    len:=Length(mat[1]); # use <mat> to keep the indices correct

    graph:=Graph(Group(()), Union(I, J+len), OnPoints,
     function(i,j)
       if i<=len and j>len then
         return mat[j-len][i]=0;
       elif j<=len and i>len then
         return mat[i-len][j]=0;
       else
         return i<>j;
       fi;
     end, true);

    names:=x-> graph.names[x];

    rectangles:=CompleteSubgraphs(graph);

    Info(InfoSemigroups, 3, "...found ", Length(rectangles));

    gens:=GeneratorsOfGroup(G);
    
    Info(InfoSemigroups, 3, 
     "finding subsemigroups arising from rectangles...");
    for r in [2..Length(rectangles)-1] do 
    #the first and last entries correspond to removing all the rows or columns
      Apply(rectangles[r], names);
       
      # add group generators
      i:=rectangles[r][1];  
      j:=First(Columns(R), j-> mat[j][i]<>0 and not j+len in rectangles[r]); 
      if IsTrivial(G) then 
        new:=[Objectify(TypeReesMatrixSemigroupElements(R),
         [i, mat[j][i]^-1, j, mat])];
      else
        new:=List(gens, x-> Objectify(TypeReesMatrixSemigroupElements(R), 
          [i, x*mat[j][i]^-1, j, mat]));
      fi;
      
      j:=First(rectangles[r], j-> j>len)-len;
      i:=First(Rows(R), i-> mat[j][i]<>0 and not i in rectangles[r]);
      if IsTrivial(G) then 
        Add(new, Objectify(TypeReesMatrixSemigroupElements(R),
         [i, mat[j][i]^-1, j, mat]));
      else
        Append(new, List(gens, x-> 
         Objectify(TypeReesMatrixSemigroupElements(R), 
          [i, x*mat[j][i]^-1, j, mat])));
      fi;

      for k in rectangles[r] do  
        if k<=len then # k in I
          for j in J do 
            Add(new, RMSElement(R, k, One(G), j));
          od;
        else # k-len in J
          for i in I do 
            Add(new, RMSElement(R, i, One(G), k-len));
          od;
        fi;
      od;
      Add(out, Semigroup(new));
    od;
    return out;
  end);
fi;

#

InstallMethod(MaximalDClasses, "for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, partial, data, pos, i, out, classes, x;

  gens:=GeneratorsOfSemigroup(s);

  partial:=PartialOrderOfDClasses(s);
  data:=SemigroupData(s);
  pos:=[];
  for x in gens do 
    i:=OrbSCCLookup(data)[Position(data, x)]-1; 
    #index of the D-class containing x 
    AddSet(pos, i);
  od;

  out:=[];
  classes:=GreensDClasses(s);
  for i in pos do 
    if not ForAny([1..Length(partial)], j-> j<>i and i in partial[j]) then 
      Add(out, classes[i]);
    fi;
  od;

  return out;
end);

#

InstallMethod(StructureDescriptionSchutzenbergerGroups, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local o, scc, out, m;

  o:=LambdaOrb(s);
  scc:=OrbSCC(o);
  out:=[];

  for m in [2..Length(scc)] do 
    AddSet(out, StructureDescription(LambdaOrbSchutzGp(o, m)));
  od;

  return out;
end);

#

InstallMethod(StructureDescriptionMaximalSubgroups, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local out, d;

  out:=[];
  for d in DClasses(s) do 
    if IsRegularClass(d) then 
      AddSet(out, StructureDescription(GroupHClass(d)));
    fi;
  od;

  return out;
end);

#

InstallMethod(GroupOfUnits, "for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, g, deg, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  deg:=DegreeOfTransformationSemigroup(s);   
 
  u:=Monoid(List(GeneratorsOfGroup(g), x-> AsTransformation(x, deg)));
  
  SetIsomorphismPermGroup(u, MappingByFunction(u, g, PermutationOfImage, 
   x-> AsTransformation(x, deg)));
   
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

#

InstallMethod(GroupOfUnits, "for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, g, deg, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  deg:=Maximum(DegreeOfPartialPermSemigroup(s),
   CodegreeOfPartialPermSemigroup(s));   
 
  u:=Monoid(List(GeneratorsOfGroup(g), x-> AsPartialPerm(x, deg)));
  
  SetIsomorphismPermGroup(u, MappingByFunction(u, g, AsPermutation, 
   x-> AsPartialPerm(x, deg)));
   
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

#

InstallMethod(GroupOfUnits, "for a Rees 0-matrix subsemigroup with generators",
[IsReesZeroMatrixSubsemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, g, i, j, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  i:=MultiplicativeNeutralElement(s)![1];
  j:=MultiplicativeNeutralElement(s)![3];

  u:=Semigroup(List(GeneratorsOfGroup(g), x-> RMSElement(s, i, x, j)));
  
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

#

InstallMethod(IdempotentGeneratedSubsemigroup, 
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s-> Semigroup(Idempotents(s), rec(small:=true)));

#

InstallMethod(IdempotentGeneratedSubsemigroup, 
"for an inverse op acting semigroup with generators",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
s-> InverseSemigroup(Idempotents(s), rec(small:=true)));

#

InstallMethod(InjectionPrincipalFactor, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local g, rep, rreps, lreps, mat, inv_l, inv_r, lambdaperm, leftact, rightact, f, rms, iso, inv, hom, i, j;

  if not IsRegularDClass(d) then
    Error("usage: <d> must be a regular D-class,");
    return;
  fi;

  g:=GroupHClass(d);
  rep:=Representative(g);
  g:=Range(IsomorphismPermGroup(g));

  rreps:=HClassReps(LClass(d, rep));
  lreps:=HClassReps(RClass(d, rep));
  mat:=[];

  inv_l:=EmptyPlist(Length(lreps));
  inv_r:=EmptyPlist(Length(rreps));

  lambdaperm:=LambdaPerm(Parent(d));
  if IsTransformationSemigroupGreensClass(d) 
    or IsPartialPermSemigroupGreensClass(d) then 
    leftact:=PROD;
  elif IsReesZeroMatrixSubsemigroup(Parent(d)) then 
    leftact:=function(x, y)
      return Objectify(TypeObj(y), [y![1], y![4][rep![3]][rep![1]]^-1
      *x*rep![2]^-1*y![2], y![3], y![4]]);
    end;
  fi;

  rightact:=StabilizerAction(Parent(d));

  for i in [1..Length(lreps)] do
    mat[i]:=[];
    for j in [1..Length(rreps)] do
      f:=lreps[i]*rreps[j];
      if f in d then
        mat[i][j]:=lambdaperm(rep, f);
        if not IsBound(inv_r[j]) then
          # could use lreps[i]*rreps[j]^-1*lreps[i] instead if there was a
          # method for ^-1...
          inv_r[j]:=leftact(mat[i][j]^-1, lreps[i]);
        fi;
        if not IsBound(inv_l[i]) then
          inv_l[i]:=rightact(rreps[j], mat[i][j]^-1);
        fi;
      else
        mat[i][j]:=0;
      fi;
    od;
  od;
  
  if NrIdempotents(d)=NrHClasses(d) then 
    rms:=ReesMatrixSemigroup(g, mat);
  else
    rms:=ReesZeroMatrixSemigroup(g, mat);
  fi;
  
  iso:=function(f)
    local o, m, i, j;
    o:=LambdaOrb(d); m:=LambdaOrbSCCIndex(d);
    i:=Position(o, LambdaFunc(Parent(d))(f));

    if i=fail or OrbSCCLookup(o)[i]<>m then
      return fail;
    fi;
    i:=Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then 
      o:=RhoOrb(d);
      m:=RhoOrbSCCIndex(d);
    fi;
    j:=Position(o, RhoFunc(Parent(d))(f));
    if j=fail or OrbSCCLookup(o)[j]<>m then
      return fail;
    fi;
    j:=Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);
    return Objectify(TypeReesMatrixSemigroupElements(rms), 
     [j, lambdaperm(rep, rep*inv_r[j]*f*inv_l[i]), i, mat]);
  end;

  inv:=function(x)
    if x![1]=0 then 
      return fail;
    fi;
    return rightact(rreps[x![1]], x![2])*lreps[x![3]];
  end;
  
  hom:=MappingByFunction(d, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);
  return hom;
end);

#

InstallMethod(IsomorphismReesMatrixSemigroup, 
"for D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass], InjectionPrincipalFactor);

#

InstallMethod(IrredundantGeneratingSubset,
"for an associative element collection",
[IsAssociativeElementCollection],
function(coll)
  local gens, nrgens, deg, out, redund, i, f;
  
  if not (IsActingSemigroup(coll) or IsGeneratorsOfActingSemigroup(coll)) then 
    Error();
  fi;

  if IsSemigroup(coll) and HasGeneratorsOfSemigroup(coll) then
    coll:=ShallowCopy(GeneratorsOfSemigroup(coll));
  fi;
  
  gens:=Set(ShallowCopy(coll)); 
  nrgens:=Length(gens); 
  deg:=ActionDegree(coll);
  coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));
  Sort(coll, function(x, y) return ActionRank(x, deg)>ActionRank(y, deg); end);
 
  out:=EmptyPlist(Length(coll));
  redund:=EmptyPlist(Length(coll));
  i:=0;

  repeat
    i:=i+1; f:=coll[i];
    if InfoLevel(InfoSemigroups)>=3 then 
      Print("at \t", i, " of \t", Length(coll), " with \t", Length(redund),
      " redundant, \t", Length(out), " non-redundant\r");
    fi;

    if not f in redund and not f in out then
      if f in Semigroup(Difference(gens, [f])) then
        AddSet(redund, f); gens:=Difference(gens, [f]);
      else
        AddSet(out, f);
      fi;
    fi;
  until Length(redund)+Length(out)=nrgens;

  if InfoLevel(InfoSemigroups)>1 then
    Print("\n");
  fi;
  return out;
end);

#

InstallMethod(IsomorphismReesMatrixSemigroup, 
"for a simple  or 0-simple acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(S)
  local D, iso, inv;
  if not (IsSimpleSemigroup(S) or IsZeroSimpleSemigroup(S)) then 
    TryNextMethod();
  fi;
  D:=GreensDClasses(S)[1];
  if IsZeroSimpleSemigroup(S) 
   and IsMultiplicativeZero(S, Representative(D)) then 
    D:=GreensDClasses(S)[2];
  fi;
  iso:=IsomorphismReesMatrixSemigroup(D);
  inv:=InverseGeneralMapping(iso);
  return MagmaIsomorphismByFunctionsNC(S, Range(iso), x-> x^iso, x->x^inv);
end);

#

InstallMethod(InversesOfSemigroupElement, 
"for acting semigroup with generators and associative element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)

  if f in s then
    return InversesOfSemigroupElementNC(s, f);
  fi;

  return fail;
end);

#

InstallMethod(InversesOfSemigroupElementNC, 
"for an acting semigroup and acting element",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)
  local regular, lambda, rank, rhorank, tester, j, o, rhos, opts, grades, rho_f, lambdarank, creator, inv, out, k, g, rho, name, i, x;

  regular:=IsRegularSemigroup(s);

  if not (regular or IsRegularSemigroupElementNC(s, f)) then
    return [];
  fi;

  lambda:=LambdaFunc(s)(f);
  rank:=LambdaRank(s)(LambdaFunc(s)(f)); 
  rhorank:=RhoRank(s);
  tester:=IdempotentTester(s);
  j:=0;

  # can't use GradedRhoOrb here since there may be inverses not D-related to f
  # JDM is this really true?
  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then 
    o:=RhoOrb(s);
    rhos:=EmptyPlist(Length(o));
    for rho in o do
      if rhorank(rho)=rank and tester(lambda, rho) then
        j:=j+1;
        rhos[j]:=rho;
      fi;
    od;
  else
      
    opts:=rec(  treehashsize:=s!.opts.hashlen.M, 
                gradingfunc:=function(o, x) return rhorank(x); end,
                onlygrades:=function(x, y) return x>=rank; end,
                onlygradesdata:=fail );
    
    for name in RecNames(LambdaOrbOpts(s)) do
      opts.(name):=LambdaOrbOpts(s).(name);
    od;

    o:=Orb(s, RhoOrbSeed(s), RhoAct(s), opts);
    Enumerate(o, infinity);
    
    grades:=Grades(o);
    rhos:=EmptyPlist(Length(o));
    for i in [2..Length(o)] do 
      if grades[i]=rank and tester(lambda, o[i]) then 
        j:=j+1;
        rhos[j]:=o[i];
      fi;
    od;
  fi;
  ShrinkAllocationPlist(rhos);
  
  rho_f:=RhoFunc(s)(f);
  lambdarank:=LambdaRank(s);
  creator:=IdempotentCreator(s);
  inv:=LambdaInverse(s);
  
  out:=[]; k:=0; 
 
  if HasLambdaOrb(s) and IsClosed(LambdaOrb(s)) then 
    o:=LambdaOrb(s);
    for i in [2..Length(o)] do
      if lambdarank(o[i])=rank and tester(o[i], rho_f) then
        for rho in rhos do
          g:=creator(lambda, rho)*inv(o[i], f);
          if regular or g in s then
            k:=k+1; 
            out[k]:=g;
          fi;
        od;
      fi;
    od;
  else
     opts:=rec(  treehashsize:=s!.opts.hashlen.M, 
                gradingfunc:=function(o, x) return lambdarank(x); end,
                onlygrades:=function(x, y) return x>=rank; end,
                onlygradesdata:=fail ); #shouldn't this be fail
    
    for name in RecNames(LambdaOrbOpts(s)) do
      opts.(name):=LambdaOrbOpts(s).(name);
    od;

    o:=Orb(s, LambdaOrbSeed(s), LambdaAct(s), opts);
    Enumerate(o);
    grades:=Grades(o);
    
    for i in [2..Length(o)] do 
      if grades[i]=rank and tester(o[i], rho_f) then
        for rho in rhos do
          g:=creator(lambda, rho)*inv(o[i], f);
          if regular or g in s then
            k:=k+1; 
            out[k]:=g;
          fi;
        od;
      fi;
    od;
  fi; 
 
  return out;
end);

#

InstallMethod(MultiplicativeNeutralElement, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, rank, lambda, max, r, rep, f;

  gens:=Generators(s);
  rank:=LambdaRank(s);
  lambda:=LambdaFunc(s);
  max:=0;
  rep:=gens[1];
  
  for f in gens do 
    r:=rank(lambda(f));
    if r>max then 
      max:=r;
      rep:=f;
    fi;
  od;

  if max=ActionDegree(s) and IsMultiplicativeElementWithOneCollection(s) then
    return One(s);
  fi;

  r:=GreensRClassOfElementNC(s, rep);

  if not NrIdempotents(r)=1 then
    Info(InfoSemigroups, 2, "the number of idempotents in the R-class of the",
    " first maximum rank");
    Info(InfoSemigroups, 2, " generator is not 1");
    return fail;
  fi;

  f:=Idempotents(r)[1];

  if ForAll(gens, x-> x*f=x and f*x=x) then
    return f;
  fi;

  Info(InfoSemigroups, 2, "the unique idempotent in the R-class of the first",
  " maximum rank");
  Info(InfoSemigroups, 2, " generator is not the identity");
  return fail;
end);

# it just so happens that the MultiplicativeNeutralElement of a semigroup of
# partial permutations has to coincide with the One. This is not the case for
# transformation semigroups

InstallMethod(MultiplicativeNeutralElement, "for a partial perm semigroup",
[IsPartialPermSemigroup], One);

#

InstallMethod(MultiplicativeZero, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local min, o, rank, i, pos, f, m, rank_i, min_found, n;
  
  min:=MinActionRank(s);
  o:=LambdaOrb(s);
  rank:=LambdaRank(s);
  
  #is there an element in s with minimum possible rank
  if IsTransformationSemigroup(s) then 
    i:=0;
    repeat 
      i:=i+1;
      pos:=EnumeratePosition(o, [i], false);
    until pos<>fail or i=ActionDegree(s);
  elif IsPartialPermSemigroup(s) then 
    pos:=EnumeratePosition(o, [], false);
  else
    pos:=LookForInOrb(o, function(o, x) return rank(x)=min; end, 2);
  fi;

  if pos<>fail and pos<>false then
    f:=EvaluateWord(GeneratorsOfSemigroup(s), 
     TraceSchreierTreeForward(o, pos));
  else
    # lambda orb is closed, find an element with minimum rank
    min_found:=rank(o[2]); pos:=2; i:=1; 
    
    while min_found>min and i<Length(o) do 
      i:=i+1;
      rank_i:=rank(o[i]);
      if rank_i<min_found then 
        min_found:=rank_i;
        pos:=i;
      fi;
    od;
    f:=EvaluateWord(GeneratorsOfSemigroup(s), TraceSchreierTreeForward(o, pos));
  fi;

  if IsIdempotent(f) and Size(GreensRClassOfElementNC(s, f))=1 then
    return f;
  fi;

  return fail;
end);

#JDM better if this returned an actual semigroup ideal!!

InstallMethod(MinimalIdeal, "for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local rank, o, pos, min, len, m, f, d, I, n, i;

  rank:=LambdaRank(s);
  o:=LambdaOrb(s);
  
  pos:=LookForInOrb(o, function(o, x) return rank(x)=MinActionRank(s); end, 2);

  if pos=false then 
    min:=rank(o[2]); pos:=2; len:=Length(o);

    for i in [3..len] do 
      m:=rank(o[i]);
      if m<min then
        pos:=i; min:=m;
      fi;
    od;
  fi;

  f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, pos));
  d:=GreensDClassOfElementNC(s, f);
  I:=Semigroup(Elements(d), rec(small:=true));
  SetMinimalDClass(s, d);
  SetIsSimpleSemigroup(I, true);
  return I; 
end);

# same method for inverse

InstallMethod(MinimalDClass, "for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local rank, o, pos, min, len, m, f, I, n, i;

  rank:=LambdaRank(s);
  o:=LambdaOrb(s);
  
  pos:=LookForInOrb(o, function(o, x) return rank(x)=MinActionRank(s); end, 2);

  if pos=false then 
    min:=rank(o[2]); pos:=2; len:=Length(o);
    for i in [3..len] do 
      m:=rank(o[i]);
      if m<min then
        pos:=i; min:=m;
      fi;
    od;
  fi;

  f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, pos));
  return GreensDClassOfElementNC(s, f);
end);

#

InstallMethod(MinimalIdeal, "for an inverse acting semigroup",
[IsActingSemigroupWithInverseOp],
function(s)
  local rank, o, pos, min, len, m, f, I, n, i;

  rank:=LambdaRank(s);
  o:=LambdaOrb(s);

  pos:=LookForInOrb(o, function(o, x) return rank(x)=MinActionRank(s); end, 2);
  
  if pos=false then 
    min:=rank(o[2]); pos:=2; len:=Length(o);
    for i in [3..len] do 
      m:=rank(o[i]); 
      if m<min then 
        pos:=i; min:=m;
      fi;
    od;
  fi;

  f:=EvaluateWord(o!.gens, TraceSchreierTreeForward(o, pos));
  I:=InverseSemigroup(Elements(GreensDClassOfElementNC(s, f)), 
   rec(small:=true));
  SetIsGroupAsSemigroup(I, true);
  return I;
end);

#

InstallMethod(PrincipalFactor, "for a D-class", 
[IsGreensDClass and IsActingSemigroupGreensClass], 
d-> Range(InjectionPrincipalFactor(d)));

#

InstallMethod(PrincipalFactor, "for a D-class",
[IsGreensDClass], AssociatedReesMatrixSemigroupOfDClass);

#

InstallMethod(SmallGeneratingSet, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  if IsEmpty(Generators(s)) then 
    return Generators(s);
  else
    return Generators(Semigroup(Generators(s), rec(small:=true)));
  fi;
end);

#

InstallMethod(SmallGeneratingSet, 
"for an acting semigroup with inverse op and generators", 
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
function(s)
  if IsEmpty(Generators(s)) then 
    return Generators(s);
  else
    return Generators(InverseSemigroup(Generators(s), rec(small:=true)));
  fi;
end);

#

InstallMethod(StructureDescription, "for an acting Brandt semigroup",
[IsActingSemigroup and IsBrandtSemigroup],
function(s)
  local x, d;
  
  x:=First(Generators(s), x-> x<>MultiplicativeZero(s));
  d:=GreensDClassOfElementNC(s, x);
  
  return Concatenation("B(", StructureDescription(GroupHClass(d)), ", ",
  String(NrRClasses(d)), ")");
end);

#

InstallMethod(StructureDescription, 
"for an acting group as semigroup",
[IsActingSemigroup and IsGroupAsSemigroup],
s-> StructureDescription(Range(IsomorphismPermGroup(s))));

#

InstallMethod(IsomorphismTransformationMonoid, "for a transformation semigroup",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local id, dom, gens, inv;

  if IsMonoid(s) then 
    return MappingByFunction(s, s, IdFunc, IdFunc);
  fi;

  if MultiplicativeNeutralElement(s)=fail then 
    Error( "usage: <s> must have a multiplicative neutral element,");
    return;
  fi;
  
  id:=MultiplicativeNeutralElement(s);
  dom:=ImageSetOfTransformation(id);
  
  gens:=List(Generators(s), x-> TransformationOp(x, dom));

  inv:=function(f)
    local out, i;

    out:=[1..DegreeOfTransformationSemigroup(s)];
    for i in [1..Length(dom)] do 
      out[dom[i]]:=dom[i^f];
    od;
    return id*Transformation(out);
  end;

  return MappingByFunction(s, Monoid(gens), f-> TransformationOp(f, dom), 
   inv);
end);

#

InstallMethod(IsomorphismTransformationSemigroup, 
"for a matrix semigroup",
[IsMatrixSemigroup], 
function(S)        
  local n, F, T;
  n:=Length(GeneratorsOfSemigroup(S)[1][1]);
  F:=FieldOfMatrixList(GeneratorsOfSemigroup(S));        
  T:=Semigroup(List(GeneratorsOfSemigroup(S), x-> 
   TransformationOp(x, Elements(F^n), OnRight)));        
  return MappingByFunction(S, T,
   x-> TransformationOp(x, Elements(F^Size(F)), OnRight));
end);

#

InstallMethod(IsomorphismPermGroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
 
  if not IsGroupAsSemigroup(s)  then
   Error( "usage: a transformation semigroup satisfying IsGroupAsSemigroup,");
   return; 
  fi;

  return MagmaIsomorphismByFunctionsNC(s, 
   Group(List(Generators(s), PermutationOfImage)), 
    PermutationOfImage, 
    x-> AsTransformation(x, DegreeOfTransformationSemigroup(s)));
end);

#

InstallMethod(IsomorphismPermGroup,
"for a partial perm semigroup with generators",
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsGroupAsSemigroup(s)  then
   Error( "usage: a partial perm semigroup satisfying IsGroupAsSemigroup,");
   return; 
  fi;

  return MagmaIsomorphismByFunctionsNC(s,
   Group(List(GeneratorsOfSemigroup(s), AsPermutation)),
    AsPermutation, x-> AsPartialPerm(x, DomainOfPartialPermCollection(s)));
end);

#

InstallMethod(IsomorphismPermGroup,
"for a subsemigroup of a Rees 0-matrix semigroup",
[IsReesZeroMatrixSubsemigroup and HasGeneratorsOfSemigroup],
function(s)
  local rep;

  if not IsGroupAsSemigroup(s)  then
   Error( "usage: a subsemigroup of a Rees 0-matrix semigroup satisfying\n", 
    "IsGroupAsSemigroup,");
   return; 
  fi;

  rep:=s.1;
  if rep![1]=0 then # special case for the group consisting of 0
    return MagmaIsomorphismByFunctionsNC(s, Group(()), x-> (), x-> rep);
  fi;

  return MagmaIsomorphismByFunctionsNC(s, 
    Group(List(GeneratorsOfSemigroup(s), x-> x![2])),
      x-> x![2], x-> RMSElement(s, rep![1], x, rep![3]));
end);

#

InstallMethod(IsomorphismTransformationSemigroup, 
"for semigroup of binary relations with generators", 
[IsBinaryRelationSemigroup and HasGeneratorsOfSemigroup], 2, 
#to beat the method for a semigroups of general mappings
function(s)        
  local n, pts, o, t, pos, i;

  n:=DegreeOfBinaryRelation(Generators(s)[1]);
  pts:=EmptyPlist(2^n);

  for i in [1..n] do 
    o:=Orb(s, [i], OnPoints); #JDM multiseed orb
    Enumerate(o);
    pts:=Union(pts, AsList(o));
  od;
  ShrinkAllocationPlist(pts);
  pos:=List([1..n], x-> Position(pts, [x]));
  t:=Semigroup(List(Generators(s), x-> TransformationOpNC(x, pts, OnPoints)));
  
  return MappingByFunction(s, t, x-> TransformationOpNC(x, pts, OnPoints),
  x-> BinaryRelationOnPoints(List([1..n], i-> pts[pos[i]^x])));
end);

#

InstallMethod(Size, "for a monogenic transformation semigroup",
[IsTransformationSemigroup and IsMonogenicSemigroup],
function(s)
  local ind;
  
  ind:=IndexPeriodOfTransformation(GeneratorsOfSemigroup(s)[1]);
  if ind[1]>0 then 
    return Sum(ind)-1;
  fi;
  return Sum(ind);
end);

#EOF
