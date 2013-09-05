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

InstallMethod(\<, "for Rees 0-matrix semigroups", 
[IsReesZeroMatrixSubsemigroup, IsReesZeroMatrixSubsemigroup], 
function(R, S)
  return GeneratorsOfSemigroup(R)<GeneratorsOfSemigroup(S);
end);

#

InstallMethod(\<, "for Rees 0-matrix semigroups", 
[IsReesZeroMatrixSemigroup, IsReesZeroMatrixSemigroup], 100,
function(R, S)
  return Size(R)<Size(S) or (Rows(R)<Rows(S) or (Rows(R)=Rows(S) and
  Columns(R)<Columns(S)) or (Rows(R)=Rows(S) and Columns(R)=Columns(S) 
    and UnderlyingSemigroup(R)<UnderlyingSemigroup(S)));
end);

#

LongestChainOfSubsemigroups:=function(R)
  if Size(R)>1 then 
    return Maximum(List(MaximalSubsemigroups(R), 
     LongestChainOfSubsemigroups))+1;
  fi;
  return 0;
end;

#

Subsemigroups:=function(R) #for a Rees 0-matrix semigroup...
  local max, o, U, V;
  
  max:=Set(MaximalSubsemigroups(R));
  o:=ShallowCopy(max);
  
  for U in o do 
    if Size(U)>1 then 
      for V in MaximalSubsemigroups(U) do 
        if not V in max then 
          AddSet(max, V);
          Add(o, V);
        fi;
      od;
    fi;
  od;

  return max;
end;

#

IsMaximalSubsemigroup:=function(S, T)
  if IsSubsemigroup(S, T) then 
    return ForAll(S, x-> x in T or Semigroup(GeneratorsOfSemigroup(T), x)=S);
  else
    return false;
  fi;
end; 

#

#InstallMethod(MaximalSubsemigroups, "for a semigroup with generators", 
#[IsSemigroup and HasGeneratorsOfSemigroup],
#function(S)
#  #assumes that the generators are irredundant
#  D:=MaximalDClasses(S);
#  otherD:=List(Filtered(GeneratorsOfSemigroup(S), x-> not DClass(S, x) in D), 
#  x-> DClass(S, x));

#  for d in D do 
#    if Size(d)=1 then 
#      Add(out, 
#

#end);

# the following method comes from Remark 1 in Graham, Graham, and Rhodes.
# and only works for Rees 0-matrix semigroup over groups

InstallMethod(MaximalSubsemigroups, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup], 
function(R)
  local G, out, mat, I, J, P, new, pos, JJ, solo, II, len, graph, names, rectangles, gens, K, L, issubsemi, H, i, j, r, k, l, x;
  
  if not IsReesZeroMatrixSemigroup(R) then 
    TryNextMethod(); 
    return;
  fi;
 
  G:=UnderlyingSemigroup(R);    
  
  if not IsGroup(G) then 
    if IsZeroSimpleSemigroup(R) then 
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

  #JDM in the case when G is not a group, and maybe R is not 0-simple, in
  #these case the generic method should be used

  out:=[];
  mat:=Matrix(R);     I:=Rows(R);         J:=Columns(R);
 
  # find the set of group elements in the matrix
  P:=Union(mat);
  if P[1]=0 then 
    Remove(P, 1);
  else  # S\{0} is a maximal subsemigroup 
        # the unique case when the missing D-class is {0}
    new:=ShallowCopy(GeneratorsOfSemigroup(R));
    pos:=Position(new, MultiplicativeZero(R)); 
    Remove(new, pos); #remove the zero, which has to be present
    Add(out, Semigroup(new));
  fi;
  
  if Length(I)=1 and Length(J)=1 and IsAbelian(G) and IsSimple(G) then 
    # the unique case when {0} is a maximal subsemigroup
    Add(out, Semigroup(MultiplicativeZero(R)));
  fi;
  
  Info(InfoSemigroups, 3, 
   "Case 1: maximal subsemigroups arising from maximal subgroups...");
  # Case 1: maximal subsemigroups of the form (IxHxJ)\cup\{0\} where H is a
  # maximal subgroup of G
  
  P:=Group(P);
  if P<>G then # every subsemigroup of S containing a cross-section of L- and
               # R-classes contains P as a maximal subgroup
    for H in MaximalSubgroups(G) do
      if IsSubgroup(H, P) then 
        Add(out, ReesZeroMatrixSubsemigroupNC(R, I, H, J));
      fi;
    od;
  fi;

  Info(InfoSemigroups, 3, "...found ", Length(out));

  #JDM could test for IsRUnipotent or IsLUnipotent here!

  Info(InfoSemigroups, 3, 
   "Case 2: maximal subsemigroups obtained by removing a column...");

  # Case 2: maximal subsemigroup of the form (IxGxJ')\cup\{0\} where J'=J\{j}
  # for some j in J, and where the resultant matrix has no zero columns or rows.

  # in the Graham-Houghton IxJ bipartite graph, we can remove any vertex <j> in
  # <J> which is not adjacent to a vertex <i> which is only adjacent to <j>.
  # So, we run through the vertices <i> of <I> and find the ones of degree 1,
  # and we discard the vertices <j> adjacent to such <i>. 
  
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
    Add(out, ReesZeroMatrixSubsemigroupNC(R, I, G, Difference(J, [j])));
  od;
  Info(InfoSemigroups, 3, "...found ", Length(JJ));

  Info(InfoSemigroups, 3, 
   "Case 3: maximal subsemigroups obtained by removing a row...");

  # Case 3: the dual of case 2.
  
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
    Add(out, ReesZeroMatrixSubsemigroupNC(R, Difference(I, [i]), G, J));
  od;

  Info(InfoSemigroups, 3, "...found ", Length(II));

  Info(InfoSemigroups, 3, 
   "Case 4: maximal subsemigroups obtained by removing a rectangle...");

  # Case 4: maximal rectangle of zeros in the matrix

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
   "finding rectangles which give rise to subsemigroups...");
  for r in [2..Length(rectangles)-1] do 
    Apply(rectangles[r], names);
    #the first and last entries correspond to removing all the rows or columns
    K:=Difference(I, rectangles[r]);
    L:=Difference(J, rectangles[r]-len);
    #a rectangle KxL describes a subsemigroups if and only if (I\K)x(J\L)
    #consists entirely of zeros
    issubsemi:=true;
    for k in K do 
      for l in L do
        if mat[l][k]<>0 then # not a subsemigroup
          issubsemi:=false;
          break;
        fi;
      od;
      if not issubsemi then 
        break;
      fi;
    od;
    if issubsemi then 
      new:=[];
      for i in I do
        if not (i in rectangles[r]) then 
          for j in J do 
            for x in gens do 
              Add(new, RMSElement(R, i, x, j));
            od;
          od;
        else 
          for j in J do 
            if not (j+len in rectangles[r]) then 
              for x in gens do 
                Add(new, RMSElement(R, i, x, j));
              od;
            fi;
          od;
        fi;
      od;
      Add(out, Semigroup(new));
    fi;
  od;
  return out;
end);

# Note that a semigroup satisfies IsTransformationMonoid only if One(s)<>fail. 

InstallMethod(MaximalDClasses, "for a semigroup with generators",
[IsSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local gens, po, classes, D, out, ideals, i, x;

  gens:=GeneratorsOfSemigroup(s);
  
  if HasPartialOrderOfDClasses(s) then 
    po:=PartialOrderOfDClasses(s);
    classes:=GreensDClasses(s);
    D:=List(gens, x-> PositionProperty(classes, d-> x in d));
    out:=[];
    for i in D do 
      if not ForAny([1..Length(po)], j-> j<>i and i in po[j]) then 
        Add(out, classes[i]);
      fi;
    od;
  else
    D:=[GreensDClassOfElementNC(s, gens[1])];
  
    for x in gens do 
      if not ForAny(D, d-> x in d) then 
        Add(D, GreensDClassOfElementNC(s, x));
      fi;
    od;
    
    ideals:=List(D, x-> SemigroupIdealByGenerators(s, [Representative(x)]));
    out:=[];

    for i in [1..Length(D)] do 
      if not ForAny([1..Length(D)], 
        j-> j<>i and Representative(D[i]) in ideals[j]) then  
        Add(out, D[i]);
      fi;
    od;
  fi;

  return out;
end);

#

InstallMethod(StructureDescriptionSchutzenbergerGroups, 
"for an acting semigroup", 
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
"for an acting semigroup", 
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

InstallMethod(GroupOfUnits, 
"for a transformation semigroup with generators",
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
  
  SetIsomorphismPermGroup(u, MappingByFunction(u, g, AsPermutation, 
   x-> AsTransformation(x, deg)));
   
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

#

InstallMethod(GroupOfUnits, 
"for a partial perm semigroup with generators",
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

InstallMethod(GroupOfUnits, 
"for a bipartition semigroup with generators",
[IsBipartitionSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, g, deg, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  deg:=DegreeOfBipartitionSemigroup(s);
  u:=Monoid(List(GeneratorsOfGroup(g), x-> AsBipartition(x, deg)));
  
  SetIsomorphismPermGroup(u, MappingByFunction(u, g, AsPermutation, 
   x-> AsBipartition(x, deg)));
   
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
  local g, rep, rreps, lreps, mat, inj, zero, bound_r, bound_l, inv_l, inv_r, lambdaperm, f, rms, iso, inv, hom, i, j;

  if not IsRegularDClass(d) then
    Error("usage: <d> must be a regular D-class,");
    return;
  elif NrIdempotents(d)=NrHClasses(d) then
    return IsomorphismReesMatrixSemigroup(d);
  fi;

  g:=GroupHClass(d);
  rep:=Representative(g);
  g:=Range(IsomorphismPermGroup(g));

  rreps:=HClassReps(LClass(d, rep));
  lreps:=HClassReps(RClass(d, rep));
  mat:=[];

  bound_r:=BlistList([1..Length(rreps)], []);
  bound_l:=BlistList([1..Length(lreps)], []);
  inv_l:=EmptyPlist(Length(lreps));
  inv_r:=EmptyPlist(Length(rreps));

  lambdaperm:=LambdaPerm(Parent(d));

  for i in [1..Length(lreps)] do
    mat[i]:=[];
    for j in [1..Length(rreps)] do
      f:=lreps[i]*rreps[j];
      if f in d then
        #mat[i][j]:=AsPermutation(f); 
        #JDM AsPermutation doesn't work for partition monoids
        mat[i][j]:=lambdaperm(rep, f);
        if not bound_r[j] then
          bound_r[j]:=true;
          inv_r[j]:=mat[i][j]^-1*lreps[i];
        fi;
        if not bound_l[i] then
          bound_l[i]:=true;
          inv_l[i]:=rreps[j]*mat[i][j]^-1;
        fi;
        mat[i][j]:=mat[i][j];
      else
        mat[i][j]:=0;
      fi;
    od;
  od;

  rms:=ReesZeroMatrixSemigroup(g, mat);
  iso:=function(f)
    local o, i, j;
    o:=LambdaOrb(d);
    i:=Position(o, LambdaFunc(Parent(d))(f));

    if i=fail then
      return fail;
    fi;
    i:=Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then 
      o:=RhoOrb(d);
    fi;
    j:=Position(o, RhoFunc(Parent(d))(f));
    if j=fail then
      return fail;
    fi;
    j:=Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);

    return Objectify(TypeReesMatrixSemigroupElements(rms), 
     [j, AsPermutation(inv_r[j]*f*inv_l[i]), i]);
  end;

  inv:=function(x)
    if x![1]=0 then 
      return fail;
    fi;
    return rreps[x![1]]*x![2]*lreps[x![3]];
  end;

  hom:=MappingByFunction(d, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);

  return hom;
end);

#

InstallMethod(IrredundantGeneratingSubset,
"for an associative element collection",
[IsAssociativeElementCollection],
function(coll)
  local gens, j, out, i, redund, f;
  
  if not IsGeneratorsOfActingSemigroup(coll) then 
    Error();
  fi;

  if IsSemigroup(coll) and HasGeneratorsOfSemigroup(coll) then
    coll:=ShallowCopy(GeneratorsOfSemigroup(coll));
  fi;
  
  gens:=Set(ShallowCopy(coll)); j:=Length(gens);
  coll:=Permuted(coll, Random(SymmetricGroup(Length(coll))));
  Sort(coll, function(x, y) return ActionRank(x)>ActionRank(y); end);
 
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
  until Length(redund)+Length(out)=j;

  if InfoLevel(InfoSemigroups)>1 then
    Print("\n");
  fi;
  return out;
end);

# this method is wrong, the returned function should have Source=s not the
# the unique D-class of S, also there is no library method for
# IsomorphismReesMatrixSemigroup of a D-class and so this doesn't work for
# (say) Rees matrix semigroups. JDM

#InstallMethod(IsomorphismReesMatrixSemigroup, 
#"for a simple semigroup with generators",
#[IsSimpleSemigroup and HasGeneratorsOfSemigroup],
#function(s)
#  return IsomorphismReesMatrixSemigroup(DClass(s, Representative(s)));
#end);

#

InstallMethod(IsomorphismReesMatrixSemigroup, 
"for D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local g, rep, rreps, lreps, mat, lambdaperm, rms, iso, inv, hom, i, j;

  if not IsRegularDClass(d) or not NrIdempotents(d)=NrHClasses(d) then
    Error("every H-class of the D-class should be a group,",
    " try InjectionPrincipalFactor instead,");
    return;
  fi;

  g:=GroupHClass(d);

  rep:=Representative(g); 
  g:=Range(IsomorphismPermGroup(g));

  rreps:=HClassReps(LClass(d, rep)); 
  lreps:=HClassReps(RClass(d, rep));
  mat:=[];
  lambdaperm:=LambdaPerm(Parent(d));

  for i in [1..Length(lreps)] do 
    mat[i]:=[];
    for j in [1..Length(rreps)] do 
      mat[i][j]:=lambdaperm(rep, lreps[i]*rreps[j]);
      #mat[i][j]:=AsPermutation(lreps[i]*rreps[j]);
    od;
  od;

  rms:=ReesMatrixSemigroup(g, mat);
  
  iso:=function(f)
    local o, i, j;
    o:=LambdaOrb(d);
    i:=Position(o, LambdaFunc(Parent(d))(f));
    if i=fail then 
      return fail;
    fi;
    i:=Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then 
      o:=RhoOrb(d);
    fi;
    j:=Position(o, RhoFunc(Parent(d))(f)); 
    if j=fail then 
      return fail;
    fi;
    j:=Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);

    return Objectify(TypeReesMatrixSemigroupElements(rms), 
    [j, AsPermutation(rreps[j])^-1*AsPermutation(f)*AsPermutation(lreps[i])^-1,
     i]);
  end;

  inv:=function(x)
    return rreps[x![1]]*x![2]*lreps[x![3]];
  end;

  hom:=MappingByFunction(d, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);

  return hom;
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
"for an acting semigroup and acting elt",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)
  local regular, lambda, rank, rhorank, tester, j, o, rhos, opts, grades, rho_f, lambdarank, creator, inv, out, k, g, rho, name, i, x;

  regular:=IsRegularSemigroup(s);

  if not (regular or IsRegularSemigroupElementNC(s, f)) then
    return [];
  fi;

  lambda:=LambdaFunc(s)(f);
  rank:=ActionRank(f); 
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
  local gens, n, rank, lambda, f, r;

  gens:=Generators(s);
  n:=Maximum(List(gens, ActionRank));

  if n=ActionDegree(s) then
    return One(s);
  fi;

  rank:=LambdaRank(s);
  lambda:=LambdaFunc(s);
  f:=First(gens, f-> rank(lambda(f))=n);

  r:=GreensRClassOfElementNC(s, f); #NC? JDM 

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
[IsGreensDClass], 
d-> Range(InjectionPrincipalFactor(d)));

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
     Error( "usage: transformation semigroup satisfying IsGroupAsSemigroup,");
     return; 
   fi;
 
   return MappingByFunction(s, Group(List(Generators(s), AsPermutation)), 
    AsPermutation, x-> AsTransformation(x, DegreeOfTransformationSemigroup(s)));
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

#EOF
