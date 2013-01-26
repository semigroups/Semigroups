#############################################################################
##
#W  attributes.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
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

# Note that a semigroup satisfies IsTransformationMonoid only if One(s)<>fail. 

# JDM this should be undocumented until fixed

InstallMethod(EmbeddingNC, "for a perm group and an acting semigroup",
[IsPermGroup, IsActingSemigroup],
function(g, s)
  local convert, creator, one, t, emb, conj;
 
  if IsTransformationSemigroup(s) then 
    convert:=x-> AsTransformation(x, DegreeOfTransformationSemigroup(s));
  elif IsPartialPermSemigroup(s) then 
    convert:=x-> AsPartialPerm(x, DomainOfPartialPermCollection(s));
    # JDM this won't work in general if Points(s) is not the correct set to act
    # on.
  fi;

  if IsMonoid(s) then 
    creator:=Monoid;
  else
    creator:=Semigroup;
  fi;

  if NrMovedPoints(g)<=ActionDegree(s) then 
    conj:=MappingPermListList(MovedPoints(g), [1..NrMovedPoints(g)]);
    emb:=x-> convert(x^conj);
    t:=creator(List(GeneratorsOfGroup(g), emb));
  else
    Error("the number of moved points of the group is greater than the ",
    "degree of the semigroup");
    return;
  fi;

  return MappingByFunction(g, t, emb, AsPermutation); 
end);

#

InstallMethod(GroupOfUnits, 
"for an acting semigroup with generators",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)
  local r, m, g, emb, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  #Group(AsPermutation(Random(r)));

  #while Size(g)<Size(r) do
  # g:=ClosureGroup(g, AsPermutation(Random(r)));
  #od;
  
  emb:=EmbeddingNC(g, s);
  u:=Range(emb);
  SetIsomorphismPermGroup(u, InverseGeneralMapping(emb));
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

InstallMethod(InjectionPrincipalFactor, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local g, rep, rreps, lreps, mat, inj, zero, bound_r, bound_l, inv_l, inv_r,
  f, rms, iso, inv, hom, i, j;

  if not IsRegularDClass(d) then
    Error("not yet implemented,");
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

  inj:=InjectionZeroMagma(g);
  SetIsTotal(inj, true);
  SetIsSingleValued(inj, true);
  g:=Range(inj);
  zero:=MultiplicativeZero(g);
  bound_r:=List([1..Length(rreps)], ReturnFalse);
  bound_l:=List([1..Length(lreps)], ReturnFalse);
  inv_l:=EmptyPlist(Length(lreps));
  inv_r:=EmptyPlist(Length(rreps));

  for i in [1..Length(lreps)] do
    mat[i]:=[];
    for j in [1..Length(rreps)] do
      f:=lreps[i]*rreps[j];
      if f in d then
        mat[i][j]:=AsPermutation(f);
        if not bound_r[j] then
          bound_r[j]:=true;
          inv_r[j]:=mat[i][j]^-1*lreps[i];
        fi;
        if not bound_l[i] then
          bound_l[i]:=true;
        inv_l[i]:=rreps[j]*mat[i][j]^-1;
        fi;
        mat[i][j]:=mat[i][j]^inj;
      else
        mat[i][j]:=zero;
      fi;
    od;
  od;

  rms:=ReesZeroMatrixSemigroup(g, mat);
  iso:=function(f)
    local o, i, j;
    o:=LambdaOrb(d);
    i:=Position(o, LambdaFunc(ParentSemigroup(d))(f));

    if i=fail then
      return fail;
    fi;
    i:=Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then 
      o:=RhoOrb(d);
    fi;
    j:=Position(o, RhoFunc(ParentSemigroup(d))(f));
    if j=fail then
      return fail;
    fi;
    j:=Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);

    return RMSElementNC(rms, j,
      AsPermutation(inv_r[j]*f*inv_l[i])^inj, i);
  end;

  inv:=function(x)
    local i, a, j;
    i:=RowOfRMSElement(x);
    a:=Images(InverseGeneralMapping(inj), UnderlyingElementOfRMSElement(x))[1];
    j:=ColumnOfRMSElement(x);
    return rreps[i]*a*lreps[j];
  end;

  hom:=MappingByFunction(d, rms, iso, inv);
  SetIsInjective(hom, true);
  SetIsTotal(hom, true);

  return hom;
end);

#

InstallMethod(IrredundantGeneratingSubset,
"for an associative element with action collection",
[IsAssociativeElementWithActionCollection],
function(coll)
  local gens, j, out, i, redund, f;
  
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

#

InstallOtherMethod(IsomorphismReesMatrixSemigroup, 
"for a simple semigroup with generators",
[IsSimpleSemigroup and HasGeneratorsOfSemigroup],
function(s)
  return IsomorphismReesMatrixSemigroup(DClass(s, Representative(s)));
end);


#

InstallOtherMethod(IsomorphismReesMatrixSemigroup, 
"for D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local g, rep, rreps, lreps, mat, rms, iso, inv, hom, i, j;

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
  
  for i in [1..Length(lreps)] do 
    mat[i]:=[];
    for j in [1..Length(rreps)] do 
      mat[i][j]:=AsPermutation(lreps[i]*rreps[j]);
    od;
  od;

  rms:=ReesMatrixSemigroup(g, mat);
  
  iso:=function(f)
    local o, i, j;
    o:=LambdaOrb(d);
    i:=Position(o, LambdaFunc(ParentSemigroup(d))(f));
    if i=fail then 
      return fail;
    fi;
    i:=Position(OrbSCC(o)[OrbSCCLookup(o)[i]], i);
    if not IsInverseOpClass(d) then 
      o:=RhoOrb(d);
    fi;
    j:=Position(o, RhoFunc(ParentSemigroup(d))(f)); 
    if j=fail then 
      return fail;
    fi;
    j:=Position(OrbSCC(o)[OrbSCCLookup(o)[j]], j);

    return RMSElementNC(rms, j,
      AsPermutation(rreps[j])^-1*AsPermutation(f)*
      AsPermutation(lreps[i])^-1, i);
  end;

  inv:=function(x)
    local i, a, j;
    i:=RowOfReesMatrixSemigroupElement(x);
    a:=UnderlyingElementOfReesMatrixSemigroupElement(x);
    j:=ColumnOfReesMatrixSemigroupElement(x);
    return rreps[i]*a*lreps[j];
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

#JDM check this works...

InstallMethod(InversesOfSemigroupElementNC, 
"for an acting semigroup and acting elt",
[IsActingSemigroup and HasGeneratorsOfSemigroup, IsAssociativeElement],
function(s, f)
  local regular, rank_f, lambda, rhorank, tester, j, o, rhos, opts, grades, rho_f, lambdarank, creator, inv, out, k, g, rho, name, i, x;

  regular:=IsRegularSemigroup(s);

  if not (regular or IsRegularSemigroupElementNC(s, f)) then
    return [];
  fi;

  rank_f:=LambdaRank(s)(f); 
  lambda:=LambdaFunc(s)(f);
  rhorank:=RhoRank(s);
  tester:=IdempotentTester(s);
  j:=0;

  # can't use GradedRhoOrb here since there may be inverses not D-related to f
  # JDM is this really true?
  if HasRhoOrb(s) and IsClosed(RhoOrb(s)) then 
    o:=RhoOrb(s);
    rhos:=EmptyPlist(Length(o));
    for rho in o do
      if rhorank(rho)=rank_f and tester(lambda, rho) then
        j:=j+1;
        rhos[j]:=rho;
      fi;
    od;
  else
      
    opts:=rec(  treehashsize:=s!.opts.hashlen.M, 
                gradingfunc:=function(o, x) return rhorank(x); end,
                onlygrades:=function(x, y) return x>=rank_f; end,
                onlygradesdata:=fail ); #shouldn't this be fail
    
    for name in RecNames(LambdaOrbOpts(s)) do
      opts.(name):=LambdaOrbOpts(s).(name);
    od;

    o:=Orb(s, RhoOrbSeed(s), RhoAct(s), opts);
    Enumerate(o, infinity);
    
    grades:=Grades(o);
    rhos:=EmptyPlist(Length(o));
    for i in [2..Length(o)] do 
      if grades[i]=rank_f and tester(lambda, o[i]) then 
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
    for x in o do
      if lambdarank(x)=rank_f and tester(x, rho_f) then
        for rho in rhos do
          g:=creator(lambda, rho)*inv(f);
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
                onlygrades:=function(x, y) return x>=rank_f; end,
                onlygradesdata:=fail ); #shouldn't this be fail
    
    for name in RecNames(LambdaOrbOpts(s)) do
      opts.(name):=LambdaOrbOpts(s).(name);
    od;

    o:=Orb(s, LambdaOrbSeed(s), LambdaAct(s), opts);
    grades:=Grades(o);
    
    for x in o do
      if grades[i]=rank_f and tester(x, rho_f) then
        for rho in rhos do
          g:=creator(lambda, rho)*inv(x, f);
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

InstallOtherMethod(MultiplicativeNeutralElement, "for an acting semigroup",
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

InstallOtherMethod(MultiplicativeNeutralElement, "for a partial perm semi",
[IsPartialPermSemigroup], One);

#

InstallOtherMethod(MultiplicativeZero, "for an acting semigroup",
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
  if pos<>fail then
    f:=EvaluateWord(GeneratorsOfSemigroup(s), 
     TraceSchreierTreeForward(o, pos));
  fi;

  # lambda orb is closed, find an element with minimum rank
  if not IsBound(f) then 
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
  local rank, o, pos, min, len, m, f, i, n;

  rank:=LambdaRank(s);
  o:=LambdaOrb(s);
  
  pos:=LookForInOrb(LambdaOrb(s), 
   function(o, x) return rank(x)=MinActionRank(s); end, 2);

  if pos=false then 
    min:=rank(o[2]); pos:=2; len:=Length(o);

    for i in [3..len] do 
      m:=rank(o[i]);
      if m<min then
        pos:=i; min:=m;
      fi;
    od;
  fi;

  f:=EvaluateWord(Generators(s), TraceSchreierTreeForward(o, pos));
  i:=Semigroup(Elements(GreensDClassOfElementNC(s, f)), rec(small:=true));

  SetIsSimpleSemigroup(i, true);
  return i; 
end);

#

InstallMethod(MinimalIdeal, "for a partial perm semi",
[IsPartialPermSemigroup],
function(s)
  local n, gens, max, bound, o, i, f, I;

  n:=ActionDegree(s);
  gens:=Generators(s);
  max:=Maximum(List(gens, ActionDegree));

  if max=n then
    bound:=2^n;
  else
    bound:=Sum([1..max], x-> Binomial(n, x));
  fi;

  o:=Orb(gens, DomainOfPartialPermCollection(s), OnIntegerSetsWithPP, 
    rec( schreier:=true,
         gradingfunc:=function(o, x) return Length(x); end,
         onlygrades:=[0..max],
         lookingfor:=function(o, x) return Length(x)=0; end));
  
  Enumerate(o, bound);

  if IsPosInt(PositionOfFound(o)) then
    i:=PositionOfFound(o);
  else
    i:=Position(Grades(o), Minimum(Grades(o)));
  fi;

  f:=EvaluateWord(gens, TraceSchreierTreeForward(o, i));
  I:=InverseSemigroup(Elements(GreensDClassOfElementNC(s, f)));
  SetIsGroupAsSemigroup(I, true);
  return I;
end);

#

InstallMethod(PrimitiveIdempotents, 
"for an acting semigroup with inverse op and generators",
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
function(s)
  local r;
  
  if MultiplicativeZero(s)=fail then 
    r:=Set(List(OrbSCC(LambdaOrb(s)), 
     x-> LambdaRank(s)(LambdaOrb(s)[x[1]])))[1];
  else
    r:=Set(List(OrbSCC(LambdaOrb(s)), 
     x-> LambdaRank(s)(LambdaOrb(s)[x[1]])))[2];
  fi;

  return Idempotents(s, r);
end);

#

InstallMethod(PrincipalFactor, "for a D-class", 
[IsGreensDClass], 
d-> Range(InjectionPrincipalFactor(d)));

#

InstallOtherMethod(SmallGeneratingSet, 
"for an acting semigroup with generators", 
[IsActingSemigroup and HasGeneratorsOfSemigroup],
s -> Generators(Semigroup(Generators(s), rec(small:=true))));

#

InstallOtherMethod(SmallGeneratingSet, 
"for an acting semigroup with inverse op and generators", 
[IsActingSemigroupWithInverseOp and HasGeneratorsOfSemigroup],
s -> Generators(InverseSemigroup(Generators(s), rec(small:=true))));

#

InstallOtherMethod(StructureDescription, "for an acting Brandt semigroup",
[IsActingSemigroup and IsBrandtSemigroup],
function(s)
  local x, d;
  
  x:=First(Generators(s), x-> x<>MultiplicativeZero(s));
  d:=GreensDClassOfElementNC(s, x);
  
  return Concatenation("B(", StructureDescription(GroupHClass(d)), ", ",
  String(NrRClasses(d)), ")");
end);

#

InstallOtherMethod(StructureDescription, 
"for an acting group as semigroup",
[IsActingSemigroup and IsGroupAsSemigroup],
s-> StructureDescription(Range(IsomorphismPermGroup(s))));

# some things to move...

# JDM expand so that this is really an isomorphism and that the range knows some
# of the properties that the domain does. 
# Also move this to partition.g*

InstallMethod(IsomorphismBipartitionSemigroup, 
"for a transformation semigroup with generators",
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
  
  return MappingByFunction(s, Semigroup(List(GeneratorsOfSemigroup(s),     
   AsBipartition)), AsBipartition, AsTransformation);
end);

# to binary.gi 

InstallOtherMethod(IsomorphismTransformationSemigroup, 
"for semigroup of binary relations",
[IsBinaryRelationSemigroup], 
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

#JDM this should be improved: inverse of the function is missing, and a similar
#approach as used in the method for IsomorphismTransformationSemigroup for a
#semigroup of binary relations should be used to reduce the number of points
#acted on. 

# to matrix.gi

InstallOtherMethod(IsomorphismTransformationSemigroup, "for a matrix semigroup",
[IsMatrixSemigroup], 
function(S)        
  local n, F, T;
  n:=Length(GeneratorsOfSemigroup(S)[1][1]);
  F:=BaseDomain(GeneratorsOfSemigroup(S)[1]);        
  T:=Semigroup(List(GeneratorsOfSemigroup(S), x-> 
   TransformationOp(x, Elements(F^n), OnRight)));        
  return MappingByFunction(S, T,
   x-> TransformationOp(x, Elements(F^Size(F)), OnRight));
end);

#


InstallMethod(IsomorphismTransformationMonoid, "for a transformation semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup],
function(s)

  if not IsMonoidAsSemigroup(s) then 
    Error( "Usage: the argument must be a transformation semigroup ",
    "satisfying IsMonoidAsSemigroup," );
    return;
  fi;

  return MappingByFunction(s, Monoid(Difference(Generators(s),
  [TransformationNC([1..DegreeOfTransformationSemigroup(s)])])), x-> x, x-> x);
end);

#

InstallOtherMethod(IsomorphismPermGroup, "for a transformation semigroup", 
[IsTransformationSemigroup and HasGeneratorsOfSemigroup],
function(s)
 
   if not IsGroupAsSemigroup(s)  then
     Error( "Usage: trans. semigroup satisfying IsGroupAsSemigroup,");
     return; 
   fi;
 
   return MappingByFunction(s, Group(List(Generators(s), AsPermutation)), 
    AsPermutation, x-> AsTransformation(x, ActionDegree(s)));
 end);

# move to pperm.gi, trans.gi, and bipartition.gi

InstallOtherMethod(IsomorphismPermGroup, 
"for a partial perm semigroup with generators", 
[IsPartialPermSemigroup and HasGeneratorsOfSemigroup],
function(s)
  if not IsGroupAsSemigroup(s)  then
    Error( "the semigroup is not a group,");
    return; 
  fi;
  return MagmaIsomorphismByFunctionsNC(s, 
   Group(List(Generators(s), AsPermutation)), 
   AsPermutation, x-> AsPartialPerm(x, DomainOfPartialPermCollection(s)));
end);


### Start of Summer School Stuff ###


InstallMethod(VagnerPrestonRepresentation, 
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)

  local gens, elts, out, dom, ran, x;

  gens:=Generators(S);
  elts:=Elements(S);
  out:=EmptyPlist(Length(gens));
  
  for x in gens do

    dom:=Set(elts*(x^-1));
    ran:=List(dom, y-> y*x);

    Add(out, PartialPermNC(List(dom, y-> Position(elts, y)), List(ran, y->
    Position(elts, y))));
    
  od;

  return InverseSemigroup(out);

end);

#

InstallMethod(SameMinorantsSubgroup, 
"for a group H-class of an inverse semigroup of partial perms",
[IsGroupHClass],
function(h)

	local e, F, f, out, i;

	e:=Representative(h);
	F:=[];
	
	# Find the minorants of e
	for f in Idempotents(ParentSemigroup(h)) do
		if NaturalLeqPP(f, e) and f<>e then
			Add(F, f);
    fi;
	od;

	h:=Elements(h);
	out:=[e];

	# Check which elements of He share the same minorants as e	
	for i in [2..Length(h)] do
		if ForAll(F, f-> NaturalLeqPP(f, h[i])) then 
    	Add(out, h[i]);
    fi;
	od;

	return out;

end);

#

InstallMethod(IsMajorantlyClosed, 
"for an inverse subsemigroup of partial permutations and an inverse subsemigroup",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermSemigroup and IsInverseSemigroup],
function(S, T)

  return IsMajorantlyClosed(S, Elements(T));

end);

#

InstallMethod(IsMajorantlyClosed, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermCollection],
function(S, T)

	local t;

  if not IsSubset(S,T) then
    Error("The second argument should be a subset of the first");
  else
    return IsMajorantlyClosedNC(S,T);
  fi;
	
	return;

end);

#

InstallMethod(IsMajorantlyClosedNC, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermCollection],
function(S, T)

  local t, iter, u, i;

	if Size(S) = Size(T) then
		return true;
	fi;
	
  i:=0;
  for t in T do
    iter:=Iterator(S);
    for u in iter do
      i:=i+1;
      if NaturalLeqPP(t, u) and not u in T then
        return false;
      fi;
    od;
  od;

  return true;

end);

#

InstallMethod(MajorantClosure, 
"for an inverse subsemigroup of partial permutations and an inverse subsemigroup",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermSemigroup and IsInverseSemigroup],
function(S, T)

  return MajorantClosure(S, Elements(T));;

end);

#

InstallMethod(MajorantClosure, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermCollection],
function(S, T)

	local t;

  if not IsSubset(S,T) then
    Error("The second argument should be a subset of the first");
  else
    return MajorantClosureNC(S,T);
  fi;
	
	return;

end);

#

InstallMethod(MajorantClosureNC, 
"for an inverse subsemigroup of partial permutations and a subset",
[IsPartialPermSemigroup and IsInverseSemigroup, IsPartialPermCollection],
function(S, T)

	local elts, n, out, t, i, val, ht, k;
	
	elts:=Elements(S);
	n:=Length(elts);
	out:=EmptyPlist(n);
	ht:=HTCreate(T[1]);
	k:=0;
	
	for t in T do
		HTAdd(ht, t, true);
  	Add(out, t);
  	k:=k+1;
 	od;

	for t in out do
		for i in [1..n] do
			if NaturalLeqPP(t, elts[i]) then
				val:=HTValue(ht, elts[i]);
				if val=fail then
					k:=k+1;
					Add(out, elts[i]);
					HTAdd(ht, elts[i], true);
					if k=Size(S) then
						return out;
					fi;
				fi;
			fi;
		od;
	od;
	
 return out;
 
end);

#

InstallMethod(RightCosetsOfInverseSemigroup, 
"for an inverse semigroup of partial permutations and an inverse subsemigroup",
[IsInverseSemigroup and IsPartialPermSemigroup, IsInverseSemigroup and IsPartialPermSemigroup],
function(S, T)

  local s, t, dupe, idem, elts, rep, usedreps, coset, out;
  
  if not IsSubset(S,T) then
    Error("The second argument should be a subsemigroup of the first");
    return;
  fi;
  if not IsMajorantlyClosed(S,T) then
  	Error("The second argument should be majorantly closed.");
  fi;

  elts:=Elements(T);
  idem:=Representative(MinimalIdeal(T));
  usedreps:=[];
  out:=[];
  
  for s in RClass(S, idem) do
    
    # Check if Ts is a duplicate coset
    dupe:=false;    
    for rep in [1..Length(usedreps)] do
      if s*usedreps[rep]^-1 in elts then
        dupe:=true;
        break;
      fi;
    od;
    	  	
    if dupe then continue; fi;	
    	
    Add(usedreps, s);
    	
    coset:=[];
    for t in elts do
      Add(coset, t*s);
    od;
    coset:=Set(coset);

    # Generate the majorant closure of Ts to create the coset

    coset:=MajorantClosure(S, coset);
      
    Add(out, coset);

  od;

  return out;

end);

#

InstallMethod(IsJoinIrreducible, 
"for an inverse semigroup of partial perms and a partial perm",
[IsInverseSemigroup and IsPartialPermSemigroup, IsPartialPerm],
function(S, x)

  local elts, i, j, k, y, singleline, minorants, minorantpoints;

  if not x in S then
		Error("The second argument should be a partial permutation within the first argument");
  fi;
  if IsMultiplicativeZero(S,x) then return false; fi;

  y:=LeftOne(x);
    
  elts:=Set(Idempotents(S));;
  i:=Position(elts, y);
  k:=0;
  singleline:=true;

  for j in [i-1,i-2 .. 1] do
    if NaturalLeqPP(elts[j], elts[i]) then
      k:=j;
      break;
    fi;
  od;
  
  if k = 0 then return true; fi;

  for j in [1..(k-1)] do 
    if NaturalLeqPP(elts[j], elts[i]) and not NaturalLeqPP(elts[j], elts[k]) then 
      singleline:=false; 
      break;
    fi;
  od;

  if singleline then return true; fi;

  if Size(HClass(S, y)) = 1 then return false; fi;
		
  minorants:=[];  
  for j in [1..k] do 
    if NaturalLeqPP(elts[j], elts[i]) then 
      Add(minorants, elts[j]); 
    fi;
  od;
    
  minorantpoints:=Union(List(minorants, f -> DomPP(f)));
  if DomPP(y) = minorantpoints then return false; fi;

  # Check if any other element in Hy has equal minorants
  for j in HClass(S, y) do
    if not j = y and ForAll(minorants, m -> NaturalLeqPP(m, j)) then
      return true;
    fi; 
  od;
  
  return false;

end);

#

InstallMethod(JoinIrreducibleDClasses, 
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)

  local elts, i, j, k, y, singleline, minorants, minorantpoints, D, out, d;
  
  D:=DClasses(S);
  elts:=Set(Idempotents(S));;
  out:=[];

  for d in D do
  
    y:=Representative(d);
    
    if IsMultiplicativeZero(S,y) then continue; fi;
    
    i:=Position(elts, y);
    k:=0;
    singleline:=true;

    for j in [i-1,i-2 .. 1] do
      if NaturalLeqPP(elts[j], elts[i]) then
        k:=j;
        break;
      fi;
    od;
  
    if k = 0 then Add(out,d); continue; fi;

    for j in [1..(k-1)] do 
      if NaturalLeqPP(elts[j], elts[i]) and not NaturalLeqPP(elts[j], elts[k]) then 
        singleline:=false; 
        break;
      fi;
    od;

    if singleline then Add(out,d); continue; fi;

    if Size(HClass(S, y)) = 1 then continue; fi;
		
    minorants:=[];  
    for j in [1..k] do 
      if NaturalLeqPP(elts[j], elts[i]) then 
        Add(minorants, elts[j]); 
      fi;
    od;
    
    minorantpoints:=Union(List(minorants, f -> DomPP(f)));
    if DomPP(y) = minorantpoints then continue; fi;

    for j in HClass(S, y) do
      if not j = y and ForAll(minorants, m -> NaturalLeqPP(m, j)) then
        Add(out,d); break;
      fi; 
    od;
  
  od;

  return out;

end);

#

InstallMethod(SmallerDegreePartialPermRepresentation, 
"for an inverse semigroup of partial permutations",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(S)

  local out, D, T, e, h, i, j, k, m, lookup, box, subbox,
        Fei, He, Se, sigma, sigmainv, FeiSigma, HeSigma, rho, rhoinv, HeSigmaRho,
        oldgens, newgens, gen, offset,
        orbits, cosets, HeCosetReps, HeCosetRepsSigma, AllCosetReps, rep, numcosets, CosetsInHe, trivialse,
        SmallerDegreeElementMap;
        
  out:=[];
  oldgens:=Generators(S);
  newgens:=[];
  for i in [1..Length(oldgens)] do newgens[i]:=[]; od;
  
  D:=JoinIrreducibleDClasses(S);

  for e in D do
				
    ##### Calculate He as a small permutation group #####
    He:=GroupHClass(e);
    trivialse:=Length(SameMinorantsSubgroup(He))=1;
    # Uncomment out this line when issue 18 is fixed
    #He:=InverseSemigroup(Elements(He), rec(small:=true));
    He:=InverseSemigroup(Elements(He));

    sigma:=IsomorphismPermGroup(He);
    sigmainv:=InverseGeneralMapping(sigma);
    HeSigma:=Range(sigma);

    rho:=SmallerDegreePermutationRepresentation(HeSigma);
    rhoinv:=InverseGeneralMapping(rho);
    HeSigmaRho:=Range(rho);   	

    # If Se is trivial, we have a special simpler case    
    if trivialse then
      orbits:=[[ActionDegree(He)+1]];
      HeCosetReps:=[Representative(e)];
      Fei:=He;
    else 
      orbits:=Orbits(HeSigmaRho);
    fi;

    for i in orbits do
    
      if not trivialse then

        # Generate Fei
        FeiSigma:=ImagesSet(rhoinv, Stabiliser(HeSigmaRho, i[1]));
        Fei:=ImagesSet(sigmainv, FeiSigma);

        # Generate reps for the cosets of Fei in He
        HeCosetRepsSigma:=RightTransversal(HeSigma, FeiSigma);
        HeCosetReps:=[];
        for j in [1..Size(HeCosetRepsSigma)] do
      	  Add(HeCosetReps, HeCosetRepsSigma[j]^sigmainv);
        od;
      
      fi; 

      # Generate reps for the HClasses in the RClass of e
      h:=HClassReps( RClassNC(e, Representative(e)) );
      CosetsInHe:=Length(HeCosetReps);
      numcosets:=Size(h)*CosetsInHe;
      
      # Generate reps for ALL the cosets that the generator will act on      
      j:=0;
      AllCosetReps:=[];
      lookup:=EmptyPlist(Length(LambdaOrb(e)));
      for k in [1..Size(h)] do
        lookup[Position(LambdaOrb(e), RanSetPP(h[k]))]:= k;
        for m in [1..Length(HeCosetReps)] do
          j:=j+1;
          AllCosetReps[j]:=HeCosetReps[m]*h[k];
        od;
      od;
			
      # Loop over the old generators of S to find action on cosets
      for j in [1..Length(oldgens)] do

        gen:=oldgens[j];
        offset:=Length(newgens[j]);

        # Loop over cosets to calculate image of each under gen
        for k in [1..numcosets] do

          rep:=AllCosetReps[k]*gen;

          # Will the new generator will be defined at this point?
          if not rep*rep^(-1) in Fei then
            Add(newgens[j], 0);
          else
            box:=lookup[Position(LambdaOrb(e), RanSetPP(rep))];
            if trivialse then
              subbox:=1;
            else
              subbox:=PositionCanonical(HeCosetRepsSigma, (rep*h[box]^(-1))^sigma);
            fi;
            Add(newgens[j], (box-1)*CosetsInHe+subbox+offset);  
          fi;

        od;									        
      od; 
    od;        
  od;
    
  newgens:=List(newgens, x->PartialPermNC(x));  
  T:=InverseSemigroup(newgens);
  oldgens:=GeneratorsOfSemigroup(S);
  newgens:=GeneratorsOfSemigroup(T);
  
	#Check whether work has actually been done
  if NrMovedPoints(T) > NrMovedPoints(S) or (NrMovedPoints(T) = NrMovedPoints(S) and ActionDegree(T) >= ActionDegree(S)) then
    
    return MagmaIsomorphismByFunctionsNC(S, S, x -> x, x -> x);
    
  else
    
    return MagmaIsomorphismByFunctionsNC(
      S,
      T,
      x -> ResultOfStraightLineProgram(SemigroupElementSLP(S, x), newgens),
      x -> ResultOfStraightLineProgram(SemigroupElementSLP(T, x), oldgens)
    );
    
  fi;
  
end);

#

#EOF